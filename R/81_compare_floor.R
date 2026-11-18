## 81_compare_floor.R
## What the per-capita floor on logged extensive covariates (flog, 40_multiverse.R,
## 2026-11-15) changed: run v5 (slog = log(x + 1)) vs run v6 (flog), same specs.
##   1. same-spec agreement of partial r by term x unit x predictor form
##   2. the five-benchmark summary (median partial r, same sign, significant) in
##      v5 and v6, default covariate set, as in 58_outline_analyses.R part S
## Output: D:/MULTIVERSE/data/spec/compare_v5_v6_*.csv
## Usage: Rscript R/81_compare_floor.R [old_run] [new_run]

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
a <- commandArgs( TRUE )
OLD <- if ( length( a ) >= 1 ) a[1] else "run_principled_v5"
NEW <- if ( length( a ) >= 2 ) a[2] else "run_principled_v6"
FORK_DIMS <- c( "construct", "hu", "ident", "outcome", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year", "outlier", "drop", "op" )

load_run <- function( dir ) {
  s <- as.data.table( read_parquet( file.path( OUT$spec, dir, "specs.parquet" ) ) )
  co <- as.data.table( read_parquet( file.path( OUT$spec, dir, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co <- merge( co, s[ , c( "spec_id", FORK_DIMS ), with = FALSE ], by = "spec_id" )
  ## partial r with df counted in metros (as in 58_outline_analyses.R)
  co[ , pr := stat / sqrt( stat^2 + pmax( n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) - k - 1, 1 ) ) ]
  co[ term != "logP" ]
}
v0 <- load_run( OLD ); v1 <- load_run( NEW )
key <- c( FORK_DIMS, "term" )
m <- merge( v0[ , c( key, "pr", "stat" ), with = FALSE ], v1[ , c( key, "pr", "stat" ), with = FALSE ], by = key, suffixes = c( "_old", "_new" ) )
log_msg( format( nrow( m ), big.mark = "," ), " matched coefficient rows" )

## 1. agreement by term x unit x predictor form
TYPEB <- c( "pf_assets", "n_pf", "contrib_total", "gov_earn_sl", "gov_emp_sl" )
m[ , tgroup := fifelse( term %in% c( "pf_assets", "n_pf" ), term, fifelse( term %in% TYPEB, "other extensive", "intensive" ) ) ]
ag <- m[ , .( rows = .N, cor_r = cor( pr_old, pr_new ), mean_abs_diff = mean( abs( pr_new - pr_old ) ),
              mean_diff = mean( pr_new - pr_old ), sign_flips = mean( sign( pr_old ) != sign( pr_new ) ),
              sig_old = mean( abs( stat_old ) > 1.96 ), sig_new = mean( abs( stat_new ) > 1.96 ) ),
         keyby = .( tgroup, unit, pred_form ) ]
print( ag[ , lapply( .SD, function( v ) if ( is.numeric( v ) ) round( v, 3 ) else v ) ], nrows = 200 )
fwrite( ag, file.path( OUT$spec, "compare_v5_v6_agreement.csv" ) )

## 2. benchmark summary, default covariate set (part S definition)
BEN <- c( "pct_ba", "pf_assets", "turnout", "pct_nonwhite", "poverty_rate" )
bench <- function( v, lab ) {
  x <- v[ op == "default" & drop == "none" & !( ident == "pooled" & est == "state_re" ) & term %in% BEN &
          ( ( construct == "count" & hu == "incl" ) | ( construct == "exp" & hu == "excl" ) ) ]
  x[ , { md <- median( pr ); .( median = md, same_sign = mean( sign( pr ) == sign( md ) ), sig = mean( abs( stat ) > 1.96 ), specs = .N ) },
     by = .( construct, term ) ][ , run := lab ]
}
b <- rbind( bench( v0, OLD ), bench( v1, NEW ) )
bw <- dcast( b, construct + term ~ run, value.var = c( "median", "same_sign", "sig" ) )
print( bw[ , lapply( .SD, function( v ) if ( is.numeric( v ) ) round( v, 3 ) else v ) ] )
fwrite( b, file.path( OUT$spec, "compare_v5_v6_benchmarks.csv" ) )

## where did foundation assets move most? by fork, default set
fa <- m[ term == "pf_assets" & op == "default" & drop == "none" & !( ident == "pooled" & est == "state_re" ) ]
by_fork <- rbindlist( lapply( c( "unit", "pred_form", "outlier", "ident", "construct" ), function( f )
  fa[ , .( fork = f, median_old = median( pr_old ), median_new = median( pr_new ), sig_old = mean( abs( stat_old ) > 1.96 ),
           sig_new = mean( abs( stat_new ) > 1.96 ) ), by = .( option = as.character( get( f ) ) ) ] ) )
print( by_fork[ , lapply( .SD, function( v ) if ( is.numeric( v ) ) round( v, 3 ) else v ) ] )
fwrite( by_fork, file.path( OUT$spec, "compare_v5_v6_pf_by_fork.csv" ) )
