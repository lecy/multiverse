## 83_sample_exclusions.R
## Are sample exclusions worth a multiverse fork? (Data Construction Plan 9.6, forks
## P1 population threshold and P2 exclusions; never run in v1-v7.)
##
## Design. A sub-universe of default-model specs from the current principled grid
## (full model, default measures, no winsorizing, no algebraic duplicates):
##   outcomes   filers (n_filer_c3), registered stock (n_bmf_c3pc), spending excluding
##              hospitals and universities (exp_c3_nohu)
##   ident      cs 2022, pooled 2015-22, within 2010-22
##   forks      every DV form, predictor form, log P on the right, unit, estimator
## Each spec is re-fit on these samples (a CBSA is dropped from every year):
##   none        all CBSAs in the unit (the multiverse as run)
##   pop50k      2022 population >= 50k (P1; changes only the metro + micro unit)
##   pop100k     2022 population >= 100k (P1)
##   pop250k     2022 population >= 250k (stricter than the plan: about half the metros)
##   no_dc       drop Washington DC metro (47900)
##   no_akhi     drop Alaska and Hawaii CBSAs
##   trim_size   drop the 5 largest and 20 smallest CBSAs in the unit (SAMI note, Rec. 7)
##   no_groupex  drop the two group-exemption artifacts flagged in 55 (Memphis 32820,
##               Mankato 31860: >60% of the 2000 stock predates 1970)
##   trim_x4     drop CBSAs with any default covariate beyond |z| > 4 in 2022 (extensive
##               covariates per capita and logged with the floor; a covariate trim, not a
##               trim on the outcome)
##   combined    no_dc + no_akhi + no_groupex + trim_size
## Benchmark: winsor1, the same spec with 1% winsorizing on the full sample. Winsorizing
## is already a fork and one of the decisions that "rarely matter" (eta^2 <= .10).
##
## Metrics per benchmark covariate (partial r with df counted in places, as in 58 part A):
##   paired: median and 90th percentile |delta r| vs none, share of sign flips, share whose
##           5% significance changes
##   eta^2 of the sample fork over the default specs, computed like the decision
##           importance table (within ident x outcome, then averaged), with and without
##           the winsor1 arm
## Outputs D:/MULTIVERSE/data/sample_excl/: specs.csv, coefs.parquet, paired.csv,
##   summary.csv, eta2.csv, dropped_units.csv
## Usage: Rscript R/83_sample_exclusions.R [workers]   (default 8)

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )
WORKERS <- if ( length( commandArgs( TRUE ) ) ) as.integer( commandArgs( TRUE )[1] ) else 8L
## with MV_MODEL=lean (the 13-covariate default of run v11) results go to sample_excl_v11/ and every
## default covariate is summarized, not only the five benchmarks
OD <- file.path( BUILD, "data", if ( MV_MODEL == "lean" ) "sample_excl_v11" else "sample_excl" ); dir.create( OD, showWarnings = FALSE )

BENCH <- if ( MV_MODEL == "lean" ) unlist( lapply( BLOCKS, `[[`, "default" ), use.names = FALSE ) else
  c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate" )
OUTS  <- c( "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu" )
GROUPEX <- c( "32820", "31860" )

## ---- specs ---------------------------------------------------------------------------
g <- build_grid( "principled" )
sp <- g[ drop == "none" & op == "default" & outlier == "none" & is.na( dup_of ) &
         outcome %in% OUTS & cs_year == 2022L ]
## winsor1 twins of the same specs (benchmark arm, full sample)
key <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit", "est" )
sw <- merge( g[ drop == "none" & op == "default" & outlier == "winsor1" & cs_year == 2022L & outcome %in% OUTS,
                c( key, "spec_id" ), with = FALSE ],
             sp[ , c( key, "spec_id" ), with = FALSE ], by = key, suffixes = c( "_w", "" ) )
log_msg( "base specs: ", nrow( sp ), "; winsor twins: ", nrow( sw ) )

## ---- samples -------------------------------------------------------------------------
panel <- load_panel()
p22 <- panel[ year == 2022 ]
## covariate trim: |z| > 4 on any default covariate in 2022, by unit
dflt <- covars_for( list( drop = "none", op = "default", op_block = NA_character_ ) )
zflag <- function( d ) {
  bad <- rep( FALSE, nrow( d ) )
  for ( v in dflt ) {
    x <- d[[ v ]]
    if ( COVS[ var == v, type ] == "B" ) x <- flog( x, d$population ) - log( d$population )
    z <- ( x - mean( x, na.rm = TRUE ) ) / sd( x, na.rm = TRUE )
    bad <- bad | ( is.finite( z ) & abs( z ) > 4 )
  }
  d$cbsa[ bad ]
}
drops <- list()
for ( u in c( "metro", "cbsa" ) ) {
  d <- if ( u == "metro" ) p22[ cbsa_type == "metro" ] else p22
  ord <- d[ order( -population ), cbsa ]
  trim_size <- c( head( ord, 5 ), tail( ord, 20 ) )
  dd <- list(
    none = character( 0 ),
    pop50k = d[ population < 5e4, cbsa ], pop100k = d[ population < 1e5, cbsa ],
    pop250k = d[ population < 2.5e5, cbsa ],
    no_dc = intersect( "47900", d$cbsa ), no_akhi = d[ state_fips %in% c( "02", "15" ), cbsa ],
    trim_size = trim_size, no_groupex = intersect( GROUPEX, d$cbsa ), trim_x4 = zflag( d ) )
  dd$combined <- unique( c( dd$no_dc, dd$no_akhi, dd$no_groupex, dd$trim_size ) )
  drops[[ u ]] <- dd
}
du <- rbindlist( lapply( names( drops ), function( u ) rbindlist( lapply( names( drops[[ u ]] ), function( s )
  data.table( unit = u, sample = s, cbsa = drops[[ u ]][[ s ]] ) ) ) ) )
du <- merge( du, p22[ , .( cbsa, cbsa_title, population ) ], by = "cbsa", all.x = TRUE )
fwrite( du[ order( unit, sample, -population ) ], file.path( OD, "dropped_units.csv" ) )
print( du[ , .N, by = .( unit, sample ) ] )
print( du[ sample == "trim_x4" ] )

SAMPLES <- names( drops$metro )
jobs <- rbind( CJ( spec_id = sp$spec_id, sample = SAMPLES ),
               data.table( spec_id = sw$spec_id_w, sample = "winsor1" ) )
jobs[ , job := .I ]
log_msg( "fits to run: ", nrow( jobs ) )

## ---- fit -----------------------------------------------------------------------------
run_job <- function( i ) {
  j <- jobs[ i ]
  s <- g[ spec_id == j$spec_id ]
  pdat <- if ( j$sample %in% c( "none", "winsor1" ) ) panel else panel[ !cbsa %in% drops[[ s$unit ]][[ j$sample ]] ]
  r <- tryCatch( run_spec( s, pdat ), error = function( e ) NULL )
  if ( is.null( r ) ) return( NULL )
  r[ , `:=`( job = j$job, sample = j$sample ) ]
  r[]
}
t0 <- proc.time()
cl <- makePSOCKcluster( WORKERS )
invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
  setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
  source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
clusterExport( cl, c( "panel", "g", "jobs", "drops", "run_job" ) )
res <- rbindlist( parLapplyLB( cl, seq_len( nrow( jobs ) ), run_job, chunk.size = 50 ), fill = TRUE )
stopCluster( cl )
log_msg( "fits finished in ", round( ( proc.time() - t0 )[[ "elapsed" ]] / 60, 1 ), " min; rows ", nrow( res ) )

## winsor twins are reported under the base spec_id
res[ sample == "winsor1", spec_id := sw$spec_id[ match( spec_id, sw$spec_id_w ) ] ]
res <- merge( res, res[ , .( k = .N ), by = job ], by = "job" )
res <- merge( res, sp[ , .( spec_id, construct, hu, outcome, ident, dv_form, pred_form, rhs_logP, unit, est ) ], by = "spec_id" )
res <- res[ !( ident == "pooled" & est == "state_re" ) ]
res[ , n_units := n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) ]
res[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
res[ , sig := p < 0.05 ]
write_parquet( res, file.path( OD, "coefs.parquet" ) )
fwrite( sp, file.path( OD, "specs.csv" ) )

## ---- paired comparison with the full sample ------------------------------------------
b <- res[ term %in% BENCH ]
base <- b[ sample == "none", .( spec_id, term, pr0 = pr, sig0 = sig ) ]
pp <- merge( b[ sample != "none" ], base, by = c( "spec_id", "term" ) )
pp[ , grp := fifelse( construct == "count", fifelse( outcome == "n_bmf_c3pc", "registered", "filers" ), "spending" ) ]
summ <- pp[ , .( specs = .N, median_abs_dr = median( abs( pr - pr0 ) ), p90_abs_dr = quantile( abs( pr - pr0 ), .9 ),
                 max_abs_dr = max( abs( pr - pr0 ) ), median_dr = median( pr - pr0 ),
                 sign_flip = mean( sign( pr ) != sign( pr0 ) & pmax( abs( pr ), abs( pr0 ) ) > .05 ),
                 sig_change = mean( sig != sig0 ), cor_r = cor( pr, pr0 ) ),
            by = .( grp, sample, term ) ]
fwrite( pp, file.path( OD, "paired.csv" ) )
fwrite( summ, file.path( OD, "summary.csv" ) )

## overall by sample (all benchmarks and groups), cs and pooled only
ov <- pp[ ident != "within", .( median_abs_dr = median( abs( pr - pr0 ) ), p90_abs_dr = quantile( abs( pr - pr0 ), .9 ),
                                sign_flip = mean( sign( pr ) != sign( pr0 ) & pmax( abs( pr ), abs( pr0 ) ) > .05 ),
                                sig_change = mean( sig != sig0 ) ), by = sample ][ order( -median_abs_dr ) ]
fwrite( ov, file.path( OD, "summary_overall.csv" ) )

## ---- eta^2 of the sample fork (decision-table definition) ------------------------------
eta2 <- function( y, g ) { g <- as.factor( g ); if ( nlevels( g ) < 2 || length( y ) < 10 ) return( NA_real_ )
  m <- tapply( y, g, mean ); n <- tapply( y, g, length )
  sum( n * ( m - mean( y ) )^2, na.rm = TRUE ) / sum( ( y - mean( y ) )^2 ) }
b[ , grp := fifelse( construct == "count", "counts", "spending" ) ]
et <- rbindlist( lapply( c( "with winsor1 arm", "exclusions only" ), function( arm ) {
  x <- if ( arm == "exclusions only" ) b[ sample != "winsor1" ] else b
  x <- x[ ident != "within" ]
  e <- x[ , .( e = eta2( pr, sample ) ), by = .( grp, term, ident, outcome ) ]
  e[ , .( eta2 = mean( e, na.rm = TRUE ) ), by = .( grp, term ) ][ , arm := arm ] } ) )
## each exclusion alone vs the full sample (a two-option fork)
et1 <- rbindlist( lapply( setdiff( c( SAMPLES, "winsor1" ), "none" ), function( smp ) {
  x <- b[ sample %in% c( "none", smp ) & ident != "within" ]
  e <- x[ , .( e = eta2( pr, sample ) ), by = .( grp, term, ident, outcome ) ]
  e[ , .( eta2 = mean( e, na.rm = TRUE ) ), by = .( grp, term ) ][ , arm := smp ] } ) )
et <- rbind( et, et1 )
fwrite( dcast( et, arm + grp ~ term, value.var = "eta2" ), file.path( OD, "eta2.csv" ) )

cat( "\n== Overall shift vs full sample (cs + pooled, five benchmarks) ==\n" ); print( ov, digits = 3 )
cat( "\n== eta^2 of the sample fork ==\n" ); print( dcast( et, arm + grp ~ term, value.var = "eta2" ), digits = 2 )
cat( "\n== Largest median shifts by group/term ==\n" ); print( summ[ order( -median_abs_dr ) ][ 1:25 ], digits = 3 )
log_msg( "done" )
