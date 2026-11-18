## 46_compare_runs.R
## Compare the expanded principled run (v2: rebuilt data + resource-measure
## fork) with the first run (v1, run_principled_v1).
##   1. identical specifications in both runs (same outcome and forks): how
##      much did the data rebuild (BMF stock fix, 2023-24 counts) move them?
##   2. resource construct: median partial r and sign stability by measure
##      (expenses / revenue x gross / net), and the eta^2 of the measure fork
## Output: D:/MULTIVERSE/data/spec/compare_v1_v2*.csv

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
FORK_DIMS <- c( "outcome", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year", "outlier", "drop", "op" )
load_run <- function( dir ) {
  s <- as.data.table( read_parquet( file.path( OUT$spec, dir, "specs.parquet" ) ) )
  co <- as.data.table( read_parquet( file.path( OUT$spec, dir, "coefs.parquet" ) ) )[ term != "logP" ]
  k <- co[ , .( k = .N ), by = spec_id ]
  co <- merge( co, k, by = "spec_id" )
  co[ , r := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
  merge( co[ , .( spec_id, term, r, p, n ) ], s[ , c( "spec_id", "construct", "hu", "ident", FORK_DIMS ), with = FALSE ], by = "spec_id" )
}
v1 <- load_run( "run_principled_v1" ); v2 <- load_run( "run_principled" )
key <- c( "construct", "hu", "ident", FORK_DIMS, "term" )
m <- merge( v1[ , c( key, "r", "n" ), with = FALSE ], v2[ , c( key, "r", "n" ), with = FALSE ], by = key, suffixes = c( "_v1", "_v2" ) )
cmp <- m[ , .( pairs = .N, cor_r = cor( r_v1, r_v2 ), mean_abs_diff = mean( abs( r_v2 - r_v1 ) ),
               sign_flips = mean( sign( r_v1 ) != sign( r_v2 ) ), n_changed = mean( n_v1 != n_v2 ) ),
          keyby = .( outcome, ident ) ]
print( cmp, digits = 3 )
fwrite( cmp, file.path( OUT$spec, "compare_v1_v2_same_specs.csv" ) )

rs <- v2[ construct == "exp" ]
rs[ , measure := fcase( grepl( "^exp.*_net$", outcome ), "expenses, net", grepl( "^exp", outcome ), "expenses, gross",
                        grepl( "^rev.*_net$", outcome ), "revenue, net", default = "revenue, gross" ) ]
tab <- rs[ , { med <- median( r ); .( specs = .N, median_r = med, sign_stability = mean( sign( r ) == sign( med ) ),
                                      share_sig = mean( p < .05, na.rm = TRUE ) ) },
           keyby = .( hu, ident, term, measure ) ]
fwrite( tab, file.path( OUT$spec, "compare_resource_measures.csv" ) )
eta2 <- function( y, g ) { g <- as.factor( g ); m <- tapply( y, g, mean ); n <- tapply( y, g, length )
  sum( n * ( m - mean( y ) )^2 ) / sum( ( y - mean( y ) )^2 ) }
fe <- rs[ , .( eta2_measure = eta2( r, measure ), eta2_rev_vs_exp = eta2( r, sub( ",.*", "", measure ) ),
               eta2_net_vs_gross = eta2( r, sub( ".*, ", "", measure ) ), eta2_dv_form = eta2( r, dv_form ),
               eta2_pred_form = eta2( r, pred_form ), eta2_est = eta2( r, est ) ), keyby = .( hu, ident, term ) ]
fwrite( fe, file.path( OUT$spec, "compare_resource_measure_eta2.csv" ) )
cat( "\n== resource construct, cross-section, default covariates ==\n" )
print( dcast( tab[ ident == "cs" & term %in% c( "pct_nonwhite", "race_hhi", "pf_assets", "turnout", "gov_earn_sl", "poverty_rate", "pct_ba" ) ],
              hu + term ~ measure, value.var = "median_r" ), digits = 3 )
cat( "\n== eta^2 of the measure fork vs other forks (cs) ==\n" )
print( fe[ ident == "cs" & term %in% c( "pct_nonwhite", "race_hhi", "pf_assets", "turnout", "gov_earn_sl", "poverty_rate", "pct_ba" ) ], digits = 2 )
cat( "\n== mean eta^2 across covariates, by stratum ==\n" )
print( fe[ , lapply( .SD, mean ), keyby = .( hu, ident ), .SDcols = patterns( "^eta2" ) ], digits = 2 )
