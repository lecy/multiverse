## 45_summarize_run.R
## First-pass results for a completed universe run (design doc section 5).
##   Goal 1 - which covariates are specification-sensitive:
##            sign stability, share significant, IQR of standardized beta,
##            variance decomposition across fork dimensions (marginal eta^2)
##   Goal 2 - how much results depend on DV form:
##            median standardized beta by DV form; Spearman correlation of
##            t-statistics across DV forms; meta-hypothesis: does a covariate's
##            R^2 on log P predict its DV-form sensitivity? (and, since
##            2026-11-15, its predictor-form sensitivity)
## Usage: Rscript R/multiverse/45_summarize_run.R [principled|kitchen|principled_v7 ...]
##   The argument names the run folder data/spec/run_<arg>/; outputs go there.
## 2026-11-15 conventions, aligned with 58_outline_analyses.R part A so the two agree:
##   - partial r counts degrees of freedom in independent places, not place-years
##     (pooled = 8 years, within = 13), because panel SEs are clustered by metro
##   - pooled state random-intercept fits are left out (model-based SEs treat
##     metro-years as independent, so their t is not comparable)
##   - size dependence uses flog() (per-capita floor) for extensive covariates,
##     as the models do from run v6 on
## The computation is npdensity::mv_summarize(); this script reads the run and writes the tables.

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( library( arrow ) )

UNIVERSE <- if ( length( commandArgs( TRUE ) ) ) commandArgs( TRUE )[1] else "principled"
run_dir <- file.path( OUT$spec, paste0( "run_", UNIVERSE ) )
specs <- as.data.table( read_parquet( file.path( run_dir, "specs.parquet" ) ) )
co    <- as.data.table( read_parquet( file.path( run_dir, "coefs.parquet" ) ) )
FORK_DIMS <- c( "outcome", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year",
                "outlier", "drop", "op" )

S <- mv_summarize( specs, co, load_panel(), mv_cfg() )
rm( co ); gc()
for ( nm in names( S ) ) fwrite( S[[ nm ]], file.path( run_dir, paste0( "summary_", nm, ".csv" ) ) )
stab <- S$stability; infl <- S$fork_influence; meta_test <- S$meta_test

## ---- console headline ----------------------------------------------------------------

cat( "\n== Stability (count / incl / cs), default-measure covariates ==\n" )
print( stab[ stratum == "count/incl/cs" & term %in% c( "pct_nonwhite", "race_hhi", "pf_assets", "turnout", "gov_earn_sl",
                                                      "poverty_rate", "pct_ba" ),
             .( term, specs, median_r = round( median_partial_r, 3 ), iqr_r = round( iqr_std_beta, 3 ),
                sign_stab = round( sign_stability, 3 ), sig05 = round( share_sig05, 3 ) ) ] )
cat( "\n== Stability (exp / excl / cs) ==\n" )
print( stab[ stratum == "exp/excl/cs" & term %in% c( "pct_nonwhite", "race_hhi", "pf_assets", "turnout", "gov_earn_sl",
                                                    "poverty_rate", "pct_ba" ),
             .( term, specs, median_r = round( median_partial_r, 3 ), iqr_r = round( iqr_std_beta, 3 ),
                sign_stab = round( sign_stability, 3 ), sig05 = round( share_sig05, 3 ) ) ] )
cat( "\n== Dominant fork per covariate (count / incl / cs) ==\n" )
ii <- infl[ stratum == "count/incl/cs" ]
ii[ , top_fork := FORK_DIMS[ apply( as.matrix( .SD ), 1, which.max ) ], .SDcols = FORK_DIMS ]
ii[ , top_eta2 := apply( as.matrix( .SD ), 1, max, na.rm = TRUE ), .SDcols = FORK_DIMS ]
print( ii[ , .( term, top_fork, top_eta2 = round( top_eta2, 2 ), dv_form = round( dv_form, 2 ),
                pred_form = round( pred_form, 2 ) ) ][ order( -top_eta2 ) ] )
cat( "\n== Meta-hypothesis: Spearman(R2 on log P, DV-form sensitivity) ==\n" )
print( meta_test )
