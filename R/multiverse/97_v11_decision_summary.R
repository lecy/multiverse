## 97_v11_decision_summary.R
## Multiverse evidence for the design-decision cards (paper/04-design-decisions.qmd) on run v11,
## the lean-default multiverse (MV_MODEL=lean in 40_multiverse.R: 13 covariates, 432,432 specs).
## Mirrors 58_outline_analyses.R part A (decision importance) with the lean model's decisions:
## measurement swaps replace the old D7 and add-on blocks; dropping the endowment (history) and
## the growth kernel are the D8 contrasts.
##
## Writes D:/MULTIVERSE/data/outline/v11/mv/:
##   decision_importance{,_long}.csv   eta^2 of each decision x covariate, counts and spending
##   option_medians.csv                median partial r by option of each decision, default model
##   stability.csv                     median, sign agreement, share significant (robust / state-clustered)
##   drop_shift.csv                    matched default vs drop-one-block pairs: median change in partial r
##   est_spearman.csv                  Spearman of covariate t across estimators, matched on other forks
##   v10_v11_benchmarks.csv            the five benchmarks' default-model medians, v10 vs v11
## Run from multiverse/ (single process; reads ~6M coefficient rows):
##   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/multiverse/97_v11_decision_summary.R > D:/MULTIVERSE/logs/97.log 2>&1

## MV_SPLIT=1: report filers, grassroots and spending as three groups instead of pooling filers with the
## registered stock as "counts". Grassroots specs come from R/notes/104_fit_grassroots_v11.R (every registered-
## stock spec refit with n_bmf_c3pc - n_filer_c3); outputs go to v11/mv_split/. D1 for the two count
## groups is then filers vs grassroots.
## The computation is npdensity::mv_decision_summary(); this script reads the runs and writes the
## tables. The lean terms, their blocks and the measurement swaps are npdensity::MV_V11_TERMS,
## MV_V11_BLOCK_OF and MV_V11_SWAP_TERM.

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ) } )
SPLIT <- Sys.getenv( "MV_SPLIT" ) == "1"
RUN <- file.path( OUT$spec, "run_principled_v11" )
OD  <- file.path( BUILD, "data", "outline", "v11", if ( SPLIT ) "mv_split" else "mv" )
dir.create( OD, recursive = TRUE, showWarnings = FALSE )
rp <- function( dir, f ) as.data.table( read_parquet( file.path( dir, f ) ) )

log_msg( "read v11" )
specs <- rp( RUN, "specs.parquet" )[ status == "ok" ]
co <- rp( RUN, "coefs.parquet" )
if ( SPLIT ) {
  GRD <- file.path( OUT$spec, "run_principled_v11_grass" )
  specs <- rbind( specs, rp( GRD, "specs.parquet" )[ status == "ok" ], fill = TRUE )
  co <- rbind( co, rp( GRD, "coefs.parquet" ), fill = TRUE )
}
V10 <- file.path( OUT$spec, "run_principled_v10" )
options( width = 250 )
R <- mv_decision_summary( specs, co, rp( V10, "specs.parquet" ), rp( V10, "coefs.parquet" ), split = SPLIT )
for ( nm in names( R ) ) fwrite( R[[ nm ]], file.path( OD, paste0( nm, ".csv" ) ) )
log_msg( "done" )
