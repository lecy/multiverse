## 44_run_universe.R
## Run a multiverse universe in resumable batches.
##   Rscript R/multiverse/44_run_universe.R [principled|kitchen] [batch_size] [workers] [run_name]
## Each worker writes its batch to BUILD/data/spec/run_<run_name>/batches/; finished
## batches are skipped on re-run. At the end the batches are combined:
##   specs.parquet  one row per specification (fork settings + status)
##   coefs.parquet  one row per specification x term
##   run_summary.csv
## The model comes from MV_MODEL (lean = run v11). The runner is
## npdensity::mv_run_universe(); this script only reads the arguments.

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )

args     <- commandArgs( TRUE )
UNIVERSE <- if ( length( args ) >= 1 ) args[1] else "principled"
BATCH    <- if ( length( args ) >= 2 ) as.integer( args[2] ) else 2000L
WORKERS  <- if ( length( args ) >= 3 ) as.integer( args[3] ) else max( 1L, parallel::detectCores() - 2L )
## optional 4th argument names the run folder (default run_<universe>), e.g. principled_v5
RUNNAME  <- if ( length( args ) >= 4 ) args[4] else UNIVERSE

log_msg( UNIVERSE, " (model ", MV_MODEL, ") -> run_", RUNNAME )
mv_run_universe( build_grid( UNIVERSE )[ is.na( dup_of ) ], load_panel(),
                 dir = file.path( OUT$spec, paste0( "run_", RUNNAME ) ), cfg = mv_cfg(),
                 batch = BATCH, workers = WORKERS )
