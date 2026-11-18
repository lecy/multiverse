## R/multiverse/run_from_release.R
## Runs the multiverse from the research frames stored in this repository (data/), with no
## other inputs: no data hub, no S3, no build.
##   Rscript R/multiverse/run_from_release.R [workers] [out_dir=replication] [run=principled_v11]
##
##   1. Recompile: reads the year slices in data/cbsa_year/ and data/county_year/, checks each
##      file against data/MANIFEST.csv (sha256), binds them, restores the sort order, and checks
##      rows, columns and column types against data/FRAMES.csv. Writes the full frames to
##      <out_dir>/data/analysis/{cbsa_year,county_year}.parquet.
##   2. Runs the universe: R/multiverse/44_run_universe.R principled with MV_MODEL=lean (run v11,
##      432,432 specifications), batch size 2000, into <out_dir>/data/spec/run_<run>/
##      (specs.parquet, coefs.parquet). Resumable: finished batches are skipped on a rerun.
##   3. Summarizes: R/multiverse/45_summarize_run.R <run>, written into the same folder.
##      For v11 also the grassroots refit (R/notes/104_fit_grassroots_v11.R) and the paper's
##      decision matrices (R/multiverse/98_decision_matrix.R with MV_SPLIT=1, written to
##      paper/figures/decision_matrix_v11_*.png and <out_dir>/data/outline/v11/matrix_split/).
##
## replicate.R (repository root) downloads a release and calls this script; use it to replicate
## from scratch.
##
## Run from the repository root with the packages in renv.lock (renv::restore()). Takes about
## 35 minutes on 22 workers; workers default to all cores but two. Set MV_RUN_V10=1 to also
## run v10 (the six-covariate default, 605,352 specifications), the comparison run of the paper.
## out_dir is used as MV_BUILD for these steps, so an existing build folder is never touched.

suppressPackageStartupMessages( { library( data.table ); library( arrow ) } )
a <- commandArgs( TRUE )
WORKERS <- if ( length( a ) >= 1 ) as.integer( a[1] ) else max( 1L, parallel::detectCores() - 2L )
OUTDIR  <- normalizePath( if ( length( a ) >= 2 ) a[2] else "replication", winslash = "/", mustWork = FALSE )
RUN     <- if ( length( a ) >= 3 ) a[3] else "principled_v11"
stopifnot( file.exists( file.path( "data", "MANIFEST.csv" ) ), file.exists( file.path( "R", "multiverse", "44_run_universe.R" ) ) )
log_msg <- function( ... ) message( format( Sys.time(), "%H:%M:%S " ), ... )

## ---- 1. recompile the frames from the slices ---------------------------------------------
man <- fread( file.path( "data", "MANIFEST.csv" ), colClasses = list( character = "sha256" ) )
frm <- fread( file.path( "data", "FRAMES.csv" ), colClasses = list( character = c( "schema_sha256", "source_sha256" ) ) )
schema_sig <- function( d ) digest::digest( paste( names( d ), vapply( d, function( x ) class( x )[1], "" ), collapse = ";" ), "sha256", serialize = FALSE )
dir.create( file.path( OUTDIR, "data", "analysis" ), recursive = TRUE, showWarnings = FALSE )
for ( i in seq_len( nrow( frm ) ) ) {
  t <- frm$frame[ i ]; m <- man[ frame == t ][ order( year ) ]
  got <- vapply( file.path( "data", m$file ), function( f ) digest::digest( f, "sha256", file = TRUE ), "" )
  bad <- m$file[ got != m$sha256 ]
  if ( length( bad ) ) stop( "slices do not match data/MANIFEST.csv: ", paste( bad, collapse = ", " ) )
  d <- rbindlist( lapply( file.path( "data", m$file ), function( f ) as.data.table( read_parquet( f ) ) ), use.names = TRUE )
  setorderv( d, strsplit( frm$sort_keys[ i ], "," )[[ 1 ]] )
  stopifnot( nrow( d ) == frm$rows[ i ], ncol( d ) == frm$cols[ i ], schema_sig( d ) == frm$schema_sha256[ i ] )
  write_parquet( d, file.path( OUTDIR, "data", "analysis", paste0( t, ".parquet" ) ) )
  log_msg( t, ": ", nrow( m ), " slices checked -> ", format( nrow( d ), big.mark = "," ), " rows x ", ncol( d ), " cols" )
}

## ---- 2. run the universe, 3. summarize -----------------------------------------------------
Sys.setenv( MV_BUILD = OUTDIR, MV_MODEL = "lean" )
rs <- file.path( R.home( "bin" ), "Rscript" )
step <- function( ... ) { log_msg( paste( ... ) ); st <- system2( rs, c( ... ) ); if ( st != 0 ) stop( "failed: ", paste( ... ) ) }
step( "R/multiverse/44_run_universe.R", "principled", "2000", WORKERS, RUN )
step( "R/multiverse/45_summarize_run.R", RUN )
if ( RUN == "principled_v11" ) {
  step( "R/notes/104_fit_grassroots_v11.R", WORKERS )            # grassroots refit -> run_principled_v11_grass
  Sys.setenv( MV_SPLIT = "1" )
  step( "R/multiverse/98_decision_matrix.R", RUN )                 # decision matrices -> paper/figures/
  Sys.unsetenv( "MV_SPLIT" )
}
if ( Sys.getenv( "MV_RUN_V10" ) == "1" ) {
  Sys.setenv( MV_MODEL = "default" )
  step( "R/multiverse/44_run_universe.R", "principled", "2000", WORKERS, "principled_v10" )
  step( "R/multiverse/45_summarize_run.R", "principled_v10" )
  Sys.setenv( MV_MODEL = "lean" )
  step( "R/multiverse/97_v11_decision_summary.R" )                 # v11 tables, v10 vs v11 benchmarks
  Sys.setenv( MV_SPLIT = "1" ); step( "R/multiverse/97_v11_decision_summary.R" ); Sys.unsetenv( "MV_SPLIT" )
}
log_msg( "done: ", file.path( OUTDIR, "data", "spec", paste0( "run_", RUN ) ) )
