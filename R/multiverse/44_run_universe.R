## 44_run_universe.R
## Run a multiverse universe in resumable batches.
##   Rscript R/multiverse/44_run_universe.R [principled|kitchen] [batch_size] [workers] [run_name]
## Each worker writes its batch to D:/MULTIVERSE/data/spec/run_<universe>/batches/;
## finished batches are skipped on re-run. At the end the batches are combined:
##   specs.parquet  one row per specification (fork settings + status)
##   coefs.parquet  one row per specification x term
##   run_summary.csv

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )

args     <- commandArgs( TRUE )
UNIVERSE <- if ( length( args ) >= 1 ) args[1] else "principled"
BATCH    <- if ( length( args ) >= 2 ) as.integer( args[2] ) else 2000L
WORKERS  <- if ( length( args ) >= 3 ) as.integer( args[3] ) else max( 1L, detectCores() - 2L )
## optional 4th argument names the run folder (default run_<universe>), e.g. principled_v5
RUNNAME  <- if ( length( args ) >= 4 ) args[4] else UNIVERSE

run_dir <- file.path( OUT$spec, paste0( "run_", RUNNAME ) )
bat_dir <- file.path( run_dir, "batches" )
dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )

panel <- load_panel()
grid  <- build_grid( UNIVERSE )[ is.na( dup_of ) ]
grid[ , batch := ( seq_len( .N ) - 1L ) %/% BATCH + 1L ]
write_parquet( grid, file.path( run_dir, "specs_grid.parquet" ) )
nb <- max( grid$batch )
todo <- setdiff( seq_len( nb ),
                 as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                  list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
log_msg( UNIVERSE, ": ", format( nrow( grid ), big.mark = "," ), " specs in ", nb, " batches; ",
         length( todo ), " to run on ", WORKERS, " workers" )

run_batch <- function( b ) {
  g <- grid[ batch == b ]
  t0 <- proc.time()[[ "elapsed" ]]
  status <- character( nrow( g ) )
  res <- vector( "list", nrow( g ) )
  for ( i in seq_len( nrow( g ) ) ) {
    r <- tryCatch( run_spec( g[ i ], panel ), error = function( e ) e )
    if ( inherits( r, "error" ) ) status[ i ] <- paste( "error:", conditionMessage( r ) )
    else if ( is.null( r ) || !nrow( r ) ) status[ i ] <- "no fit"
    else { status[ i ] <- "ok"; res[[ i ]] <- r }
  }
  co <- rbindlist( res )
  st <- data.table( spec_id = g$spec_id, status = status )
  arrow::write_parquet( st, file.path( bat_dir, sprintf( "status_%04d.parquet", b ) ) )
  ## coefs written last: its presence marks the batch complete
  arrow::write_parquet( co, file.path( bat_dir, sprintf( "coefs_%04d.parquet", b ) ) )
  data.table( batch = b, specs = nrow( g ), ok = sum( status == "ok" ),
              sec = proc.time()[[ "elapsed" ]] - t0, worker = Sys.getpid() )
}

t_all <- system.time( {
  if ( length( todo ) ) {
    cl <- makePSOCKcluster( WORKERS )
    on.exit( try( stopCluster( cl ), silent = TRUE ), add = TRUE )
    invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
      setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
      source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" )
      data.table::setDTthreads( 1L ); NULL } ) )
    clusterExport( cl, c( "panel", "grid", "bat_dir", "run_batch" ) )
    bt <- rbindlist( parLapplyLB( cl, todo, run_batch ) )
    stopCluster( cl )
    old <- if ( file.exists( f <- file.path( run_dir, "batch_timing.csv" ) ) ) fread( f ) else NULL
    fwrite( rbind( old, bt ), file.path( run_dir, "batch_timing.csv" ) )
  }
} )
log_msg( "batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )

## ---- combine ---------------------------------------------------------------------

co <- rbindlist( lapply( list.files( bat_dir, pattern = "^coefs_", full.names = TRUE ),
                         function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
st <- rbindlist( lapply( list.files( bat_dir, pattern = "^status_", full.names = TRUE ),
                         function( f ) as.data.table( read_parquet( f ) ) ) )
specs <- merge( grid, st, by = "spec_id", all.x = TRUE )
specs[ is.na( status ), status := "not run" ]
write_parquet( specs, file.path( run_dir, "specs.parquet" ) )
write_parquet( co, file.path( run_dir, "coefs.parquet" ) )

summ <- specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                    errors = sum( grepl( "^error", status ) ), not_run = sum( status == "not run" ) ),
               keyby = .( construct, hu, ident ) ]
summ <- rbind( summ, summ[ , c( .( construct = "ALL", hu = "", ident = "" ),
                                lapply( .SD, sum ) ), .SDcols = c( "specs", "ok", "no_fit", "errors", "not_run" ) ] )
fwrite( summ, file.path( run_dir, "run_summary.csv" ) )
print( summ )
log_msg( "coefficient rows: ", format( nrow( co ), big.mark = "," ) )
errs <- specs[ grepl( "^error", status ), .N, by = status ][ order( -N ) ]
if ( nrow( errs ) ) { fwrite( errs, file.path( run_dir, "errors.csv" ) ); print( head( errs, 10 ) ) }
