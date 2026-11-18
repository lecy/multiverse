## 42_timing_test.R
## Configure the multiverse, size the principled universe, and time a random
## sample of 1,000 specifications to estimate total run time.
##   1. build the full grid and count specifications by stratum
##   2. draw 1,000 specs (seed 20261112), stratified by identification
##   3. run them serially (per-spec timing) and in parallel (throughput)
##   4. extrapolate to the full universe, serial and parallel
## Outputs: D:/MULTIVERSE/data/spec/{grid_summary,timing_sample,timing_results,
##          timing_estimates}.csv and the 1,000 specs' coefficient table

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
library( parallel )

panel <- load_panel()
log_msg( "panel: ", nrow( panel ), " CBSA-years" )

kitchen <- build_grid( "kitchen" )
pfull   <- build_grid( "principled" )
grid    <- pfull[ is.na( dup_of ) ]                     # the analysis curve
sizes <- data.table(
  universe = c( "kitchen (all forks crossed)", "kitchen, duplicates removed",
                "principled, incl. duplicates", "principled curve (run)" ),
  specs = c( nrow( kitchen ), kitchen[ is.na( dup_of ), .N ], nrow( pfull ), nrow( grid ) ) )
fwrite( sizes, file.path( OUT$spec, "universe_sizes.csv" ) )
print( sizes )
print( pfull[ , .N, by = .( dup_rule = fcoalesce( dup_of, "kept" ) ) ] )
fwrite( grid[ , .N, by = .( construct, hu, outcome, ident ) ],
        file.path( OUT$spec, "grid_summary.csv" ) )
strata <- grid[ , .N, by = .( construct, hu, ident ) ]
log_msg( "principled curve: ", format( nrow( grid ), big.mark = "," ), " specifications" )
print( strata )
rm( kitchen )

## ---- sample -----------------------------------------------------------------

set.seed( 20261112 )
n_id <- c( cs = 400L, pooled = 300L, within = 300L )
samp <- rbindlist( lapply( names( n_id ), function( i ) grid[ ident == i ][ sample( .N, n_id[ i ] ) ] ) )
fwrite( samp, file.path( OUT$spec, "timing_sample.csv" ) )

## ---- serial: per-spec timing ---------------------------------------------

run_timed <- function( i, samp, panel ) {
  s <- samp[ i ]
  t0 <- proc.time()[[ "elapsed" ]]
  r <- tryCatch( run_spec( s, panel ), error = function( e ) NULL )
  data.table( spec_id = s$spec_id, sec = proc.time()[[ "elapsed" ]] - t0,
              ok = !is.null( r ) && nrow( r ) > 0, n_terms = if ( is.null( r ) ) 0L else nrow( r ),
              n = if ( is.null( r ) ) NA_integer_ else r$n[1] )
}

N_SERIAL <- 300L
set.seed( 7 )
ser_idx <- sample( nrow( samp ), N_SERIAL )                # random across identification strata
log_msg( "serial run: ", N_SERIAL, " specs" )
t_serial <- system.time(
  ser <- rbindlist( lapply( ser_idx, run_timed, samp = samp, panel = panel ) ) )
ser <- merge( ser, samp, by = "spec_id" )

## ---- parallel: all 1,000 -----------------------------------------------------

n_workers <- max( 1L, detectCores() - 2L )
log_msg( "parallel run: 1,000 specs on ", n_workers, " workers" )
cl <- makePSOCKcluster( n_workers )
invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
                                setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
                                source( "R/00_setup.R" ); source( "R/40_multiverse.R" )
                                data.table::setDTthreads( 1L )      # avoid 14 x 16 thread oversubscription
                                NULL } ) )
clusterExport( cl, c( "panel", "samp" ) )
## chunked dispatch (one task per chunk) keeps per-task overhead small
chunks <- split( sample( nrow( samp ) ), rep_len( seq_len( n_workers * 4L ), nrow( samp ) ) )
t_par <- system.time(
  par_res <- unlist( parLapplyLB( cl, chunks, function( idx ) lapply( idx, function( i ) {
    s <- samp[ i ]; tryCatch( run_spec( s, panel ), error = function( e ) NULL ) } ) ),
    recursive = FALSE ) )
stopCluster( cl )
coefs <- rbindlist( par_res )
fwrite( coefs, file.path( OUT$spec, "timing_sample_coefs.csv" ) )

## ---- results and extrapolation ----------------------------------------------

fwrite( ser, file.path( OUT$spec, "timing_results.csv" ) )
per_spec <- mean( ser$sec )
by_est <- ser[ , .( n = .N, mean_sec = mean( sec ), ok = mean( ok ) ), by = est ]
by_dv  <- ser[ , .( n = .N, mean_sec = mean( sec ), ok = mean( ok ) ), by = dv_form ]
by_id  <- ser[ , .( n = .N, mean_sec = mean( sec ), ok = mean( ok ) ), by = ident ]

## weight per-spec time by the universe's mix of estimators x identification
mix <- grid[ , .N, by = .( est, ident ) ]
cell <- ser[ , .( mean_sec = mean( sec ) ), by = .( est, ident ) ]
mix <- merge( mix, cell, by = c( "est", "ident" ), all.x = TRUE )
mix[ is.na( mean_sec ), mean_sec := per_spec ]
serial_total_sec <- mix[ , sum( N * mean_sec ) ]
par_rate <- nrow( samp ) / t_par[[ "elapsed" ]]                 # specs per second, parallel

est <- data.table(
  metric = c( "universe_specs", "sample_serial_specs", "serial_sec_per_spec_mean",
              "serial_sec_per_spec_median", "parallel_workers", "parallel_1000_elapsed_sec",
              "parallel_specs_per_sec", "est_serial_total_hours", "est_parallel_total_hours",
              "success_rate_serial", "success_rate_parallel", "coef_rows_1000" ),
  value = c( nrow( grid ), N_SERIAL, per_spec, median( ser$sec ), n_workers, t_par[[ "elapsed" ]],
             par_rate, serial_total_sec / 3600, nrow( grid ) / par_rate / 3600,
             mean( ser$ok ), mean( !vapply( par_res, is.null, logical( 1 ) ) ), nrow( coefs ) ) )
fwrite( est, file.path( OUT$spec, "timing_estimates.csv" ) )
print( est ); print( by_est ); print( by_dv ); print( by_id )
fwrite( rbind( by_est[ , .( dim = "est", level = est, n, mean_sec, ok ) ],
               by_dv[ , .( dim = "dv_form", level = dv_form, n, mean_sec, ok ) ],
               by_id[ , .( dim = "ident", level = ident, n, mean_sec, ok ) ] ),
        file.path( OUT$spec, "timing_by_fork.csv" ) )
