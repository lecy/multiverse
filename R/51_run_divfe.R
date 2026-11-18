## 51_run_divfe.R
## Add the Census-division FE estimator (est = "div_fe") to a finished
## principled run without re-running the specifications it already has.
##   Rscript R/51_run_divfe.R test            run 8 div_fe specs and print them
##   Rscript R/51_run_divfe.R run [workers]   run every div_fe spec -> run_principled_divfe/
##   Rscript R/51_run_divfe.R merge           v2 (run_principled/) + div_fe -> run_principled_v3/
## The merge maps the v2 specifications onto the current grid's spec_ids by
## their fork settings, so run_principled_v3 is the full current universe.

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )

args <- commandArgs( TRUE )
MODE <- if ( length( args ) ) args[1] else "run"
WORKERS <- if ( length( args ) >= 2 ) as.integer( args[2] ) else max( 1L, detectCores() - 2L )
BATCH <- 2000L

KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
           "est", "cs_year", "outlier", "drop", "op" )
v2_dir  <- file.path( OUT$spec, "run_principled" )
div_dir <- file.path( OUT$spec, "run_principled_divfe" )
v3_dir  <- file.path( OUT$spec, "run_principled_v3" )
bat_dir <- file.path( div_dir, "batches" )
dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
div  <- grid[ est == "div_fe" ]
log_msg( "current grid: ", format( nrow( grid ), big.mark = "," ), " specs; div_fe: ", format( nrow( div ), big.mark = "," ) )

if ( MODE == "test" ) {
  panel <- load_panel()
  set.seed( 1 )
  t <- div[ c( which( ident == "cs" & dv_form == "loglog" )[ 1 ], which( ident == "pooled" & dv_form == "sami" )[ 1 ],
               which( dv_form == "pois_free" & ident == "cs" )[ 1 ], which( dv_form == "pois_off" & ident == "pooled" )[ 1 ],
               sample( nrow( div ), 4 ) ) ]
  for ( i in seq_len( nrow( t ) ) ) { r <- run_spec( t[ i ], panel )
    cat( "\n--", t$ident[ i ], t$dv_form[ i ], t$pred_form[ i ], t$outcome[ i ], "\n" )
    print( r[ , .( term, estimate = signif( estimate, 3 ), stat = round( stat, 2 ), p_state = signif( p_state, 2 ), n ) ] ) }
}

if ( MODE == "run" ) {
  panel <- load_panel()
  div[ , batch := ( seq_len( .N ) - 1L ) %/% BATCH + 1L ]
  write_parquet( div, file.path( div_dir, "specs_grid.parquet" ) )
  nb <- max( div$batch )
  todo <- setdiff( seq_len( nb ), as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                  list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
  log_msg( "div_fe: ", nb, " batches; ", length( todo ), " to run on ", WORKERS, " workers" )
  run_batch <- function( b ) {
    g <- div[ batch == b ]; status <- character( nrow( g ) ); res <- vector( "list", nrow( g ) )
    for ( i in seq_len( nrow( g ) ) ) {
      r <- tryCatch( run_spec( g[ i ], panel ), error = function( e ) e )
      if ( inherits( r, "error" ) ) status[ i ] <- paste( "error:", conditionMessage( r ) )
      else if ( is.null( r ) || !nrow( r ) ) status[ i ] <- "no fit"
      else { status[ i ] <- "ok"; res[[ i ]] <- r } }
    arrow::write_parquet( data.table( spec_id = g$spec_id, status = status ), file.path( bat_dir, sprintf( "status_%04d.parquet", b ) ) )
    arrow::write_parquet( rbindlist( res ), file.path( bat_dir, sprintf( "coefs_%04d.parquet", b ) ) )
    b }
  t_all <- system.time( if ( length( todo ) ) {
    cl <- makePSOCKcluster( WORKERS ); on.exit( try( stopCluster( cl ), silent = TRUE ), add = TRUE )
    invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
      setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
      source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
    clusterExport( cl, c( "panel", "div", "bat_dir", "run_batch" ) )
    invisible( parLapplyLB( cl, todo, run_batch ) ); stopCluster( cl ) } )
  log_msg( "div_fe batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )
}

if ( MODE == "compare" ) {
  ## estimator comparison in the merged run: partial r by est, default covariates
  specs <- as.data.table( read_parquet( file.path( v3_dir, "specs.parquet" ) ) )
  co <- as.data.table( read_parquet( file.path( v3_dir, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
  x <- merge( co[ term != "logP" ], specs[ ident != "within", .( spec_id, construct, hu, ident, est ) ], by = "spec_id" )
  DEF <- c( "race_hhi", "pf_assets", "turnout", "gov_earn_sl", "poverty_rate", "pct_ba" )
  cmp <- x[ term %in% DEF, { med <- median( pr ); .( specs = .N, median_r = med, pos = mean( pr > 0 ), sig = mean( p < 0.05, na.rm = TRUE ),
                                                     sig_state = mean( p_state < 0.05, na.rm = TRUE ) ) },
            keyby = .( construct, hu, ident, term, est ) ]
  fwrite( cmp, file.path( v3_dir, "compare_estimators.csv" ) )
  w <- dcast( cmp, construct + hu + ident + term ~ est, value.var = "median_r" )
  print( w[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  ## agreement of covariate t-statistics between estimators, matched on every other fork
  key <- setdiff( c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit", "cs_year", "outlier", "drop", "op" ), "est" )
  xx <- merge( co[ term != "logP", .( spec_id, term, stat ) ], specs[ ident != "within", c( "spec_id", "est", key ), with = FALSE ], by = "spec_id" )
  ww <- dcast( xx, as.formula( paste( paste( c( key, "term" ), collapse = " + " ), "~ est" ) ), value.var = "stat" )
  ag <- ww[ , { cm <- cor( .SD, method = "spearman", use = "pairwise.complete.obs" )
                .( ols_state = cm[ "ols", "state_fe" ], ols_div = cm[ "ols", "div_fe" ], div_state = cm[ "div_fe", "state_fe" ],
                   div_re = cm[ "div_fe", "state_re" ], state_re = cm[ "state_fe", "state_re" ] ) },
            by = .( construct, hu, ident ), .SDcols = c( "ols", "state_fe", "div_fe", "state_re" ) ]
  fwrite( ag, file.path( v3_dir, "compare_estimators_tstat_spearman.csv" ) )
  print( ag[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
}

if ( MODE == "merge" ) {
  rd <- function( d, p ) rbindlist( lapply( list.files( d, pattern = p, full.names = TRUE ), function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
  ## div_fe results (spec_ids are already the current grid's)
  dco <- rd( bat_dir, "^coefs_" ); dst <- rd( bat_dir, "^status_" )
  ## v2 results, remapped from v2 spec_ids to current spec_ids by fork settings
  v2s <- as.data.table( read_parquet( file.path( v2_dir, "specs.parquet" ) ) )
  v2c <- as.data.table( read_parquet( file.path( v2_dir, "coefs.parquet" ) ) )
  map <- merge( v2s[ , c( "spec_id", "status", KEYS ), with = FALSE ], grid[ , c( "spec_id", KEYS ), with = FALSE ],
                by = KEYS, suffixes = c( "_v2", "" ) )
  log_msg( "v2 specs: ", nrow( v2s ), "; mapped to current grid: ", nrow( map ),
           "; current non-div_fe specs: ", grid[ est != "div_fe", .N ] )
  stopifnot( nrow( map ) == nrow( v2s ), nrow( map ) == grid[ est != "div_fe", .N ] )
  v2c <- merge( v2c, map[ , .( spec_id_v2, spec_id ) ], by.x = "spec_id", by.y = "spec_id_v2" )
  v2c[ , spec_id := spec_id.y ][ , spec_id.y := NULL ]
  st <- rbind( map[ , .( spec_id, status ) ], dst )
  specs <- merge( grid, st, by = "spec_id", all.x = TRUE )
  specs[ is.na( status ), status := "not run" ]
  co <- rbind( v2c[ , names( dco ), with = FALSE ], dco )
  dir.create( v3_dir, showWarnings = FALSE )
  write_parquet( specs, file.path( v3_dir, "specs.parquet" ) ); write_parquet( co, file.path( v3_dir, "coefs.parquet" ) )
  summ <- specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                      errors = sum( grepl( "^error", status ) ), not_run = sum( status == "not run" ) ),
                 keyby = .( construct, hu, ident, est ) ]
  fwrite( summ, file.path( v3_dir, "run_summary.csv" ) )
  print( summ[ , lapply( .SD, sum ), by = est, .SDcols = c( "specs", "ok", "no_fit", "errors", "not_run" ) ] )
  log_msg( "v3: ", format( nrow( specs ), big.mark = "," ), " specs, ", format( nrow( co ), big.mark = "," ), " coefficient rows" )
}
