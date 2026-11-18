## 85_civic_fork.R
## Civic-capacity and religion add-on blocks (BLOCKS$civic, BLOCKS$religion in
## 40_multiverse.R, added 2026-11-16). The measures were built on 2026-11-12 but never
## entered the grid:
##   civic:respn        census mail response rate (the clean civic measure)
##   civic:sk2          Rupasingha social-capital index re-estimated without nonprofit
##                      density and turnout (still contains association establishments,
##                      which are mostly nonprofits, so it partly counts the outcome)
##   religion:hhi       denominational concentration (RCMS)
##   religion:adherent  adherents per resident (RCMS)
## Each alternative ADDS one covariate to the full default model. They are skipped in the
## within stratum (observed only in survey waves; within-metro variance 0.003-5%). Their
## specs are appended after the v7 grid, so no v7 spec_id changes.
##
##   Rscript R/multiverse/85_civic_fork.R test             grid stability vs v7 + 300 sample specs
##   Rscript R/multiverse/85_civic_fork.R run [workers]    civic/religion specs -> data/spec/run_principled_civic/
##   Rscript R/multiverse/85_civic_fork.R merge            v7 + civic -> data/spec/run_principled_v8/
##   Rscript R/multiverse/85_civic_fork.R compare          added terms, and how the default covariates move (v8)

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )

args <- commandArgs( TRUE )
MODE <- if ( length( args ) ) args[1] else "run"
WORKERS <- if ( MODE == "run" && length( args ) >= 2 ) as.integer( args[2] ) else 8L
BATCH <- 1000L
KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
           "est", "cs_year", "outlier", "drop", "op" )
base_dir <- file.path( OUT$spec, "run_principled_v7" )
add_dir  <- file.path( OUT$spec, "run_principled_civic" )
v8_dir   <- file.path( OUT$spec, "run_principled_v8" )
bat_dir  <- file.path( add_dir, "batches" )
ADD_BLOCKS <- c( "civic", "religion" )
ADD_OPS  <- unlist( lapply( ADD_BLOCKS, function( b ) paste0( b, ":", names( BLOCKS[[ b ]]$alt ) ) ) )
ADD_VARS <- unique( unlist( lapply( ADD_BLOCKS, function( b ) unlist( BLOCKS[[ b ]]$alt ) ) ) )

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
new  <- grid[ op %in% ADD_OPS ]
log_msg( "grid: ", format( nrow( grid ), big.mark = "," ), " specs; civic/religion specs: ", format( nrow( new ), big.mark = "," ) )

run_batch <- function( b ) {
  g <- new[ batch == b ]; status <- character( nrow( g ) ); res <- vector( "list", nrow( g ) )
  for ( i in seq_len( nrow( g ) ) ) {
    r <- tryCatch( run_spec( g[ i ], panel ), error = function( e ) e )
    if ( inherits( r, "error" ) ) status[ i ] <- paste( "error:", conditionMessage( r ) )
    else if ( is.null( r ) || !nrow( r ) ) status[ i ] <- "no fit"
    else { status[ i ] <- "ok"; res[[ i ]] <- r } }
  arrow::write_parquet( data.table( spec_id = g$spec_id, status = status ), file.path( bat_dir, sprintf( "status_%04d.parquet", b ) ) )
  arrow::write_parquet( rbindlist( res ), file.path( bat_dir, sprintf( "coefs_%04d.parquet", b ) ) )
  b }

## ---- test ------------------------------------------------------------------------------
if ( MODE == "test" ) {
  print( new[ , .N, keyby = .( op, ident ) ] )
  v7 <- as.data.table( read_parquet( file.path( base_dir, "specs.parquet" ) ) )[ order( spec_id ) ]
  K <- c( "spec_id", KEYS )
  diff <- sum( vapply( K, function( k ) sum( v7[[ k ]] != grid[[ k ]][ seq_len( nrow( v7 ) ) ], na.rm = TRUE ), numeric( 1 ) ) )
  log_msg( "v7 ", nrow( v7 ), " specs; grid ", nrow( grid ), "; cells that differ in the first ", nrow( v7 ), " rows: ", diff )
  stopifnot( diff == 0, all( grid[ -seq_len( nrow( v7 ) ), op ] %in% ADD_OPS ) )
  panel <- load_panel()
  print( panel[ year %in% c( 2015, 2019, 2022 ), lapply( .SD, function( z ) sum( !is.na( z ) ) ), .SDcols = ADD_VARS, by = .( cbsa_type, year ) ] )
  set.seed( 1 ); smp <- new[ sample( .N, 300 ) ]
  st <- vapply( seq_len( nrow( smp ) ), function( i ) { r <- tryCatch( run_spec( smp[ i ], panel ), error = function( e ) e )
    if ( inherits( r, "error" ) ) paste( "error:", conditionMessage( r ) ) else if ( is.null( r ) || !nrow( r ) ) "no fit" else "ok" }, character( 1 ) )
  print( table( st ) )
  for ( o in ADD_OPS ) {
    s <- new[ op == o & ident == "cs" & outcome == "n_filer_c3" & dv_form == "loglog" & pred_form == "log" & unit == "metro" & outlier == "none" & est == "ols" ][ 1 ]
    cat( "\n", o, "\n" ); print( run_spec( s, panel )[ , .( term, estimate = signif( estimate, 3 ), p = signif( p, 2 ), n ) ] )
  }
}

## ---- run -------------------------------------------------------------------------------
if ( MODE == "run" ) {
  dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )
  panel <- load_panel()
  new[ , batch := ( seq_len( .N ) - 1L ) %/% BATCH + 1L ]
  write_parquet( new, file.path( add_dir, "specs_grid.parquet" ) )
  nb <- max( new$batch )
  todo <- setdiff( seq_len( nb ), as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                  list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
  log_msg( "civic: ", nb, " batches; ", length( todo ), " to run on ", WORKERS, " workers" )
  t_all <- system.time( if ( length( todo ) ) {
    cl <- makePSOCKcluster( WORKERS ); on.exit( try( stopCluster( cl ), silent = TRUE ), add = TRUE )
    invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
      setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
      source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
    clusterExport( cl, c( "panel", "new", "bat_dir", "run_batch" ) )
    invisible( parLapplyLB( cl, todo, run_batch ) ); stopCluster( cl ) } )
  log_msg( "civic batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )
  rd <- function( p ) rbindlist( lapply( list.files( bat_dir, pattern = p, full.names = TRUE ), function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
  st <- rd( "^status_" ); print( st[ , .N, by = .( status = sub( ":.*", "", status ) ) ] )
}

## ---- merge: v7 + civic -> v8 -------------------------------------------------------------
if ( MODE == "merge" ) {
  rd <- function( d, p ) rbindlist( lapply( list.files( d, pattern = p, full.names = TRUE ), function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
  ## map by fork settings, never by spec_id alone
  remap <- function( specs_keys, co, st, label ) {
    mp <- merge( specs_keys[ , c( "spec_id", KEYS ), with = FALSE ], grid[ , c( "spec_id", KEYS ), with = FALSE ],
                 by = KEYS, suffixes = c( "_old", "" ) )
    log_msg( label, ": ", nrow( specs_keys ), " specs, mapped ", nrow( mp ), ", renumbered ", mp[ spec_id_old != spec_id, .N ] )
    stopifnot( nrow( mp ) == nrow( specs_keys ) )
    co <- merge( co, mp[ , .( spec_id_old, spec_id ) ], by.x = "spec_id", by.y = "spec_id_old" )[ , spec_id := spec_id.y ][ , spec_id.y := NULL ]
    st <- merge( st, mp[ , .( spec_id_old, spec_id ) ], by.x = "spec_id", by.y = "spec_id_old" )[ , spec_id := spec_id.y ][ , spec_id.y := NULL ]
    list( co = co, st = st ) }
  bs <- as.data.table( read_parquet( file.path( base_dir, "specs.parquet" ) ) )
  a <- remap( bs, as.data.table( read_parquet( file.path( base_dir, "coefs.parquet" ) ) ), bs[ , .( spec_id, status ) ], "v7" )
  cv <- as.data.table( read_parquet( file.path( add_dir, "specs_grid.parquet" ) ) )[ op %in% ADD_OPS ]
  b <- remap( cv, rd( bat_dir, "^coefs_" )[ spec_id %in% cv$spec_id ], rd( bat_dir, "^status_" )[ spec_id %in% cv$spec_id ], "civic" )
  stopifnot( nrow( bs ) + nrow( cv ) == nrow( grid ) )
  specs <- merge( grid, rbind( a$st, b$st ), by = "spec_id", all.x = TRUE ); specs[ is.na( status ), status := "not run" ]
  co <- rbind( a$co, b$co[ , names( a$co ), with = FALSE ] )
  dir.create( v8_dir, showWarnings = FALSE )
  write_parquet( specs, file.path( v8_dir, "specs.parquet" ) ); write_parquet( co, file.path( v8_dir, "coefs.parquet" ) )
  summ <- specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                      errors = sum( grepl( "^error", status ) ), not_run = sum( status == "not run" ) ),
                 keyby = .( construct, hu, ident, block = fifelse( op %in% ADD_OPS, sub( ":.*", "", op ), "-" ) ) ]
  fwrite( summ, file.path( v8_dir, "run_summary.csv" ) ); print( summ, nrows = 200 )
  log_msg( "v8: ", format( nrow( specs ), big.mark = "," ), " specs (", specs[ status == "ok", .N ], " ok), ",
           format( nrow( co ), big.mark = "," ), " coefficient rows" )
}

## ---- compare: the added terms, and how the default covariates move ------------------------
if ( MODE == "compare" ) {
  specs <- as.data.table( read_parquet( file.path( v8_dir, "specs.parquet" ) ) )
  co <- as.data.table( read_parquet( file.path( v8_dir, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co <- merge( co, specs[ , .( spec_id, ident, est ) ], by = "spec_id" )
  co <- co[ !( ident == "pooled" & est == "state_re" ) ]
  co[ , n_units := n / fifelse( ident == "pooled", 8, 1 ) ]
  co[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]          # partial r, df in places (as in 58 part A)
  s2 <- specs[ op %in% c( "default", ADD_OPS ) & drop == "none" & ident != "within" ]
  x <- merge( co[ , !c( "ident", "est" ) ], s2, by = "spec_id" )
  aterm <- x[ term %in% ADD_VARS, { med <- median( pr ); .( specs = .N, median_r = med, iqr_r = IQR( pr ),
              sign_stability = mean( sign( pr ) == sign( med ) ), sig05 = mean( p < .05, na.rm = TRUE ),
              sig05_state = mean( p_state < .05, na.rm = TRUE ) ) },
              keyby = .( op, term, construct, hu, ident ) ]
  pk <- setdiff( KEYS, "op" )
  w <- dcast( x[ !term %in% ADD_VARS ], as.formula( paste( paste( c( pk, "term" ), collapse = " + " ), "~ op" ) ), value.var = "pr" )
  shift <- rbindlist( lapply( ADD_OPS, function( o ) {
    z <- w[ is.finite( default ) & is.finite( get( o ) ) ]
    z[ , .( add = o, pairs = .N, median_r_default = median( default ), median_r_add = median( get( o ) ),
            median_change = median( get( o ) - default ), share_shrink = mean( abs( get( o ) ) < abs( default ) ),
            sign_flips = mean( sign( get( o ) ) != sign( default ) & pmax( abs( get( o ) ), abs( default ) ) > .05 ),
            cor = cor( default, get( o ) ) ),
       keyby = .( construct, hu, ident, term ) ] } ) )
  ## each added term against the five benchmarks' own correlations (2022 metros)
  panel <- load_panel(); m22 <- panel[ year == 2022 & cbsa_type == "metro" ]
  m22[ , `:=`( l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ) ) ]
  cm <- round( cor( m22[ , c( ADD_VARS, "turnout", "pct_ba", "pct_nonwhite", "poverty_rate", "l_pf", "l_gov", "logP" ), with = FALSE ],
                    use = "pairwise" ), 2 )
  fwrite( as.data.table( cm, keep.rownames = "var" ), file.path( v8_dir, "civic_correlations.csv" ) )
  fwrite( aterm, file.path( v8_dir, "civic_term.csv" ) ); fwrite( shift, file.path( v8_dir, "civic_shift.csv" ) )
  r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
  print( cm[ ADD_VARS, ] )
  print( r3( aterm ), nrows = 200 )
  print( r3( shift[ term != "logP" ][ , .( pairs = sum( pairs ), median_abs_change = median( abs( median_change ) ),
                                           max_abs_change = max( abs( median_change ) ), sign_flips = weighted.mean( sign_flips, pairs ),
                                           cor = weighted.mean( cor, pairs ) ), keyby = .( add, ident ) ] ) )
  bench <- c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate" )
  print( r3( shift[ term %in% bench & hu == "incl" | term %in% bench & construct == "exp" & hu == "excl" ][
    , .( add, construct, hu, ident, term, median_r_default, median_r_add, median_change, sign_flips ) ][ order( -abs( median_change ) ) ][ 1:30 ] ) )
}
