## 82_lag_fork.R
## Population-lag covariates and their fork (block "lag" in 40_multiverse.R, added
## 2026-11-15). Nonprofit density lags population change (72_catchup_lag.R,
## 79_kernel_lag.R, notes/catchup-lag-results.md section 2.8). The lag enters as a
## kernel of past log population change, G[t] = a * G[t-1] + dlogP[t] with
## a = 0.5^(1/half-life): growth k years ago has weight a^k. It is an add-on block
## (no default): each alternative ADDS kernel terms to the full default model, in
## every stratum including within (the kernels vary over time). Its specs are appended
## after the base grid, so existing spec_ids are unchanged.
##   lag:g5      G at a 5-year half-life   (local projections, metro trends removed)
##   lag:g10     G at a 10-year half-life  (kernel fit in levels, filers)
## A third alternative, lag:pascal (G and H = a * H[t-1] + G[t] at a 2.5-year
## half-life), was run and dropped the same day: its two terms are collinear and split
## the effect (notes 2.9). build still writes lag_p25g / lag_p25h to the covariate
## file, and data/spec/run_principled_lag/ still holds its 18,084 specs (the batches
## follow the three-alternative numbering); merge keeps only the current lag ops and
## maps them to the current grid by fork settings. Do not re-run "run" into that
## folder: delete its batches first.
## The kernels need population back to 1970: decennial census county counts (1970,
## 1980, 1990; raw/census-hist/, as in 73_pre2000_growth.R) mapped to 2023 counties,
## summed to CBSAs and interpolated log-linearly to the panel's 2000 value.
##   - partial county coverage: the covered counties' growth is applied to the CBSA;
##   - no coverage (Connecticut: 2023 planning regions do not nest in old counties):
##     the state's growth is applied.
##
##   Rscript R/82_lag_fork.R build            -> data/cov/cov_lag_kernel.csv (+ coverage diagnostics)
##   Rscript R/82_lag_fork.R test             runs 300 lag specs, checks the grid against a saved copy
##   Rscript R/82_lag_fork.R run [workers]    lag specs -> data/spec/run_principled_lag/
##   Rscript R/82_lag_fork.R merge            v6 + lag -> data/spec/run_principled_v7/
##   Rscript R/82_lag_fork.R compare          lag terms and matched default vs +lag pairs (v7)

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )

args <- commandArgs( TRUE )
MODE <- if ( length( args ) ) args[1] else "run"
WORKERS <- if ( MODE == "run" && length( args ) >= 2 ) as.integer( args[2] ) else 8L
BATCH <- 1000L
COV_FILE <- file.path( OUT$cov, "cov_lag_kernel.csv" )
HIST <- file.path( BUILD, "raw", "census-hist" )
KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
           "est", "cs_year", "outlier", "drop", "op" )
base_dir <- file.path( OUT$spec, "run_principled_v6" )
lag_dir  <- file.path( OUT$spec, "run_principled_lag" )
v7_dir   <- file.path( OUT$spec, "run_principled_v7" )
bat_dir  <- file.path( lag_dir, "batches" )
LAG_OPS  <- paste0( "lag:", names( BLOCKS$lag$alt ) )
LAG_VARS <- unique( unlist( BLOCKS$lag$alt ) )

## ---- build the covariate --------------------------------------------------------
if ( MODE == "build" ) {
  ## decennial county counts (parsers as in 73_pre2000_growth.R)
  read_old <- function( f ) {
    L <- readLines( file.path( HIST, f ), warn = FALSE )
    i <- grep( "^[0-9]{5} ", L ); out <- list()
    for ( k in i ) {
      fips <- substr( L[ k ], 1, 5 ); if ( !is.null( out[[ fips ]] ) || grepl( "000$", fips ) ) next
      tk <- strsplit( trimws( substring( L[ k ], 7 ) ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ]
      if ( !length( nums ) ) { tk <- strsplit( trimws( L[ k + 1 ] ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ] }
      if ( length( nums ) ) out[[ fips ]] <- as.numeric( nums[ 1 ] )
    }
    data.table( fips = names( out ), pop = unlist( out ) ) }
  p70 <- read_old( "e7079co.txt" ); p80 <- read_old( "e8089co.txt" )
  L <- readLines( file.path( HIST, "99c8_00.txt" ), warn = FALSE ); L <- L[ grepl( "^1 +[0-9]{5} ", L ) ]
  p90 <- rbindlist( lapply( L, function( x ) { tk <- strsplit( trimws( x ), " +" )[[ 1 ]]
    nums <- tk[ grepl( "^[0-9,]+$", tk ) ][ -1 ]
    data.table( fips = tk[ 2 ], pop = as.numeric( gsub( ",", "", nums[ length( nums ) ] ) ) ) } ) )
  RECODE <- c( "12025" = "12086", "51780" = "51083", "51560" = "51005", "51515" = "51019",
               "51123" = "51800", "51695" = "51800", "30113" = "30067" )
  hist <- rbindlist( list( p70[ , year := 1970 ], p80[ , year := 1980 ], p90[ , year := 1990 ] ) )
  hist[ fips %in% names( RECODE ), fips := RECODE[ fips ] ]
  hist <- hist[ , .( pop = sum( pop ) ), by = .( fips, year ) ]
  log_msg( "counties: 1970 ", hist[ year == 1970, .N ], ", 1980 ", hist[ year == 1980, .N ], ", 1990 ", hist[ year == 1990, .N ] )

  sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "state_fips" ) ) )
  sp <- sp[ cbsa_type %in% c( "metro", "micro" ) & !is.na( cbsa ) & cbsa != "" ]
  cp <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )[ year == 2000 ]
  sp <- merge( sp, cp[ , .( geoid, pop2000 = population ) ], by = "geoid", all.x = TRUE )
  ## state growth ratios (historical state total / 2000 state total) for CBSAs with no matched counties
  st <- rbindlist( lapply( c( 1970, 1980, 1990 ), function( y )
    hist[ year == y, .( year = y, hp = sum( pop ) ), by = .( state_fips = substr( fips, 1, 2 ) ) ] ) )
  st <- merge( st, cp[ , .( p2000 = sum( population ) ), by = .( state_fips = substr( geoid, 1, 2 ) ) ], by = "state_fips" )
  st[ , ratio_state := hp / p2000 ]
  ## CBSA ratio of historical to 2000 population from the covered counties
  rt <- rbindlist( lapply( c( 1970, 1980, 1990 ), function( y ) { h <- hist[ year == y ]
    sp[ , { f <- geoid %in% h$fips
      .( year = y, coverage = sum( pop2000[ f ], na.rm = TRUE ) / sum( pop2000, na.rm = TRUE ),
         ratio_cty = if ( any( f ) ) sum( h$pop[ match( geoid[ f ], h$fips ) ] ) / sum( pop2000[ f ], na.rm = TRUE ) else NA_real_,
         main_state = names( which.max( tapply( pop2000, state_fips, sum, na.rm = TRUE ) ) ) ) },
       by = .( cbsa, cbsa_title, cbsa_type ) ] } ) )
  rt <- merge( rt, st[ , .( main_state = state_fips, year, ratio_state ) ], by = c( "main_state", "year" ), all.x = TRUE )
  rt[ , source := fifelse( coverage >= 0.98, "counties", fifelse( coverage > 0, "partial counties", "state" ) ) ]
  rt[ , ratio := fifelse( is.na( ratio_cty ) | coverage == 0, ratio_state, ratio_cty ) ]
  cat( "history source by CBSA-year:\n" ); print( rt[ , .N, keyby = .( cbsa_type, source ) ] )
  print( rt[ source != "counties", .( cbsa_title, year, coverage = round( coverage, 3 ), source ) ][ order( cbsa_title, year ) ], nrows = 60 )

  ## annual log population 1970-2024 anchored to the panel's 2000 value
  panel <- load_panel()[ , .( cbsa, year, logP ) ]
  a00 <- panel[ year == 2000, .( cbsa, l2000 = logP ) ]
  w <- merge( dcast( rt, cbsa ~ year, value.var = "ratio" ), a00, by = "cbsa" )
  setnames( w, c( "1970", "1980", "1990" ), c( "r70", "r80", "r90" ) )
  pre <- w[ , .( year = 1970:1999, logP = approx( c( 1970, 1980, 1990, 2000 ), c( l2000 + log( c( r70, r80, r90 ) ), l2000 ), xout = 1970:1999 )$y ), by = cbsa ]
  full <- rbind( pre, panel[ cbsa %in% w$cbsa ] ); setorder( full, cbsa, year )
  rec <- function( x, g, a ) ave( x, g, FUN = function( z ) as.numeric( stats::filter( z, a, method = "recursive" ) ) )
  a_of <- function( h ) 0.5^( 1 / h )
  full[ , dp := c( 0, diff( logP ) ), by = cbsa ]
  full[ , `:=`( lag_g5 = rec( dp, cbsa, a_of( 5 ) ), lag_g10 = rec( dp, cbsa, a_of( 10 ) ), lag_p25g = rec( dp, cbsa, a_of( 2.5 ) ) ) ]
  full[ , lag_p25h := rec( lag_p25g, cbsa, a_of( 2.5 ) ) ]
  out <- full[ year >= 2000, .( cbsa, year, lag_g5, lag_g10, lag_p25g, lag_p25h ) ]
  out <- merge( out, unique( rt[ , .( cbsa, hist_source = fifelse( all( source == "counties" ), "counties",
                                         fifelse( any( source == "state" ), "state", "partial counties" ) ) ), by = cbsa ][ , .( cbsa, hist_source ) ] ),
                by = "cbsa" )
  fwrite( out, COV_FILE ); fwrite( rt, file.path( OUT$cov, "cov_lag_kernel_coverage.csv" ) )
  log_msg( "wrote ", COV_FILE, ": ", uniqueN( out$cbsa ), " CBSAs x ", uniqueN( out$year ), " years" )
  print( out[ year %in% c( 2010, 2022 ), lapply( .SD, function( z ) round( c( mean = mean( z ), sd = sd( z ), min = min( z ), max = max( z ) ), 4 ) ),
              .SDcols = LAG_VARS, by = year ] )
  ## check against the 79_kernel_lag.R construction (metros with full coverage; same 2000 anchor)
  c9 <- fread( file.path( BUILD, "data", "catchup", "c9_metro_1970_2024.csv" ), colClasses = list( character = "cbsa" ) )[ , .( cbsa, p1970, p1980, p1990 ) ]
  chk <- merge( c9, w[ , .( cbsa, l2000, r70, r80, r90 ) ], by = "cbsa" )
  chk[ , `:=`( d70 = log( p1970 ) - ( l2000 + log( r70 ) ), d90 = log( p1990 ) - ( l2000 + log( r90 ) ) ) ]
  cat( "log 1970 / 1990 level vs c9 (metros), abs diff quantiles:\n" ); print( round( quantile( abs( chk$d70 ), c( .5, .9, .99, 1 ) ), 4 ) )
  print( round( quantile( abs( chk$d90 ), c( .5, .9, .99, 1 ) ), 4 ) )
  quit( save = "no" )
}

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
new  <- grid[ op %in% LAG_OPS ]
log_msg( "grid: ", format( nrow( grid ), big.mark = "," ), " specs; lag specs: ", format( nrow( new ), big.mark = "," ) )

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

## ---- test: grid stability + a sample of lag specs ---------------------------------------
if ( MODE == "test" ) {
  print( new[ , .N, keyby = .( op, ident ) ] )
  ## existing spec_ids must be unchanged: compare with the grid saved before the block was added
  if ( length( args ) >= 2 && file.exists( args[ 2 ] ) ) {
    old <- as.data.table( read_parquet( args[ 2 ] ) ); cur <- build_grid( "principled" )
    same <- identical( old[ , c( "spec_id", KEYS, "dup_of" ), with = FALSE ], cur[ seq_len( nrow( old ) ), c( "spec_id", KEYS, "dup_of" ), with = FALSE ] )
    log_msg( "old grid ", nrow( old ), " rows; new ", nrow( cur ), "; first rows identical: ", same )
    stopifnot( same, all( cur[ -seq_len( nrow( old ) ), op ] %in% LAG_OPS ) )
  }
  panel <- load_panel()
  print( panel[ , lapply( .SD, function( z ) sum( !is.na( z ) ) ), .SDcols = LAG_VARS, by = cbsa_type ] )
  set.seed( 1 ); smp <- new[ sample( .N, 300 ) ]
  t0 <- Sys.time()
  st <- vapply( seq_len( nrow( smp ) ), function( i ) { r <- tryCatch( run_spec( smp[ i ], panel ), error = function( e ) e )
    if ( inherits( r, "error" ) ) paste( "error:", conditionMessage( r ) ) else if ( is.null( r ) || !nrow( r ) ) "no fit" else "ok" }, character( 1 ) )
  log_msg( "300 specs in ", round( as.numeric( difftime( Sys.time(), t0, units = "secs" ) ), 1 ), " s" ); print( table( st ) )
  ## one example per alternative: default model + lag terms, within and cross-section
  for ( o in LAG_OPS ) for ( id in c( "cs", "within" ) ) {
    s <- new[ op == o & ident == id & outcome == "n_filer_c3" & dv_form == "loglog" & pred_form == "log" & unit == "metro" & outlier == "none" & est == "ols" ][ 1 ]
    if ( nrow( s ) ) { cat( "\n", o, id, "\n" ); print( run_spec( s, panel )[ , .( term, estimate = signif( estimate, 3 ), se = signif( se, 3 ), p = signif( p, 2 ), n ) ] ) }
  }
}

## ---- run -------------------------------------------------------------------------------
if ( MODE == "run" ) {
  dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )
  panel <- load_panel()
  new[ , batch := ( seq_len( .N ) - 1L ) %/% BATCH + 1L ]
  write_parquet( new, file.path( lag_dir, "specs_grid.parquet" ) )
  nb <- max( new$batch )
  todo <- setdiff( seq_len( nb ), as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                  list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
  log_msg( "lag: ", nb, " batches; ", length( todo ), " to run on ", WORKERS, " workers" )
  t_all <- system.time( if ( length( todo ) ) {
    cl <- makePSOCKcluster( WORKERS ); on.exit( try( stopCluster( cl ), silent = TRUE ), add = TRUE )
    invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
      setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
      source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
    clusterExport( cl, c( "panel", "new", "bat_dir", "run_batch" ) )
    invisible( parLapplyLB( cl, todo, run_batch ) ); stopCluster( cl ) } )
  log_msg( "lag batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )
  rd <- function( p ) rbindlist( lapply( list.files( bat_dir, pattern = p, full.names = TRUE ), function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
  st <- rd( "^status_" ); print( st[ , .N, by = .( status = sub( ":.*", "", status ) ) ] )
}

## ---- merge: v6 + lag -> v7 ---------------------------------------------------------------
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
  stopifnot( file.exists( file.path( base_dir, "specs.parquet" ) ) )        # v6 must be complete
  bs <- as.data.table( read_parquet( file.path( base_dir, "specs.parquet" ) ) )
  a <- remap( bs, as.data.table( read_parquet( file.path( base_dir, "coefs.parquet" ) ) ), bs[ , .( spec_id, status ) ], "v6" )
  lg <- as.data.table( read_parquet( file.path( lag_dir, "specs_grid.parquet" ) ) )
  dropped <- lg[ !op %in% LAG_OPS, .N, by = op ]
  if ( nrow( dropped ) ) { log_msg( "lag run: leaving out alternatives no longer in the block:" ); print( dropped ) }
  lg <- lg[ op %in% LAG_OPS ]
  b <- remap( lg, rd( bat_dir, "^coefs_" )[ spec_id %in% lg$spec_id ], rd( bat_dir, "^status_" )[ spec_id %in% lg$spec_id ], "lag" )
  stopifnot( nrow( bs ) + nrow( lg ) == nrow( grid ) )
  specs <- merge( grid, rbind( a$st, b$st ), by = "spec_id", all.x = TRUE ); specs[ is.na( status ), status := "not run" ]
  co <- rbind( a$co, b$co[ , names( a$co ), with = FALSE ] )
  dir.create( v7_dir, showWarnings = FALSE )
  write_parquet( specs, file.path( v7_dir, "specs.parquet" ) ); write_parquet( co, file.path( v7_dir, "coefs.parquet" ) )
  summ <- specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                      errors = sum( grepl( "^error", status ) ), not_run = sum( status == "not run" ) ),
                 keyby = .( construct, hu, ident, lag = fifelse( op %in% LAG_OPS, op, "-" ) ) ]
  fwrite( summ, file.path( v7_dir, "run_summary.csv" ) ); print( summ, nrows = 200 )
  log_msg( "v7: ", format( nrow( specs ), big.mark = "," ), " specs (", specs[ status == "ok", .N ], " ok), ",
           format( nrow( co ), big.mark = "," ), " coefficient rows" )
}

## ---- compare: lag terms, and how the covariates move when the lag is added --------------------
if ( MODE == "compare" ) {
  specs <- as.data.table( read_parquet( file.path( v7_dir, "specs.parquet" ) ) )
  co <- as.data.table( read_parquet( file.path( v7_dir, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]                 # partial r
  s2 <- specs[ op %in% c( "default", LAG_OPS ) & drop == "none" ]
  x <- merge( co, s2, by = "spec_id" )
  lterm <- x[ term %in% LAG_VARS, { med <- median( pr ); .( specs = .N, median_est = median( estimate ), median_r = med,
              sign_stability = mean( sign( pr ) == sign( med ) ), sig05 = mean( p < .05, na.rm = TRUE ) ) },
              keyby = .( op, term, construct, hu, ident ) ]
  pk <- setdiff( KEYS, "op" )
  w <- dcast( x[ !term %in% LAG_VARS ], as.formula( paste( paste( c( pk, "term" ), collapse = " + " ), "~ op" ) ), value.var = "pr" )
  shift <- rbindlist( lapply( LAG_OPS, function( o ) {
    z <- w[ is.finite( default ) & is.finite( get( o ) ) ]
    z[ , .( lag = o, pairs = .N, median_r_default = median( default ), median_r_lag = median( get( o ) ),
            median_change = median( get( o ) - default ), share_shrink = mean( abs( get( o ) ) < abs( default ) ),
            sign_flips = mean( sign( get( o ) ) != sign( default ) ), cor = cor( default, get( o ) ) ),
       keyby = .( construct, hu, ident, term ) ] } ) )
  fwrite( lterm, file.path( v7_dir, "lag_term.csv" ) ); fwrite( shift, file.path( v7_dir, "lag_shift.csv" ) )
  r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
  print( r3( lterm ), nrows = 200 )
  print( r3( shift[ term != "logP" ][ , .( pairs = sum( pairs ), median_abs_change = median( abs( median_change ) ),
                                           max_abs_change = max( abs( median_change ) ), sign_flips = weighted.mean( sign_flips, pairs ),
                                           cor = weighted.mean( cor, pairs ) ), keyby = .( lag, ident ) ] ) )
  print( r3( shift[ term != "logP" & lag == "lag:g10" ][ order( -abs( median_change ) ) ][ 1:25 ] ) )
}
