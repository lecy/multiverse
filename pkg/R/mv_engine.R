#' Multiverse specification grid
#'
#' `mv_grid()` builds the specification grid of a universe. Strata (outcome construct x
#' hospital/university treatment x identification) are never pooled in one curve; the
#' forks are crossed within them.
#'
#' * `universe = "principled"`: the analysis curve. Covariate forks one at a time:
#'   alternative measures only in the full model, leave-one-block-out only with the
#'   default measures; human-services outcomes left out.
#' * `universe = "kitchen"`: the full cross product of every fork.
#'
#' In both, rows with `dup_of` set are algebraic duplicates of another specification
#' (identical coefficients on the covariates); the runs leave them out. Add-on blocks
#' (blocks with no default) are built and appended block by block, so adding an add-on
#' block never renumbers the existing `spec_id`s.
#'
#' `mv_op_variants()`, `mv_block_sets()` and `mv_addon_blocks()` are the pieces: the
#' covariate variants, the leave-one-out sets, and the add-on blocks. `mv_covars()` gives
#' the covariates of one specification.
#'
#' @param universe `"principled"` or `"kitchen"`.
#' @param model `"default"` or `"lean"`, used when `cfg` is not given.
#' @param cfg A configuration from [mv_spec_config()].
#' @param s One row of the grid (a list or one-row data.table).
#' @return `mv_grid()`: a data.table, one row per specification, with `spec_id`.
#' @examples
#' g <- mv_grid( "principled", model = "lean" )
#' nrow( g[ is.na( dup_of ) ] )
#' @export
mv_grid <- function( universe = c( "principled", "kitchen" ), model = "default", cfg = mv_spec_config( model ) ) {
  universe <- match.arg( universe )
  OUTCOMES <- cfg$outcomes; FORKS <- cfg$forks
  ## strata
  st <- rbindlist( lapply( names( OUTCOMES ), function( k )
          rbindlist( lapply( names( OUTCOMES[[ k ]] ), function( h )
            data.table( construct = k, hu = h, outcome = OUTCOMES[[ k ]][[ h ]] ) ) ) ) )
  ## principled: subsector outcomes (human services) are a separate extension
  if ( universe == "principled" ) st <- st[ !grepl( "_hs$", outcome ) ]
  st <- st[ , .( ident = names( cfg$ident ) ), by = .( construct, hu, outcome ) ]

  st[ , k := 1L ]
  drops <- mv_block_sets( cfg )$drop
  make <- function( ov ) {
    if ( !nrow( ov ) ) return( NULL )
    g <- CJ( dv_form = FORKS$dv_form, pred_form = FORKS$pred_form, rhs_logP = FORKS$rhs_logP,
             unit = FORKS$unit, est = FORKS$est, cs_year = FORKS$cs_year, outlier = FORKS$outlier,
             drop = drops, op = ov$op )
    g <- merge( g, ov, by = "op" )
    g <- g[ is.na( op_block ) | op_block != drop ]                     # can't vary a dropped block
    if ( universe == "principled" ) g <- g[ drop == "none" | op == "default" ]
    g <- g[ !( dv_form %in% c( "loglog", "pois_free" ) & rhs_logP == FALSE ) ]
    g <- g[ !( dv_form %in% c( "pois_off", "pois_free" ) & est == "state_re" ) ]
    g <- unique( g )
    g[ , k := 1L ]
    full <- merge( st, g, by = "k", allow.cartesian = TRUE )[ , k := NULL ]
    ## identification-specific restrictions
    full <- full[ !( ident != "cs" & cs_year != 2022L ) ]               # year fork only in cs
    full <- full[ !( ident == "within" & est != "ols" ) ]              # within: metro + year FE only
    ## time-invariant add-ons are absorbed by metro FE
    full[ !( ident == "within" & op_block %in% cfg$time_invariant_blocks ) ]
  }
  ov <- mv_op_variants( cfg )
  ab <- mv_addon_blocks( cfg )
  addon <- ov$op_block %in% ab
  full <- rbind( make( ov[ !addon ] ),
                 rbindlist( lapply( ab, function( b ) make( ov[ op_block %in% b ] ) ) ) )

  ## algebraic duplicates (exact; identical covariate coefficients)
  full[ , logP_rhs := rhs_logP | dv_form %in% c( "loglog", "pois_free" ) ]
  full[ , dup_of := NA_character_ ]
  ## log(Y/P) + log P  ==  log Y + log P  (log P coefficient shifts by 1)
  full[ outlier == "none" & dv_form == "logpc" & rhs_logP == TRUE, dup_of := "loglog" ]
  ## Poisson offset(log P) + free log P  ==  Poisson with free log P
  full[ outlier == "none" & dv_form == "pois_off" & rhs_logP == TRUE, dup_of := "pois_free" ]
  ## cross-section only: SAMI_Y + log P  ==  log Y + log P
  full[ is.na( dup_of ) & outlier == "none" & ident == "cs" & dv_form == "sami" & rhs_logP == TRUE, dup_of := "loglog" ]
  ## residualized predictors alongside log P == logged/level predictors (FWL)
  full[ is.na( dup_of ) & outlier == "none" & ident == "cs" & pred_form == "sami" & logP_rhs == TRUE, dup_of := "pred_log" ]
  full[ , logP_rhs := NULL ]
  full[ , spec_id := .I ]
  full[]
}

#' @rdname mv_grid
#' @export
mv_addon_blocks <- function( cfg ) {
  B <- cfg$blocks
  names( B )[ vapply( B, function( z ) length( z$default ) == 0, logical( 1 ) ) ]
}

#' @rdname mv_grid
#' @export
mv_op_variants <- function( cfg ) {
  B <- cfg$blocks
  v <- data.table( op = "default", op_block = NA_character_ )
  for ( b in names( B ) ) for ( a in names( B[[ b ]]$alt ) )
    v <- rbind( v, data.table( op = paste0( b, ":", a ), op_block = b ) )
  v
}

#' @rdname mv_grid
#' @export
mv_block_sets <- function( cfg ) data.table( drop = c( "none", setdiff( names( cfg$blocks ), mv_addon_blocks( cfg ) ) ) )

#' @rdname mv_grid
#' @export
mv_covars <- function( s, cfg ) {
  B <- cfg$blocks
  blocks <- setdiff( names( B ), s$drop )
  unlist( lapply( blocks, function( b ) {
    if ( !is.na( s$op_block ) && s$op_block == b ) B[[ b ]]$alt[[ sub( "^[^:]+:", "", s$op ) ]]
    else B[[ b ]]$default } ) )
}

#' Read the CBSA-year research frame for the models
#'
#' Reads `cbsa_year.parquet` and adds `logP` (log population) and `division` (Census
#' division). Use [mv_read_frame()] to rebuild the frame from the released slices first.
#'
#' @param path Path of `cbsa_year.parquet`, or a data.frame already read.
#' @return A data.table.
#' @export
mv_load_panel <- function( path ) {
  d <- if ( is.data.frame( path ) ) data.table::copy( data.table::as.data.table( path ) )
       else data.table::as.data.table( arrow::read_parquet( path ) )
  d[ , logP := log( population ) ]
  d[ , division := unname( DIVISION[ state_fips ] ) ]
  miss <- setdiff( c( "hist_pre1970", "lag_g5", "lag_g10", "gini_m" ), names( d ) )
  if ( length( miss ) ) stop( if ( is.character( path ) ) basename( path ) else "panel", " lacks ",
                              paste( miss, collapse = ", " ), ": rebuild it with R/build/E_assemble/30_assemble_analysis.R" )
  d[]
}

#' Fit one multiverse specification
#'
#' Builds the outcome and predictors in the forms the specification asks for, fits it
#' (OLS with fixed effects, Poisson, or a random state intercept) and returns one row per
#' covariate (and log population, when on the right-hand side).
#'
#' Predictors are fitted on unit-SD scale for numerical conditioning and reported in
#' original units; `std_beta` is the in-sample standardized coefficient. Primary standard
#' errors are heteroskedasticity-robust (cross-section), metro-clustered (panels) or
#' state-clustered (state and division fixed effects); every fixest fit also reports
#' state-clustered errors (`se_state`, `p_state`).
#'
#' @param s One row of [mv_grid()].
#' @param panel The research frame from [mv_load_panel()].
#' @param cfg A configuration from [mv_spec_config()]; must match the grid.
#' @return A data.table (`term`, `estimate`, `se`, `stat`, `p`, `se_state`, `p_state`,
#'   `r2`, `n`, `std_beta`, `spec_id`), or `NULL` when the sample has fewer than 30 rows
#'   or the fit fails.
#' @export
mv_run_spec <- function( s, panel, cfg ) {
  s <- as.list( s )
  covs <- cfg$covs
  xs <- mv_covars( s, cfg )
  if ( s$ident %in% c( "within", "within_long" ) ) xs <- setdiff( xs, cfg$time_invariant_vars )
  ## within_long (2000-2022) is opt-in: not in ident, run by 57_run_within_long.R
  yrs <- switch( s$ident, cs = s$cs_year, pooled = 2015:2022, within = 2010:2022, within_long = 2000:2022 )
  d <- panel[ year %in% yrs ]
  if ( s$unit == "metro" ) d <- d[ cbsa_type == "metro" ]
  if ( !"division" %in% names( d ) ) d[ , division := unname( DIVISION[ state_fips ] ) ]
  d <- d[ , c( "cbsa", "year", "state_fips", "division", "logP", "population", s$outcome, xs ), with = FALSE ]
  d <- d[ stats::complete.cases( d ) & population > 0 ]
  y <- d[[ s$outcome ]]

  ## dependent variable
  if ( s$dv_form %in% c( "logpc", "loglog", "sami" ) ) { keep <- y > 0; d <- d[ keep ]; y <- y[ keep ] }
  yv <- switch( s$dv_form,
    pc        = y / d$population,
    logpc     = log( y / d$population ),
    loglog    = log( y ),
    sami      = resid_logP( log( y ), d$logP, d$year ),
    pois_off  = y,
    pois_free = y )
  if ( s$outlier == "winsor1" && !s$dv_form %in% c( "pois_off", "pois_free" ) ) yv <- winsor( yv )

  ## predictors
  X <- list()
  for ( v in xs ) {
    x <- d[[ v ]]; ty <- covs[ var == v, type ]
    xv <- if ( ty == "B" ) switch( s$pred_form,
            raw = x, pc = x / d$population, log = flog( x, d$population ),
            sami = resid_logP( flog( x, d$population ), d$logP, d$year ) )
          else if ( s$pred_form == "sami" ) resid_logP( x, d$logP, d$year ) else x
    if ( s$outlier == "winsor1" ) xv <- winsor( xv )
    X[[ v ]] <- xv
  }
  m <- data.table( y = yv, as.data.table( X ), logP = d$logP, cbsa = d$cbsa, year = d$year,
                   state = d$state_fips, division = d$division, lpop = log( d$population ) )
  m <- m[ is.finite( y ) & Reduce( `&`, lapply( xs, function( v ) is.finite( m[[ v ]] ) ) ) ]
  if ( nrow( m ) < 30 ) return( NULL )

  ## numerical conditioning: fit on unit-SD predictors, report in original units
  sc <- vapply( xs, function( v ) { s0 <- stats::sd( m[[ v ]] ); if ( is.finite( s0 ) && s0 > 0 ) s0 else 1 },
                numeric( 1 ) )
  for ( v in xs ) set( m, j = v, value = m[[ v ]] / sc[[ v ]] )

  use_logP <- s$rhs_logP || s$dv_form %in% c( "loglog", "pois_free" )
  rhs <- paste( c( xs, if ( use_logP ) "logP" ), collapse = " + " )
  fe <- switch( s$ident,
    cs     = switch( s$est, state_fe = "state", div_fe = "division", "0" ),
    pooled = switch( s$est, state_fe = "state + year", div_fe = "division + year", "year" ),
    within = "cbsa + year", within_long = "cbsa + year" )
  vc <- switch( s$ident,
    cs     = switch( s$est, ols = "hetero", state_fe = ~state, div_fe = ~state, state_re = NA ),
    pooled = switch( s$est, ols = ~cbsa, state_fe = ~state, div_fe = ~state, state_re = NA ),
    within = ~cbsa, within_long = ~cbsa )

  f <- stats::as.formula( paste( "y ~", rhs, "|", fe ) )
  fit <- tryCatch( {
    if ( s$est == "state_re" ) {
      fr <- stats::as.formula( paste( "y ~", rhs, if ( s$ident == "pooled" ) "+ factor(year)", "+ (1|state)" ) )
      r <- lme4::lmer( fr, data = m, REML = FALSE )
      cf <- summary( r )$coefficients
      data.table( term = rownames( cf ), estimate = cf[ , 1 ], se = cf[ , 2 ], stat = cf[ , 3 ],
                  p = 2 * stats::pnorm( -abs( cf[ , 3 ] ) ), se_state = NA_real_, p_state = NA_real_,
                  r2 = NA_real_, n = nrow( m ) )
    } else {
      r <- if ( s$dv_form == "pois_off" ) fixest::fepois( f, data = m, offset = ~lpop, vcov = vc )
           else if ( s$dv_form == "pois_free" ) fixest::fepois( f, data = m, vcov = vc )
           else fixest::feols( f, data = m, vcov = vc )
      ct <- fixest::coeftable( r )
      cs2 <- tryCatch( fixest::coeftable( summary( r, vcov = ~state ) ), error = function( e ) ct * NA )
      data.table( term = rownames( ct ), estimate = ct[ , 1 ], se = ct[ , 2 ], stat = ct[ , 3 ],
                  p = ct[ , 4 ], se_state = cs2[ rownames( ct ), 2 ], p_state = cs2[ rownames( ct ), 4 ],
                  r2 = tryCatch( as.numeric( fixest::r2( r, if ( inherits( r, "fixest" ) && r$method == "fepois" ) "pr2" else "r2" ) ),
                                 error = function( e ) NA_real_ ),
                  n = stats::nobs( r ) )
    }
  }, error = function( e ) NULL )
  if ( is.null( fit ) ) return( NULL )
  fit <- fit[ term %in% c( xs, "logP" ) ]
  ## Poisson coefficients are semi-elasticities: scale by the dispersion of the log outcome
  sdy <- switch( s$dv_form,
    pois_off  = stats::sd( log( pmax( m$y, 1e-9 ) ) - m$lpop ),
    pois_free = stats::sd( log( pmax( m$y, 1e-9 ) ) ),
    stats::sd( m$y ) )
  fit[ , std_beta := estimate * vapply( term, function( v ) stats::sd( m[[ v ]] ), numeric( 1 ) ) / sdy ]
  k <- ifelse( fit$term %in% xs, sc[ fit$term ], 1 )
  fit[ , `:=`( estimate = estimate / k, se = se / k, se_state = se_state / k ) ]
  fit[ , spec_id := s$spec_id ]
  fit[]
}

## one batch: fits its specs, writes status_<b> then coefs_<b> (coefs marks it complete)
.mv_batch <- function( b, grid, panel, cfg, bat_dir, fit = mv_run_spec ) {
  g <- grid[ batch == b ]
  t0 <- proc.time()[[ "elapsed" ]]
  status <- character( nrow( g ) )
  res <- vector( "list", nrow( g ) )
  for ( i in seq_len( nrow( g ) ) ) {
    r <- tryCatch( fit( g[ i ], panel, cfg ), error = function( e ) e )
    if ( inherits( r, "error" ) ) status[ i ] <- paste( "error:", conditionMessage( r ) )
    else if ( is.null( r ) || !nrow( r ) ) status[ i ] <- "no fit"
    else { status[ i ] <- "ok"; res[[ i ]] <- r }
  }
  co <- rbindlist( res )
  st <- data.table( spec_id = g$spec_id, status = status )
  arrow::write_parquet( st, file.path( bat_dir, sprintf( "status_%04d.parquet", b ) ) )
  arrow::write_parquet( co, file.path( bat_dir, sprintf( "coefs_%04d.parquet", b ) ) )
  data.table( batch = b, specs = nrow( g ), ok = sum( status == "ok" ),
              sec = proc.time()[[ "elapsed" ]] - t0, worker = Sys.getpid() )
}

## on a worker: the run objects sit in its global environment (set by mv_run_universe);
## defined at top level so that only a reference to the namespace is sent with each task
.mv_worker_batch <- function( b ) .mv_batch( b, .mv_grid, .mv_panel, .mv_cfg, .mv_bat_dir, .mv_fit )

#' Run a multiverse universe in resumable batches
#'
#' Splits the grid into batches, fits them on `workers` parallel R processes, and combines
#' them. Each batch is written to `dir/batches/` (`status_NNNN.parquet`, then
#' `coefs_NNNN.parquet`, whose presence marks the batch complete); finished batches are
#' skipped when the run is started again. At the end it writes into `dir`:
#' `specs_grid.parquet`, `specs.parquet` (one row per specification with its status),
#' `coefs.parquet` (one row per specification and term), `run_summary.csv`,
#' `batch_timing.csv` and, if any, `errors.csv`.
#'
#' @param grid The specifications to run: [mv_grid()] without the duplicates
#'   (`grid[ is.na( dup_of ) ]`).
#' @param panel The research frame from [mv_load_panel()].
#' @param dir Run folder (created).
#' @param cfg The configuration the grid was built from.
#' @param batch Specifications per batch.
#' @param workers Parallel workers; 1 runs in this process.
#' @param fit The function fitting one specification, `function( s, panel, cfg )`.
#' @param verbose Print the run summary.
#' @return The specs table, invisibly.
#' @export
mv_run_universe <- function( grid, panel, dir, cfg, batch = 2000L, workers = max( 1L, parallel::detectCores() - 2L ),
                             fit = mv_run_spec, verbose = TRUE ) {
  grid <- data.table::copy( data.table::as.data.table( grid ) )
  bat_dir <- file.path( dir, "batches" )
  dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )
  grid[ , batch := ( seq_len( .N ) - 1L ) %/% as.integer( batch ) + 1L ]
  arrow::write_parquet( grid, file.path( dir, "specs_grid.parquet" ) )
  nb <- max( grid$batch )
  todo <- setdiff( seq_len( nb ),
                   as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                    list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
  log_msg( format( nrow( grid ), big.mark = "," ), " specs in ", nb, " batches; ",
           length( todo ), " to run on ", workers, " workers" )

  t_all <- system.time( if ( length( todo ) ) {
    if ( workers <= 1L ) {
      fixest::setFixest_notes( FALSE )
      bt <- rbindlist( lapply( todo, .mv_batch, grid = grid, panel = panel, cfg = cfg, bat_dir = bat_dir, fit = fit ) )
    } else {
      cl <- parallel::makePSOCKcluster( workers )
      on.exit( try( parallel::stopCluster( cl ), silent = TRUE ), add = TRUE )
      parallel::clusterCall( cl, function( libs ) {
        Sys.setenv( OMP_NUM_THREADS = "1" ); .libPaths( libs )
        suppressPackageStartupMessages( library( npdensity ) )
        fixest::setFixest_notes( FALSE ); fixest::setFixest_nthreads( 1 ); data.table::setDTthreads( 1L )
        NULL }, .libPaths() )
      parallel::clusterCall( cl, function( ... ) { list2env( list( ... ), globalenv() ); NULL },
                             .mv_grid = grid, .mv_panel = panel, .mv_cfg = cfg, .mv_bat_dir = bat_dir, .mv_fit = fit )
      bt <- rbindlist( parallel::parLapplyLB( cl, todo, .mv_worker_batch ) )
      parallel::stopCluster( cl )
    }
    f <- file.path( dir, "batch_timing.csv" )
    old <- if ( file.exists( f ) ) fread( f ) else NULL
    fwrite( rbind( old, bt ), f )
  } )
  log_msg( "batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )

  ## combine
  rd <- function( p ) lapply( list.files( bat_dir, pattern = p, full.names = TRUE ),
                              function( f ) as.data.table( arrow::read_parquet( f ) ) )
  co <- rbindlist( rd( "^coefs_" ), fill = TRUE )
  st <- rbindlist( rd( "^status_" ) )
  specs <- merge( grid, st, by = "spec_id", all.x = TRUE )
  specs[ is.na( status ), status := "not run" ]
  arrow::write_parquet( specs, file.path( dir, "specs.parquet" ) )
  arrow::write_parquet( co, file.path( dir, "coefs.parquet" ) )

  summ <- specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                      errors = sum( grepl( "^error", status ) ), not_run = sum( status == "not run" ) ),
                 keyby = .( construct, hu, ident ) ]
  summ <- rbind( summ, summ[ , c( .( construct = "ALL", hu = "", ident = "" ),
                                  lapply( .SD, sum ) ), .SDcols = c( "specs", "ok", "no_fit", "errors", "not_run" ) ] )
  fwrite( summ, file.path( dir, "run_summary.csv" ) )
  if ( verbose ) print( summ )
  log_msg( "coefficient rows: ", format( nrow( co ), big.mark = "," ) )
  errs <- specs[ grepl( "^error", status ), .N, by = status ][ order( -N ) ]
  if ( nrow( errs ) ) { fwrite( errs, file.path( dir, "errors.csv" ) ); if ( verbose ) print( utils::head( errs, 10 ) ) }
  invisible( specs )
}
