## 55_endowment_fork.R
## Historical endowment covariate and its fork.
## The covariate: density of 501(c)(3) public charities in the 2000 BMF stock
## that were ruled before 1970 (see 52_endowment_change.R). The fork is part of
## 40_multiverse.R (block "history", added 2026-11-12): an add-on block with no
## default whose alternative "pre1970" adds hist_pre1970 to the full default
## model, under every non-covariate fork except the within stratum. Its specs
## are appended after the base grid, so existing spec_ids are unchanged.
## Metros where > 60% of the 2000 stock predates 1970 are group-exemption
## artifacts (Mankato, Memphis) and are set to NA.
##
##   Rscript R/55_endowment_fork.R build             -> data/cov/cov_endowment.csv
##   Rscript R/55_endowment_fork.R run [workers]     endowment specs -> run_principled_endow/
##   Rscript R/55_endowment_fork.R merge             v3 + endowment -> run_principled_v4/ (canonical spec_ids)
##   Rscript R/55_endowment_fork.R compare           matched default vs +endowment pairs

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )

args <- commandArgs( TRUE )
MODE <- if ( length( args ) ) args[1] else "run"
WORKERS <- if ( length( args ) >= 2 ) as.integer( args[2] ) else 4L
BATCH <- 1000L
COV_FILE <- file.path( OUT$cov, "cov_endowment.csv" )
SHARE_MAX <- 0.6
KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
           "est", "cs_year", "outlier", "drop", "op" )
v3_dir  <- file.path( OUT$spec, "run_principled_v3" )
end_dir <- file.path( OUT$spec, "run_principled_endow" )
v4_dir  <- file.path( OUT$spec, "run_principled_v4" )
bat_dir <- file.path( end_dir, "batches" )

## ---- build the covariate --------------------------------------------------------
if ( MODE == "build" ) {
  bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "subsec", "pf", "ryear" ),
                colClasses = list( character = c( "EIN", "geoid", "subsec" ) ) )
  b0 <- bmf[ year == 2000 & subsec %in% c( "03", "3" ) & pf == FALSE & !is.na( geoid ) ]
  sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )
  b0 <- merge( b0, sp[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
  e <- b0[ , .( n_bmf2000 = .N, n_pre1970 = sum( ryear < 1970, na.rm = TRUE ),
                n_pre1960 = sum( ryear < 1960, na.rm = TRUE ) ), by = cbsa ]
  e[ , share_pre1970 := n_pre1970 / n_bmf2000 ]
  e[ , artifact := share_pre1970 > SHARE_MAX ]
  e[ , hist_pre1970 := fifelse( artifact, NA_real_, as.numeric( n_pre1970 ) ) ]
  fwrite( e, COV_FILE )
  log_msg( "CBSAs: ", nrow( e ), "; flagged as group-exemption artifacts: ", e[ artifact == TRUE, .N ] )
  print( e[ artifact == TRUE ][ order( -share_pre1970 ) ] )
  quit( save = "no" )
}

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
new  <- grid[ op == "history:pre1970" ]
log_msg( "grid: ", format( nrow( grid ), big.mark = "," ), " specs; endowment specs: ", format( nrow( new ), big.mark = "," ) )

if ( MODE == "run" ) {
  dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )
  panel <- load_panel()
  new[ , batch := ( seq_len( .N ) - 1L ) %/% BATCH + 1L ]
  write_parquet( new, file.path( end_dir, "specs_grid.parquet" ) )
  nb <- max( new$batch )
  todo <- setdiff( seq_len( nb ), as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                  list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
  log_msg( "endowment: ", nb, " batches; ", length( todo ), " to run on ", WORKERS, " workers" )
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
  t_all <- system.time( if ( length( todo ) ) {
    cl <- makePSOCKcluster( WORKERS ); on.exit( try( stopCluster( cl ), silent = TRUE ), add = TRUE )
    invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
      setwd( "C:/Users/jdlec/Dropbox (Personal)/04 - PAPERS/01 - In Progress/38 - resource environments/multiverse" )
      source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
    clusterExport( cl, c( "panel", "new", "bat_dir", "run_batch" ) )
    invisible( parLapplyLB( cl, todo, run_batch ) ); stopCluster( cl ) } )
  log_msg( "endowment batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )
}

if ( MODE == "merge" ) {
  ## batch spec_ids may come from an earlier numbering: map everything by fork settings
  rd <- function( d, p ) rbindlist( lapply( list.files( d, pattern = p, full.names = TRUE ), function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
  remap <- function( specs_keys, co, st, label ) {
    mp <- merge( specs_keys[ , c( "spec_id", KEYS ), with = FALSE ], grid[ , c( "spec_id", KEYS ), with = FALSE ],
                 by = KEYS, suffixes = c( "_old", "" ) )
    log_msg( label, ": ", nrow( specs_keys ), " specs, mapped ", nrow( mp ), ", renumbered ", mp[ spec_id_old != spec_id, .N ] )
    stopifnot( nrow( mp ) == nrow( specs_keys ) )
    co <- merge( co, mp[ , .( spec_id_old, spec_id ) ], by.x = "spec_id", by.y = "spec_id_old" )[ , spec_id := spec_id.y ][ , spec_id.y := NULL ]
    st <- merge( st, mp[ , .( spec_id_old, spec_id ) ], by.x = "spec_id", by.y = "spec_id_old" )[ , spec_id := spec_id.y ][ , spec_id.y := NULL ]
    list( co = co, st = st ) }
  bs <- as.data.table( read_parquet( file.path( v3_dir, "specs.parquet" ) ) )
  a <- remap( bs, as.data.table( read_parquet( file.path( v3_dir, "coefs.parquet" ) ) ), bs[ , .( spec_id, status ) ], "v3" )
  eg <- as.data.table( read_parquet( file.path( end_dir, "specs_grid.parquet" ) ) )
  b <- remap( eg, rd( bat_dir, "^coefs_" ), rd( bat_dir, "^status_" ), "endowment" )
  stopifnot( nrow( bs ) + nrow( eg ) == nrow( grid ) )
  specs <- merge( grid, rbind( a$st, b$st ), by = "spec_id", all.x = TRUE ); specs[ is.na( status ), status := "not run" ]
  co <- rbind( a$co, b$co[ , names( a$co ), with = FALSE ] )
  dir.create( v4_dir, showWarnings = FALSE )
  write_parquet( specs, file.path( v4_dir, "specs.parquet" ) ); write_parquet( co, file.path( v4_dir, "coefs.parquet" ) )
  summ <- specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                      errors = sum( grepl( "^error", status ) ), not_run = sum( status == "not run" ) ),
                 keyby = .( construct, hu, ident, endowment = op == "history:pre1970" ) ]
  fwrite( summ, file.path( v4_dir, "run_summary.csv" ) )
  log_msg( "v4: ", format( nrow( specs ), big.mark = "," ), " specs (", specs[ status == "ok", .N ], " ok), ",
           format( nrow( co ), big.mark = "," ), " coefficient rows" )
}

if ( MODE == "compare" ) {
  specs <- as.data.table( read_parquet( file.path( v4_dir, "specs.parquet" ) ) )
  co <- as.data.table( read_parquet( file.path( v4_dir, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
  s2 <- specs[ op %in% c( "default", "history:pre1970" ) & drop == "none" & ident != "within" ]
  x <- merge( co, s2, by = "spec_id" )
  eterm <- x[ term == "hist_pre1970", { med <- median( pr ); .( specs = .N, median_r = med, sign_stability = mean( sign( pr ) == sign( med ) ),
              sig05 = mean( p < .05, na.rm = TRUE ), sig05_state = mean( p_state < .05, na.rm = TRUE ) ) },
              keyby = .( construct, hu, ident ) ]
  eterm_fork <- x[ term == "hist_pre1970", .( median_r = median( pr ), pos = mean( pr > 0 ) ), keyby = .( construct, ident, pred_form ) ]
  pk <- setdiff( KEYS, "op" )
  w <- dcast( x[ term != "hist_pre1970" ], as.formula( paste( paste( c( pk, "term" ), collapse = " + " ), "~ op" ) ), value.var = "pr" )
  setnames( w, c( "default", "history:pre1970" ), c( "r0", "r1" ) )
  w <- w[ is.finite( r0 ) & is.finite( r1 ) ]
  shift <- w[ , .( pairs = .N, median_r_default = median( r0 ), median_r_endow = median( r1 ),
                   median_change = median( r1 - r0 ), share_shrink = mean( abs( r1 ) < abs( r0 ) ),
                   sign_flips = mean( sign( r1 ) != sign( r0 ) ), cor = cor( r0, r1 ) ),
              keyby = .( construct, hu, ident, term ) ]
  fwrite( eterm, file.path( v4_dir, "endowment_term.csv" ) ); fwrite( eterm_fork, file.path( v4_dir, "endowment_term_by_predform.csv" ) )
  fwrite( shift, file.path( v4_dir, "endowment_shift.csv" ) )
  print( eterm[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
}
