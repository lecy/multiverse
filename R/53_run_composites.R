## 53_run_composites.R
## Add the composite-index operationalizations (41_composites.R) to a finished
## principled run without re-running the specifications it already has.
##   Rscript R/53_run_composites.R test             8 composite specs + FWL duplicate check
##   Rscript R/53_run_composites.R run [workers]    every composite spec -> run_principled_cmp/
##   Rscript R/53_run_composites.R merge [base_dir] base run + composites -> run_principled_cmp_merged/
## base_dir defaults to run_principled_v3 if present, else run_principled. The
## merge maps the base specifications onto the extended grid's spec_ids by
## fork settings (spec_ids in the extended grid differ from the default grid).

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
source( "R/41_composites.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )

args <- commandArgs( TRUE )
MODE <- if ( length( args ) ) args[1] else "run"
WORKERS <- if ( MODE == "run" && length( args ) >= 2 ) as.integer( args[2] ) else max( 1L, detectCores() - 2L )
BATCH <- 2000L

KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
           "est", "cs_year", "outlier", "drop", "op" )
cmp_dir <- file.path( OUT$spec, "run_principled_cmp" )
out_dir <- file.path( OUT$spec, "run_principled_cmp_merged" )
bat_dir <- file.path( cmp_dir, "batches" )
dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )

full <- build_grid( "principled" )
grid <- full[ is.na( dup_of ) ]
cmp  <- grid[ op %in% COMP_OPS ]
log_msg( "extended grid: ", format( nrow( grid ), big.mark = "," ), " specs; composite: ",
         format( nrow( cmp ), big.mark = "," ), " (", length( COMP_OPS ), " options)" )

if ( MODE == "test" ) {
  panel <- load_panel(); cp <- composite_panels( panel )
  fwrite( attr( cp, "constants" ), file.path( cmp_dir, "composite_constants.csv" ) )
  ## coverage and correlations of the log-base composites (metros, 2019)
  m19 <- cp$log[ cbsa_type == "metro" & year == 2019, names( COMPOSITE_DEFS ), with = FALSE ]
  print( sapply( m19, function( z ) sum( !is.na( z ) ) ) ); print( round( cor( m19, use = "pairwise" ), 2 ) )
  set.seed( 1 )
  t <- cmp[ c( which( ident == "cs" & dv_form == "loglog" & pred_form == "log" )[ 1 ],
               which( ident == "pooled" & dv_form == "sami" & pred_form == "sami" )[ 1 ],
               which( ident == "within" & dv_form == "sami" )[ 1 ],
               which( dv_form == "pois_off" & ident == "pooled" )[ 1 ], sample( nrow( cmp ), 4 ) ) ]
  for ( i in seq_len( nrow( t ) ) ) { r <- run_spec_comp( t[ i ], cp )
    cat( "\n--", t$op[ i ], t$ident[ i ], t$dv_form[ i ], t$pred_form[ i ], t$est[ i ], t$outcome[ i ], "\n" )
    print( r[ , .( term, estimate = signif( estimate, 3 ), stat = round( stat, 2 ), p = signif( p, 2 ), n ) ] ) }
  ## FWL check: SAMI-arm composite + log P == log-arm composite + log P (cs)
  d <- full[ op %in% COMP_OPS & dup_of == "pred_log" ][ sample( .N, 10 ) ]
  chk <- rbindlist( lapply( seq_len( nrow( d ) ), function( i ) {
    s <- d[ i ]; tw <- copy( s ); tw$pred_form <- "log"
    tw <- full[ tw[ , ..KEYS ], on = KEYS ]
    a <- run_spec_comp( s, cp ); b <- run_spec_comp( tw, cp )
    m <- merge( a[ term != "logP", .( term, ea = estimate ) ], b[ term != "logP", .( term, eb = estimate ) ], by = "term" )
    data.table( op = s$op, dv_form = s$dv_form, max_rel_diff = max( abs( m$ea - m$eb ) / pmax( abs( m$eb ), 1e-12 ) ) ) } ) )
  print( chk )
}

if ( MODE == "run" ) {
  panel <- load_panel(); cpanels <- composite_panels( panel ); rm( panel )
  fwrite( attr( cpanels, "constants" ), file.path( cmp_dir, "composite_constants.csv" ) )
  cmp[ , batch := ( seq_len( .N ) - 1L ) %/% BATCH + 1L ]
  write_parquet( cmp, file.path( cmp_dir, "specs_grid.parquet" ) )
  nb <- max( cmp$batch )
  todo <- setdiff( seq_len( nb ), as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                  list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
  log_msg( "composites: ", nb, " batches; ", length( todo ), " to run on ", WORKERS, " workers" )
  run_batch <- function( b ) {
    g <- cmp[ batch == b ]; status <- character( nrow( g ) ); res <- vector( "list", nrow( g ) )
    for ( i in seq_len( nrow( g ) ) ) {
      r <- tryCatch( run_spec_comp( g[ i ], cpanels ), error = function( e ) e )
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
      source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); source( "R/41_composites.R" )
      data.table::setDTthreads( 1L ); NULL } ) )
    clusterExport( cl, c( "cpanels", "cmp", "bat_dir", "run_batch" ) )
    invisible( parLapplyLB( cl, todo, run_batch ) ); stopCluster( cl ) } )
  log_msg( "composite batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )
}

if ( MODE == "merge" ) {
  base_dir <- if ( length( args ) >= 2 ) args[2] else {
    v3 <- file.path( OUT$spec, "run_principled_v3" )
    if ( file.exists( file.path( v3, "specs.parquet" ) ) ) v3 else file.path( OUT$spec, "run_principled" ) }
  log_msg( "base run: ", base_dir )
  rd <- function( d, p ) rbindlist( lapply( list.files( d, pattern = p, full.names = TRUE ),
                                           function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
  cco <- rd( bat_dir, "^coefs_" ); cst <- rd( bat_dir, "^status_" )
  bs <- as.data.table( read_parquet( file.path( base_dir, "specs.parquet" ) ) )
  bc <- as.data.table( read_parquet( file.path( base_dir, "coefs.parquet" ) ) )
  map <- merge( bs[ , c( "spec_id", "status", KEYS ), with = FALSE ], grid[ , c( "spec_id", KEYS ), with = FALSE ],
                by = KEYS, suffixes = c( "_base", "" ) )
  log_msg( "base specs: ", nrow( bs ), "; mapped: ", nrow( map ), "; extended non-composite specs: ",
           grid[ !op %in% COMP_OPS, .N ] )
  stopifnot( nrow( map ) == nrow( bs ) )
  bc <- merge( bc, map[ , .( spec_id_base, spec_id ) ], by.x = "spec_id", by.y = "spec_id_base" )
  bc[ , spec_id := spec_id.y ][ , spec_id.y := NULL ]
  st <- rbind( map[ , .( spec_id, status ) ], cst )
  specs <- merge( grid, st, by = "spec_id", all.x = TRUE )
  specs[ is.na( status ), status := "not run" ]
  co <- rbind( bc[ , intersect( names( bc ), names( cco ) ), with = FALSE ],
               cco[ , intersect( names( bc ), names( cco ) ), with = FALSE ] )
  dir.create( out_dir, showWarnings = FALSE )
  write_parquet( specs, file.path( out_dir, "specs.parquet" ) ); write_parquet( co, file.path( out_dir, "coefs.parquet" ) )
  summ <- specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                      errors = sum( grepl( "^error", status ) ), not_run = sum( status == "not run" ) ),
                 keyby = .( construct, hu, ident, composite = op %in% COMP_OPS ) ]
  fwrite( summ, file.path( out_dir, "run_summary.csv" ) ); print( summ )
  log_msg( "merged: ", format( nrow( specs ), big.mark = "," ), " specs, ", format( nrow( co ), big.mark = "," ),
           " coefficient rows -> ", out_dir )
}
