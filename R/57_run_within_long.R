## 57_run_within_long.R
## Within-metro identification over a longer window: every "within" spec of the
## principled universe (default grid + composite options from 41_composites.R)
## re-run with ident = "within_long" (two-step year-specific SAMIs, metro + year
## FE, metro-clustered SEs, 2000-2022 instead of 2010-2022).
## Why: the share of covariate variance within metros grows 2-3x with the
## longer window (71_fe_frequency.R); sampling every 5 years does not help.
## Coverage: contributions (2010+) and net-of-pass-through outcomes (2012+)
## have no earlier years, so those specs keep their shorter sample (flagged).
## Caveats: crosses the CORE -> SOI splice (2011); ACS measures (% BA,
## % foreign-born, Gini) are interpolated 2001-2009 from the 2000 census;
## RCMS 2000 lacks Black Protestant counts (rel_2000_noblack).
##   Rscript R/57_run_within_long.R test            8 specs, side by side with their 2010-2022 twins
##   Rscript R/57_run_within_long.R run [workers]   all within_long specs -> run_principled_wl/
##   Rscript R/57_run_within_long.R summary         paired comparison with the 2010-2022 within specs

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
source( "R/41_composites.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )
options( width = 220 )

args <- commandArgs( TRUE )
MODE <- if ( length( args ) ) args[1] else "run"
WORKERS <- if ( MODE == "run" && length( args ) >= 2 ) as.integer( args[2] ) else max( 1L, detectCores() - 2L )
BATCH <- 1000L
KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
           "est", "cs_year", "outlier", "drop", "op" )
wl_dir  <- file.path( OUT$spec, "run_principled_wl" )
bat_dir <- file.path( wl_dir, "batches" )
base_dir <- file.path( OUT$spec, "run_principled_cmp_merged" )      # holds the 2010-2022 within twins
dir.create( bat_dir, recursive = TRUE, showWarnings = FALSE )
OFF <- 3e7

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
## specs whose data start in 2010/2012 would reproduce their twins exactly: skip
NO_EXT <- grid$ident == "within" & ( grepl( "_net$", grid$outcome ) |
          grid$op %in% c( "philanthropy:contrib_total", "philanthropy:contrib_per_return", "philanthropy:contrib_share_agi",
                          "philanthropy:giving_index", "philanthropy:both_index" ) )
log_msg( "within specs without pre-2010 data (skipped): ", sum( NO_EXT ) )
wl <- copy( grid[ ident == "within" & !NO_EXT ] )
wl[ , twin_id := spec_id ][ , ident := "within_long" ][ , spec_id := OFF + seq_len( .N ) ]
log_msg( "within specs (2010-2022): ", format( nrow( wl ), big.mark = "," ), " -> within_long copies" )

if ( MODE == "test" ) {
  panel <- load_panel(); cp <- composite_panels( panel )
  set.seed( 2 )
  t <- wl[ c( which( op == "default" & dv_form == "sami" & pred_form == "sami" & outcome == "n_filer_c3" )[ 1 ],
              which( op == "default" & dv_form == "pois_off" & outcome == "exp_c3_nohu" )[ 1 ],
              which( op %in% COMP_OPS & outcome == "n_bmf_c3pc" )[ 1 ],
              which( grepl( "contrib", op ) )[ 1 ], which( outcome == "exp_c3_net" )[ 1 ], sample( .N, 3 ) ) ]
  for ( i in seq_len( nrow( t ) ) ) {
    tw <- grid[ spec_id == t$twin_id[ i ] ]
    a <- run_spec_comp( t[ i ], cp ); b <- run_spec_comp( tw, cp )
    m <- merge( a[ , .( term, est_long = signif( estimate, 3 ), t_long = round( stat, 2 ), n_long = n ) ],
                b[ , .( term, est_13 = signif( estimate, 3 ), t_13 = round( stat, 2 ), n_13 = n ) ], by = "term" )
    cat( "\n--", t$op[ i ], t$dv_form[ i ], t$pred_form[ i ], t$unit[ i ], t$outcome[ i ], "\n" ); print( m )
  }
}

if ( MODE == "run" ) {
  panel <- load_panel(); cpanels <- composite_panels( panel ); rm( panel )
  wl[ , batch := ( seq_len( .N ) - 1L ) %/% BATCH + 1L ]
  write_parquet( wl, file.path( wl_dir, "specs_grid.parquet" ) )
  nb <- max( wl$batch )
  todo <- setdiff( seq_len( nb ), as.integer( sub( "^coefs_(\\d+)\\.parquet$", "\\1",
                                  list.files( bat_dir, pattern = "^coefs_\\d+\\.parquet$" ) ) ) )
  log_msg( "within_long: ", nb, " batches; ", length( todo ), " to run on ", WORKERS, " workers" )
  run_batch <- function( b ) {
    g <- wl[ batch == b ]; status <- character( nrow( g ) ); res <- vector( "list", nrow( g ) )
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
    clusterExport( cl, c( "cpanels", "wl", "bat_dir", "run_batch" ) )
    invisible( parLapplyLB( cl, todo, run_batch ) ); stopCluster( cl ) } )
  log_msg( "within_long batches finished in ", round( t_all[[ "elapsed" ]] / 60, 1 ), " min" )
  rd <- function( p ) rbindlist( lapply( list.files( bat_dir, pattern = p, full.names = TRUE ),
                                         function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
  st <- rd( "^status_" ); co <- rd( "^coefs_" )
  specs <- merge( wl, st, by = "spec_id", all.x = TRUE ); specs[ is.na( status ), status := "not run" ]
  write_parquet( specs, file.path( wl_dir, "specs.parquet" ) ); write_parquet( co, file.path( wl_dir, "coefs.parquet" ) )
  print( specs[ , .( specs = .N, ok = sum( status == "ok" ), no_fit = sum( status == "no fit" ),
                     errors = sum( grepl( "^error", status ) ) ), keyby = .( construct, hu ) ] )
}

if ( MODE == "summary" ) {
  ws <- as.data.table( read_parquet( file.path( wl_dir, "specs.parquet" ) ) )[ status == "ok" ]
  wc <- as.data.table( read_parquet( file.path( wl_dir, "coefs.parquet" ) ) )
  bc <- as.data.table( read_parquet( file.path( base_dir, "coefs.parquet" ) ) )[ spec_id %in% ws$twin_id ]
  pr <- function( co ) { co[ , k := .N, by = spec_id ]; co[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]; co }
  wc <- pr( wc ); bc <- pr( bc )
  m <- merge( wc[ term != "logP", .( spec_id, term, est_l = estimate, se_l = se, t_l = stat, p_l = p, ps_l = p_state, pr_l = pr, n_l = n ) ],
              ws[ , .( spec_id, twin_id, construct, hu, outcome, op, dv_form, pred_form, unit ) ], by = "spec_id" )
  m <- merge( m, bc[ term != "logP", .( twin_id = spec_id, term, est_s = estimate, se_s = se, t_s = stat, p_s = p, ps_s = p_state, pr_s = pr, n_s = n ) ],
              by = c( "twin_id", "term" ) )
  m[ , extended := n_l > 1.2 * n_s ]
  m[ , composite := op %in% COMP_OPS ]
  log_msg( "paired term rows: ", nrow( m ), "; specs: ", uniqueN( m$spec_id ), "; extended sample: ",
           round( 100 * mean( m$extended ) ), "% of rows" )
  summ <- function( x ) list( rows = nrow( x ), n_ratio = median( x$n_l / x$n_s ),
    se_ratio = median( x$se_l / x$se_s, na.rm = TRUE ), med_abs_t_13 = median( abs( x$t_s ) ), med_abs_t_23 = median( abs( x$t_l ) ),
    share_t_higher = mean( abs( x$t_l ) > abs( x$t_s ) ), sig_13 = mean( x$p_s < 0.05 ), sig_23 = mean( x$p_l < 0.05 ),
    sig_state_13 = mean( x$ps_s < 0.05, na.rm = TRUE ), sig_state_23 = mean( x$ps_l < 0.05, na.rm = TRUE ),
    same_sign = mean( sign( x$est_l ) == sign( x$est_s ) ), cor_pr = cor( x$pr_l, x$pr_s ) )
  overall <- m[ , summ( .SD ), by = .( construct, extended ) ]
  byterm  <- m[ extended == TRUE, summ( .SD ), by = .( construct, term ) ][ order( construct, -abs( med_abs_t_23 ) ) ]
  ## term-level stability across the within universe, 13 vs 23 years
  stab <- function( pr, p ) { med <- median( pr ); list( median_r = med, sign_stab = mean( sign( pr ) == sign( med ) ),
                                                        sig_same = mean( p < 0.05 & sign( pr ) == sign( med ) ) ) }
  st13 <- m[ extended == TRUE, stab( pr_s, p_s ), by = .( construct, term ) ]
  st23 <- m[ extended == TRUE, stab( pr_l, p_l ), by = .( construct, term ) ]
  stc <- merge( st13, st23, by = c( "construct", "term" ), suffixes = c( "_13", "_23" ) )
  fwrite( overall, file.path( wl_dir, "summary_overall.csv" ) ); fwrite( byterm, file.path( wl_dir, "summary_by_term.csv" ) )
  fwrite( stc, file.path( wl_dir, "summary_term_stability.csv" ) )
  r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
  cat( "\n== Overall: 2000-2022 vs 2010-2022 within specs (paired) ==\n" ); print( r3( overall ) )
  cat( "\n== By term (specs whose sample actually extends) ==\n" ); print( r3( byterm ), nrows = 200 )
  cat( "\n== Term stability across the within universe ==\n" ); print( r3( stc[ order( construct, -abs( median_r_23 ) ) ] ), nrows = 200 )
}
