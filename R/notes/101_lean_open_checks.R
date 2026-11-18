## 101_lean_open_checks.R
## Three checks that rested on the six-covariate model, refit on the 13-covariate default
## (MV_MODEL=lean, run v11). The specs are the v11 grid's own; only the sample, outcome or
## window changes.
##   F  revenue floor: filers above the $50k real floor (n_filer_c3) vs all 990/990-EZ
##      filers (n_filer_c3_nofloor), every default-model 2022 metro cross-section spec
##   M  multi-state metros: the same specs for filers, registrations and spending on all
##      metros vs metros inside one state (state FE is the estimator at issue; all four
##      estimators are reported)
##   W  longer within window: every within spec except contribution measures and net
##      outcomes (their data start in 2010 / 2012), fit on 2010-2022 and 2000-2022. The
##      metro Gini (gini_m) starts in 2010, so both windows use the county-weighted Gini
##      (gini, 2000 on; r = .92 with gini_m) in its place; the 2010-2022 refit is also
##      compared with v11 to show the swap is harmless.
## Partial r with df counted in metros (within / years in the window).
## Outputs D:/MULTIVERSE/data/outline/v11/open_checks/.
## Usage: Rscript R/notes/101_lean_open_checks.R [F] [M] [W] [workers]

Sys.setenv( MV_MODEL = "lean" )
source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )
options( width = 200 )
args <- commandArgs( TRUE )
PARTS <- intersect( args, c( "F", "M", "W" ) ); if ( !length( PARTS ) ) PARTS <- c( "F", "M", "W" )
WORKERS <- suppressWarnings( as.integer( args[ grepl( "^[0-9]+$", args ) ] ) ); if ( !length( WORKERS ) ) WORKERS <- 12L
OD <- file.path( BUILD, "data", "outline", "v11", "open_checks" ); dir.create( OD, recursive = TRUE, showWarnings = FALSE )
TERMS <- unlist( lapply( BLOCKS, `[[`, "default" ), use.names = FALSE )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
verdict <- function( pr, p ) fifelse( p >= .05, 0L, as.integer( sign( pr ) ) )

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
panel <- load_panel()
CS22 <- grid[ ident == "cs" & cs_year == 2022L & op == "default" & drop == "none" & outlier == "none" & unit == "metro" ]

fit_set <- function( sp, p, variant, y_new = NULL ) rbindlist( lapply( seq_len( nrow( sp ) ), function( i ) {
  s <- copy( sp[ i ] ); if ( !is.null( y_new ) ) s[ , outcome := y_new ]
  r <- tryCatch( run_spec( s, p ), error = function( e ) NULL )
  if ( is.null( r ) || !nrow( r ) ) return( NULL )
  r[ , `:=`( spec_id = sp$spec_id[ i ], variant = variant, k = .N ) ][] } ) )
add_pr <- function( r, years ) { r[ , pr := stat / sqrt( stat^2 + pmax( n / years - k - 1, 1 ) ) ]; r[] }

## ---- F: revenue floor ------------------------------------------------------------------------
if ( "F" %in% PARTS ) {
  log_msg( "F: revenue floor" )
  m <- panel[ cbsa_type == "metro" ]
  nat <- m[ year %in% c( 2012, 2019, 2022 ), .( floor = sum( n_filer_c3 ), nofloor = sum( n_filer_c3_nofloor, na.rm = TRUE ) ), keyby = year ]
  nat[ , share_below := 1 - floor / nofloor ]
  m22 <- m[ year == 2022 & n_filer_c3 > 0 & n_filer_c3_nofloor > 0 ]
  bf <- lm( log( n_filer_c3 ) ~ logP, m22 ); bn <- lm( log( n_filer_c3_nofloor ) ~ logP, m22 )
  nat[ , `:=`( beta_floor_2022 = coef( bf )[ 2 ], beta_nofloor_2022 = coef( bn )[ 2 ], sami_r_2022 = cor( resid( bf ), resid( bn ) ) ) ]
  sp <- CS22[ outcome == "n_filer_c3" ]
  r <- add_pr( rbind( fit_set( sp, panel, "floor" ), fit_set( sp, panel, "nofloor", "n_filer_c3_nofloor" ) ), 1 )
  w <- dcast( r[ term %in% TERMS ], spec_id + term ~ variant, value.var = c( "pr", "p" ) )
  f1 <- w[ , .( specs = .N, floor = median( pr_floor ), nofloor = median( pr_nofloor ),
                med_abs_diff = median( abs( pr_nofloor - pr_floor ) ), max_abs_diff = max( abs( pr_nofloor - pr_floor ) ),
                sign_flips = mean( sign( pr_floor ) != sign( pr_nofloor ) ),
                verdict_changes = mean( verdict( pr_floor, p_floor ) != verdict( pr_nofloor, p_nofloor ) ) ), keyby = term ]
  f1 <- f1[ order( -abs( floor ) ) ]
  fwrite( nat, file.path( OD, "f_floor_national.csv" ) ); fwrite( f1, file.path( OD, "f_floor_terms.csv" ) )
  write_parquet( r, file.path( OD, "f_coefs.parquet" ) )
  cat( "\n== F: revenue floor ==\n" ); print( r3( nat ) ); print( r3( f1 ) )
  cat( "all terms: median |diff|", round( w[ , median( abs( pr_nofloor - pr_floor ) ) ], 3 ),
       " max", round( w[ , max( abs( pr_nofloor - pr_floor ) ) ], 3 ), " cor", round( w[ , cor( pr_floor, pr_nofloor ) ], 3 ), "\n" )
}

## ---- M: multi-state metros ---------------------------------------------------------------------
if ( "M" %in% PARTS ) {
  log_msg( "M: multi-state metros" )
  sp0 <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "csa", "state_fips" ) ) )
  cp <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )[ year == 2022 ]
  sp0 <- merge( sp0, cp[ , .( geoid, population ) ], by = "geoid" )
  ms <- merge( sp0[ cbsa_type == "metro" ], panel[ year == 2022 & cbsa_type == "metro", .( cbsa, state_assigned = state_fips ) ], by = "cbsa" )
  per <- ms[ , .( pop = sum( population ), n_states = uniqueN( state_fips ),
                  n_div = uniqueN( unname( DIVISION[ state_fips ] ) ) ), by = cbsa ]
  single <- per[ n_states == 1, cbsa ]; onediv <- per[ n_div == 1, cbsa ]
  log_msg( "M: metros ", nrow( per ), "; multi-state ", sum( per$n_states > 1 ), "; multi-division ", sum( per$n_div > 1 ) )
  sp <- CS22[ outcome %in% c( "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu" ) ]
  r <- add_pr( rbind( fit_set( sp, panel, "all" ),
                      fit_set( sp, panel[ cbsa %in% single ], "single_state" ),
                      fit_set( sp[ est == "div_fe" ], panel[ cbsa %in% onediv ], "single_division" ) ), 1 )
  r <- merge( r, sp[ , .( spec_id, outcome, est ) ], by = "spec_id" )
  w <- dcast( r[ term %in% TERMS ], spec_id + outcome + est + term ~ variant, value.var = "pr" )
  m1 <- w[ , .( specs = .N, all = median( all ), single_state = median( single_state ),
                med_abs_diff = median( abs( single_state - all ) ), max_abs_diff = max( abs( single_state - all ) ),
                sign_flips = mean( sign( single_state ) != sign( all ) & pmax( abs( all ), abs( single_state ) ) > .05 ) ),
           keyby = .( outcome, est, term ) ]
  m2 <- w[ , .( specs = .N, med_abs_diff = median( abs( single_state - all ) ), p90_abs_diff = quantile( abs( single_state - all ), .9 ),
                max_abs_diff = max( abs( single_state - all ) ), term_max = term[ which.max( abs( single_state - all ) ) ] ), keyby = .( outcome, est ) ]
  m3 <- w[ est == "div_fe", .( specs = .N, med_abs_diff = median( abs( single_division - all ) ), max_abs_diff = max( abs( single_division - all ) ),
                               term_max = term[ which.max( abs( single_division - all ) ) ] ), keyby = outcome ]
  fwrite( per, file.path( OD, "m_metro_states.csv" ) ); fwrite( m1, file.path( OD, "m_multistate_terms.csv" ) )
  fwrite( m2, file.path( OD, "m_multistate_by_est.csv" ) ); fwrite( m3, file.path( OD, "m_multidivision.csv" ) )
  write_parquet( r, file.path( OD, "m_coefs.parquet" ) )
  cat( "\n== M: all metros vs single-state metros, by estimator ==\n" ); print( r3( m2 ) )
  cat( "\n== M: division FE, all vs single-division metros ==\n" ); print( r3( m3 ) )
  cat( "\n== M: state FE by term ==\n" ); print( r3( m1[ est == "state_fe" ] ), nrows = 100 )
}

## ---- W: within 2000-2022 vs 2010-2022 ------------------------------------------------------
if ( "W" %in% PARTS ) {
  wsp <- grid[ ident == "within" & !grepl( "_net$", outcome ) & !grepl( "contrib", op ) ]
  jobs <- rbind( wsp[ , .( spec_id, variant = "w13" ) ], wsp[ , .( spec_id, variant = "w23" ) ] )
  log_msg( "W: ", nrow( wsp ), " within specs; fits ", nrow( jobs ), " on ", WORKERS, " workers" )
  pg <- copy( panel ); pg[ , gini_m := gini ]
  run_job <- function( i ) {
    j <- jobs[ i ]; s <- copy( wsp[ spec_id == j$spec_id ] )
    if ( j$variant == "w23" ) s[ , ident := "within_long" ]
    r <- tryCatch( run_spec( s, pg ), error = function( e ) NULL )
    if ( is.null( r ) || !nrow( r ) ) return( NULL )
    r[ , `:=`( spec_id = j$spec_id, variant = j$variant, k = .N ) ][] }
  environment( run_job ) <- globalenv()
  for ( v in c( "jobs", "wsp", "pg", "run_job" ) ) assign( v, get( v ), envir = globalenv() )
  t0 <- proc.time()
  cl <- makePSOCKcluster( WORKERS ); on.exit( try( stopCluster( cl ), silent = TRUE ), add = TRUE )
  invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1", MV_MODEL = "lean" )
    setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
    source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
  clusterExport( cl, c( "jobs", "wsp", "pg", "run_job" ), envir = globalenv() )
  r <- rbindlist( parLapplyLB( cl, seq_len( nrow( jobs ) ), run_job, chunk.size = 50 ), fill = TRUE )
  stopCluster( cl )
  log_msg( "W: fits in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min; rows ", nrow( r ) )
  r[ , years := fifelse( variant == "w23", 23, 13 ) ]
  r <- add_pr( r, r$years )
  write_parquet( r, file.path( OD, "w_coefs.parquet" ) )

  m <- merge( r[ variant == "w23" & term != "logP", .( spec_id, term, est_l = estimate, se_l = se, t_l = stat, p_l = p, pr_l = pr, n_l = n ) ],
              r[ variant == "w13" & term != "logP", .( spec_id, term, est_s = estimate, se_s = se, t_s = stat, p_s = p, pr_s = pr, n_s = n ) ],
              by = c( "spec_id", "term" ) )
  m <- merge( m, wsp[ , .( spec_id, construct, outcome, op, dv_form, pred_form, unit ) ], by = "spec_id" )
  m[ , extended := n_l > 1.2 * n_s ]
  summ <- function( x ) list( rows = nrow( x ), specs = uniqueN( x$spec_id ), n_ratio = median( x$n_l / x$n_s ),
    se_ratio = median( x$se_l / x$se_s, na.rm = TRUE ), med_abs_t_13 = median( abs( x$t_s ) ), med_abs_t_23 = median( abs( x$t_l ) ),
    share_t_higher = mean( abs( x$t_l ) > abs( x$t_s ) ), sig_13 = mean( x$p_s < .05 ), sig_23 = mean( x$p_l < .05 ),
    same_sign = mean( sign( x$est_l ) == sign( x$est_s ) ), cor_pr = cor( x$pr_l, x$pr_s ), med_abs_dpr = median( abs( x$pr_l - x$pr_s ) ) )
  overall <- m[ , summ( .SD ), keyby = .( construct, extended ) ]
  byterm <- m[ extended == TRUE & term %in% TERMS, summ( .SD ), keyby = .( construct, term ) ]
  stab <- function( pr, p ) { med <- median( pr ); list( median_r = med, sign_stab = mean( sign( pr ) == sign( med ) ),
                                                        sig_same = mean( p < .05 & sign( pr ) == sign( med ) ) ) }
  stc <- merge( m[ extended == TRUE & term %in% TERMS, stab( pr_s, p_s ), by = .( construct, term ) ],
                m[ extended == TRUE & term %in% TERMS, stab( pr_l, p_l ), by = .( construct, term ) ],
                by = c( "construct", "term" ), suffixes = c( "_13", "_23" ) )
  ## the Gini swap: 2010-2022 refit (county-weighted Gini) vs the v11 run (metro Gini)
  v11 <- as.data.table( read_parquet( file.path( OUT$spec, "run_principled_v11", "coefs.parquet" ) ) )[ spec_id %in% wsp$spec_id ]
  v11[ , k := .N, by = spec_id ][ , pr := stat / sqrt( stat^2 + pmax( n / 13 - k - 1, 1 ) ) ]
  g <- merge( r[ variant == "w13" & term %in% setdiff( TERMS, "gini_m" ), .( spec_id, term, pr_swap = pr ) ],
              v11[ , .( spec_id, term, pr_v11 = pr ) ], by = c( "spec_id", "term" ) )
  gsw <- g[ , .( rows = .N, cor = cor( pr_swap, pr_v11 ), med_abs_diff = median( abs( pr_swap - pr_v11 ) ),
                 p90_abs_diff = quantile( abs( pr_swap - pr_v11 ), .9 ) ), keyby = term ]
  fwrite( overall, file.path( OD, "w_overall.csv" ) ); fwrite( byterm, file.path( OD, "w_by_term.csv" ) )
  fwrite( stc, file.path( OD, "w_term_stability.csv" ) ); fwrite( gsw, file.path( OD, "w_gini_swap.csv" ) )
  cat( "\n== W: 2000-2022 vs 2010-2022 within specs (paired) ==\n" ); print( r3( overall ) )
  cat( "\n== W: by term (extended samples) ==\n" ); print( r3( byterm[ order( construct, -abs( med_abs_t_23 ) ) ] ), nrows = 100 )
  cat( "\n== W: term stability, 13 vs 23 years ==\n" ); print( r3( stc[ order( construct, -abs( median_r_23 ) ) ] ), nrows = 100 )
  cat( "\n== W: Gini swap vs v11 (other terms) ==\n" ); print( r3( gsw ) )
}
log_msg( "done" )
