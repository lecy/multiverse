## 99_interaction_screen_v11.R
## The interaction screen of 80_interaction_screen.R (parts 1-3) repeated in the 13-covariate lean default
## model (BLOCKS_LEAN in 40_multiverse.R; paper/02-variables.qmd; run v11). Results note:
## notes/interaction-screen-v11-results.md.
##
##   Part 1  2022 metros, log Y on the 13 covariates + log P, filer counts and spending excl. hospitals /
##           universities. Each candidate term is added ONE AT A TIME to the full main-effects model:
##           78 pairwise products, 13 squares, 13 x * log P (104 tests per outcome). Partial r / t / p (HC1),
##           with and without x^2 and z^2 controls (pairs and x * log P; the log P^2 control for the latter);
##           Benjamini-Hochberg within outcome over the 104, and over the old 20-term family (pairs, squares
##           and x * log P of the five benchmarks) for comparison with script 80.
##   Part 1b diagnostics: (a) which new covariate absorbs the old stable terms (old six + one new covariate at a
##           time); (b) the BH survivors without metros with |z| > 4 on any lean covariate, and winsorized.
##   Part 2  several terms at once: the old set (10 benchmark pairs + 5 squares; the 10 pairs alone), each
##           outcome's BH survivors, the union of both outcomes' survivors, all 13 x * log P, and all 78 pairs
##           + 13 squares. Joint Wald (HC1), adjusted R2, 10-fold CV RMSE (20 repeats) vs main effects, and
##           every main effect's coefficient in each model (sign changes flagged). The survivor sets are
##           selected on the full sample, so their CV is optimistic; "bh_selected_in_fold" redoes the screen
##           and BH inside each training fold (5 repeats, compared with the main model on the same folds).
##   Part 3  mini-multiverse, every term one at a time: identification (cs 2022, cs 2019, pooled 2015-22 with
##           year FE) x unit (metro, all CBSAs) x estimator (OLS, state FE, division FE) x outliers (none,
##           winsorized 1%) x 5 outcomes (3 count, 2 spending) = 108 count and 72 spending specs per term.
##           Share with the median sign, share significant (with that sign).
##
## Arm "old6": the old six-covariate model (script 80's default) on the SAME rows (metros with every lean
## covariate), for its 20 terms only, so a change can be split into data / sample drift (script 80 vs old6)
## and the model change (old6 vs lean).
##
## Covariates: rates as is; size-scaled totals (pf_assets, gov_earn_sl, tr_medicaid, hist_pre1970) as
## flog(x, pop) residualized on log P by year (SAMI form; with log P on the right this is the log-log model by
## FWL, as in script 80). Religion enters as fractionalization, rel_frac = 1 - rel_hhi. hist_pre1970 is NA for
## Memphis and Mankato, so the 2022 sample is 385 metros. Predictors (and log P) are z-scored in each
## estimation sample: an interaction coefficient is the change in x's effect (log points per SD) per SD of z,
## and main effects in an interaction model are effects at the mean. Eagle Pass is not split out (script 80
## showed the floor removes its leverage).
## Outputs: D:/MULTIVERSE/data/interactions/v11/; figure paper/figures/d6_lean_interactions.png.
## Run from multiverse/ (part 3 runs on [workers] PSOCK workers, default 6, at most 6):
##   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/99_interaction_screen_v11.R > D:/MULTIVERSE/logs/99.log 2>&1

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )          # load_panel(), flog(), resid_logP(), winsor(); BLOCKS is not used
suppressPackageStartupMessages( { library( ggplot2 ); library( parallel ) } )
setFixest_notes( FALSE ); setFixest_nthreads( 1 ); data.table::setDTthreads( 2 )
args <- commandArgs( TRUE )
WORKERS <- suppressWarnings( as.integer( args[ grepl( "^[0-9]+$", args ) ] ) ); if ( !length( WORKERS ) ) WORKERS <- 6L
WORKERS <- min( WORKERS, 6L )
OD <- file.path( BUILD, "data", "interactions", "v11" ); dir.create( OD, recursive = TRUE, showWarnings = FALSE )
FIG <- "paper/figures"
options( width = 250 )

VARS <- c( poverty_rate = "Poverty", pct_65plus = "% 65+", gini_m = "Gini", pct_nonwhite = "% nonwhite",
           rel_frac = "Religious frac.", age_sd = "Age SD", pf_assets = "Foundation assets", pct_ba = "% BA",
           turnout = "Turnout", lag_g10 = "Growth kernel", gov_earn_sl = "S&L govt earnings",
           tr_medicaid = "Medicaid transfers", hist_pre1970 = "Pre-1970 endowment" )
X13 <- names( VARS )
OLD6 <- c( "pct_ba", "pf_assets", "turnout", "pct_nonwhite", "poverty_rate", "gov_earn_sl" )
X5 <- X13[ X13 %in% c( "pct_ba", "pf_assets", "turnout", "pct_nonwhite", "poverty_rate" ) ]   # in X13 order
TOT <- c( "pf_assets", "gov_earn_sl", "tr_medicaid", "hist_pre1970" )
LAB <- c( VARS, logP = "log P" )
OUTL <- c( n_filer_c3 = "Filer counts", n_filer_c3_nohu = "Filers excl. hosp/univ", n_bmf_c3pc = "Registered stock",
           exp_c3_nohu = "Spending excl. hosp/univ", exp_c3 = "Spending (all)" )
ORD <- c( X13, "logP" )
key_of <- function( a, b ) { o <- match( a, ORD ) <= match( b, ORD ); paste( ifelse( o, a, b ), ifelse( o, b, a ), sep = ":" ) }
OLD_STABLE <- c( key_of( "turnout", "turnout" ), key_of( "turnout", "poverty_rate" ),
                 key_of( "turnout", "pct_nonwhite" ), key_of( "pct_ba", "pct_nonwhite" ) )

mk_terms <- function( xs ) {
  pr <- as.data.table( t( combn( xs, 2 ) ) ); setnames( pr, c( "x", "w" ) ); pr[ , kind := "pair" ]
  tt <- rbind( pr, data.table( x = xs, w = xs, kind = "square" ), data.table( x = xs, w = "logP", kind = "x_logP" ) )
  tt[ , key := paste( x, w, sep = ":" ) ]
  tt[ , term := fifelse( kind == "square", paste0( LAB[ x ], "²" ), paste( LAB[ x ], "×", LAB[ w ] ) ) ]
  tt[ , old20 := x %in% X5 & ( w %in% X5 | w == "logP" ) ]
  tt[] }
T13 <- mk_terms( X13 ); T5 <- mk_terms( X5 )
stopifnot( nrow( T13 ) == 104, nrow( T5 ) == 20, all( T5$key %in% T13$key ), all( OLD_STABLE %in% T5$key ) )
ARMS <- list( lean = list( main = X13, terms = T13 ), old6 = list( main = OLD6, terms = T5 ) )

panel <- load_panel()                                    # gini_m, lag_g10, hist_pre1970 attached
panel[ , rel_frac := 1 - rel_hhi ]

## estimation sample: every lean covariate observed (both arms use the same rows)
make_data <- function( outcome, years = 2022L, unit = "metro", outlier = "none", drop = character( 0 ) ) {
  d <- panel[ year %in% years & !cbsa %in% drop ]
  if ( unit == "metro" ) d <- d[ cbsa_type == "metro" ]
  d <- d[ , c( "cbsa", "year", "state_fips", "division", "logP", "population", outcome, X13 ), with = FALSE ]
  d <- d[ complete.cases( d ) & population > 0 & get( outcome ) > 0 ]
  d[ , ly := log( get( outcome ) ) ]
  for ( v in TOT ) set( d, j = v, value = resid_logP( flog( d[[ v ]], d$population ), d$logP, d$year ) )
  if ( outlier == "winsor1" ) for ( v in c( "ly", X13 ) ) set( d, j = v, value = winsor( d[[ v ]] ) )
  for ( v in c( X13, "logP" ) ) set( d, j = paste0( "z_", v ), value = ( d[[ v ]] - mean( d[[ v ]] ) ) / sd( d[[ v ]] ) )
  d[]
}
fe_of <- function( est, pooled ) {
  f <- switch( est, ols = "", state_fe = "state_fips", div_fe = "division" )
  if ( pooled ) f <- paste( c( if ( nzchar( f ) ) f, "year" ), collapse = " + " )
  if ( nzchar( f ) ) paste( "|", f ) else ""
}
vc_of <- function( est, pooled ) if ( est != "ols" ) ~state_fips else if ( pooled ) ~cbsa else "hetero"
main_rhs <- function( main ) paste( paste0( "z_", c( main, "logP" ) ), collapse = " + " )

## one term (x:w, x^2 when x == w) added to the full model; d is modified in place (columns iv, sqx, sqw)
fit_int <- function( d, main, x, w, est = "ols", pooled = FALSE, squares = FALSE ) {
  zx <- d[[ paste0( "z_", x ) ]]; zw <- d[[ paste0( "z_", w ) ]]
  set( d, j = "iv", value = zx * zw ); set( d, j = "sqx", value = zx^2 ); set( d, j = "sqw", value = zw^2 )
  extra <- if ( squares && x != w ) { if ( w == "logP" ) " + sqw" else " + sqx + sqw" } else ""
  f <- as.formula( paste( "ly ~", main_rhs( main ), "+ iv", extra, fe_of( est, pooled ) ) )
  r <- tryCatch( feols( f, data = d, vcov = vc_of( est, pooled ) ), error = function( e ) NULL )
  if ( is.null( r ) ) return( NULL )
  ct <- coeftable( r ); k <- which( rownames( ct ) == "iv" )
  if ( !length( k ) ) return( NULL )
  df_res <- ( if ( pooled ) uniqueN( d$cbsa ) else nobs( r ) ) - length( coef( r ) ) - 1
  t <- ct[ k, 3 ]
  data.table( estimate = ct[ k, 1 ], se = ct[ k, 2 ], t = t, p = ct[ k, 4 ], pr = t / sqrt( t^2 + max( df_res, 1 ) ), n = nobs( r ) )
}

d22 <- make_data( "n_filer_c3" )
m22 <- panel[ year == 2022 & cbsa_type == "metro" ]
log_msg( "2022 metros ", nrow( m22 ), "; with every lean covariate and filers > 0: ", nrow( d22 ),
         "; dropped: ", paste( m22[ !cbsa %in% d22$cbsa, cbsa_title ], collapse = " | " ) )
log_msg( "max |z| per covariate (2022 metros): ",
         paste( sprintf( "%s %.1f", X13, sapply( X13, function( v ) max( abs( d22[[ paste0( "z_", v ) ]] ) ) ) ), collapse = ", " ) )

## ===========================================================================
## Part 1: one at a time, 2022 metros
## ===========================================================================
log_msg( "Part 1: interaction matrix" )
O2 <- c( "n_filer_c3", "exp_c3_nohu" )
## main effects of each arm, for comparison
ME <- rbindlist( lapply( names( ARMS ), function( a ) rbindlist( lapply( O2, function( o ) {
  d <- make_data( o ); r <- feols( as.formula( paste( "ly ~", main_rhs( ARMS[[ a ]]$main ) ) ), d, vcov = "hetero" )
  ct <- coeftable( r )[ -1, , drop = FALSE ]; df <- nobs( r ) - length( coef( r ) ) - 1
  data.table( arm = a, outcome = o, var = sub( "^z_", "", rownames( ct ) ), b = ct[ , 1 ], t = ct[ , 3 ], p = ct[ , 4 ],
              pr = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + df ), n = nobs( r ), adj_r2 = r2( r, "ar2" ) )
} ) ) ) )
fwrite( ME, file.path( OD, "p1_main_effects.csv" ) )
cat( "\n== main effects, 2022 metros (partial r) ==\n" )
print( dcast( ME, var ~ arm + outcome, value.var = "pr" )[ order( match( var, ORD ) ) ][ , lapply( .SD, function( v ) if ( is.numeric( v ) ) round( v, 3 ) else v ) ] )

P1 <- rbindlist( lapply( names( ARMS ), function( a ) rbindlist( lapply( O2, function( o ) {
  d <- make_data( o ); A <- ARMS[[ a ]]; TT <- A$terms
  rbindlist( lapply( seq_len( nrow( TT ) ), function( i ) {
    x <- TT$x[ i ]; w <- TT$w[ i ]
    r0 <- fit_int( d, A$main, x, w ); rs <- if ( TT$kind[ i ] != "square" ) fit_int( d, A$main, x, w, squares = TRUE ) else NULL
    cbind( data.table( arm = a, outcome = o ), TT[ i ],
           data.table( cor_centered = cor( d[[ paste0( "z_", x ) ]] * d[[ paste0( "z_", w ) ]], d$ly ),
                       b = r0$estimate, se = r0$se, t = r0$t, p = r0$p, pr = r0$pr, n = r0$n,
                       b_sq = if ( is.null( rs ) ) NA_real_ else rs$estimate, pr_sq = if ( is.null( rs ) ) NA_real_ else rs$pr,
                       p_sq = if ( is.null( rs ) ) NA_real_ else rs$p ) )
  } ) )
} ) ) ) )
P1[ , `:=`( p_bh = p.adjust( p, "BH" ), p_holm = p.adjust( p, "holm" ) ), by = .( arm, outcome ) ]   # 104 (lean) / 20 (old6)
P1[ old20 == TRUE, p_bh20 := p.adjust( p, "BH" ), by = .( arm, outcome ) ]                         # the old 20-term family
fwrite( P1, file.path( OD, "p1_interaction_matrix.csv" ) )
r3 <- function( dt ) dt[ , lapply( .SD, function( v ) if ( is.numeric( v ) ) round( v, 3 ) else v ) ]
cat( "\n== Part 1, lean: terms with p < .05 ==\n" )
print( r3( P1[ arm == "lean" & p < .05, .( outcome, term, kind, b, pr, p = signif( p, 2 ), p_bh = signif( p_bh, 2 ),
                                            pr_sq, p_sq = signif( p_sq, 2 ) ) ][ order( outcome, p ) ] ), nrows = 300 )
cat( "\n== Part 1, the old 20 terms: old6 vs lean, same rows ==\n" )
print( r3( dcast( P1[ old20 == TRUE ], key + term ~ arm + outcome, value.var = c( "pr", "p_bh20" ) ) ), nrows = 100 )
SURV <- P1[ arm == "lean" & p_bh < .05, .( outcome, key ) ]
log_msg( "BH survivors (lean, 104 tests): ", paste( SURV[ , .N, by = outcome ][ , paste( outcome, N ) ], collapse = ", " ) )

## ---- Part 1b: diagnostics ------------------------------------------------------------------
## (a) absorption: which added covariate absorbs the old stable terms? old six + one new covariate at a time
NEW7 <- setdiff( X13, OLD6 )
ABS <- rbindlist( lapply( O2, function( o ) {
  d <- make_data( o )
  rbindlist( lapply( c( "none", NEW7, "all" ), function( v ) {
    main <- switch( v, none = OLD6, all = X13, c( OLD6, v ) )
    rbindlist( lapply( c( OLD_STABLE, key_of( "poverty_rate", "pf_assets" ) ), function( k ) {
      xw <- strsplit( k, ":" )[[ 1 ]]; r <- fit_int( d, main, xw[1], xw[2] )
      as.data.table( list( outcome = o, added = v, key = k, term = T13[ key == k ]$term, pr = r$pr, p = r$p ) ) } ) )
  } ) )
} ) )
fwrite( ABS, file.path( OD, "p1b_absorption.csv" ) )
cat( "\n== Part 1b (a): partial r of the old stable terms, old six + one new covariate (2022 metros) ==\n" )
print( r3( dcast( ABS, outcome + term ~ factor( added, levels = c( "none", NEW7, "all" ) ), value.var = "pr" ) ) )
## (b) leverage: the lean BH survivors without metros that have |z| > 4 on any lean covariate in 2022, and
## with every covariate winsorized at 1/99%
EXTREME <- d22[ , cbsa[ Reduce( `|`, lapply( X13, function( v ) abs( get( paste0( "z_", v ) ) ) > 4 ) ) ] ]
log_msg( "metros with |z| > 4 on a lean covariate: ", length( EXTREME ), " (",
         paste( panel[ year == 2022 & cbsa %in% EXTREME, cbsa_title ], collapse = " | " ), ")" )
LEV <- rbindlist( lapply( O2, function( o ) {
  dl <- list( base = make_data( o ), no_extreme = make_data( o, drop = EXTREME ), winsor1 = make_data( o, outlier = "winsor1" ) )
  rbindlist( lapply( unique( SURV$key ), function( k ) {
    xw <- strsplit( k, ":" )[[ 1 ]]
    rbindlist( lapply( names( dl ), function( s ) { r <- fit_int( dl[[ s ]], X13, xw[1], xw[2] )
      as.data.table( list( outcome = o, sample = s, key = k, term = T13[ key == k ]$term, pr = r$pr, p = r$p, n = r$n ) ) } ) )
  } ) )
} ) )
LEV[ , survivor_own := paste( outcome, key ) %in% SURV[ , paste( outcome, key ) ] ]
fwrite( LEV, file.path( OD, "p1b_leverage.csv" ) )
cat( "\n== Part 1b (b): BH survivors without extreme metros / winsorized ==\n" )
print( r3( dcast( LEV[ survivor_own == TRUE ], outcome + term ~ sample, value.var = c( "pr", "p" ) ) ) )

## ===========================================================================
## Part 2: several terms at once
## ===========================================================================
log_msg( "Part 2: joint tests and cross-validation" )
cv_rmse <- function( d, f, K = 10, reps = 20 ) {
  mean( vapply( seq_len( reps ), function( r ) {
    set.seed( r ); fold <- sample( rep( 1:K, length.out = nrow( d ) ) )
    e <- unlist( lapply( 1:K, function( k ) { m <- lm( f, d[ fold != k ] ); d$ly[ fold == k ] - predict( m, d[ fold == k ] ) } ) )
    sqrt( mean( e^2 ) ) }, numeric( 1 ) ) )
}
icol <- function( k ) paste0( "I_", sub( ":", "__", k ) )
SETS <- function( o ) list(
  main = character( 0 ),
  old_pairs = T5[ kind == "pair" ]$key,
  old_pairs_sq = T5[ kind %in% c( "pair", "square" ) ]$key,
  bh_own = SURV[ outcome == o ]$key,
  bh_union = unique( SURV$key ),
  bh_own_no_logP = SURV[ outcome == o & !grepl( ":logP$", key ) ]$key,
  x_logP = T13[ kind == "x_logP" ]$key,
  all_pairs_sq = T13[ kind %in% c( "pair", "square" ) ]$key )
P2 <- list(); P2me <- list()
for ( o in O2 ) {
  d <- make_data( o )
  for ( k in T13$key ) { xw <- strsplit( k, ":" )[[ 1 ]]; set( d, j = icol( k ), value = d[[ paste0( "z_", xw[1] ) ]] * d[[ paste0( "z_", xw[2] ) ]] ) }
  S <- SETS( o ); m0 <- NULL
  for ( s in names( S ) ) {
    if ( s != "main" && !length( S[[ s ]] ) ) next
    f <- as.formula( paste( "ly ~", main_rhs( X13 ), if ( length( S[[ s ]] ) ) paste( "+", paste( icol( S[[ s ]] ), collapse = " + " ) ) ) )
    m <- feols( f, d, vcov = "hetero" ); if ( s == "main" ) m0 <- m
    wt <- if ( length( S[[ s ]] ) ) tryCatch( wald( m, keep = "^I_", print = FALSE ), error = function( e ) NULL ) else NULL
    P2[[ length( P2 ) + 1 ]] <- data.table( outcome = o, model = s, k_added = length( S[[ s ]] ), terms = paste( S[[ s ]], collapse = " " ),
      r2 = r2( m, "r2" ), adj_r2 = r2( m, "ar2" ), wald_F = if ( is.null( wt ) ) NA_real_ else wt$stat,
      wald_p = if ( is.null( wt ) ) NA_real_ else wt$p, cv_rmse = cv_rmse( d, f ), n = nobs( m ) )
    ct <- coeftable( m ); mv <- paste0( "z_", c( X13, "logP" ) ); df <- nobs( m ) - length( coef( m ) ) - 1
    P2me[[ length( P2me ) + 1 ]] <- data.table( outcome = o, model = s, var = sub( "^z_", "", mv ), b = ct[ mv, 1 ], t = ct[ mv, 3 ],
                                                p = ct[ mv, 4 ], pr = ct[ mv, 3 ] / sqrt( ct[ mv, 3 ]^2 + df ) )
  }
  ## honest CV for the BH-survivor model: the 104-term screen and BH are redone inside each training fold
  ## (the bh_own CV above selects on the full sample, so it is optimistic). 5 repeats; main model on the same folds.
  fmain <- as.formula( paste( "ly ~", main_rhs( X13 ) ) )
  cvs <- lapply( 1:5, function( r ) {
    set.seed( r ); fold <- sample( rep( 1:10, length.out = nrow( d ) ) )
    e <- lapply( 1:10, function( k ) {
      tr <- d[ fold != k ]; te <- d[ fold == k ]
      pv <- vapply( T13$key, function( kk ) { m <- feols( as.formula( paste( "ly ~", main_rhs( X13 ), "+", icol( kk ) ) ), tr, vcov = "hetero" )
                                               pvalue( m )[[ icol( kk ) ]] }, numeric( 1 ) )
      sel <- T13$key[ p.adjust( pv, "BH" ) < .05 ]
      fs <- as.formula( paste( "ly ~", main_rhs( X13 ), if ( length( sel ) ) paste( "+", paste( icol( sel ), collapse = " + " ) ) ) )
      list( e_sel = te$ly - predict( lm( fs, tr ), te ), e_main = te$ly - predict( lm( fmain, tr ), te ), n_sel = length( sel ) ) } )
    c( sel = sqrt( mean( unlist( lapply( e, `[[`, "e_sel" ) )^2 ) ), main = sqrt( mean( unlist( lapply( e, `[[`, "e_main" ) )^2 ) ),
       n_sel = mean( sapply( e, `[[`, "n_sel" ) ) ) } )
  cvs <- colMeans( do.call( rbind, cvs ) )
  P2[[ length( P2 ) + 1 ]] <- data.table( outcome = o, model = "bh_selected_in_fold", k_added = cvs[[ "n_sel" ]], terms = "",
    r2 = NA_real_, adj_r2 = NA_real_, wald_F = NA_real_, wald_p = NA_real_, cv_rmse = cvs[[ "sel" ]], n = nrow( d ),
    cv_main_same_folds = cvs[[ "main" ]] )
}
P2 <- rbindlist( P2, fill = TRUE ); P2me <- rbindlist( P2me )
P2[ , `:=`( d_adj_r2 = adj_r2 - adj_r2[ model == "main" ], cv_change_pct = 100 * ( cv_rmse / cv_rmse[ model == "main" ] - 1 ) ), by = outcome ]
P2[ model == "bh_selected_in_fold", cv_change_pct := 100 * ( cv_rmse / cv_main_same_folds - 1 ) ]
P2me[ , `:=`( b_main = b[ model == "main" ], p_main = p[ model == "main" ] ), by = .( outcome, var ) ]
P2me[ , `:=`( sign_change = sign( b ) != sign( b_main ), sig_change = ( p < .05 ) != ( p_main < .05 ) ) ]
fwrite( P2, file.path( OD, "p2_joint_cv.csv" ) ); fwrite( P2me, file.path( OD, "p2_main_effects.csv" ) )
cat( "\n== Part 2 ==\n" ); print( r3( P2[ , !"terms" ] ) )
cat( "\n== Part 2: main-effect coefficients by model (per SD) ==\n" )
for ( o in O2 ) { cat( o, "\n" ); print( r3( dcast( P2me[ outcome == o ], var ~ model, value.var = "b" )[ order( match( var, ORD ) ) ] ) ) }
cat( "\nsign changes:\n" ); print( r3( P2me[ sign_change == TRUE ] ) )
cat( "\nsignificance changes (p < .05):\n" ); print( r3( P2me[ sig_change == TRUE ] ) )

## ===========================================================================
## Part 3: mini-multiverse, every term one at a time
## ===========================================================================
log_msg( "Part 3: stability across forks (", WORKERS, " workers)" )
G <- CJ( outcome = names( OUTL ), ident = c( "cs2022", "cs2019", "pooled" ), unit = c( "metro", "cbsa" ),
         est = c( "ols", "state_fe", "div_fe" ), outlier = c( "none", "winsor1" ) )
G[ , job := .I ]
run_job <- function( g ) {
  s <- G[ job == g ]
  yrs <- switch( s$ident, cs2022 = 2022L, cs2019 = 2019L, pooled = 2015:2022 )
  d <- make_data( s$outcome, yrs, s$unit, s$outlier )
  rbindlist( lapply( names( ARMS ), function( a ) {
    A <- ARMS[[ a ]]
    rbindlist( lapply( seq_len( nrow( A$terms ) ), function( i ) {
      r <- fit_int( d, A$main, A$terms$x[ i ], A$terms$w[ i ], s$est, pooled = s$ident == "pooled" )
      if ( is.null( r ) ) NULL else cbind( s, data.table( arm = a ), A$terms[ i, .( x, w, kind, key, term ) ], r )
    } ) )
  } ) )
}
t0 <- proc.time()
cl <- makePSOCKcluster( WORKERS )
invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
  setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
  source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); setFixest_notes( FALSE ); NULL } ) )
clusterExport( cl, c( "panel", "G", "ARMS", "X13", "TOT", "make_data", "fe_of", "vc_of", "main_rhs", "fit_int", "run_job" ), envir = globalenv() )
P3 <- rbindlist( parLapplyLB( cl, G$job, run_job ) )
stopCluster( cl )
log_msg( "part 3 fits in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min; rows ", nrow( P3 ) )
P3[ , construct := fifelse( grepl( "^exp", outcome ), "spending", "counts" ) ]
fwrite( P3, file.path( OD, "p3_stability.csv" ) )
S3 <- P3[ , { ms <- sign( median( pr ) )
  .( specs = .N, median_pr = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
     same_sign = mean( sign( pr ) == ms ), signif = mean( p < .05 ), signif_same = mean( p < .05 & sign( pr ) == ms ) ) },
  by = .( arm, construct, x, w, kind, key, term ) ]
## flags: 2022 BH survivor (lean, own construct), one of the four stable terms of script 80
S3[ , bh_2022 := ( construct == "counts" & key %in% SURV[ outcome == "n_filer_c3" ]$key ) |
                 ( construct == "spending" & key %in% SURV[ outcome == "exp_c3_nohu" ]$key ) ]
S3[ , bh_2022_any := key %in% SURV$key ]
S3[ , old_stable := key %in% OLD_STABLE ]
S3[ , stable := same_sign >= .95 & signif_same >= .5 ]
fwrite( S3, file.path( OD, "p3_stability_summary.csv" ) )
cat( "\n== Part 3, lean: BH survivors, the four old stable terms, and every term with same_sign >= .9 ==\n" )
print( r3( S3[ arm == "lean" & ( bh_2022_any | old_stable | same_sign >= .9 ) ][ order( construct, -abs( median_pr ) ),
            .( construct, term, kind, specs, median_pr, q25, q75, same_sign, signif, signif_same, bh_2022, old_stable, stable ) ] ), nrows = 400 )
cat( "\n== Part 3: x * log P, lean ==\n" )
print( r3( S3[ arm == "lean" & kind == "x_logP" ][ order( construct, -abs( median_pr ) ),
            .( construct, term, median_pr, same_sign, signif, signif_same ) ] ), nrows = 100 )
cat( "\n== Part 3: the old 20 terms, old6 vs lean (same rows) ==\n" )
print( r3( dcast( S3[ key %in% T5$key ], construct + term ~ arm, value.var = c( "median_pr", "same_sign", "signif_same" ) )[ order( construct, -abs( median_pr_lean ) ) ] ), nrows = 100 )

## ===========================================================================
## Figure: lean interaction matrix (partial r of the added term, 2022 metros)
## ===========================================================================
fig <- copy( P1[ arm == "lean" ] )
## lower triangle: column = earlier variable, row = later variable (log P last)
fig[ , `:=`( col = factor( LAB[ x ], levels = LAB[ X13 ] ), row = factor( LAB[ w ], levels = rev( LAB ) ) ) ]
fig[ , star := fcase( p_bh < .05, "**", p < .05, "*", default = "" ) ]
fig[ , lab := sub( "^(-?)0\\.", "\\1.", sprintf( "%.2f", pr ) ) ]
fig[ , lab := paste0( lab, star ) ]
fig[ , panel := factor( c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. hospitals, universities" )[ outcome ],
                        levels = c( "Filer counts", "Spending excl. hospitals, universities" ) ) ]
lim <- max( abs( fig$pr ), na.rm = TRUE )
nsv <- P1[ arm == "lean" & p_bh < .05, .N, by = outcome ]
p <- ggplot( fig, aes( col, row, fill = pr ) ) +
  geom_tile( colour = "white", linewidth = 0.8 ) +
  geom_text( aes( label = lab, fontface = ifelse( p_bh < .05, "bold", "plain" ) ), size = 2.15, colour = "grey15" ) +
  scale_fill_gradient2( low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 0, limits = c( -lim, lim ),
                        name = "Partial r of\nadded term" ) +
  facet_wrap( ~ panel, nrow = 1 ) + coord_equal() +
  labs( title = "Interaction screen in the 13-covariate model: each term added one at a time, 2022 metros",
        subtitle = sprintf( "Off-diagonal: x × z; diagonal: x²; bottom row: x × log population. %d metros. * p < .05 (HC1); ** (bold) survives Benjamini-Hochberg over 104 tests (counts %d, spending %d)",
                            nrow( d22 ), nsv[ outcome == "n_filer_c3", sum( N ) ], nsv[ outcome == "exp_c3_nohu", sum( N ) ] ),
        x = NULL, y = NULL ) +
  theme_minimal( base_size = 9.5 ) +
  theme( panel.grid = element_blank(), axis.text.x = element_text( angle = 40, hjust = 1 ),
         plot.title.position = "plot", plot.subtitle = element_text( colour = "grey30", size = 7.8 ),
         strip.text = element_text( face = "bold", hjust = 0 ), legend.position = "right",
         plot.background = element_rect( fill = "white", colour = NA ) )
ggsave( file.path( FIG, "d6_lean_interactions.png" ), p, width = 14, height = 7.6, dpi = 200, bg = "white" )
file.copy( file.path( FIG, "d6_lean_interactions.png" ), file.path( OD, "fig_interaction_matrix.png" ), overwrite = TRUE )
log_msg( "done: ", OD )
