## 105_govgrant_mechanism.R
## Mechanism check for the government terms of the 13-covariate model: do Medicaid
## transfers and S&L government earnings predict how much metro nonprofits depend on
## government grants (Form 990 line 1e; 24_npo_govgrants.R)? If they do, their positive
## density effects read as interdependence / third-party government (government funds
## nonprofits) rather than gap-filling. See notes/government-funding-measures.md.
##
## Line 1e is partly the outcome, so it is used here only as an outcome, never as a
## predictor of density.
##
## Parts
##   A. dependence ~ 13 covariates (+ log P), metros:
##        dep_rev  = log( gov grants / revenue ), excl. hospitals and universities
##        dep_inc  = share of full-990 filers with any government grants (logit)
##      years: 2022 (fully observed), 2017 covariates with 2015-2018 mean dependence
##      (pre-COVID; 7-11% of dollars imputed); estimators OLS (HC1), division FE, state FE
##      (state-clustered); robustness with hospitals and universities included
##   B. population split, 2022: log count of full-990 filers with / without government
##      grants (and all filers) on the 13 covariates + log P (log-log)
## Model form copied from 96_lean_card_evidence.R (mform, tidy_fit): totals flog(x, pop) with
## log P on the right, rates as is, standardized; partial r = t / sqrt(t^2 + df).
##
## Inputs : load_panel() (40_multiverse.R); D:/MULTIVERSE/data/govgrants/{govgrants_cbsa_year.csv,
##          orgyear_govgrants.parquet}
## Outputs: D:/MULTIVERSE/data/govgrants/mechanism_{dependence,split}.csv
## Run from multiverse/:  Rscript R/105_govgrant_mechanism.R

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )          # load_panel(), flog(), resid_logP()
suppressPackageStartupMessages( library( arrow ) )
setFixest_notes( FALSE ); setFixest_nthreads( 1 )
OD <- file.path( BUILD, "data", "govgrants" )
options( width = 200 )

LEAN <- c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate",
           "gov_earn_sl", "tr_medicaid", "lag_g10", "hist_pre1970",
           "pct_65plus", "gini_m", "rel_frac", "age_sd" )
TOT  <- c( "pf_assets", "gov_earn_sl", "tr_medicaid", "hist_pre1970" )
LAB  <- c( pct_ba = "% BA", pf_assets = "Foundation assets", pct_nonwhite = "% nonwhite", turnout = "Turnout",
           poverty_rate = "Poverty rate", gov_earn_sl = "S&L govt earnings", tr_medicaid = "Medicaid transfers",
           lag_g10 = "Growth kernel", hist_pre1970 = "Pre-1970 endowment", pct_65plus = "% 65+",
           gini_m = "Gini (metro)", rel_frac = "Religious fract.", age_sd = "Age SD", logP = "log P" )
pr_of <- function( t, df ) t / sqrt( t^2 + df )

## ---- helpers (from 96_lean_card_evidence.R) ---------------------------------------------
mform <- function( d, xs ) {
  X <- lapply( xs, function( v ) {
    x <- d[[ v ]]
    if ( v %in% TOT ) x <- flog( x, d$population )
    ( x - mean( x ) ) / sd( x ) } )
  setNames( as.data.table( X ), xs ) }
tidy_fit <- function( r ) {
  ct <- coeftable( r ); ct <- ct[ rownames( ct ) != "(Intercept)", , drop = FALSE ]
  df <- degrees_freedom( r, "resid" )
  data.table( term = rownames( ct ), est = ct[ , 1 ], t = ct[ , 3 ], p = ct[ , 4 ], r = pr_of( ct[ , 3 ], df ),
              r2 = as.numeric( r2( r, "r2" ) ), n = nobs( r ) ) }
## yy is already transformed; covariates in log form with log P on the right
fit <- function( d, yy, est = "ols" ) {
  ok <- is.finite( yy ) & complete.cases( d[ , LEAN, with = FALSE ] ) & d$population > 0
  d <- d[ ok ]; yy <- yy[ ok ]
  m <- cbind( data.table( yy = yy, logP = d$logP, state = d$state_fips, division = d$division ), mform( d, LEAN ) )
  rhs <- paste( c( LEAN, "logP" ), collapse = " + " )
  fe <- switch( est, ols = NULL, div_fe = "division", state_fe = "state" )
  vc <- if ( is.null( fe ) ) "hetero" else ~state
  tidy_fit( feols( as.formula( paste( "yy ~", rhs, if ( !is.null( fe ) ) paste( "|", fe ) ) ), m, vcov = vc ) ) }
logit <- function( p ) log( p / ( 1 - p ) )

## ---- data -------------------------------------------------------------------------------
P <- load_panel()
P[ , rel_frac := 1 - rel_hhi ]
P[ , cbsa := as.character( cbsa ) ]
G <- fread( file.path( OD, "govgrants_cbsa_year.csv" ), colClasses = list( character = "cbsa" ) )
G <- G[ cbsa_type == "metro" ]

## 2015-2018 mean dependence (pre-COVID)
Gm <- G[ year %in% 2015:2018, .( gg_c3_nohu = sum( gg_c3_nohu ), rev_c3_nohu = sum( rev_c3_nohu ),
                                gg_c3 = sum( gg_c3 ), rev_c3 = sum( rev_c3 ),
                                imp = sum( gg_c3 - gg_c3_obs ) / sum( gg_c3 ) ), by = cbsa ][ , year := 2017L ]
D <- rbind( merge( P[ year == 2022 & cbsa_type == "metro" ], G[ year == 2022, .( cbsa, g_gg_nohu = gg_c3_nohu, g_rev_nohu = rev_c3_nohu, g_gg = gg_c3, g_rev = rev_c3 ) ], by = "cbsa" )[ , window := "2022" ],
            merge( P[ year == 2017 & cbsa_type == "metro" ], Gm[ , .( cbsa, g_gg_nohu = gg_c3_nohu, g_rev_nohu = rev_c3_nohu, g_gg = gg_c3, g_rev = rev_c3 ) ], by = "cbsa" )[ , window := "2015-18" ] )

## incidence: full-990 filers with any government grants (observed returns only)
O <- as.data.table( read_parquet( file.path( OD, "orgyear_govgrants.parquet" ),
                                  col_select = c( "year", "status", "geoid", "hosp_univ", "gg" ) ) )
spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
O <- merge( O[ status == "observed" & !is.na( geoid ) & !hosp_univ ], spine[ cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
inc <- O[ , .( n990 = .N, n990_gg = sum( gg > 0 ) ), by = .( cbsa, year ) ]
inc <- rbind( inc[ year == 2022, .( cbsa, n990, n990_gg, window = "2022" ) ],
              inc[ year %in% 2015:2018, .( n990 = sum( n990 ), n990_gg = sum( n990_gg ), window = "2015-18" ), by = cbsa ] )
D <- merge( D, inc, by = c( "cbsa", "window" ), all.x = TRUE )
log_msg( "metros by window: ", paste( capture.output( print( D[ , .N, by = window ] ) ), collapse = " " ) )

## ---- A. dependence ~ 13 covariates --------------------------------------------------------
OUTC <- list(
  dep_rev      = function( d ) log( d$g_gg_nohu / d$g_rev_nohu ),
  dep_rev_all  = function( d ) log( d$g_gg / d$g_rev ),
  dep_inc      = function( d ) logit( d$n990_gg / d$n990 ) )
resA <- rbindlist( lapply( c( "2022", "2015-18" ), function( w ) {
  d <- D[ window == w ]
  rbindlist( lapply( names( OUTC ), function( o ) {
    rbindlist( lapply( c( "ols", "div_fe", "state_fe" ), function( e ) {
      fit( d, OUTC[[ o ]]( d ), e )[ , `:=`( outcome = o, window = w, estimator = e ) ] } ) ) } ) ) } ) )
fwrite( resA, file.path( OD, "mechanism_dependence.csv" ) )

show <- function( res, cols ) {
  w <- dcast( res[ term != "logP" ], term ~ ..., value.var = "r", fun.aggregate = mean )
  w <- w[ order( -get( names( w )[ 2 ] ) ) ]; w[ , term := LAB[ term ] ]
  print( w[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 2 ) else z ) ] ) }
cat( "\n== A. partial r: log gov-grant share of revenue (excl. H&U), by window x estimator ==\n" )
show( resA[ outcome == "dep_rev", .( term, window, estimator, r ) ] )
cat( "\n== A. partial r: incidence (logit share of 990 filers with any gov grants) ==\n" )
show( resA[ outcome == "dep_inc", .( term, window, estimator, r ) ] )
cat( "\n== A. robustness: all c3 incl. H&U, OLS ==\n" )
show( resA[ outcome == "dep_rev_all" & estimator == "ols", .( term, window, r ) ] )
cat( "\nR2 by outcome/window/estimator:\n" )
print( unique( resA[ , .( outcome, window, estimator, r2 = round( r2, 3 ), n ) ] ) )

## ---- B. population split, 2022 ----------------------------------------------------------
d <- D[ window == "2022" ]
SPLIT <- list( all_990 = log( d$n990 ), with_gg = log( d$n990_gg ), without_gg = log( d$n990 - d$n990_gg ),
               filers_all = log( d$n_filer_c3 ) )
resB <- rbindlist( lapply( names( SPLIT ), function( o ) rbindlist( lapply( c( "ols", "div_fe", "state_fe" ), function( e )
  fit( d, SPLIT[[ o ]], e )[ , `:=`( outcome = o, estimator = e ) ] ) ) ) )
fwrite( resB, file.path( OD, "mechanism_split.csv" ) )
cat( "\n== B. partial r, log count of full-990 filers (excl. H&U) with / without government grants, 2022, OLS ==\n" )
show( resB[ estimator == "ols", .( term, outcome, r ) ] )
cat( "\n== B. with vs without government grants, by estimator ==\n" )
show( resB[ outcome %in% c( "with_gg", "without_gg" ), .( term, outcome, estimator, r ) ] )
