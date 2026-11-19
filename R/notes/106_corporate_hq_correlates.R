## 106_corporate_hq_correlates.R
## Is corporate-headquarters presence a distinct metro trait, or a proxy for something
## the models already hold (income, education, inequality, generosity, foundations,
## social capital, market concentration, size)? Measures from 28_cov_corporate_hq.R:
##   hq_pub        public-company HQs (SEC EDGAR, 2000-24)
##   hq_pub_large  large-accelerated-filer HQs (2011-24)
##   cbp_hq_est    CBP NAICS 551114 managing-office establishments (2000-23)
##   cbp_hq_est100 551114 establishments with 100+ employees
##   cbp_hq_emp    551114 employment
## Every extensive variable is in SAMI form (log total, floored per capita with flog(),
## net of log P year by year), as in the multiverse models; rates as they are.
##
## Parts (metros only):
##   A. correlations of each HQ measure with the comparison variables, three ways:
##        between : metro means over the years each pair is observed
##        within  : metro-demeaned (pooled metro-years)
##        twoway  : metro- and year-demeaned (alternating projections)
##   B. R^2 of each HQ measure on the 13 lean-model covariates (+ log P): between metros
##      (metro means, 2011-23) and two-way within (metro and year FE). Low R^2 = the HQ
##      measure carries variation the model does not already hold.
## Outputs: <MV_BUILD>/data/diagnostics/corphq_correlates.csv, corphq_r2.csv

source( "R/lib/00_setup.R" )
options( width = 200 )
## flog(), resid_logP() and TIME_INVARIANT_VARS as in R/lib/40_multiverse.R (copied so
## this script does not need fixest)
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
resid_logP <- function( y, logP, year ) {
  out <- rep( NA_real_, length( y ) )
  for ( t in unique( year ) ) {
    i <- which( year == t & is.finite( y ) & is.finite( logP ) )
    if ( length( i ) > 3 ) { X <- cbind( 1, logP[ i ] ); out[ i ] <- y[ i ] - X %*% qr.coef( qr( X ), y[ i ] ) }
  }
  out
}
TIME_INVARIANT_VARS <- "hist_pre1970"

d <- as.data.table( arrow::read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
d[ , logP := log( population ) ]
d <- d[ cbsa_type == "metro" & year %in% YEARS ]
sami <- function( x ) resid_logP( flog( x, d$population ), d$logP, d$year )

HQ <- c( hq_pub = "Public-company HQs (EDGAR)", hq_pub_large = "Large-filer HQs (EDGAR, 2011+)",
         cbp_hq_est = "CBP 551114 establishments", cbp_hq_est100 = "CBP 551114 est., 100+ employees",
         cbp_hq_emp = "CBP 551114 employment" )
HQ <- HQ[ names( HQ ) %in% names( d ) ]
HQ <- HQ[ vapply( names( HQ ), function( v ) sum( d[[ v ]] > 0, na.rm = TRUE ) > 100, TRUE ) ]
for ( v in names( HQ ) ) set( d, j = paste0( "z_", v ), value = sami( d[[ v ]] ) )

## comparison variables: label, construct, transform (sami / log / rate)
CMP <- fread( text = "
var|label|construct|tf
logP|Log population|size|rate
pcpi|Per-capita personal income (log)|income|log
med_hh_income|Median household income (log)|income|log
qcew_avg_pay|Average private pay (log)|income|log
gdp_per_worker|Real GDP per job (log)|income|log
pct_ba|Share with a BA|education|rate
gini_m|Household-income Gini (2010+)|inequality|rate
poverty_rate|Poverty rate|need|rate
unemp_rate|Unemployment rate|need|rate
pct_65plus|Share 65+|need|rate
contrib_total|Itemized contributions (SAMI)|generosity|sami
contrib_per_return|Contributions per return (log)|generosity|log
contrib_share_agi|Contributions / AGI|generosity|rate
pf_assets|Foundation assets (SAMI)|foundations|sami
n_pf|Private foundations (SAMI)|foundations|sami
pf_grants|Foundation grants paid (SAMI)|foundations|sami
socap_sk2|Social capital index (Rupasingha)|social capital|rate
socap_assn_n|Associations (SAMI)|social capital|sami
turnout|Presidential turnout|social capital|rate
rel_hhi|Religious concentration (HHI)|heterogeneity|rate
pct_nonwhite|Share nonwhite|heterogeneity|rate
pct_foreign_born|Share foreign-born|heterogeneity|rate
age_sd|Age dispersion|heterogeneity|rate
mkt_hhi_rev_local|Nonprofit revenue HHI (local)|market concentration|rate
mkt_cr4_rev_local|Nonprofit revenue CR4 (local)|market concentration|rate
patents_frac|Patents (SAMI)|innovation|sami
gdp_real|Real GDP (SAMI)|economy|sami
gov_earn_sl|State and local gov. earnings (SAMI)|government|sami
tr_medicaid|Medicaid transfers (SAMI)|government|sami
lag_g10|Population growth, 10-year kernel|growth|rate
n_filer_c3|Active 501(c)(3) filers (SAMI) [outcome]|nonprofit sector|sami
exp_c3|501(c)(3) expenses (SAMI) [outcome]|nonprofit sector|sami
", sep = "|" )
CMP <- CMP[ var %in% names( d ) ]
for ( i in seq_len( nrow( CMP ) ) ) {
  x <- as.numeric( d[[ CMP$var[ i ] ]] )
  set( d, j = paste0( "c_", CMP$var[ i ] ), value = switch( CMP$tf[ i ],
       sami = sami( x ), log = log( ifelse( x > 0, x, NA_real_ ) ), rate = x ) )
}

## ---- A. correlations ----------------------------------------------------------------------
demean2 <- function( x, g1, g2, it = 30 ) {
  for ( k in seq_len( it ) ) { x <- x - ave( x, g1 ); x <- x - ave( x, g2 ) }
  x
}
cor3 <- function( a, b ) {
  ok <- is.finite( a ) & is.finite( b )
  m <- d$cbsa[ ok ]; y <- d$year[ ok ]; a <- a[ ok ]; b <- b[ ok ]
  if ( length( a ) < 200 ) return( list( between = NA_real_, within = NA_real_, twoway = NA_real_, n = length( a ) ) )
  mb <- data.table( m, a, b )[ , .( a = mean( a ), b = mean( b ) ), by = m ]
  list( between = cor( mb$a, mb$b ),
        within = cor( a - ave( a, m ), b - ave( b, m ) ),
        twoway = cor( demean2( a, m, y ), demean2( b, m, y ) ),
        n = length( a ) )
}
resA <- rbindlist( lapply( names( HQ ), function( h ) rbindlist( lapply( seq_len( nrow( CMP ) ), function( i ) {
  r <- cor3( d[[ paste0( "z_", h ) ]], d[[ paste0( "c_", CMP$var[ i ] ) ]] )
  data.table( hq = h, var = CMP$var[ i ], label = CMP$label[ i ], construct = CMP$construct[ i ], as.data.table( r ) )
} ) ) ) )
## HQ measures with each other
pairs <- CJ( a = names( HQ ), b = names( HQ ) )[ a < b ]
resH <- rbindlist( lapply( seq_len( nrow( pairs ) ), function( i ) {
  r <- cor3( d[[ paste0( "z_", pairs$a[ i ] ) ]], d[[ paste0( "z_", pairs$b[ i ] ) ]] )
  data.table( hq = pairs$a[ i ], var = pairs$b[ i ], label = HQ[ pairs$b[ i ] ], construct = "HQ measure", as.data.table( r ) )
} ) )
resA <- rbind( resH, resA )
fwrite( resA, file.path( OUT$diag, "corphq_correlates.csv" ) )

## ---- B. R^2 on the lean model's covariates --------------------------------------------------
LEAN <- c( "poverty_rate", "pct_65plus", "gini_m", "pct_nonwhite", "rel_hhi", "age_sd", "pf_assets",
           "pct_ba", "turnout", "lag_g10", "gov_earn_sl", "tr_medicaid", "hist_pre1970" )
EXT <- c( "pf_assets", "gov_earn_sl", "tr_medicaid" )
X <- d[ , c( "cbsa", "year", "logP" ), with = FALSE ]
for ( v in LEAN ) set( X, j = v, value = if ( v %in% EXT ) flog( d[[ v ]], d$population ) else as.numeric( d[[ v ]] ) )
r2 <- function( y, M ) { ok <- is.finite( y ) & stats::complete.cases( M ); summary( lm( y[ ok ] ~ M[ ok, , drop = FALSE ] ) )$r.squared }
resB <- rbindlist( lapply( names( HQ ), function( h ) {
  y <- flog( d[[ h ]], d$population )                       # log total; log P on the right
  ok <- is.finite( y ) & complete.cases( X[ , c( "logP", LEAN ), with = FALSE ] ) & d$year %in% 2011:2023
  Z <- data.table( X[ ok ], y = y[ ok ] )
  mb <- Z[ , lapply( .SD, mean ), by = cbsa, .SDcols = c( "y", "logP", LEAN ) ]
  between_logP <- r2( mb$y, as.matrix( mb[ , "logP" ] ) )
  between_all  <- r2( mb$y, as.matrix( mb[ , c( "logP", LEAN ), with = FALSE ] ) )
  ## SAMI of the HQ measure on the covariates (size removed first)
  between_sami <- r2( resid( lm( mb$y ~ mb$logP ) ), as.matrix( mb[ , LEAN, with = FALSE ] ) )
  tv <- setdiff( c( "logP", LEAN ), TIME_INVARIANT_VARS )
  W <- Z[ , c( list( y = demean2( y, cbsa, year ) ),
               lapply( .SD, function( v ) demean2( v, cbsa, year ) ) ), .SDcols = tv ]
  within_all <- r2( W$y, as.matrix( W[ , tv, with = FALSE ] ) )
  data.table( hq = h, metros = nrow( mb ), metro_years = nrow( Z ),
              between_r2_logP = between_logP, between_r2_logP_lean = between_all,
              between_r2_sami_on_lean = between_sami, twoway_r2_on_lean = within_all )
} ) )
fwrite( resB, file.path( OUT$diag, "corphq_r2.csv" ) )

## ---- print ------------------------------------------------------------------------------------
cat( "\n== A. correlations with each HQ measure (SAMI form), metros ==\n" )
for ( h in names( HQ ) ) {
  cat( "\n--", HQ[ h ], "--\n" )
  x <- resA[ hq == h | ( construct == "HQ measure" & var == h ) ]
  x <- x[ order( construct != "HQ measure", -abs( between ) ) ]
  x[ construct == "HQ measure" & var == h, label := HQ[ hq ] ]   # name the other measure of the pair
  print( x[ , .( label, construct, between = round( between, 2 ), within = round( within, 2 ),
                 twoway = round( twoway, 2 ), n ) ], nrows = 100 )
}
cat( "\n== B. R^2 of each HQ measure (log total) on the 13 lean covariates, metros 2011-23 ==\n" )
print( resB[ , lapply( .SD, function( z ) if ( is.numeric( z ) && max( z ) <= 1 ) round( z, 3 ) else z ) ] )

## ---- C. robustness: how much variation is within metros, and the pre-2017 CBP series ---------
## CBP drops cells under 3 establishments from 2017, so check the within correlations on 2000-16.
wshare <- rbindlist( lapply( names( HQ ), function( h ) {
  z <- d[[ paste0( "z_", h ) ]]; ok <- is.finite( z )
  data.table( hq = h, within_share = var( z[ ok ] - ave( z[ ok ], d$cbsa[ ok ] ) ) / var( z[ ok ] ) )
} ) )
cat( "\n== C1. share of SAMI variance that is within metros ==\n" )
print( wshare[ , .( hq, within_share = round( within_share, 3 ) ) ] )
d_all <- d; d <- d_all[ year <= 2016 ]
resC <- rbindlist( lapply( c( "cbp_hq_est", "cbp_hq_est100", "hq_pub" ), function( h ) rbindlist( lapply( seq_len( nrow( CMP ) ), function( i ) {
  r <- cor3( d[[ paste0( "z_", h ) ]], d[[ paste0( "c_", CMP$var[ i ] ) ]] )
  data.table( hq = h, label = CMP$label[ i ], as.data.table( r ) )
} ) ) ) )
d <- d_all
cat( "\n== C2. 2000-2016 only: largest |within| and |two-way| correlation with any comparison variable ==\n" )
print( resC[ , .( max_abs_within = round( max( abs( within ), na.rm = TRUE ), 2 ),
                  max_abs_twoway = round( max( abs( twoway ), na.rm = TRUE ), 2 ),
                  top = label[ which.max( abs( twoway ) ) ] ), by = hq ] )
fwrite( resC, file.path( OUT$diag, "corphq_correlates_2000_2016.csv" ) )
