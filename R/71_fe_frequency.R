## 71_fe_frequency.R
## Does observing the panel at 5-year intervals improve within-metro estimates?
##   Part 1  signal vs noise in changes: for each covariate (SAMI form), the
##           variance of 1-, 5- and 10-year changes and the autocorrelation of
##           annual changes. Benchmarks: a steady trend gives var(D5)/var(D1)
##           = 25, a random walk 5, pure year-to-year noise around a fixed
##           level ~1. Interpolated series have perfectly persistent changes.
##   Part 2  the same within models under different time designs:
##           annual FE 2010-2022 | 5-year waves FE (2012/17/22; 2010/15/20) |
##           stacked non-overlapping 5-year differences | 10-year long
##           difference 2012->2022
##           for the default six covariates and the ten additive composites.
## Predictors are scaled by one fixed SD (pooled SAMI-form levels, 2010-2022),
## so coefficients and SEs are comparable across designs.
## Metros only. Writes to D:/MULTIVERSE/data/collinearity/ (p4_*).

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ) } )
setFixest_notes( FALSE )
CO <- file.path( BUILD, "data", "collinearity" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
## log of an extensive covariate with a per-capita floor (as flog() in 40_multiverse.R, run v6 on;
## switched here 2026-11-16). log(x + 1) put zero-asset metros (Eagle Pass, TX) ~11 SD below the mean.
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
options( width = 220 )

d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )[ cbsa_type == "metro" ]
setorder( d, cbsa, year )
d[ , logP := log( population ) ]
for ( v in c( "pf_assets", "n_pf", "pf_grants", "contrib_total", "gov_earn_sl", "gov_emp_sl", "rel_cng" ) )
  set( d, j = paste0( "l_", v ), value = flog( d[[ v ]], d$population ) )
d[ , `:=`( l_pcpi = log( pcpi ), l_mhi = log( med_hh_income ), l_contrib_per_return = log( contrib_per_return ) ) ]

## how each covariate is measured (drives how much real annual change exists)
FREQ <- c( race_hhi = "annual (PEP)", pct_nonwhite = "annual (PEP)", age_sd = "annual (PEP)", pct_65plus = "annual (PEP)",
  pct_foreign_born = "ACS 5-yr rolling", pct_ba = "ACS 5-yr rolling", gini = "ACS 5-yr rolling",
  poverty_rate = "annual model (SAIPE)", l_mhi = "annual model (SAIPE)", l_pcpi = "annual (BEA)",
  l_pf_assets = "annual (990-PF; interp. 2016-18)", l_pf_grants = "annual (990-PF; interp. 2016-18)", l_n_pf = "annual (990-PF; interp. 2016-18)",
  l_contrib_total = "annual (IRS; 2018 break)", l_contrib_per_return = "annual (IRS; 2018 break)", contrib_share_agi = "annual (IRS; 2018 break)",
  turnout = "every 4 yrs, interpolated", dem_margin = "every 4 yrs, interpolated", socap_respn = "to 2014, then flat",
  pct_adherent = "2010, 2020, interpolated", l_rel_cng = "2010, 2020, interpolated", rel_hhi = "2010, 2020, interpolated",
  l_gov_earn_sl = "annual (BEA)", l_gov_emp_sl = "annual (BEA)", gov_emp_share = "annual (BEA)" )
IV <- names( FREQ )
DEFAULT <- c( "race_hhi", "l_pf_assets", "turnout", "l_gov_earn_sl", "poverty_rate", "pct_ba" )
COMPOSITES <- list(
  affluence = c( l_pcpi = 1, l_mhi = 1, pct_ba = 1, poverty_rate = -1 ),
  diversity = c( pct_nonwhite = 1, pct_foreign_born = 1, race_hhi = -1 ),
  age_structure = c( age_sd = 1, pct_65plus = 1 ), inequality = c( gini = 1 ), rel_homogeneity = c( rel_hhi = 1 ),
  foundations = c( l_pf_assets = 1, l_pf_grants = 1, l_n_pf = 1 ),
  indiv_giving = c( l_contrib_total = 1, l_contrib_per_return = 1, contrib_share_agi = 1 ),
  civic = c( turnout = 1, socap_respn = 1 ), rel_conservative = c( pct_adherent = 1, l_rel_cng = 1, dem_margin = -1 ),
  government = c( l_gov_earn_sl = 1, l_gov_emp_sl = 1, gov_emp_share = 1 ) )
OUTV <- c( "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu" )

resid_logP <- function( y, logP, year ) {
  out <- rep( NA_real_, length( y ) )
  for ( t in unique( year ) ) {
    i <- which( year == t & is.finite( y ) & is.finite( logP ) )
    if ( length( i ) > 3 ) out[ i ] <- resid( lm( y[ i ] ~ logP[ i ] ) )
  }
  out
}

## SAMI forms, metros 2010-2022, balanced on every item
S <- d[ year %in% 2010:2022, c( "cbsa", "year", "state_fips", "population", "logP", IV, OUTV ), with = FALSE ]
S <- S[ complete.cases( S ) & n_filer_c3 > 0 & exp_c3_nohu > 0 & n_bmf_c3pc > 0 ]
S <- S[ , if ( .N == 13 ) .SD, by = cbsa ]
for ( v in IV ) set( S, j = v, value = resid_logP( S[[ v ]], S$logP, S$year ) )
for ( y in OUTV ) set( S, j = paste0( "s_", y ), value = resid_logP( log( S[[ y ]] ), S$logP, S$year ) )
## additive composites (z constants: pooled 2015-2022 SAMI forms, as in 70)
ref <- S[ year %in% 2015:2022 ]
for ( k in names( COMPOSITES ) ) {
  w <- COMPOSITES[[ k ]]
  z <- sapply( names( w ), function( v ) ( S[[ v ]] - mean( ref[[ v ]] ) ) / sd( ref[[ v ]] ) )
  if ( is.null( dim( z ) ) ) z <- matrix( z, ncol = 1 )
  set( S, j = paste0( "A_", k ), value = as.numeric( z %*% w ) / length( w ) )
}
AV <- paste0( "A_", names( COMPOSITES ) )
log_msg( "balanced panel: ", uniqueN( S$cbsa ), " metros x 13 years" )
setorder( S, cbsa, year )

## ---- Part 1: change variance by horizon --------------------------------------------
chg <- function( x, yr, k ) { o <- match( yr - k, yr ); x - x[ o ] }
V <- c( IV, AV, paste0( "s_", OUTV ) )
p1 <- rbindlist( lapply( V, function( v ) {
  x <- S[ , .( cbsa, year, x = get( v ) ) ]
  x[ , `:=`( d1 = chg( x, year, 1 ), d5 = chg( x, year, 5 ), d10 = chg( x, year, 10 ) ), by = cbsa ]
  x[ , d1lag := shift( d1 ), by = cbsa ]
  dm <- x$x - ave( x$x, x$cbsa ) - ave( x$x, x$year ) + mean( x$x )
  data.table( var = v, freq = if ( v %in% names( FREQ ) ) FREQ[[ v ]] else if ( grepl( "^A_", v ) ) "composite" else "outcome",
    within_share = var( dm ) / var( x$x ),
    sd_d1 = sd( x$d1, na.rm = TRUE ), sd_d5 = sd( x[ year %in% c( 2017, 2022 ), d5 ] ),
    sd_d10 = sd( x[ year == 2022, d10 ] ),
    ratio5 = var( x[ year %in% c( 2017, 2022 ), d5 ] ) / var( x$d1, na.rm = TRUE ),
    ratio10 = var( x[ year == 2022, d10 ] ) / var( x$d1, na.rm = TRUE ),
    ac_d1 = cor( x$d1, x$d1lag, use = "complete.obs" ) )
} ) )
fwrite( r3( p1 ), file.path( CO, "p4_change_variance.csv" ) )
print( r3( p1 ), nrows = 100 )

## ---- Part 2: time designs ------------------------------------------------------------
SC <- vapply( c( IV, AV ), function( v ) sd( S[[ v ]] ), numeric( 1 ) )   # fixed scaling
scaled <- function( DT, xs ) { for ( v in xs ) set( DT, j = v, value = DT[[ v ]] / SC[[ v ]] ); DT }
diffs <- function( DT, pairs, vars ) rbindlist( lapply( pairs, function( p ) {
  a <- DT[ year == p[ 1 ] ]; b <- DT[ year == p[ 2 ] ]
  m <- merge( a, b, by = "cbsa", suffixes = c( ".a", ".b" ) )
  out <- data.table( cbsa = m$cbsa, state_fips = m$state_fips.b, period = paste0( p[ 1 ], "-", p[ 2 ] ) )
  for ( v in vars ) set( out, j = v, value = m[[ paste0( v, ".b" ) ]] - m[[ paste0( v, ".a" ) ]] )
  out } ) )

DESIGNS <- list(
  annual_fe   = list( kind = "fe", years = 2010:2022 ),
  waves_12_17_22 = list( kind = "fe", years = c( 2012, 2017, 2022 ) ),
  waves_10_15_20 = list( kind = "fe", years = c( 2010, 2015, 2020 ) ),
  stacked_d5  = list( kind = "diff", pairs = list( c( 2012, 2017 ), c( 2017, 2022 ) ) ),
  long_d10    = list( kind = "diff", pairs = list( c( 2012, 2022 ) ) ),
  annual_d1   = list( kind = "diff", pairs = lapply( 2011:2022, function( t ) c( t - 1, t ) ) ) )
SETS <- list( default = DEFAULT, composites = AV )

res <- list(); fits <- list()
for ( y in OUTV ) for ( sn in names( SETS ) ) for ( dn in names( DESIGNS ) ) {
  xs <- SETS[[ sn ]]; ds <- DESIGNS[[ dn ]]; yv <- paste0( "s_", y )
  if ( ds$kind == "fe" ) {
    m <- scaled( copy( S[ year %in% ds$years ] ), xs )
    r <- feols( as.formula( paste( yv, "~", paste( xs, collapse = " + " ), "| cbsa + year" ) ), data = m, cluster = ~cbsa )
    wr2 <- as.numeric( r2( r, "wr2" ) )
  } else {
    m <- scaled( diffs( S, ds$pairs, c( yv, xs ) ), xs )
    multi <- length( ds$pairs ) > 1
    r <- feols( as.formula( paste( yv, "~", paste( xs, collapse = " + " ), if ( multi ) "| period" ) ), data = m,
                vcov = if ( multi ) ~cbsa else "hetero" )
    wr2 <- as.numeric( r2( r, if ( multi ) "wr2" else "r2" ) )
  }
  ct <- coeftable( r )
  res[[ length( res ) + 1 ]] <- data.table( outcome = y, set = sn, design = dn, term = rownames( ct ),
    est = ct[ , 1 ], se = ct[ , 2 ], t = ct[ , 3 ], p = ct[ , 4 ] )[ term != "(Intercept)" ]
  fits[[ length( fits ) + 1 ]] <- data.table( outcome = y, set = sn, design = dn, n = nobs( r ), r2_within = wr2,
    n_sig05 = sum( ct[ rownames( ct ) != "(Intercept)", 4 ] < 0.05 ), median_abs_t = median( abs( ct[ rownames( ct ) != "(Intercept)", 3 ] ) ),
    median_se = median( ct[ rownames( ct ) != "(Intercept)", 2 ] ) )
}
res <- rbindlist( res ); fits <- rbindlist( fits )
fwrite( r3( res ), file.path( CO, "p4_design_coefs.csv" ) ); fwrite( r3( fits ), file.path( CO, "p4_design_fit.csv" ) )
print( r3( fits ), nrows = 100 )
res[ , lab := sprintf( "%6.3f (%5.3f)%s", est, se, ifelse( p < .01, "**", ifelse( p < .05, "* ", "  " ) ) ) ]
for ( y in OUTV ) for ( sn in names( SETS ) ) {
  cat( "\n=====", y, sn, "\n" )
  print( dcast( res[ outcome == y & set == sn ], term ~ factor( design, levels = names( DESIGNS ) ), value.var = "lab" ) )
}
