## 91_government_measures.R
## First look at the direct vs indirect government measures
## (22_cov_government.R, 12_cov_bea.R; notes/government-funding-measures.md)
## in the benchmark cross-section models.
##
##   Part 1  scaling exponents (log X on log P) and correlations among the
##           measures (SAMI / rate forms), metros, 2012 and 2022
##   Part 2  benchmark model (log Y on log P + % BA, foundation assets, turnout,
##           % nonwhite, poverty) with each government measure entered in
##           place of the default (state + local earnings), alone and in
##           direct + indirect pairs. Partial r = t / sqrt(t^2 + df), HC1 or
##           state-clustered. Also reports how far the five benchmark partial
##           r move when government enters.
## Samples: 2022 and 2012 (Census of Governments years: no interpolation),
## metros and all CBSAs; OLS and state FE.
## Writes D:/MULTIVERSE/data/government/.

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ) } )
setFixest_notes( FALSE )
OD <- file.path( BUILD, "data", "government" ); dir.create( OD, showWarnings = FALSE )
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
rs <- function( y, logP ) resid( lm( y ~ logP ) )
z <- function( x ) ( x - mean( x ) ) / sd( x )

panel <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
panel[ , logP := log( population ) ]
panel[ , cog_noned := cog_curop - cog_educ_curop ]
panel[ , lsgov_emp := qcew_lgov_emp + qcew_sgov_emp ]

## measures: extensive (SAMI form of flog) or rate (as is)
EXT <- c( A1_gov_earn_sl = "gov_earn_sl", A2_lgov_emp = "qcew_lgov_emp", A2b_lsgov_emp = "lsgov_emp",
          A3_cog_dge = "cog_dge", A3b_cog_noned = "cog_noned",
          B1_tr_medmaint = "tr_medmaint", B1b_tr_medicaid = "tr_medicaid", B2_cog_welfare = "cog_welfare" )
RATE <- c( A4_acs_pct_gov = "acs_pct_gov", C2_labor_intensity = "gov_labor_intensity",
           C3_indirect_share = "gov_indirect_share" )
LAB <- c( A1_gov_earn_sl = "A1 S&L government earnings (current default)",
          A2_lgov_emp = "A2 local government jobs (QCEW)", A2b_lsgov_emp = "A2b state + local government jobs (QCEW)",
          A3_cog_dge = "A3 local direct general expenditure (CoG)", A3b_cog_noned = "A3b local current operations excl. education (CoG)",
          A4_acs_pct_gov = "A4 % residents working for government (ACS)",
          B1_tr_medmaint = "B1 Medicare + Medicaid + income maintenance (BEA)", B1b_tr_medicaid = "B1b Medicaid (BEA)",
          B2_cog_welfare = "B2 local public welfare spending (CoG)",
          C1_delivery = "C1 delivery residual: local gov jobs given local spending",
          C2_labor_intensity = "C2 local gov wages / local current operations",
          C3_indirect_share = "C3 transfers / (transfers + S&L gov wages)" )
FIVE <- c( "pct_ba", "pf", "turnout", "pct_nonwhite", "poverty_rate" )
OUTC <- c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. hosp/univ" )

make <- function( yr, unit, outcome ) {
  d <- panel[ year == yr & ( unit == "all" | cbsa_type == "metro" ) ]
  need <- c( outcome, "pct_ba", "pf_assets", "turnout", "pct_nonwhite", "poverty_rate", unname( EXT ), unname( RATE ),
             "cog_curop" )
  d <- d[ complete.cases( d[ , ..need ] ) & get( outcome ) > 0 ]
  d[ , ly := log( get( outcome ) ) ]
  d[ , pf := rs( flog( pf_assets, population ), logP ) ]
  for ( k in names( EXT ) ) set( d, j = k, value = rs( flog( d[[ EXT[ k ] ]], d$population ), d$logP ) )
  for ( k in names( RATE ) ) set( d, j = k, value = d[[ RATE[ k ] ]] )
  d[ , C2_labor_intensity := log( C2_labor_intensity ) ]
  d[ , C1_delivery := resid( lm( flog( qcew_lgov_emp, population ) ~ flog( cog_curop, population ) ) ) ]
  for ( v in c( FIVE, names( LAB ) ) ) set( d, j = v, value = z( d[[ v ]] ) )
  d[]
}

## ---- Part 1: scaling and correlations ---------------------------------------------

sc <- rbindlist( lapply( c( 2012L, 2022L ), function( yr ) {
  d <- panel[ year == yr & cbsa_type == "metro" ]
  rbindlist( lapply( names( EXT ), function( k ) {
    x <- d[[ EXT[ k ] ]]; ok <- is.finite( x ) & x > 0
    m <- lm( log( x[ ok ] ) ~ d$logP[ ok ] )
    data.table( year = yr, measure = k, label = LAB[ k ], n = sum( ok ),
                beta = coef( m )[2], se = sqrt( vcov( m )[ 2, 2 ] ) ) } ) ) } ) )
sc[ , `:=`( beta = round( beta, 3 ), se = round( se, 3 ) ) ]
fwrite( sc, file.path( OD, "p1_scaling.csv" ) ); print( sc )

d22 <- make( 2022L, "metro", "n_filer_c3" )
cm <- round( cor( d22[ , c( names( LAB ), "ly", FIVE ), with = FALSE ] ), 2 )
fwrite( as.data.table( cm, keep.rownames = "var" ), file.path( OD, "p1_cor_2022_metro.csv" ) )
print( cm[ names( LAB ), c( names( LAB ), "pct_ba", "poverty_rate" ) ] )

## ---- Part 2: benchmark models -------------------------------------------------------

prt <- function( m, v ) {
  ct <- coeftable( m ); if ( !v %in% rownames( ct ) ) return( NA_real_ )
  t <- ct[ v, 3 ]; t / sqrt( t^2 + degrees_freedom( m, "t" ) )
}
SPECS <- c( list( none = character( 0 ) ), as.list( setNames( names( LAB ), names( LAB ) ) ),
            list( "A1+B1" = c( "A1_gov_earn_sl", "B1_tr_medmaint" ), "A2+B1" = c( "A2_lgov_emp", "B1_tr_medmaint" ),
                  "A3+B1" = c( "A3_cog_dge", "B1_tr_medmaint" ), "A3+B2" = c( "A3_cog_dge", "B2_cog_welfare" ),
                  "A2+A3" = c( "A2_lgov_emp", "A3_cog_dge" ), "A3+B1+C1" = c( "A3_cog_dge", "B1_tr_medmaint", "C1_delivery" ) ) )
res <- list()
for ( yr in c( 2022L, 2012L ) ) for ( unit in c( "metro", "all" ) ) for ( oc in names( OUTC ) ) {
  d <- make( yr, unit, oc )
  for ( est in c( "ols", "state_fe" ) ) for ( s in names( SPECS ) ) {
    g <- SPECS[[ s ]]
    f <- as.formula( paste( "ly ~ logP +", paste( c( FIVE, g ), collapse = " + " ),
                            if ( est == "state_fe" ) "| state_fips" else "" ) )
    m <- feols( f, d, vcov = if ( est == "ols" ) "hetero" else ~state_fips )
    res[[ length( res ) + 1 ]] <- data.table( year = yr, unit, outcome = oc, est, spec = s, n = nobs( m ),
      term = c( FIVE, g ), role = c( rep( "benchmark", 5 ), rep( "government", length( g ) ) ),
      b = coef( m )[ c( FIVE, g ) ], pr = sapply( c( FIVE, g ), prt, m = m ),
      p = pvalue( m )[ c( FIVE, g ) ], r2 = r2( m, "ar2" ) )
  }
}
res <- rbindlist( res )
res[ , `:=`( b = round( b, 4 ), pr = round( pr, 3 ), p = signif( p, 3 ), r2 = round( r2, 4 ) ) ]
fwrite( res, file.path( OD, "p2_models.csv" ) )

## government terms, entered alone and in pairs
gt <- res[ role == "government" ]
gw <- dcast( gt, spec + term ~ year + unit + outcome + est, value.var = "pr" )
fwrite( gw, file.path( OD, "p2_government_partial_r.csv" ) )
options( width = 250 )
cat( "\n== partial r of government terms (cols: year_unit_outcome_est) ==\n" )
print( gw[ , c( "spec", "term", grep( "^2022_metro", names( gw ), value = TRUE ),
                grep( "^2012_metro_n_filer", names( gw ), value = TRUE ) ), with = FALSE ] )

## do the five benchmark partial r move when government enters?
base <- res[ spec == "A1_gov_earn_sl" & role == "benchmark", .( year, unit, outcome, est, term, pr_a1 = pr ) ]
mv <- merge( res[ role == "benchmark" ], base, by = c( "year", "unit", "outcome", "est", "term" ) )
mv <- mv[ , .( max_abs_shift = max( abs( pr - pr_a1 ) ), mean_abs_shift = mean( abs( pr - pr_a1 ) ),
               sign_changes = sum( sign( pr ) != sign( pr_a1 ) & ( abs( pr ) > 0.05 | abs( pr_a1 ) > 0.05 ) ) ),
          by = .( spec ) ]
fwrite( mv, file.path( OD, "p2_benchmark_shift.csv" ) )
cat( "\n== shift in the five benchmark partial r vs the current default (A1), all samples ==\n" ); print( mv )
fit <- res[ term == "pct_ba", .( adj_r2 = mean( r2 ) ), by = .( year, unit, outcome, est, spec ) ]
fit <- dcast( fit, spec ~ year + unit + outcome + est, value.var = "adj_r2" )
fwrite( fit, file.path( OD, "p2_adj_r2.csv" ) )
log_msg( "wrote ", OD )
