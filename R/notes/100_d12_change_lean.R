## 100_d12_change_lean.R
## D12 Evidence 2 (58_outline_analyses.R part P, change model) refit with the 13-covariate lean default.
## Old model (58 P): change in the filer / grassroots SAMI from 2000-02 to 2022-24 on the 2000-02 SAMI,
## 2000-2024 population growth and the six old covariates measured in 2022 (diversity = % nonwhite: 58's
## "race_hhi" column holds pct_nonwhite from run v5 on, see divx() there), then
## + mean filer age (2022, c12_age_by_metro.csv). Old: R2 .22 -> .40, age r -.45, growth -.28 -> -.47.
##
## Here every covariate is measured at the START of the window (the 13 lean terms as in 96: totals
## flog(x, pop), every term residualized on log P of the start year and standardized; HC1 SEs).
## Windows:
##   w10  2010-12 -> 2022-24, covariates 2010 (main lean version: all 13 terms exist in 2010)
##   w00  2000-02 -> 2022-24, covariates 2000 (the card's window; gini_m starts 2010, so 12 terms;
##        arm lean_gini10 adds the 2010 Gini as a post-start proxy). Every other term exists in 2000:
##        rates from census 2000 / interpolated series, totals from 2000 data, lag_g10 at 2000 from
##        1970-2000 census history, the endowment from the 2000 BMF.
##   w12  2012-14 -> 2022-24, covariates 2012 (check: the start window clears the 2011 IRS
##        auto-revocations, which purged registered non-filers, i.e. grassroots)
## Growth: in-window growth g_in = log(P2024 / P_start) as in the old model. The growth kernel is measured
## at the start year, so its inputs (growth up to the start year) are disjoint from g_in; arm lean_k22
## shows what the 2022 kernel (which loads on in-window growth) would do, arm lean_nok drops the kernel.
## Age: mean filer age (2022; the card's measure, partly an outcome of in-window entry) and, as a check,
## mean age of the registered 501(c)(3) public-charity stock in the start year (BMF ruling years).
## Arms per window (each with and without age): card6_22 (the card's six covariates, 2022 values: a
## replication; card6_22_c9 = the same on the card's 363-metro c9 sample, w00 only), old6 (96's six, pct_nonwhite, start year), lean, lean + start-year age, lean_nok,
## lean_k22, (w00) lean_gini10.
## Writes D:/MULTIVERSE/data/outline/v11/single/d12_change_lean*.csv. Single process.
##   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/notes/100_d12_change_lean.R > D:/MULTIVERSE/logs/100.log 2>&1

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )          # load_panel(), flog()
suppressPackageStartupMessages( library( arrow ) )
setFixest_notes( FALSE ); setFixest_nthreads( 1 ); data.table::setDTthreads( 2 )
OD <- file.path( BUILD, "data", "outline", "v11", "single" ); dir.create( OD, recursive = TRUE, showWarnings = FALSE )
options( width = 250 )

LEAN <- c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate",
           "gov_earn_sl", "tr_medicaid", "lag_g10", "hist_pre1970",
           "pct_65plus", "gini_m", "rel_frac", "age_sd" )
OLD6 <- c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate", "gov_earn_sl" )
TOT  <- c( "pf_assets", "gov_earn_sl", "tr_medicaid", "hist_pre1970" )
CARD6 <- c( nonwhite_22 = "pct_nonwhite", pf_22 = "pf_assets", turnout_22 = "turnout", gov_22 = "gov_earn_sl",
            pov_22 = "poverty_rate", ba_22 = "pct_ba" )                    # the card's covariates, 2022 (with g_c)
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
pr_of <- function( t, df ) t / sqrt( t^2 + df )

## ---- panel and SAMIs ---------------------------------------------------------------------------
P <- load_panel()
P[ , rel_frac := 1 - rel_hhi ]
M <- P[ cbsa_type == "metro" ]
M[ , n_grass := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]                     # as 73_pre2000_growth.R
sami_y <- function( y, lp ) { ok <- is.finite( y ) & y > 0; z <- rep( NA_real_, length( y ) )
  z[ ok ] <- resid( lm( log( y[ ok ] ) ~ lp[ ok ] ) ); z }
M[ , `:=`( s_n = sami_y( n_filer_c3, logP ), s_g = sami_y( n_grass, logP ) ), by = year ]
wavg <- function( v, yrs, nm ) setnames( M[ year %in% yrs, .( z = mean( get( v ), na.rm = TRUE ) ), by = cbsa ], "z", nm )

## check against the card's outcome file (c9_metro_1970_2024.csv, 73)
c9 <- fread( file.path( BUILD, "data", "catchup", "c9_metro_1970_2024.csv" ), colClasses = list( character = "cbsa" ) )
chk <- Reduce( function( a, b ) merge( a, b, by = "cbsa" ), list( c9[ , .( cbsa, s0_n_filer_c3, s1_n_filer_c3, s0_n_grass_c3pc, s1_n_grass_c3pc ) ],
  wavg( "s_n", 2000:2002, "s0n" ), wavg( "s_n", 2022:2024, "s1n" ), wavg( "s_g", 2000:2002, "s0g" ), wavg( "s_g", 2022:2024, "s1g" ) ) )
log_msg( "SAMI check vs c9 (", nrow( chk ), " metros): max |diff| s0n ", signif( chk[ , max( abs( s0n - s0_n_filer_c3 ) ) ], 2 ),
         ", s1n ", signif( chk[ , max( abs( s1n - s1_n_filer_c3 ) ) ], 2 ), ", s0g ", signif( chk[ , max( abs( s0g - s0_n_grass_c3pc ) ) ], 2 ),
         ", s1g ", signif( chk[ , max( abs( s1g - s1_n_grass_c3pc ) ) ], 2 ) )

## ---- sector age ---------------------------------------------------------------------------------
a12 <- fread( file.path( BUILD, "data", "catchup", "c12_age_by_metro.csv" ), colClasses = list( character = "cbsa" ) )[
  , .( cbsa, age_f = mean_age_filers, age_all22 = mean_age_all ) ]
## start-year age of the registered stock: mean (year - ruling year), 501(c)(3) public charities in the BMF
SP <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )[ cbsa_type == "metro", .( geoid, cbsa ) ]
b <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "subsec", "pf", "ryear" ),
            colClasses = list( character = c( "EIN", "geoid", "subsec" ) ) )
b <- b[ year %in% c( 2000L, 2010L, 2012L, 2022L ) & as.integer( subsec ) == 3L & pf == FALSE & is.finite( ryear ) & ryear >= 1800 & ryear <= year & !is.na( geoid ) ]
b <- unique( b, by = c( "EIN", "year" ) )[ , geoid := sprintf( "%05d", as.integer( geoid ) ) ]
ALLG <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = "geoid" ) )$geoid
log_msg( "BMF orgs by year (share not on the county spine): ",
         paste( b[ , .( N = .N, u = round( mean( !geoid %in% ALLG ), 3 ) ), keyby = year ][ , paste0( year, " ", N, " (", u, ")" ) ], collapse = "; " ) )
ab <- merge( b, SP, by = "geoid" )[ , .( age0 = mean( year - ryear ), n_reg = .N ), by = .( cbsa, year ) ]; rm( b ); gc( verbose = FALSE )
x22 <- merge( ab[ year == 2022 ], a12, by = "cbsa" )
log_msg( "start-age check: BMF-built 2022 mean age vs c12 mean_age_all, r = ", round( x22[ , cor( age0, age_all22 ) ], 3 ),
         ", mean diff ", round( x22[ , mean( age0 - age_all22 ) ], 2 ), " (", nrow( x22 ), " metros)" )

## ---- one window's analysis data -----------------------------------------------------------------
tr <- function( x, pop, lp, tot = FALSE ) { if ( tot ) x <- flog( x, pop ); z <- resid( lm( x ~ lp ) ); z / sd( z ) }
build <- function( S, s0yrs ) {
  LV <- setdiff( LEAN, "gini_m" )
  d <- M[ year == S, c( "cbsa", "cbsa_title", "population", "logP", LV ), with = FALSE ]
  d <- merge( d, M[ year == max( S, 2010L ), .( cbsa, gini_m ) ], by = "cbsa" )        # 2010 proxy when S < 2010
  d <- merge( d, M[ year == 2024, .( cbsa, pop24 = population ) ], by = "cbsa" )
  d <- merge( d, M[ year == 2022, .( cbsa, logP22 = logP, pop22 = population, lag_g10_22 = lag_g10, nonwhite_22 = pct_nonwhite,
                                      pf_22 = pf_assets, turnout_22 = turnout, gov_22 = gov_earn_sl, pov_22 = poverty_rate, ba_22 = pct_ba ) ], by = "cbsa" )
  d <- Reduce( function( a, z ) merge( a, z, by = "cbsa" ), list( d, wavg( "s_n", s0yrs, "s0n" ), wavg( "s_n", 2022:2024, "s1n" ),
                                                                  wavg( "s_g", s0yrs, "s0g" ), wavg( "s_g", 2022:2024, "s1g" ),
                                                                  a12[ , .( cbsa, age_f ) ], ab[ year == S, .( cbsa, age0 ) ] ) )
  n0 <- nrow( d ); drop <- d[ !complete.cases( d ), cbsa_title ]
  d <- d[ complete.cases( d ) ]
  log_msg( "  window from ", S, ": ", nrow( d ), " of ", n0, " metros; dropped: ", paste( drop, collapse = " | " ) )
  raw <- copy( d )
  d[ , `:=`( d_n = s1n - s0n, d_g = s1g - s0g, g_raw = log( pop24 / population ) ) ]
  d[ , `:=`( g_in = tr( g_raw, population, logP ), g_c = tr( g_raw, pop22, logP22 ) ) ]      # g_c: card arms (58 P used 2022 log P)
  for ( v in LEAN ) set( d, j = v, value = tr( d[[ v ]], d$population, d$logP, v %in% TOT ) )
  d[ , lag_g10_22 := tr( lag_g10_22, pop22, logP22 ) ]
  for ( v in names( CARD6 ) ) set( d, j = v, value = tr( d[[ v ]], d$pop22, d$logP22, CARD6[[ v ]] %in% TOT ) )
  d[ , `:=`( age_f = tr( age_f, pop22, logP22 ), age0 = tr( age0, population, logP ) ) ]
  list( d = d, raw = raw ) }

fitc <- function( d, y, xs ) {
  r <- feols( as.formula( paste( y, "~", paste( xs, collapse = " + " ) ) ), d, vcov = "hetero" )
  ct <- coeftable( r ); ct <- ct[ rownames( ct ) != "(Intercept)", , drop = FALSE ]; df <- degrees_freedom( r, "resid" )
  data.table( term = rownames( ct ), b = ct[ , 1 ], se = ct[ , 2 ], t = ct[ , 3 ], p = ct[ , 4 ], r = pr_of( ct[ , 3 ], df ),
              lo = pr_of( ct[ , 3 ] - 1.96, df ), hi = pr_of( ct[ , 3 ] + 1.96, df ), r2 = as.numeric( r2( r, "r2" ) ), n = nobs( r ) ) }

WIN <- list( w10 = list( S = 2010L, s0 = 2010:2012 ), w00 = list( S = 2000L, s0 = 2000:2002 ), w12 = list( S = 2012L, s0 = 2012:2014 ) )
res <- list(); cors <- list()
for ( w in names( WIN ) ) {
  S <- WIN[[ w ]]$S; log_msg( "window ", w )
  B <- build( S, WIN[[ w ]]$s0 ); d <- B$d
  LS <- if ( S < 2010 ) setdiff( LEAN, "gini_m" ) else LEAN
  ARMS <- list( card6_22 = names( CARD6 ), old6 = OLD6, lean = LS, lean_nok = setdiff( LS, "lag_g10" ),
                lean_k22 = c( setdiff( LS, "lag_g10" ), "lag_g10_22" ) )
  if ( S < 2010 ) ARMS$lean_gini10 <- c( LS, "gini_m" )
  AGE <- list( none = character( 0 ), age_f = "age_f", age0 = "age0" )
  for ( y in c( "d_n", "d_g" ) ) { s0 <- if ( y == "d_n" ) "s0n" else "s0g"
    for ( ar in names( ARMS ) ) for ( ag in names( AGE ) ) {
      if ( ag == "age0" && !ar %in% c( "lean", "old6" ) ) next
      res[[ length( res ) + 1 ]] <- fitc( d, y, c( s0, if ( ar == "card6_22" ) "g_c" else "g_in", ARMS[[ ar ]], AGE[[ ag ]] ) )[ , `:=`( window = w, start = S, outcome = y, arm = ar, age = ag ) ] }
    ## replication on the card's sample (c9: Connecticut and incomplete 1970-90 coverage dropped)
    if ( w == "w00" ) for ( ag in c( "none", "age_f" ) )
      res[[ length( res ) + 1 ]] <- fitc( d[ cbsa %in% c9$cbsa ], y, c( s0, "g_c", names( CARD6 ), AGE[[ ag ]] ) )[ , `:=`( window = w, start = S, outcome = y, arm = "card6_22_c9", age = ag ) ] }
  rw <- B$raw
  cors[[ w ]] <- data.table( window = w, n = nrow( d ),
    cor_kernelS_gin = cor( rw$lag_g10, d$g_raw ), cor_kernel22_gin = cor( rw$lag_g10_22, d$g_raw ), cor_kernelS_kernel22 = cor( rw$lag_g10, rw$lag_g10_22 ),
    cor_agef_gin = cor( rw$age_f, d$g_raw ), cor_age0_gin = cor( rw$age0, d$g_raw ), cor_agef_age0 = cor( rw$age_f, rw$age0 ),
    cor_agef_dn = cor( rw$age_f, d$d_n ), cor_age0_dn = cor( rw$age0, d$d_n ), cor_gin_dn = cor( d$g_raw, d$d_n ),
    cor_agef_kernelS = cor( rw$age_f, rw$lag_g10 ), cor_age0_endow = cor( d$age0, d$hist_pre1970 ),
    sd_dn = sd( d$d_n ), sd_dg = sd( d$d_g ) )
}
res <- rbindlist( res ); cors <- rbindlist( cors )
res[ term == "g_c", term := "g_in" ]                                      # same growth, residualized on 2022 log P
fwrite( res, file.path( OD, "d12_change_lean.csv" ) ); fwrite( cors, file.path( OD, "d12_change_lean_cor.csv" ) )
cat( "\n== correlations (raw values; g = in-window log growth) ==\n" ); print( r3( cors ) )

## summary: R2 and the partial r of the terms the card discusses
KEY <- c( "age_f", "age0", "g_in", "lag_g10", "lag_g10_22", "turnout", "turnout_22", "s0n", "s0g", "hist_pre1970", "pct_ba", "ba_22",
          "pf_assets", "pf_22", "poverty_rate", "pov_22", "tr_medicaid", "rel_frac", "pct_nonwhite", "nonwhite_22" )
sm <- dcast( res[ term %in% KEY ], window + outcome + arm + age + n + r2 ~ term, value.var = "r" )
setcolorder( sm, c( "window", "outcome", "arm", "age", "n", "r2", intersect( KEY, names( sm ) ) ) )
sm <- sm[ order( match( window, names( WIN ) ), outcome, match( arm, c( "card6_22_c9", "card6_22", "old6", "lean", "lean_nok", "lean_k22", "lean_gini10" ) ), match( age, c( "none", "age_f", "age0" ) ) ) ]
fwrite( sm, file.path( OD, "d12_change_lean_summary.csv" ) )
for ( w in names( WIN ) ) for ( y in c( "d_n", "d_g" ) ) {
  cat( "\n== partial r,", w, y, "==\n" ); z <- sm[ window == w & outcome == y ]
  print( r3( z[ , which( colSums( !is.na( z ) ) > 0 ), with = FALSE ] ) ) }
## full lean tables, main window
for ( y in c( "d_n", "d_g" ) ) { cat( "\n== lean, w10,", y, ": partial r, none / + mean filer age / + start-year age ==\n" )
  print( r3( dcast( res[ window == "w10" & outcome == y & arm == "lean" ], term ~ age, value.var = "r" ) ) ) }
log_msg( "done: ", OD )
