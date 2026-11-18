## 89_need_socap_diversity.R
## Three measurement questions about the default covariates (2026-11-16):
##   1. need       is poverty the right need measure, or inequality (Gini)? Is a need index better?
##   2. civic      what loads on social capital (Rupasingha items, religion, turnout, census response,
##                 residential attachment)? Does turnout still dominate once census response is in?
##   3. diversity  how do turnout and census response relate to racial, religious, age and
##                 economic diversity?
##
## The panel's gini and med_hh_income are population-weighted means of county values, so they
## miss inequality between the counties of a metro. 'build' sums ACS county income brackets to
## the CBSA and computes metro-level inequality directly, plus need and attachment rates:
##   gini_m      grouped Gini of the metro household income distribution (B19001; brackets at
##               midpoints, top bracket mean from a Pareto tail fitted to 150k+/200k+)
##   inc_hhi5    HHI of the metro's household shares in the five NATIONAL income quintiles
##               (B19080 US cutoffs; 0.2 = the national mix, 1 = everyone in one quintile).
##               econ_div = (1 - inc_hhi5) / 0.8 (1 = national mix)
##   deep_pov    share of people below 50% of poverty (C17002); near_pov share below 200%
##   snap_rate   households receiving SNAP (B22003); rent_burden renters paying 30%+ (B25070)
##   own_rate    owner-occupied share (B25003); stay_1yr same house a year ago (B07003)
## ACS 5-year, stamped with the end year (as in 17_cov_acs.R), 2015-2022.
##
## Modes
##   build      fetch (cached in raw/cov/acs89) and write data/need_socap/acs_need_cbsa.csv
##   describe   correlations, factor structure, size gradients, diversity vs turnout/response
##   horse      default-model sub-universe with alternative need / civic specifications
##   figs       figures
## Outputs D:/MULTIVERSE/data/need_socap/
## Usage: Rscript R/89_need_socap_diversity.R build|describe|horse [workers]|figs

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
args <- commandArgs( TRUE ); MODE <- if ( length( args ) ) args[1] else "describe"
OD <- file.path( BUILD, "data", "need_socap" ); dir.create( OD, showWarnings = FALSE )
FN <- file.path( OD, "acs_need_cbsa.csv" )
## 2010-2024 since 2026-11-16 (was 2015-2022): the inequality fork needs gini_m in the
## within stratum (2010-2022); describe/horse still use 2015-2022
ACS_YEARS <- 2010:2024

## ---- build ---------------------------------------------------------------------------
if ( MODE == "build" ) {
  source( "R/geo_helpers.R" )
  suppressPackageStartupMessages( library( tidycensus ) )
  key_line <- grep( "^KEY <-", readLines( file.path( DROPBOX, "04 - PAPERS", "01 - In Progress",
                                                       "38 - resource environments", "CENSUS-DATA.R" ) ), value = TRUE )
  Sys.setenv( CENSUS_API_KEY = sub( '^KEY <- "([^"]+)".*$', "\\1", key_line ) ); rm( key_line )
  cache <- file.path( OUT$raw_cov, "acs89" ); dir.create( cache, recursive = TRUE, showWarnings = FALSE )

  V <- c( setNames( sprintf( "B19001_%03d", 1:17 ), c( "hh_tot", paste0( "hh_", 1:16 ) ) ),
          setNames( sprintf( "C17002_%03d", 1:8 ), c( "pr_tot", paste0( "pr_", 1:7 ) ) ),
          snap_den = "B22003_001", snap_num = "B22003_002",
          rb_tot = "B25070_001", rb_1 = "B25070_007", rb_2 = "B25070_008", rb_3 = "B25070_009",
          rb_4 = "B25070_010", rb_nc = "B25070_011",
          ten_tot = "B25003_001", ten_own = "B25003_002",
          mob_tot = "B07003_001", mob_same = "B07003_004" )
  get_year <- function( y, geo ) {
    f <- file.path( cache, sprintf( "acs5_%s_%d.csv", geo, y ) )
    if ( file.exists( f ) ) return( fread( f, colClasses = list( character = "GEOID" ) ) )
    vv <- switch( geo, county = V, us = c( q1 = "B19080_001", q2 = "B19080_002", q3 = "B19080_003", q4 = "B19080_004" ),
                  cbsa = c( gini_pub = "B19083_001" ) )
    gg <- switch( geo, county = "county", us = "us", cbsa = "metropolitan statistical area/micropolitan statistical area" )
    d <- as.data.table( get_acs( geography = gg, variables = vv, year = y, survey = "acs5", output = "wide" ) )
    d[ , year := y ]; fwrite( d, f ); d
  }
  cty <- rbindlist( lapply( ACS_YEARS, get_year, geo = "county" ), fill = TRUE )
  us  <- rbindlist( lapply( ACS_YEARS, get_year, geo = "us" ), fill = TRUE )
  pub <- rbindlist( lapply( ACS_YEARS, get_year, geo = "cbsa" ), fill = TRUE )
  log_msg( "county rows ", nrow( cty ), "; us ", nrow( us ), "; published cbsa ", nrow( pub ) )

  cnt <- sub( "E$", "", grep( "E$", names( cty ), value = TRUE ) )
  cnt <- setdiff( cnt, "NAM" )
  d <- cty[ , c( "GEOID", "year", paste0( cnt, "E" ) ), with = FALSE ]
  setnames( d, c( "geoid", "year", cnt ) )
  d <- d[ substr( geoid, 1, 2 ) != "72" ]
  d[ , geoid := recode_county( geoid ) ]
  d <- d[ , lapply( .SD, sum, na.rm = TRUE ), by = .( geoid, year ), .SDcols = cnt ]
  d <- ct_legacy_to_pr( d, cnt, "total" )
  spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
  d <- merge( d, spine[ cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
  m <- d[ , lapply( .SD, sum, na.rm = TRUE ), by = .( cbsa, year ), .SDcols = cnt ]

  ## income distribution from 16 brackets (national quintile cutoffs merged first: merge
  ## reorders rows, and the bracket matrix must follow the final order)
  usq <- us[ , .( year, q1 = q1E, q2 = q2E, q3 = q3E, q4 = q4E ) ]
  m <- merge( m, usq, by = "year" )
  lo <- c( 0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75, 100, 125, 150, 200 ) * 1000
  hi <- c( lo[ -1 ], Inf )
  hh <- as.matrix( m[ , paste0( "hh_", 1:16 ), with = FALSE ] )
  alpha <- pmin( pmax( log( ( hh[ , 15 ] + hh[ , 16 ] ) / hh[ , 16 ] ) / log( 200 / 150 ), 1.5 ), 4 )
  gini_g <- function( n, a ) {
    mid <- c( ( lo[ -16 ] + hi[ -16 ] ) / 2, 200000 * a / ( a - 1 ) )
    p <- n / sum( n ); inc <- p * mid; L <- cumsum( inc ) / sum( inc ); Lp <- c( 0, L[ -16 ] )
    1 - sum( p * ( L + Lp ) )
  }
  ## share of households below income c (uniform within brackets, Pareto above 200k)
  cdf_at <- function( n, a, c ) {
    p <- n / sum( n ); F <- 0
    for ( k in 1:16 ) {
      if ( k < 16 ) F <- F + p[ k ] * min( max( ( c - lo[ k ] ) / ( hi[ k ] - lo[ k ] ), 0 ), 1 )
      else if ( c > 200000 ) F <- F + p[ k ] * ( 1 - ( 200000 / c )^a )
    }
    F
  }
  m[ , gini_m := vapply( seq_len( .N ), function( i ) gini_g( hh[ i, ], alpha[ i ] ), numeric( 1 ) ) ]
  m[ , inc_hhi5 := vapply( seq_len( .N ), function( i ) {
    F <- vapply( c( q1[ i ], q2[ i ], q3[ i ], q4[ i ] ), function( c ) cdf_at( hh[ i, ], alpha[ i ], c ), numeric( 1 ) )
    s <- diff( c( 0, F, 1 ) ); sum( s^2 ) }, numeric( 1 ) ) ]
  m[ , `:=`( econ_div = ( 1 - inc_hhi5 ) / 0.8,
             deep_pov = pr_1 / pr_tot, near_pov = ( pr_1 + pr_2 + pr_3 + pr_4 + pr_5 + pr_6 ) / pr_tot,
             pov_acs = ( pr_1 + pr_2 ) / pr_tot,
             snap_rate = snap_num / snap_den,
             rent_burden = ( rb_1 + rb_2 + rb_3 + rb_4 ) / ( rb_tot - rb_nc ),
             own_rate = ten_own / ten_tot, stay_1yr = mob_same / mob_tot,
             hh_low = vapply( seq_len( .N ), function( i ) cdf_at( hh[ i, ], alpha[ i ], q1[ i ] ), numeric( 1 ) ) ) ]
  m[ , hh_high := vapply( seq_len( .N ), function( i ) 1 - cdf_at( hh[ i, ], alpha[ i ], q4[ i ] ), numeric( 1 ) ) ]

  ## validation against the published CBSA Gini (same code; delineations differ for a few)
  pub <- pub[ , .( cbsa = GEOID, year, gini_pub = gini_pubE ) ]
  m <- merge( m, pub, by = c( "cbsa", "year" ), all.x = TRUE )
  print( m[ !is.na( gini_pub ), .( n = .N, r = round( cor( gini_m, gini_pub ), 3 ),
                                   mean_m = round( mean( gini_m ), 3 ), mean_pub = round( mean( gini_pub ), 3 ) ), by = year ] )
  out <- m[ , .( cbsa, year, gini_m, gini_pub, inc_hhi5, econ_div, hh_low, hh_high, deep_pov, pov_acs, near_pov,
                 snap_rate, rent_burden, own_rate, stay_1yr, hh_tot ) ]
  setorder( out, cbsa, year )
  fwrite( out, FN )
  log_msg( "wrote ", FN, ": ", nrow( out ), " rows, ", uniqueN( out$cbsa ), " CBSAs" )
  ## covariate file for the multiverse "inequality" add-on block (read by load_panel)
  fwrite( out[ , .( cbsa, year, gini_m ) ], file.path( OUT$cov, "cov_gini_metro.csv" ) )
  quit( save = "no" )
}

## ---- shared: panel + derived measures ----------------------------------------------------
source( "R/40_multiverse.R" )
panel <- load_panel()
an <- fread( FN, colClasses = list( character = "cbsa" ) )
## load_panel already carries gini_m (cov_gini_metro.csv) since the inequality block was added
panel <- merge( panel, an[ , !setdiff( intersect( c( "hh_tot", names( panel ) ), names( an ) ), c( "cbsa", "year" ) ), with = FALSE ],
                by = c( "cbsa", "year" ), all.x = TRUE )
agec <- grep( "^pop_age_", names( panel ), value = TRUE )
panel[ , age_hhi := Reduce( `+`, lapply( agec, function( v ) ( get( v ) / pop_total )^2 ) ) ]
panel[ , `:=`( race_div = 1 - race_hhi, rel_div = 1 - rel_hhi, age_div = 1 - age_hhi,
               l_mhi = log( med_hh_income ), l_pcpi = log( pcpi ),
               l_cong = log( rel_cng / population * 1e4 ), l_assn = log( pmax( socap_assn_n, 1 ) / population * 1e4 ) ) ]
for ( v in c( "relig", "civic", "bus", "pol", "prof", "labor", "bowl", "fitness", "golf", "sport" ) )
  set( panel, j = paste0( "l_assn_", v ), value = log( ( panel[[ paste0( "socap_n_", v ) ]] + 0.5 ) / panel$population * 1e4 ) )
## per-year composite indices over all CBSAs (PC1 of z-scores; sign: need up, attachment up)
pc1 <- function( d, vars, sign_var ) {
  X <- scale( as.matrix( d[ , vars, with = FALSE ] ) )
  ok <- complete.cases( X ); s <- rep( NA_real_, nrow( d ) )
  if ( sum( ok ) < 30 ) return( s )
  p <- prcomp( X[ ok, ] ); sc <- p$x[ , 1 ]
  if ( cor( sc, X[ ok, sign_var ] ) < 0 ) sc <- -sc
  s[ ok ] <- sc / sd( sc ); s
}
NEED_ITEMS <- c( "poverty_rate", "deep_pov", "near_pov", "snap_rate", "unemp_rate", "rent_burden", "l_mhi" )
CIVIC_ITEMS <- c( "turnout", "socap_respn", "own_rate", "stay_1yr", "l_cong" )
panel[ , need_idx := pc1( .SD, NEED_ITEMS, "poverty_rate" ), by = year ]
panel[ , civic_idx := pc1( .SD, CIVIC_ITEMS, "turnout" ), by = year ]
panel[ , filer_sami := resid_logP( log( n_filer_c3 ), logP, year ) ]
panel[ , bmf_sami := resid_logP( log( n_bmf_c3pc ), logP, year ) ]
panel[ , exp_sami := resid_logP( log( pmax( exp_c3_nohu, 1 ) ), logP, year ) ]
z <- function( x ) ( x - mean( x, na.rm = TRUE ) ) / sd( x, na.rm = TRUE )
LAB <- c( poverty_rate = "Poverty (SAIPE)", deep_pov = "Deep poverty (<50%)", near_pov = "Near poverty (<200%)",
          pov_acs = "Poverty (ACS)", snap_rate = "SNAP households", unemp_rate = "Unemployment",
          rent_burden = "Rent burden 30%+", l_mhi = "log median HH income", l_pcpi = "log per-capita income",
          gini = "Gini (county-weighted)", gini_m = "Gini (metro, brackets)", gini_pub = "Gini (ACS metro)",
          econ_div = "Income-quintile diversity", hh_low = "HH in US bottom quintile", hh_high = "HH in US top quintile",
          pct_ba = "% BA+", need_idx = "Need index (PC1)", turnout = "Turnout", socap_respn = "Census mail response",
          own_rate = "Homeownership", stay_1yr = "Same house 1 yr", l_cong = "log congregations pc",
          pct_adherent = "Religious adherence", rel_hhi = "Denominational HHI", l_assn = "log associations pc",
          socap_sk2 = "Rupasingha index (sk2)", civic_idx = "Civic index (PC1)", race_div = "Racial diversity (1-HHI)",
          pct_nonwhite = "% nonwhite", rel_div = "Religious diversity (1-HHI)", age_sd = "Age SD", age_div = "Age diversity (1-HHI)",
          pct_foreign_born = "% foreign born", logP = "log population", filer_sami = "Filer SAMI", bmf_sami = "Registered SAMI",
          exp_sami = "Spending SAMI (no H/U)", dem_margin = "Dem margin", pct_65plus = "% 65+" )
lab <- function( v ) ifelse( v %in% names( LAB ), LAB[ v ], v )

## ---- describe ------------------------------------------------------------------------------
if ( MODE == "describe" ) {
  m <- panel[ cbsa_type == "metro" & year %in% c( 2019, 2022 ) ]
  cr <- function( d, vars ) round( cor( d[ , vars, with = FALSE ], use = "pairwise.complete.obs" ), 2 )
  partial_on <- function( d, y, x, ctrl ) {          # partial r of y and x given ctrl + division FE
    vv <- c( y, x, ctrl, "division" ); dd <- d[ complete.cases( d[ , vv, with = FALSE ] ) ]
    rhs <- paste( c( ctrl, "factor(division)" ), collapse = "+" )
    cor( resid( lm( as.formula( paste( y, "~", rhs ) ), dd ) ), resid( lm( as.formula( paste( x, "~", rhs ) ), dd ) ) )
  }
  ## benchmark model: SAMI outcome on residualized covariates, cs OLS (the multiverse default
  ## model in sami/sami form); returns the partial r of x
  m[ , l_pf := flog( pf_assets, population ) ]
  m[ , l_gov := flog( gov_earn_sl, population ) ]
  bench_pr <- function( d, y, x, others ) {
    vv <- c( y, x, others ); dd <- d[ complete.cases( d[ , c( vv, "logP" ), with = FALSE ] ) ]
    for ( v in c( x, others ) ) set( dd, j = v, value = resid_logP( dd[[ v ]], dd$logP, dd$year ) )
    r <- feols( as.formula( paste( y, "~", paste( c( x, others ), collapse = "+" ) ) ), dd, vcov = "hetero" )
    t <- tstat( r )[ x ]; unname( t / sqrt( t^2 + nobs( r ) - length( c( x, others ) ) - 1 ) )
  }

  ## 1. need measures ---------------------------------------------------------------------
  NV <- c( "poverty_rate", "pov_acs", "deep_pov", "near_pov", "snap_rate", "unemp_rate", "rent_burden", "l_mhi", "l_pcpi",
           "gini", "gini_m", "gini_pub", "econ_div", "hh_low", "hh_high", "pct_ba", "need_idx" )
  d19 <- m[ year == 2019 ]
  c1 <- cr( d19, c( NV, "logP", "filer_sami", "bmf_sami", "exp_sami" ) )
  fwrite( as.data.table( c1, keep.rownames = "var" ), file.path( OD, "need_correlations_2019.csv" ) )
  print( c1[ NV, c( "poverty_rate", "gini_m", "gini_pub", "econ_div", "need_idx", "logP", "filer_sami", "bmf_sami", "exp_sami" ) ] )
  X <- scale( as.matrix( d19[ , NEED_ITEMS, with = FALSE ] ) ); X <- X[ complete.cases( X ), ]
  p <- prcomp( X ); log_msg( "need PCA variance shares: ", paste( round( summary( p )$importance[ 2, 1:4 ], 2 ), collapse = " " ) )
  print( round( p$rotation[ , 1:2 ], 2 ) )
  ## each need measure in the benchmark model, replacing poverty (pct_ba kept), by outcome and year
  base_others <- c( "pct_ba", "pct_nonwhite", "turnout", "l_pf", "l_gov" )
  nr <- rbindlist( lapply( c( 2019, 2022 ), function( yy ) rbindlist( lapply( c( "filer_sami", "bmf_sami", "exp_sami" ), function( y )
          rbindlist( lapply( setdiff( NV, "pct_ba" ), function( x ) data.table( year = yy, outcome = y, measure = x,
            r_bivar = cor( m[ year == yy ][[ y ]], m[ year == yy ][[ x ]], use = "complete.obs" ),
            r_size = cor( m[ year == yy ]$logP, m[ year == yy ][[ x ]], use = "complete.obs" ),
            pr_model = bench_pr( m[ year == yy ], y, x, base_others ),
            pr_with_pov = if ( x == "poverty_rate" ) NA_real_ else bench_pr( m[ year == yy ], y, x, c( "poverty_rate", base_others ) ) ) ) ) ) ) ) )
  fwrite( nr, file.path( OD, "need_benchmark.csv" ) )
  print( dcast( nr[ year == 2022 ], measure ~ outcome, value.var = "pr_model", fun = function( x ) round( x, 3 ) ) )
  print( dcast( nr[ year == 2022 ], measure ~ outcome, value.var = "pr_with_pov", fun = function( x ) round( x, 3 ) ) )
  print( unique( nr[ year == 2022, .( measure, r_size = round( r_size, 2 ) ) ] ) )

  ## 2. social capital block -----------------------------------------------------------------
  SV <- c( "turnout", "socap_respn", "own_rate", "stay_1yr", "l_cong", "pct_adherent", "rel_hhi", "l_assn", "socap_sk2",
           paste0( "l_assn_", c( "relig", "civic", "bus", "pol", "prof", "labor", "bowl", "fitness", "golf", "sport" ) ) )
  c2 <- cr( d19, c( SV, "civic_idx", "pct_ba", "poverty_rate", "pct_nonwhite", "logP", "filer_sami", "bmf_sami", "exp_sami" ) )
  fwrite( as.data.table( c2, keep.rownames = "var" ), file.path( OD, "civic_correlations_2019.csv" ) )
  print( c2[ c( SV[ 1:9 ], "civic_idx" ), c( SV[ 1:9 ], "civic_idx", "logP", "filer_sami", "bmf_sami" ) ] )
  fa_items <- c( "turnout", "socap_respn", "own_rate", "stay_1yr", "l_cong", "pct_adherent",
                 paste0( "l_assn_", c( "relig", "civic", "bus", "pol", "prof", "labor", "bowl", "fitness", "golf", "sport" ) ) )
  ## size-adjusted items (residual on log P, the SAMI first stage), then factor analysis.
  ## Rupasingha category counts are not carried past their last wave (2014): take them from 2014.
  cat_v <- grep( "^l_assn_", fa_items, value = TRUE )
  dfa <- merge( d19[ , !cat_v, with = FALSE ], panel[ year == 2014, c( "cbsa", cat_v ), with = FALSE ], by = "cbsa" )
  dfa <- dfa[ complete.cases( dfa[ , c( fa_items, "logP" ), with = FALSE ] ) ]
  log_msg( "factor analysis: ", nrow( dfa ), " metros" )
  Xr <- as.data.table( lapply( fa_items, function( v ) resid( lm( dfa[[ v ]] ~ dfa$logP ) ) ) ); setnames( Xr, fa_items )
  fa <- lapply( 1:3, function( k ) tryCatch( factanal( Xr, factors = k, rotation = "varimax" ), error = function( e ) NULL ) )
  for ( k in 1:3 ) if ( !is.null( fa[[ k ]] ) ) {
    log_msg( "factanal ", k, " factors, p = ", signif( fa[[ k ]]$PVAL, 2 ) ); print( round( unclass( fa[[ k ]]$loadings ), 2 ) ) }
  L <- as.data.table( round( unclass( fa[[ 3 ]]$loadings ), 3 ), keep.rownames = "item" )
  fwrite( L, file.path( OD, "civic_factor_loadings.csv" ) )
  Xc <- scale( as.matrix( d19[ , CIVIC_ITEMS, with = FALSE ] ) ); Xc <- Xc[ complete.cases( Xc ), ]
  pc <- prcomp( Xc ); log_msg( "civic PCA variance shares: ", paste( round( summary( pc )$importance[ 2, 1:3 ], 2 ), collapse = " " ) )
  print( round( pc$rotation[ , 1:2 ], 2 ) )
  ## benchmark: turnout alone, + census response, + each attachment item
  oth <- c( "pct_ba", "poverty_rate", "pct_nonwhite", "l_pf", "l_gov" )
  sr <- rbindlist( lapply( c( 2019, 2022 ), function( yy ) rbindlist( lapply( c( "filer_sami", "bmf_sami", "exp_sami" ), function( y ) {
    dd <- m[ year == yy ]
    rbind(
      data.table( set = "turnout", term = "turnout", pr = bench_pr( dd, y, "turnout", oth ) ),
      data.table( set = "respn", term = "socap_respn", pr = bench_pr( dd, y, "socap_respn", oth ) ),
      rbindlist( lapply( c( "socap_respn", "own_rate", "stay_1yr", "l_cong", "pct_adherent", "rel_hhi", "socap_sk2", "civic_idx" ), function( x )
        data.table( set = paste0( "turnout+", x ), term = c( "turnout", x ),
                    pr = c( bench_pr( dd, y, "turnout", c( x, oth ) ), bench_pr( dd, y, x, c( "turnout", oth ) ) ) ) ) ),
      data.table( set = "civic_idx", term = "civic_idx", pr = bench_pr( dd, y, "civic_idx", oth ) ) )[ , `:=`( year = yy, outcome = y ) ] } ) ) ) )
  fwrite( sr, file.path( OD, "civic_benchmark.csv" ) )
  print( dcast( sr[ year == 2022 ], set + term ~ outcome, value.var = "pr", fun = function( x ) round( x, 3 ) ) )

  ## 3. diversity vs turnout / census response ------------------------------------------------
  DV <- c( "race_div", "pct_nonwhite", "rel_div", "age_sd", "age_div", "gini_m", "econ_div", "pct_foreign_born" )
  ctrl <- c( "logP", "pct_ba", "poverty_rate", "l_mhi", "pct_65plus" )
  dr <- rbindlist( lapply( c( 2019, 2022 ), function( yy ) rbindlist( lapply( c( "turnout", "socap_respn", "civic_idx" ), function( y )
          rbindlist( lapply( DV, function( x ) { dd <- m[ year == yy ]
            data.table( year = yy, outcome = y, diversity = x,
                        r = cor( dd[[ y ]], dd[[ x ]], use = "complete.obs" ),
                        pr_ctrl = partial_on( dd, y, x, ctrl ),
                        pr_ctrl_race = if ( x %in% c( "race_div", "pct_nonwhite" ) ) NA_real_ else partial_on( dd, y, x, c( ctrl, "pct_nonwhite" ) ) ) } ) ) ) ) ) )
  fwrite( dr, file.path( OD, "diversity_civic.csv" ) )
  print( dcast( dr[ year == 2019 ], diversity ~ outcome, value.var = c( "r", "pr_ctrl", "pr_ctrl_race" ), fun = function( x ) round( x, 2 ) ) )
  ## joint model: standardized, controls + division FE, state-clustered
  jm <- list()
  for ( y in c( "turnout", "socap_respn" ) ) for ( rv in c( "race_div", "pct_nonwhite" ) ) {
    vv <- c( y, rv, "rel_div", "age_div", "gini_m", "econ_div", ctrl )
    dd <- d19[ complete.cases( d19[ , c( vv, "division" ), with = FALSE ] ) ]
    for ( v in vv ) set( dd, j = v, value = z( dd[[ v ]] ) )
    r <- feols( as.formula( paste( y, "~", paste( vv[ -1 ], collapse = "+" ), "| division" ) ), dd, vcov = ~state_fips )
    jm[[ paste( y, rv ) ]] <- r
    ct <- as.data.table( coeftable( r ), keep.rownames = "term" )
    fwrite( ct[ , `:=`( outcome = y, race_measure = rv, n = nobs( r ) ) ], file.path( OD, "diversity_joint.csv" ), append = length( jm ) > 1 )
  }
  print( etable( jm, digits = 2 ) )
  quit( save = "no" )
}

## ---- horse: default-model sub-universe with alternative need / civic sets ------------------
## Default specs (full model, default measures, no winsorizing, no algebraic duplicates) for
## filers, registered stock and spending without hospitals/universities, cs 2022, cs 2019 and
## pooled 2015-22, every DV form, predictor form, log P, unit and estimator. Each is re-fit with
## the need block or the political block swapped for an alternative set. All new measures are
## rates (type A: used as is, residualized on log P in the all-SAMI arm).
NEW_A <- c( "near_pov", "deep_pov", "need_idx", "gini_m", "gini", "hh_high", "hh_low", "econ_div",
            "own_rate", "stay_1yr", "civic_idx", "l_cong" )
ALT_NEED <- list( near = c( "near_pov", "pct_ba" ), deep = c( "deep_pov", "pct_ba" ), idx = c( "need_idx", "pct_ba" ),
                  gini = c( "gini_m", "pct_ba" ), pov_gini = c( "poverty_rate", "gini_m", "pct_ba" ),
                  pov_gini_cty = c( "poverty_rate", "gini", "pct_ba" ),
                  pov_top = c( "poverty_rate", "hh_high", "pct_ba" ), pov_econdiv = c( "poverty_rate", "econ_div", "pct_ba" ),
                  pov_gini_top = c( "poverty_rate", "gini_m", "hh_high", "pct_ba" ) )
ALT_CIVIC <- list( respn = "socap_respn", turn_respn = c( "turnout", "socap_respn" ), turn_own = c( "turnout", "own_rate" ),
                   turn_stay = c( "turnout", "stay_1yr" ), turn_cong = c( "turnout", "l_cong" ), civic_idx = "civic_idx",
                   turn_civic_idx = c( "turnout", "civic_idx" ) )
if ( MODE == "horse" ) {
  suppressPackageStartupMessages( library( parallel ) )
  WORKERS <- if ( length( args ) >= 2 ) as.integer( args[2] ) else 8L
  COVS <- rbind( COVS, data.table( var = setdiff( NEW_A, COVS$var ), block = "added", type = "A" ) )
  BLOCKS$need$alt <- c( BLOCKS$need$alt, ALT_NEED )
  BLOCKS$political$alt <- c( BLOCKS$political$alt, ALT_CIVIC )
  OUTS <- c( "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu" )
  g0 <- build_grid( "principled" )
  sp <- g0[ drop == "none" & op == "default" & outlier == "none" & is.na( dup_of ) & outcome %in% OUTS & ident != "within" ]
  alts <- rbind( data.table( op = "default", op_block = NA_character_ ),
                 data.table( op = paste0( "need:", names( ALT_NEED ) ), op_block = "need" ),
                 data.table( op = paste0( "political:", names( ALT_CIVIC ) ), op_block = "political" ) )
  jobs <- CJ( spec_id = sp$spec_id, op = alts$op )[ alts, on = "op" ]
  jobs[ , job := .I ]
  log_msg( "base specs ", nrow( sp ), "; fits ", nrow( jobs ) )
  pnl <- panel[ year %in% c( 2015:2022 ) ]
  run_job <- function( i ) {
    j <- jobs[ i ]; s <- copy( sp[ spec_id == j$spec_id ] ); s[ , `:=`( op = j$op, op_block = j$op_block ) ]
    r <- tryCatch( run_spec( s, pnl ), error = function( e ) NULL )
    if ( is.null( r ) ) return( NULL )
    r[ , job := j$job ][]
  }
  t0 <- proc.time()
  cl <- makePSOCKcluster( WORKERS )
  invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
    setwd( "C:/Users/jdlec/Dropbox (Personal)/04 - PAPERS/01 - In Progress/38 - resource environments/multiverse" )
    source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
  clusterExport( cl, c( "pnl", "sp", "jobs", "run_job", "COVS", "BLOCKS" ) )
  res <- rbindlist( parLapplyLB( cl, seq_len( nrow( jobs ) ), run_job, chunk.size = 40 ), fill = TRUE )
  stopCluster( cl )
  log_msg( "fits finished in ", round( ( proc.time() - t0 )[[ "elapsed" ]] / 60, 1 ), " min; rows ", nrow( res ) )
  res <- merge( res, jobs[ , .( job, op ) ], by = "job" )
  res <- merge( res, res[ , .( k = .N ), by = job ], by = "job" )
  res <- merge( res, sp[ , .( spec_id, construct, hu, outcome, ident, cs_year, dv_form, pred_form, rhs_logP, unit, est ) ], by = "spec_id" )
  res[ , n_units := n / fifelse( ident == "pooled", 8, 1 ) ]
  res[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
  res[ , pool := fifelse( ident == "pooled", "pooled", paste0( "cs", cs_year ) ) ]
  write_parquet( res, file.path( OD, "horse_coefs.parquet" ) )
  fwrite( jobs, file.path( OD, "horse_jobs.csv" ) )
  quit( save = "no" )
}

## ---- horse summary -------------------------------------------------------------------------
if ( MODE == "horse_summary" ) {
  res <- as.data.table( read_parquet( file.path( OD, "horse_coefs.parquet" ) ) )
  res[ , grp := fcase( outcome == "n_filer_c3", "filers", outcome == "n_bmf_c3pc", "registered", default = "spending" ) ]
  res <- res[ !( ident == "pooled" & est == "state_re" ) ]
  ## terms of the swapped block
  sw <- res[ term != "logP" & ( ( grepl( "^need", op ) | op == "default" ) & term %in% c( "poverty_rate", unique( unlist( ALT_NEED ) ) ) |
                                ( grepl( "^political", op ) | op == "default" ) & term %in% c( "turnout", unique( unlist( ALT_CIVIC ) ) ) ) ]
  st <- sw[ , .( specs = .N, median_r = median( pr ), p10 = quantile( pr, .1 ), p90 = quantile( pr, .9 ),
                 sign_pos = mean( pr > 0 ), sig05 = mean( p < .05 ) ), by = .( grp, op, term ) ]
  fwrite( st, file.path( OD, "horse_terms.csv" ) )
  ## fit vs default (same spec): change in R2
  r2 <- unique( res[ , .( spec_id, op, grp, pool, r2 ) ] )
  r2 <- merge( r2[ op != "default" ], r2[ op == "default", .( spec_id, r2_0 = r2 ) ], by = "spec_id" )
  fr <- r2[ is.finite( r2 ) & is.finite( r2_0 ), .( specs = .N, median_dR2 = median( r2 - r2_0 ), share_better = mean( r2 > r2_0 ) ), by = .( grp, op ) ]
  fwrite( fr, file.path( OD, "horse_fit.csv" ) )
  ## how the other default terms move when a block is swapped
  oth <- c( "pct_ba", "pct_nonwhite", "pf_assets", "gov_earn_sl", "turnout", "poverty_rate" )
  b0 <- res[ op == "default" & term %in% oth, .( spec_id, term, pr0 = pr ) ]
  sh <- merge( res[ op != "default" & term %in% oth ], b0, by = c( "spec_id", "term" ) )
  shs <- sh[ , .( median_r0 = median( pr0 ), median_r = median( pr ), median_change = median( pr - pr0 ) ), by = .( grp, op, term ) ]
  fwrite( shs, file.path( OD, "horse_shift.csv" ) )
  options( width = 200 )
  print( dcast( st, op + term ~ grp, value.var = "median_r", fun = function( x ) round( x, 3 ) ) )
  print( dcast( st, op + term ~ grp, value.var = "sig05", fun = function( x ) round( x, 2 ) ) )
  print( dcast( fr, op ~ grp, value.var = "median_dR2", fun = function( x ) round( x, 4 ) ) )
  print( shs[ abs( median_change ) > .02 ][ order( op, term, grp ) ], digits = 2 )
  quit( save = "no" )
}

## ---- figs ------------------------------------------------------------------------------------
## 1. fig_ns_raw_vs_sami   the old slide plots (raw counts vs poverty / inequality / income) next to
##                         the same measures against the filer SAMI: the raw gradients are mostly size
## 2. fig_ns_index_vs_item added-variable plots: does an index (or a second measure) explain density
##                         beyond the block's dominant item? x = candidate | dominant item + rest of the
##                         default model, y = filer SAMI | the same. Flat = the dominant item suffices.
## 3. fig_ns_horse         median partial r across the sub-universe for each alternative set
## 4. fig_ns_diversity     turnout and census response vs five kinds of diversity, raw and adjusted
## 5. fig_ns_civic_factors loadings of the size-adjusted civic items on three factors
if ( MODE == "figs" ) {
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  FD <- file.path( OD, "figures" ); dir.create( FD, showWarnings = FALSE )
  NOTES <- "notes/need-socap-figures"; dir.create( NOTES, showWarnings = FALSE )
  save_fig <- function( p, name, w, h ) {
    ggsave( file.path( FD, paste0( name, ".png" ) ), p, width = w, height = h, dpi = 200, bg = "white" )
    file.copy( file.path( FD, paste0( name, ".png" ) ), NOTES, overwrite = TRUE ) }
  th <- theme_minimal( base_size = 10 ) + theme( panel.grid.minor = element_blank(), plot.title = element_text( face = "bold" ) )
  m <- panel[ cbsa_type == "metro" & year == 2019 ]
  m[ , short := sub( ",.*$", "", sub( "-.*$", "", cbsa_title ) ) ]

  ## 1 ------------------------------------------------------------------------------------------
  xs <- c( poverty_rate = "Poverty rate (%)", gini_m = "Gini (metro)", med_hh_income = "Median household income" )
  long <- rbindlist( lapply( names( xs ), function( v ) rbind(
    m[ , .( x = get( v ), y = n_filer_c3, measure = xs[[ v ]], panel = "A. Raw count (log scale)", logP ) ],
    m[ , .( x = get( v ), y = filer_sami, measure = xs[[ v ]], panel = "B. Density relative to size (filer SAMI)", logP ) ] ) ) )
  long[ , measure := factor( measure, xs ) ]
  rr <- long[ , .( r = cor( x, if ( grepl( "^A", panel[1] ) ) log( y ) else y, use = "complete.obs" ) ), by = .( panel, measure ) ]
  pA <- ggplot( long[ grepl( "^A", panel ) ], aes( x, y ) ) + geom_point( aes( colour = logP ), size = .8, alpha = .7 ) +
    geom_smooth( method = "loess", se = FALSE, colour = "#c0392b", linewidth = .8, formula = y ~ x ) +
    scale_y_log10( labels = scales::comma ) + scale_colour_viridis_c( "log pop", option = "C" ) +
    facet_wrap( ~ measure, scales = "free_x" ) + labs( x = NULL, y = "Filing 501(c)(3)s", title = "A. Raw counts: the gradients are mostly metro size" ) +
    geom_text( data = rr[ grepl( "^A", panel ) ], aes( x = Inf, y = Inf, label = sprintf( "r = %.2f", r ) ), hjust = 1.1, vjust = 1.5, size = 3 ) + th
  pB <- ggplot( long[ grepl( "^B", panel ) ], aes( x, y ) ) + geom_point( aes( colour = logP ), size = .8, alpha = .7 ) +
    geom_smooth( method = "loess", se = FALSE, colour = "#c0392b", linewidth = .8, formula = y ~ x ) +
    scale_colour_viridis_c( "log pop", option = "C" ) + geom_hline( yintercept = 0, linetype = 3 ) +
    facet_wrap( ~ measure, scales = "free_x" ) + labs( x = NULL, y = "Filer SAMI", title = "B. Relative to the scaling line" ) +
    geom_text( data = rr[ grepl( "^B", panel ) ], aes( x = Inf, y = Inf, label = sprintf( "r = %.2f", r ) ), hjust = 1.1, vjust = 1.5, size = 3 ) + th
  pB <- pB + scale_x_continuous( labels = function( b ) ifelse( b > 1000, scales::dollar( b, scale = 1e-3, suffix = "k" ), b ) )
  pA <- pA + scale_x_continuous( labels = function( b ) ifelse( b > 1000, scales::dollar( b, scale = 1e-3, suffix = "k" ), b ) )
  save_fig( pA / pB + plot_layout( guides = "collect" ) + plot_annotation( caption = "387 metros, 2019. Lines: loess." ),
            "fig_ns_raw_vs_sami", 9, 6.5 )

  ## 2 ------------------------------------------------------------------------------------------
  m[ , `:=`( l_pf = resid_logP( flog( pf_assets, population ), logP, year ), l_gov = resid_logP( flog( gov_earn_sl, population ), logP, year ) ) ]
  av <- function( y, cand, keep ) {
    rest <- c( keep, "pct_ba", "pct_nonwhite", "l_pf", "l_gov", "logP" )
    rest <- unique( setdiff( rest, cand ) )
    dd <- m[ complete.cases( m[ , c( y, cand, rest ), with = FALSE ] ) ]
    f <- function( v ) resid( lm( as.formula( paste( v, "~", paste( rest, collapse = "+" ) ) ), dd ) )
    data.table( cbsa = dd$cbsa, short = dd$short, x = f( cand ), y = f( y ) )
  }
  cases <- list( c( "need_idx", "poverty_rate", "Need index | poverty" ), c( "gini_m", "poverty_rate", "Gini | poverty" ),
                 c( "hh_high", "poverty_rate", "Top-quintile share | poverty" ),
                 c( "socap_respn", "turnout", "Census response | turnout" ), c( "civic_idx", "turnout", "Civic index | turnout" ),
                 c( "turnout", "socap_respn", "Turnout | census response" ) )
  avd <- rbindlist( lapply( cases, function( k ) {
    a <- av( "filer_sami", k[1], c( k[2], if ( k[2] == "turnout" || k[1] == "turnout" ) "poverty_rate" else "turnout" ) )
    a[ , `:=`( case = k[3], block = fifelse( k[2] == "poverty_rate", "Need", "Civic" ) ) ] } ) )
  avd[ , case := factor( case, sapply( cases, `[`, 3 ) ) ]
  avs <- avd[ , { f <- lm( y ~ x ); t <- summary( f )$coefficients[ 2, 3 ]
                  .( pr = t / sqrt( t^2 + .N - 9 ), lab = sprintf( "partial r = %.2f", t / sqrt( t^2 + .N - 9 ) ) ) }, by = case ]
  avd[ , extreme := rank( -abs( x * y ) ) <= 4, by = case ]
  p2 <- ggplot( avd, aes( x, y ) ) + geom_hline( yintercept = 0, colour = "grey80" ) + geom_vline( xintercept = 0, colour = "grey80" ) +
    geom_point( size = .8, alpha = .6, colour = "grey30" ) +
    geom_smooth( method = "lm", formula = y ~ x, colour = "#c0392b", fill = "#f5b7b1", linewidth = .8 ) +
    geom_text_repel( data = avd[ extreme == TRUE ], aes( label = short ), size = 2.4, colour = "#2c3e50", min.segment.length = 0 ) +
    geom_label( data = avs, aes( x = Inf, y = -Inf, label = lab ), hjust = 1.05, vjust = -.4, size = 3, label.size = 0, fill = "white" ) +
    facet_wrap( ~ case, scales = "free_x", ncol = 3 ) +
    labs( x = "Candidate measure, net of the dominant item and the rest of the default model",
          y = "Filer SAMI, net of the same", title = "Does the candidate add anything beyond the dominant item?",
          caption = "387 metros, 2019. Rest of the model: % BA, % nonwhite, foundation assets, state/local government earnings, log population." ) + th
  save_fig( p2, "fig_ns_index_vs_item", 9.5, 6.5 )

  ## 3 ------------------------------------------------------------------------------------------
  st <- fread( file.path( OD, "horse_terms.csv" ) )
  st[ , block := fifelse( grepl( "^political", op ) | ( op == "default" & term == "turnout" ), "Civic / political block", "Need block" ) ]
  st <- st[ !( op == "default" & !term %in% c( "poverty_rate", "turnout" ) ) ]
  st[ , set := sub( "^(need|political):", "", op ) ][ set == "default", set := "default" ]
  st[ , lab_term := lab( term ) ]
  st[ , set := factor( set, rev( unique( set ) ) ) ]
  st[ , grp := factor( grp, c( "filers", "registered", "spending" ), c( "Filers", "Registered", "Spending (no H/U)" ) ) ]
  p3 <- ggplot( st, aes( median_r, set, colour = lab_term ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = p10, xmax = p90 ), position = position_dodge( width = .6 ), alpha = .5 ) +
    geom_point( position = position_dodge( width = .6 ), size = 1.6 ) +
    facet_grid( block ~ grp, scales = "free_y", space = "free_y" ) +
    labs( x = "Partial r (median, 10th-90th percentile across specs)", y = NULL, colour = NULL,
          title = "Alternative need and civic measures across the default-model sub-universe",
          caption = "cs 2022, cs 2019 and pooled 2015-22; every DV form, predictor form, unit and estimator; no winsorizing." ) +
    th + theme( legend.position = "bottom" ) + guides( colour = guide_legend( ncol = 4 ) )
  save_fig( p3, "fig_ns_horse", 10, 8 )

  ## 4 ------------------------------------------------------------------------------------------
  dr <- fread( file.path( OD, "diversity_civic.csv" ) )[ year == 2019 & outcome != "civic_idx" ]
  dr <- melt( dr, id.vars = c( "year", "outcome", "diversity" ), measure.vars = c( "r", "pr_ctrl", "pr_ctrl_race" ) )
  dr[ , variable := factor( variable, c( "r", "pr_ctrl", "pr_ctrl_race" ),
                            c( "Bivariate", "+ size, education, poverty, income, age 65+, division", "... + % nonwhite" ) ) ]
  dr[ , outcome := factor( outcome, c( "turnout", "socap_respn" ), c( "Turnout", "Census mail response" ) ) ]
  dr[ , diversity := factor( lab( diversity ), rev( lab( c( "race_div", "pct_nonwhite", "pct_foreign_born", "rel_div", "age_div", "age_sd", "gini_m", "econ_div" ) ) ) ) ]
  p4 <- ggplot( dr[ is.finite( value ) ], aes( value, diversity, colour = variable ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_point( position = position_dodge( width = .6 ), size = 2 ) + facet_wrap( ~ outcome ) +
    scale_colour_manual( values = c( "grey55", "#2471a3", "#c0392b" ) ) +
    labs( x = "Correlation (partial r after controls)", y = NULL, colour = NULL, title = "Diversity and civic participation across metros",
          caption = "387 metros, 2019. Census mail response is the 2014 Rupasingha wave carried forward." ) +
    th + theme( legend.position = "bottom" ) + guides( colour = guide_legend( ncol = 1 ) )
  save_fig( p4, "fig_ns_diversity", 8.5, 5 )

  ## 5 ------------------------------------------------------------------------------------------
  L <- melt( fread( file.path( OD, "civic_factor_loadings.csv" ) ), id.vars = "item", variable.name = "factor", value.name = "loading" )
  L[ , item_lab := fifelse( grepl( "^l_assn_", item ), paste( "Assn:", sub( "^l_assn_", "", item ) ), lab( item ) ) ]
  L[ , item_lab := factor( item_lab, rev( unique( item_lab ) ) ) ]
  L[ , factor := factor( factor, c( "Factor1", "Factor2", "Factor3" ),
                         c( "1. Organized\ninterests", "2. Participation\n& attachment", "3. Congregations" ) ) ]
  p5 <- ggplot( L, aes( factor, item_lab, fill = loading ) ) + geom_tile( colour = "white" ) +
    geom_text( aes( label = sprintf( "%.2f", loading ) ), size = 2.8 ) +
    scale_fill_gradient2( low = "#2471a3", mid = "white", high = "#c0392b", limits = c( -1, 1 ) ) +
    labs( x = NULL, y = NULL, title = "Civic items: three factors (size-adjusted, varimax)",
          caption = "387 metros; ACS/turnout 2019, Rupasingha category counts 2014, RCMS 2020 (interpolated)." ) + th
  save_fig( p5, "fig_ns_civic_factors", 7, 6 )
  log_msg( "figures in ", FD, " and ", NOTES )
  quit( save = "no" )
}
