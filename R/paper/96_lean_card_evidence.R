## 96_lean_card_evidence.R
## Single-model evidence tables of the design-decision cards (outline Part II), refit with the
## 13-covariate lean default model (BLOCKS_LEAN in 40_multiverse.R; paper/02-variables.qmd) in place
## of the old 6-covariate default. Each part mirrors the 58_outline_analyses.R part (or script) that
## produced the old table: same year, sample rule, SEs and option construction. Every table is also
## refit with the old six covariates on the SAME sample (arm "old6"), so a change can be split into
## data / sample drift (card vs old6) and the model change (old6 vs lean).
##
## Lean set (copied from BLOCKS_LEAN defaults; MV_MODEL is not used):
##   need: poverty_rate, pct_65plus; heterogeneity: gini_m, pct_nonwhite, religious fractionalization
##   (rel_frac = 1 - rel_hhi), age_sd; philanthropy: pf_assets; education: pct_ba; civic: turnout;
##   growth: lag_g10; direct government: gov_earn_sl; indirect government: tr_medicaid;
##   history: hist_pre1970 (time-invariant).
## Size-scaled totals (pf_assets, gov_earn_sl, tr_medicaid, hist_pre1970) enter as flog(x, pop) (per-capita
## floor set within the estimation sample), with log P on the right (log-log) or residualized on log P
## (SAMI form); rates as is. Covariates are standardized (per-SD coefficients); partial r = t / sqrt(t^2 + df),
## df counted in places.
##
## Parts (default: all):
##   D1      58 G  lean log-log cross-section 2022: filers, grassroots, registered stock, spending
##   D2      58 H  the model at six units, covariates rebuilt from county data (all 13 terms buildable:
##                 Gini from summed ACS B19001 brackets, religious HHI from summed RCMS 2020 group
##                 adherents, age SD from summed age bins, growth kernel from summed county population
##                 1970-2022, endowment from 2000 BMF rulings by county)
##   D2size  86 B + 90 for the single lean SAMI spec: metros < / >= 250k; quadratic size interaction
##   D3      52 C3 hybrid (CRE) between / within effects, 2010-2022 (endowment between-only)
##   D4      58 J  DV form: log-log, SAMI, log per capita (+/- log P), linear per capita
##   D5      58 K  metro OLS vs Poisson PML (log P on the right), spending without the top 5%
##   D5c     58 K  county subsector OLS / log(n+1) / Poisson with lean county covariates
##   D8      58 M  lean -/+ endowment, +/- BLS market share, - growth kernel
##   D9      58 E  between-state / division variance shares; OLS, division FE, state FE, state RE
##   D10     58 N  money measure (spending series as corrected 2026-11-17)
##   D11     58 C  Poisson subsector models of 2022 counts; sign agreement
##   D12     58 P  lean -/+ mean filer age (and - endowment)
## Outputs: D:/MULTIVERSE/data/outline/v11/single/*.csv; figures paper/figures/d<card>_lean_*.png.
## Results note: notes/lean-card-evidence.md. Single process.
## Run from multiverse/:
##   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/paper/96_lean_card_evidence.R > D:/MULTIVERSE/logs/96.log 2>&1

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )          # load_panel(), flog(), resid_logP(), DIVISION; BLOCKS is not used
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ) } )
setFixest_notes( FALSE ); setFixest_nthreads( 1 ); data.table::setDTthreads( 2 )
OD <- file.path( BUILD, "data", "outline", "v11", "single" ); dir.create( OD, recursive = TRUE, showWarnings = FALSE )
FIG <- "paper/figures"; dir.create( FIG, recursive = TRUE, showWarnings = FALSE )
ALLP <- c( "D1", "D2", "D2size", "D3", "D4", "D5", "D5c", "D8", "D9", "D10", "D11", "D12" )
PARTS <- if ( length( commandArgs( TRUE ) ) ) commandArgs( TRUE ) else ALLP
options( width = 250 )

## ---- model sets and labels -------------------------------------------------------------
LEAN <- c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate",
           "gov_earn_sl", "tr_medicaid", "lag_g10", "hist_pre1970",
           "pct_65plus", "gini_m", "rel_frac", "age_sd" )
OLD6 <- c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate", "gov_earn_sl" )
SETS <- list( old6 = OLD6, lean = LEAN )
TOT  <- c( "pf_assets", "gov_earn_sl", "tr_medicaid", "hist_pre1970" )     # size-scaled totals
LAB  <- c( pct_ba = "% BA", pf_assets = "Foundation assets", pct_nonwhite = "% nonwhite", turnout = "Turnout",
           poverty_rate = "Poverty rate", gov_earn_sl = "S&L govt earnings", tr_medicaid = "Medicaid transfers",
           lag_g10 = "Growth kernel (10-yr)", hist_pre1970 = "Pre-1970 endowment", pct_65plus = "% 65+",
           gini_m = "Gini (metro)", rel_frac = "Religious fractionalization", age_sd = "Age SD",
           bls_share = "Nonprofit market share", age_f = "Mean filer age", age_r = "Median age, all registered",
           age_d = "Dollar-weighted age excl. H&U", logP = "log P" )
TORD <- names( LAB )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
pr_of <- function( t, df ) t / sqrt( t^2 + df )
## print a term x column table of one value, rows in the lean order
wide <- function( res, cols, value = "r", rows = "term" ) {
  w <- dcast( res, as.formula( paste( rows, "~", paste( cols, collapse = " + " ) ) ), value.var = value )
  if ( rows == "term" ) { w <- w[ order( match( term, TORD ) ) ]; w[ , term := fifelse( term %in% names( LAB ), LAB[ term ], term ) ] }
  r3( w ) }

## ---- data ----------------------------------------------------------------------------------
P <- load_panel()                                    # gini_m, lag_g5/10, hist_pre1970 attached
P[ , `:=`( rel_frac = 1 - rel_hhi, n_grass = n_bmf_c3pc - n_filer_c3 ) ]
M22 <- P[ year == 2022 & cbsa_type == "metro" ]
lean_ok <- function( d ) d[ complete.cases( d[ , c( LEAN, "population" ), with = FALSE ] ) & population > 0 ]
log_msg( "2022 metros: ", nrow( M22 ), "; with every lean covariate: ", nrow( lean_ok( M22 ) ),
         "; missing: ", paste( M22[ !cbsa %in% lean_ok( M22 )$cbsa, cbsa_title ], collapse = " | " ) )

## model-form covariates on a sample (rows = places or place-years with population, logP, year)
##   log   : totals flog(x, pop), rates as is (log P goes on the right)
##   sami  : the log form residualized on log P (by year)
##   logpc : totals as log per capita;  pc : totals per capita (linear)
mform <- function( d, xs, form = "log", std = TRUE ) {
  X <- lapply( xs, function( v ) {
    x <- d[[ v ]]
    if ( v %in% TOT ) x <- switch( form, log = , sami = flog( x, d$population ),
                                   logpc = flog( x, d$population ) - log( d$population ), pc = x / d$population )
    if ( form == "sami" ) x <- resid_logP( x, d$logP, d$year )
    if ( std ) x <- ( x - mean( x ) ) / sd( x )
    x } )
  setNames( as.data.table( X ), xs ) }

## tidy a fixest fit; df = residual df unless given
tidy_fit <- function( r, df = NULL ) {
  ct <- coeftable( r ); ct <- ct[ rownames( ct ) != "(Intercept)", , drop = FALSE ]
  if ( is.null( df ) ) df <- degrees_freedom( r, "resid" )
  data.table( term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], t = ct[ , 3 ], p = ct[ , 4 ],
              r = pr_of( ct[ , 3 ], df ), lo = pr_of( ct[ , 3 ] - 1.96, df ), hi = pr_of( ct[ , 3 ] + 1.96, df ),
              r2 = tryCatch( as.numeric( r2( r, "r2" ) ), error = function( e ) NA_real_ ), n = nobs( r ), df = df ) }

## one cross-section fit.
##   form "loglog": log Y on log P + log-form covariates;  "sami": SAMI on SAMI-form covariates (no log P)
##   "logpcP" / "logpc": log(Y/P) on log per capita totals + rates, with / without log P; "pc": Y/P on per capita
##   est: ols (HC1) | div_fe | state_fe (state-clustered) | state_re (lmer, model-based t) | fe = a column name
##   df_rule: "resid" (n - coefs), "places" (n - k - 1, k = terms incl. log P; 58 part A), "clusters" (G - k - 1)
fit_cs <- function( d, y, xs, form = "loglog", est = "ols", fe = NULL, df_rule = "resid" ) {
  d <- d[ is.finite( d[[ y ]] ) & population > 0 ]
  if ( form != "pc" ) d <- d[ d[[ y ]] > 0 ]
  d <- d[ complete.cases( d[ , xs, with = FALSE ] ) ]
  X <- mform( d, xs, switch( form, loglog = "log", sami = "sami", logpcP = , logpc = "logpc", pc = "pc" ) )
  yy <- switch( form, loglog = log( d[[ y ]] ), sami = resid_logP( log( d[[ y ]] ), d$logP, d$year ),
                logpcP = , logpc = log( d[[ y ]] / d$population ), pc = d[[ y ]] / d$population )
  m <- cbind( data.table( yy = yy, logP = d$logP, state = d$state_fips,
                          division = if ( "division" %in% names( d ) ) d$division else NA,
                          cbsa = if ( "cbsa" %in% names( d ) ) d$cbsa else NA ), X )
  terms <- c( xs, if ( form %in% c( "loglog", "logpcP" ) ) "logP" )
  rhs <- paste( terms, collapse = " + " )
  k <- length( terms )
  if ( est == "state_re" ) {
    r <- lme4::lmer( as.formula( paste( "yy ~", rhs, "+ ( 1 | state )" ) ), m, REML = FALSE )
    cf <- summary( r )$coefficients; cf <- cf[ rownames( cf ) != "(Intercept)", , drop = FALSE ]
    df <- nrow( m ) - k - 1
    return( data.table( term = rownames( cf ), est = cf[ , 1 ], se = cf[ , 2 ], t = cf[ , 3 ], p = 2 * pnorm( -abs( cf[ , 3 ] ) ),
                        r = pr_of( cf[ , 3 ], df ), lo = pr_of( cf[ , 3 ] - 1.96, df ), hi = pr_of( cf[ , 3 ] + 1.96, df ),
                        r2 = NA_real_, n = nrow( m ), df = df ) )
  }
  fev <- switch( est, div_fe = "division", state_fe = "state", fe )
  vc <- if ( is.null( fev ) ) "hetero" else if ( est %in% c( "div_fe", "state_fe" ) ) ~state else as.formula( paste( "~", fev ) )
  r <- feols( as.formula( paste( "yy ~", rhs, if ( !is.null( fev ) ) paste( "|", fev ) ) ), m, vcov = vc )
  df <- switch( df_rule, resid = NULL, places = nrow( m ) - k - 1, clusters = uniqueN( m[[ fev ]] ) - k - 1 )
  tidy_fit( r, df ) }

## figure: partial r with 95% interval, one colour per arm
PAL <- c( "#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#6250d6", "#8a8f98" )
thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                  plot.subtitle = element_text( colour = "grey30" ), legend.position = "bottom",
                                                  strip.text = element_text( face = "bold", hjust = 0 ) )
dotfig <- function( pd, col, file, title, subtitle, facet = NULL, w = 11, h = 7, ncol = 2 ) {
  pd <- copy( pd )[ term %in% names( LAB ) & term != "logP" ]
  lv <- intersect( TORD, unique( pd$term ) )
  pd[ , label := factor( LAB[ term ], rev( LAB[ lv ] ) ) ]
  p <- ggplot( pd, aes( r, label, colour = .data[[ col ]] ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.75 ), linewidth = 0.65 ) +
    geom_point( position = position_dodge( width = 0.75 ), size = 1.8 ) +
    scale_colour_manual( values = PAL, name = NULL ) + labs( title = title, subtitle = subtitle, x = "Partial r", y = NULL ) + thm
  if ( !is.null( facet ) ) p <- p + facet_wrap( as.formula( paste( "~", facet ) ), ncol = ncol )
  ggsave( file.path( FIG, file ), p, width = w, height = h, dpi = 170, bg = "white" )
  log_msg( "  figure ", file ) }

## ===========================================================================
## D1 (58 G): one model, four outcome populations
## ===========================================================================
if ( "D1" %in% PARTS ) {
  log_msg( "D1: outcome populations" )
  OUT1 <- c( n_filer_c3 = "Filers", n_grass = "Grassroots", n_bmf_c3pc = "Registered stock", exp_c3_nohu = "Spending excl. H&U" )
  m <- lean_ok( M22 )
  res <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( lapply( names( OUT1 ), function( y )
    fit_cs( m, y, SETS[[ s ]], "loglog" )[ , `:=`( set = s, outcome = OUT1[[ y ]] ) ] ) ) ) )
  fwrite( res, file.path( OD, "d1_outcome_models.csv" ) )
  cat( "\n== D1: partial r, log-log 2022 ==\n" ); print( wide( res, c( "set", "outcome" ) ) )
  print( unique( res[ , .( set, outcome, n, r2 = round( r2, 3 ) ) ] ) )
  dotfig( res[ set == "lean" ][ , outcome := factor( outcome, OUT1 ) ], "outcome", "d1_lean_outcomes.png",
          "D1. The lean model, four outcome populations", sprintf( "Partial r with 95%% interval; log-log with log P, %d metros, 2022", nrow( m ) ), h = 7.5 )
}

## ===========================================================================
## County build shared by D2 and D5c: lean covariates for any grouping of counties
## ===========================================================================
if ( any( c( "D2", "D5c" ) %in% PARTS ) ) {
  log_msg( "county build" )
  source( "R/lib/geo_helpers.R" ); source( "R/lib/13_16_cov_helpers.R" )
  SPINE <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "csa", "state_fips" ) ) )
  cy <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "county_year.parquet" ) ) )[ year == 2022 ]
  cy <- merge( cy[ , !c( "cbsa", "cbsa_type", "state_fips" ) ], SPINE[ , .( geoid, cbsa, cbsa_type, csa ) ], by = "geoid" )
  AGEB <- grep( "^pop_age_", names( cy ), value = TRUE ); stopifnot( length( AGEB ) == 18 )
  HH <- paste0( "hh_", 1:16 )
  cc <- cy[ , c( list( geoid = geoid, cbsa = cbsa, cbsa_type = cbsa_type, csa = csa, state_fips = substr( geoid, 1, 2 ),
                       population = population, n_filer_c3 = n_filer_c3, exp_c3_nohu = exp_c3_nohu,
                       pf_assets = pf_assets, gov_earn_sl = gov_earn_sl, tr_medicaid = tr_medicaid,
                       edu_ba_num = edu_ba_num, edu_den = edu_den, pov_n = pov_n, pov_universe = pov_universe,
                       votes = turnout * pop_18plus, pop18v = fifelse( is.na( turnout ), NA_real_, as.numeric( pop_18plus ) ),
                       n_nhwhite = n_nhwhite, race_den = race_den, pop_65plus = pop_65plus, pop_total = pop_total,
                       rel_hhi_cy = rel_hhi, age_sd_cy = age_sd, pct_65plus_cy = pct_65plus ), .SD ), .SDcols = AGEB ]
  ## income brackets, ACS 2018-22 county file cached by 89_need_socap_diversity.R (same steps as its build)
  ac <- fread( file.path( OUT$raw_cov, "acs89", "acs5_county_2022.csv" ), colClasses = list( character = "GEOID" ) )
  ac <- ac[ , c( "GEOID", paste0( HH, "E" ) ), with = FALSE ]; setnames( ac, c( "geoid", HH ) )
  ac <- ac[ substr( geoid, 1, 2 ) != "72" ][ , geoid := recode_county( geoid ) ][ , year := 2022L ]
  ac <- ac[ , lapply( .SD, sum, na.rm = TRUE ), by = .( geoid, year ), .SDcols = HH ]
  ac <- ct_legacy_to_pr( ac, HH, "total" )
  cc <- merge( cc, ac[ , c( "geoid", HH ), with = FALSE ], by = "geoid", all.x = TRUE )
  ## pre-1970 endowment by county: 2000 BMF 501(c)(3) public charities ruled before 1970 (55_endowment_fork.R)
  b <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "year", "geoid", "subsec", "pf", "ryear" ),
              colClasses = list( character = c( "geoid", "subsec" ) ) )
  b <- b[ year == 2000 & subsec %in% c( "03", "3" ) & pf == FALSE & !is.na( geoid ) ]
  eb <- b[ , .( hist_b = .N, hist_n = sum( ryear < 1970, na.rm = TRUE ) ), by = geoid ]; rm( b ); gc()
  cc <- merge( cc, eb, by = "geoid", all.x = TRUE ); cc[ is.na( hist_b ), `:=`( hist_b = 0L, hist_n = 0L ) ]
  ## RCMS 2020 group adherents, harmonized onto 2023 counties (18_cov_rcms.R); 2022 = 2020 carried forward
  x20 <- as.data.table( readxl::read_excel( file.path( OUT$raw_cov, "rcms", "2020_USRC_Group_Detail.xlsx" ), sheet = "2020 Group by County" ) )
  setnames( x20, c( "geoid", "state", "county", "group", "group_name", "cng", "adh", "sh_adh", "sh_pop" ) )
  g20 <- x20[ , .( geoid = pad( geoid, 5 ), group, cng = as.numeric( cng ), adh = as.numeric( adh ) ) ]; rm( x20 )
  g20[ , gkey := .GRP, by = group ]
  gh <- harmonize_counties( g20[ , .( geoid, year = gkey, cng, adh ) ], counts = c( "cng", "adh" ) )
  setnames( gh, "year", "gkey" ); gh <- gh[ geoid %in% SPINE$geoid ]
  hhi <- function( a ) { a <- a[ is.finite( a ) & a > 0 ]; if ( !length( a ) ) NA_real_ else sum( ( a / sum( a ) )^2 ) }
  rel_for <- function( map ) merge( gh, map, by = "geoid" )[ , .( rel_frac = 1 - hhi( tapply( adh, gkey, sum, na.rm = TRUE ) ) ), by = unit ]
  ## population-lag kernel (82_lag_fork.R build) for any grouping: decennial county counts 1970-1990,
  ## annual PEP 2000-2022; unit ratio of historical to 2000 population from the covered counties, state
  ## ratio when none is covered. Connecticut legacy counties are moved to planning regions here (82 used
  ## the state ratio for them).
  HIST <- file.path( BUILD, "raw", "census-hist" )
  read_old <- function( f ) {
    L <- readLines( file.path( HIST, f ), warn = FALSE )
    i <- grep( "^[0-9]{5} ", L ); out <- list()
    for ( k in i ) {
      fips <- substr( L[ k ], 1, 5 ); if ( !is.null( out[[ fips ]] ) || grepl( "000$", fips ) ) next
      tk <- strsplit( trimws( substring( L[ k ], 7 ) ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ]
      if ( !length( nums ) ) { tk <- strsplit( trimws( L[ k + 1 ] ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ] }
      if ( length( nums ) ) out[[ fips ]] <- as.numeric( nums[ 1 ] )
    }
    data.table( fips = names( out ), pop = unlist( out ) ) }
  p70 <- read_old( "e7079co.txt" ); p80 <- read_old( "e8089co.txt" )
  L <- readLines( file.path( HIST, "99c8_00.txt" ), warn = FALSE ); L <- L[ grepl( "^1 +[0-9]{5} ", L ) ]
  p90 <- rbindlist( lapply( L, function( x ) { tk <- strsplit( trimws( x ), " +" )[[ 1 ]]
    nums <- tk[ grepl( "^[0-9,]+$", tk ) ][ -1 ]
    data.table( fips = tk[ 2 ], pop = as.numeric( gsub( ",", "", nums[ length( nums ) ] ) ) ) } ) )
  RECODE <- c( "12025" = "12086", "51780" = "51083", "51560" = "51005", "51515" = "51019",
               "51123" = "51800", "51695" = "51800", "30113" = "30067" )
  kh <- rbindlist( list( p70[ , year := 1970L ], p80[ , year := 1980L ], p90[ , year := 1990L ] ) )
  kh[ fips %in% names( RECODE ), fips := RECODE[ fips ] ]
  kh <- kh[ , .( hpop = sum( pop ) ), by = .( geoid = fips, year ) ]
  kh <- ct_legacy_to_pr( kh, "hpop", "total" )[ , .( geoid, year, pop = hpop ) ]   # ("pop" clashes inside ct_legacy_to_pr)
  cp <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )[ year <= 2022 ]
  st <- merge( kh[ , .( hp = sum( pop ) ), by = .( state_fips = substr( geoid, 1, 2 ), year ) ],
               cp[ year == 2000, .( p2000 = sum( population ) ), by = .( state_fips = substr( geoid, 1, 2 ) ) ], by = "state_fips" )
  st[ , ratio_state := hp / p2000 ]
  kernel_for <- function( map, half_life = 10 ) {
    u <- merge( map, cp[ year == 2000, .( geoid, pop2000 = population ) ], by = "geoid", all.x = TRUE )
    rt <- rbindlist( lapply( c( 1970L, 1980L, 1990L ), function( y ) { h <- kh[ year == y ]
      u[ , { f <- geoid %in% h$geoid & !is.na( pop2000 )
        .( year = y, coverage = sum( pop2000[ f ] ) / sum( pop2000, na.rm = TRUE ),
           ratio_cty = if ( any( f ) ) sum( h$pop[ match( geoid[ f ], h$geoid ) ] ) / sum( pop2000[ f ] ) else NA_real_,
           main_state = names( which.max( tapply( pop2000, substr( geoid, 1, 2 ), sum, na.rm = TRUE ) ) ) ) }, by = unit ] } ) )
    rt <- merge( rt, st[ , .( main_state = state_fips, year, ratio_state ) ], by = c( "main_state", "year" ), all.x = TRUE )
    rt[ , ratio := fifelse( is.na( ratio_cty ) | coverage == 0, ratio_state, ratio_cty ) ]
    ann <- merge( map, cp, by = "geoid" )[ , .( logP = log( sum( population ) ) ), by = .( unit, year ) ]
    w <- merge( dcast( rt, unit ~ year, value.var = "ratio" ), ann[ year == 2000, .( unit, l2000 = logP ) ], by = "unit" )
    setnames( w, c( "1970", "1980", "1990" ), c( "r70", "r80", "r90" ) )
    pre <- w[ , .( year = 1970:1999, logP = approx( c( 1970, 1980, 1990, 2000 ), c( l2000 + log( c( r70, r80, r90 ) ), l2000 ), xout = 1970:1999 )$y ), by = unit ]
    full <- rbind( pre, ann[ unit %in% w$unit ] ); setorder( full, unit, year )
    full[ , dp := c( 0, diff( logP ) ), by = unit ]
    a <- 0.5^( 1 / half_life )
    full[ , G := as.numeric( stats::filter( dp, a, method = "recursive" ) ), by = unit ]
    merge( full[ year == 2022, .( unit, lag_g10 = G ) ], rt[ , .( min_cov = min( coverage ) ), by = unit ], by = "unit" ) }
  ## aggregate counties to units; rates recomputed from numerators and denominators
  mid <- c( seq( 2.5, 82.5, by = 5 ), 87.5 )
  lo <- c( 0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75, 100, 125, 150, 200 ) * 1000; hi <- c( lo[ -1 ], Inf )
  gini_g <- function( n, a ) { if ( !is.finite( sum( n ) ) || sum( n ) <= 0 ) return( NA_real_ )
    mid_i <- c( ( lo[ -16 ] + hi[ -16 ] ) / 2, 200000 * a / ( a - 1 ) )
    p <- n / sum( n ); inc <- p * mid_i; L <- cumsum( inc ) / sum( inc ); Lp <- c( 0, L[ -16 ] )
    1 - sum( p * ( L + Lp ) ) }
  SUMS <- c( "population", "n_filer_c3", "exp_c3_nohu", "pf_assets", "gov_earn_sl", "tr_medicaid", "edu_ba_num", "edu_den",
             "pov_n", "pov_universe", "votes", "pop18v", "n_nhwhite", "race_den", "pop_65plus", "pop_total", AGEB, HH, "hist_n", "hist_b" )
  build_unit <- function( z, key ) {
    z <- copy( z )[ , unit := get( key ) ]
    a <- z[ , c( list( counties = .N, cbsa = cbsa[ which.max( population ) ], state_fips = state_fips[ which.max( population ) ] ),
                 lapply( .SD, sum, na.rm = TRUE ) ), by = unit, .SDcols = SUMS ]
    a[ , `:=`( year = 2022L, logP = log( population ), pct_ba = edu_ba_num / edu_den, poverty_rate = pov_n / pov_universe,
               turnout = votes / pop18v, pct_nonwhite = 1 - n_nhwhite / race_den, pct_65plus = pop_65plus / pop_total,
               hist_pre1970 = fifelse( hist_b > 0 & hist_n / hist_b > 0.6, NA_real_, as.numeric( hist_n ) ) ) ]
    A <- as.matrix( a[ , AGEB, with = FALSE ] ); A <- A / rowSums( A ); mu <- as.vector( A %*% mid )
    a[ , age_sd := sqrt( pmax( rowSums( A * outer( rep( 1, nrow( A ) ), mid )^2 ) - mu^2, 0 ) ) ]
    H <- as.matrix( a[ , HH, with = FALSE ] )
    al <- pmin( pmax( log( ( H[ , 15 ] + H[ , 16 ] ) / H[ , 16 ] ) / log( 200 / 150 ), 1.5 ), 4 ); al[ !is.finite( al ) ] <- 4
    a[ , gini_m := vapply( seq_len( .N ), function( i ) gini_g( H[ i, ], al[ i ] ), numeric( 1 ) ) ]
    map <- unique( z[ , .( geoid, unit ) ] )
    a <- merge( a, rel_for( map ), by = "unit", all.x = TRUE )
    a <- merge( a, kernel_for( map ), by = "unit", all.x = TRUE )
    for ( v in c( "pct_ba", "poverty_rate", "turnout", "pct_nonwhite", "pct_65plus", "gini_m", "rel_frac", "age_sd", "lag_g10" ) )
      set( a, i = which( !is.finite( a[[ v ]] ) ), j = v, value = NA_real_ )
    a[ , c( AGEB, HH ) := NULL ]
    a[] }
  UNITS <- list(
    "County (all)"               = build_unit( cc, "geoid" ),
    "Metro CBSA"                 = build_unit( cc[ cbsa_type == "metro" ], "cbsa" ),
    "Metro + micro CBSA"         = build_unit( cc[ cbsa_type %in% c( "metro", "micro" ) ], "cbsa" ),
    "Micropolitan only"          = build_unit( cc[ cbsa_type == "micro" ], "cbsa" ),
    "CSA or metro (metro areas)" = build_unit( cc[ cbsa_type == "metro" ][ , grp := fifelse( !is.na( csa ) & csa != "", paste0( "csa", csa ), cbsa ) ], "grp" ) )
  UNITS[[ "County (all)" ]][ , geoid := unit ]
  ## checks: county-built metro covariates vs the panel's metro values (2022)
  chk <- merge( UNITS[[ "Metro CBSA" ]], M22, by.x = "unit", by.y = "cbsa", suffixes = c( "", ".panel" ) )
  ck <- rbindlist( lapply( LEAN, function( v ) { a <- chk[[ v ]]; b <- chk[[ paste0( v, ".panel" ) ]]
    if ( v %in% TOT ) { a <- log( pmax( a, 1 ) ); b <- log( pmax( b, 1 ) ) }
    ok <- is.finite( a ) & is.finite( b )
    data.table( term = v, n = sum( ok ), r = cor( a[ ok ], b[ ok ] ), max_abs_diff = max( abs( a[ ok ] - b[ ok ] ) ),
                na_county_build = sum( !is.finite( a ) ), na_panel = sum( !is.finite( b ) ) ) } ) )
  cat( "\n== county-built metro covariates vs panel (totals in logs) ==\n" ); print( r3( ck ) )
  fwrite( ck, file.path( OD, "d2_build_check.csv" ) )
  cty_cmp <- UNITS[[ "County (all)" ]][ cc, on = c( unit = "geoid" ) ]
  cat( "county check: cor(age_sd, county_year) =", round( cor( cty_cmp$age_sd, cty_cmp$age_sd_cy, use = "p" ), 4 ),
       "; cor(1 - rel_frac, county_year rel_hhi) =", round( cor( 1 - cty_cmp$rel_frac, cty_cmp$rel_hhi_cy, use = "p" ), 4 ),
       "; cor(pct_65plus) =", round( cor( cty_cmp$pct_65plus, cty_cmp$pct_65plus_cy, use = "p" ), 4 ), "\n" )
  for ( u in names( UNITS ) ) fwrite( UNITS[[ u ]], file.path( OD, sprintf( "d2_unit_covariates_%s.csv", gsub( "[^a-z]+", "_", tolower( u ) ) ) ) )
  miss <- rbindlist( lapply( names( UNITS ), function( u ) { z <- UNITS[[ u ]]
    data.table( unit = u, units = nrow( z ), t( sapply( LEAN, function( v ) sum( !is.finite( z[[ v ]] ) ) ) ) ) } ) )
  cat( "\n== units with a missing lean covariate, by term ==\n" ); print( miss )
  fwrite( miss, file.path( OD, "d2_missing_terms.csv" ) )
}

## ===========================================================================
## D2 (58 H): the model at six units
## ===========================================================================
if ( "D2" %in% PARTS ) {
  log_msg( "D2: units" )
  ulev <- c( names( UNITS ), "Counties within metros (metro FE)" )
  fitu <- function( z, u, y, xs, fe = NULL ) {
    zz <- z[ complete.cases( z[ , xs, with = FALSE ] ) ]
    out <- fit_cs( zz, y, xs, "loglog", fe = fe, df_rule = if ( is.null( fe ) ) "resid" else "clusters" )
    b0 <- coef( lm( log( zz[[ y ]][ zz[[ y ]] > 0 ] ) ~ zz$logP[ zz[[ y ]] > 0 ] ) )[ 2 ]
    out[ , `:=`( unit = u, outcome = y, units_all = nrow( z ), dropped_na = nrow( z ) - nrow( zz ), zeros = zz[ get( y ) <= 0, .N ], beta_bivariate = b0 ) ] }
  res <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( c(
    lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y ) rbindlist( lapply( names( UNITS ), function( u ) {
      z <- UNITS[[ u ]][ complete.cases( UNITS[[ u ]][ , LEAN, with = FALSE ] ) ]   # same sample for both sets
      fitu( z, u, y, SETS[[ s ]] ) } ) ) ),
    lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y ) {
      z <- UNITS[[ "County (all)" ]][ cbsa %in% M22$cbsa ]; z <- z[ complete.cases( z[ , LEAN, with = FALSE ] ) ]
      z[ , cbsa := as.character( cbsa ) ]
      fitu( z, "Counties within metros (metro FE)", y, SETS[[ s ]], fe = "cbsa" ) } ) ) )[ , set := s ] ) )
  fwrite( res, file.path( OD, "d2_units.csv" ) )
  print( unique( res[ set == "lean", .( unit, outcome, n, units_all, dropped_na, zeros, beta_bivariate = round( beta_bivariate, 3 ) ) ] ) )
  for ( s in names( SETS ) ) for ( y in c( "n_filer_c3", "exp_c3_nohu" ) ) {
    cat( sprintf( "\n== D2: partial r, %s, %s ==\n", y, s ) )
    print( wide( res[ set == s & outcome == y ][ , unit := factor( unit, ulev ) ], "unit" ) ) }
  dotfig( res[ set == "lean" & outcome == "n_filer_c3" ][ , unit := factor( unit, ulev ) ], "unit", "d2_lean_units.png",
          "D2. The lean filer-count model at six geographic units, 2022",
          "Partial r with 95% interval; log-log with log P; all 13 covariates rebuilt from county data for every unit", h = 8.5 )
}

## ===========================================================================
## D2 size check (86 B + 90, single lean SAMI spec): metros < / >= 250k; quadratic size interaction
## ===========================================================================
if ( "D2size" %in% PARTS ) {
  log_msg( "D2size: size split and quadratic size interaction" )
  OUTS <- c( n_filer_c3 = "Filers", n_grass = "Grassroots", exp_c3_nohu = "Spending" )
  m <- lean_ok( M22 )
  KS <- list( lt250k = m[ population < 2.5e5 ], ge250k = m[ population >= 2.5e5 ], all = m )
  log_msg( "  metros < 250k: ", nrow( KS$lt250k ), ", >= 250k: ", nrow( KS$ge250k ) )
  ss <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( lapply( names( OUTS ), function( y ) rbindlist( lapply( names( KS ), function( k )
    fit_cs( KS[[ k ]], y, SETS[[ s ]], "sami", df_rule = "places" )[ , `:=`( set = s, outcome = y, sample = k ) ] ) ) ) ) ) )
  ss[ , se_pr := ( 1 - r^2 ) / sqrt( df ) ]
  w <- dcast( ss, set + outcome + term ~ sample, value.var = c( "r", "se_pr", "n" ) )
  w[ , `:=`( diff = r_ge250k - r_lt250k, z = ( r_ge250k - r_lt250k ) / sqrt( se_pr_ge250k^2 + se_pr_lt250k^2 ) ) ]
  fwrite( ss, file.path( OD, "d2_size_split_models.csv" ) ); fwrite( w, file.path( OD, "d2_size_split.csv" ) )
  cat( "\n== D2 size split: partial r, SAMI spec, 2022 ==\n" )
  print( r3( w[ , .( set, outcome, term = LAB[ term ], all = r_all, small = r_lt250k, large = r_ge250k, diff, z ) ][ order( set, outcome, match( names( LAB )[ match( term, LAB ) ], TORD ) ) ] ), nrows = 200 )
  ## quadratic size interaction, one focal covariate at a time (90_size_moderation_screen.R design)
  SIZES <- c( "100k" = 1e5, "250k" = 2.5e5, "1M" = 1e6, "4M" = 4e6 )
  FOC <- c( "poverty_rate", "pct_ba", "pf_assets", "turnout", "pct_nonwhite" )
  qd <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( lapply( names( OUTS ), function( y ) {
    xs <- SETS[[ s ]]
    d <- m[ is.finite( m[[ y ]] ) & m[[ y ]] > 0 ]
    X <- mform( d, xs, "sami" )
    mm <- cbind( data.table( yy = resid_logP( log( d[[ y ]] ), d$logP, d$year ), logP = d$logP ), X )
    mc <- mean( mm$logP ); mm[ , c := logP - mc ]; mm[ , c2 := c^2 ]
    rbindlist( lapply( FOC, function( fx ) {
      mm[ , ic := mm[[ fx ]] * c ]; mm[ , ic2 := mm[[ fx ]] * c2 ]
      rl <- feols( as.formula( paste( "yy ~", paste( c( xs, "c", "ic" ), collapse = " + " ) ) ), mm, vcov = "hetero" )
      rq <- feols( as.formula( paste( "yy ~", paste( c( xs, "c", "c2", "ic", "ic2" ), collapse = " + " ) ) ), mm, vcov = "hetero" )
      cl <- coeftable( rl ); cq <- coeftable( rq )
      wd <- wald( rq, keep = "^ic2?$", print = FALSE )
      b <- coef( rq )[ c( fx, "ic", "ic2" ) ]; V <- vcov( rq )[ c( fx, "ic", "ic2" ), c( fx, "ic", "ic2" ) ]
      me <- vapply( SIZES, function( p ) { a <- log( p ) - mc; g <- c( 1, a, a^2 ); sum( g * b ) / sqrt( drop( t( g ) %*% V %*% g ) ) }, numeric( 1 ) )
      data.table( set = s, outcome = y, focal = fx, n = nobs( rq ), t_lin = cl[ "ic", 3 ], p_lin = cl[ "ic", 4 ],
                  t_c = cq[ "ic", 3 ], t_c2 = cq[ "ic2", 3 ], p_c2 = cq[ "ic2", 4 ], p_joint = wd$p, t_main_center = cq[ fx, 3 ],
                  me_t_100k = me[[ 1 ]], me_t_250k = me[[ 2 ]], me_t_1M = me[[ 3 ]], me_t_4M = me[[ 4 ]] ) } ) ) } ) ) ) )
  fwrite( qd, file.path( OD, "d2_size_quadratic.csv" ) )
  cat( "\n== D2 quadratic size interaction (t of x*c^2; joint Wald p; t of the effect at each size) ==\n" ); print( r3( qd ), nrows = 100 )
  ## figure: lean poverty, effect t by size
  fl <- melt( qd[ set == "lean" ], id.vars = c( "outcome", "focal" ), measure.vars = patterns( "^me_t_" ), variable.name = "size", value.name = "t" )
  fl[ , `:=`( size = factor( sub( "me_t_", "", size ), names( SIZES ) ), outcome = factor( OUTS[ outcome ], OUTS ), focal = factor( LAB[ focal ], LAB[ FOC ] ) ) ]
  p <- ggplot( fl, aes( size, t, colour = outcome, group = outcome ) ) +
    geom_hline( yintercept = c( -1.96, 1.96 ), colour = "#c3c2b7", linewidth = .3, linetype = "22" ) + geom_hline( yintercept = 0, colour = "grey40" ) +
    geom_line( linewidth = .6 ) + geom_point( size = 1.9 ) + facet_wrap( ~ focal, nrow = 1 ) + scale_colour_manual( values = PAL, name = NULL ) +
    labs( title = "D2. Effect of each benchmark by metro size, lean SAMI model, quadratic size interaction",
          subtitle = sprintf( "t of the marginal effect at 100k, 250k, 1M and 4M; %d metros, 2022; dashed: |t| = 1.96", nrow( m ) ),
          x = "Metro population", y = "t" ) + thm
  ggsave( file.path( FIG, "d2_lean_size.png" ), p, width = 11.5, height = 4.4, dpi = 170, bg = "white" )
}

## ===========================================================================
## D3 (52 C3): hybrid / correlated random effects, 2010-2022
## ===========================================================================
if ( "D3" %in% PARTS ) {
  log_msg( "D3: hybrid (CRE) models" )
  pn <- P[ cbsa_type == "metro" & year %in% 2010:2022 ]
  OUTS <- c( y_n = "n_filer_c3", y_g = "n_grass", y_e = "exp_c3_nohu" )
  for ( k in names( OUTS ) ) set( pn, j = k, value = resid_logP( log( ifelse( pn[[ OUTS[[ k ]] ]] > 0, pn[[ OUTS[[ k ]] ]], NA_real_ ) ), pn$logP, pn$year ) )
  ## totals: per-capita floor set by year, then residualized on log P by year (SAMI form); rates as is
  for ( v in TOT ) { pn[ , ( v ) := as.numeric( get( v ) ) ]; pn[ , ( v ) := flog( get( v ), population ), by = year ]
                     set( pn, j = v, value = resid_logP( pn[[ v ]], pn$logP, pn$year ) ) }
  cre <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( lapply( names( OUTS ), function( y ) {
    xs <- SETS[[ s ]]; tv <- setdiff( xs, "hist_pre1970" )
    d <- pn[ is.finite( get( y ) ) ][ complete.cases( pn[ is.finite( get( y ) ), xs, with = FALSE ] ) ]
    for ( v in tv ) { d[ , ( paste0( "b_", v ) ) := mean( get( v ) ), by = cbsa ]; d[ , ( paste0( "w_", v ) ) := get( v ) - get( paste0( "b_", v ) ) ] }
    if ( "hist_pre1970" %in% xs ) d[ , b_hist_pre1970 := mean( hist_pre1970 ), by = cbsa ]
    terms <- c( paste0( "w_", tv ), paste0( "b_", xs ) )
    sdx <- sapply( terms, function( v ) sd( d[[ v ]] ) )
    for ( v in terms ) set( d, j = v, value = d[[ v ]] / sdx[[ v ]] )
    setnames( d, y, "yy" )
    r <- lme4::lmer( as.formula( paste( "yy ~", paste( terms, collapse = " + " ), "+ factor( year ) + ( 1 | cbsa )" ) ), d, REML = FALSE )
    cf <- summary( r )$coefficients; cf <- cf[ rownames( cf ) %in% terms, , drop = FALSE ]
    vc <- as.data.frame( lme4::VarCorr( r ) ); G <- uniqueN( d$cbsa )
    data.table( set = s, outcome = OUTS[[ y ]], term = rownames( cf ), est = cf[ , 1 ], se = cf[ , 2 ], t = cf[ , 3 ],
                std = cf[ , 1 ] / sd( d$yy ), pr = pr_of( cf[ , 3 ], G - length( terms ) - 1 ), sd_term = sdx[ rownames( cf ) ],
                metros = G, n = nrow( d ), icc = vc$vcov[ vc$grp == "cbsa" ] / sum( vc$vcov ) ) } ) ) ) )
  cre[ , `:=`( part = fifelse( grepl( "^w_", term ), "within", "between" ), var = sub( "^[wb]_", "", term ) ) ]
  fwrite( cre, file.path( OD, "d3_hybrid_cre.csv" ) )
  cat( "\n== D3: CRE standardized effects (SD of SAMI per SD of the term) ==\n" )
  for ( s in names( SETS ) ) { cat( "--", s, "\n" ); print( wide( cre[ set == s ][ , term := var ], c( "outcome", "part" ), "std" ) ) }
  cat( "\n== D3: CRE t ==\n" ); print( wide( cre[ set == "lean" ][ , term := var ], c( "outcome", "part" ), "t" ) )
  print( unique( cre[ , .( set, outcome, metros, n, icc = round( icc, 3 ) ) ] ) )
}

## ===========================================================================
## D4 (58 J): DV form
## ===========================================================================
if ( "D4" %in% PARTS ) {
  log_msg( "D4: DV form" )
  m <- lean_ok( M22 )
  Y <- c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. H&U" )
  FORMS <- c( loglog = "Log-log", sami = "SAMI", logpcP = "Log per capita + log P", logpc = "Log per capita, no log P", pc = "Per capita (linear), no log P" )
  res <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( lapply( names( Y ), function( y ) rbindlist( lapply( names( FORMS ), function( f )
    fit_cs( m, y, SETS[[ s ]], f )[ , `:=`( set = s, outcome = y, form = f ) ] ) ) ) ) ) )
  fwrite( res, file.path( OD, "d4_forms.csv" ) )
  for ( s in names( SETS ) ) { cat( "\n== D4: partial r,", s, "==\n" ); print( wide( res[ set == s & term != "logP" ][ , form := factor( form, names( FORMS ) ) ], c( "outcome", "form" ) ) ) }
  ## size correlation of each covariate as entered in the per capita model
  Xpc <- mform( m, LEAN, "logpc", std = FALSE )
  sz <- data.table( term = LEAN, r_logP_logpc = sapply( LEAN, function( v ) cor( Xpc[[ v ]], m$logP ) ),
                    r_logP_log = sapply( LEAN, function( v ) cor( mform( m, v, "log", FALSE )[[ 1 ]], m$logP ) ) )
  print( r3( sz ) ); fwrite( sz, file.path( OD, "d4_size_cor.csv" ) )
  dotfig( res[ set == "lean" & form %in% c( "loglog", "logpc", "pc" ) ][ , `:=`( form = factor( FORMS[ form ], FORMS[ c( "loglog", "logpc", "pc" ) ] ), outcome = factor( Y[ outcome ], Y ) ) ],
          "form", "d4_lean_dv_form.png", "D4. DV form changes the lean model's results",
          sprintf( "Partial r with 95%% interval, %d metros, 2022; per capita forms use per capita totals and no log P", nrow( m ) ), facet = "outcome", h = 7.5 )
}

## ===========================================================================
## D5 (58 K, metro): OLS vs Poisson PML with log P on the right; Poisson spending without the top 5%
## ===========================================================================
if ( "D5" %in% PARTS ) {
  log_msg( "D5: metro OLS vs Poisson" )
  m <- lean_ok( M22 )
  mres <- rbindlist( lapply( names( SETS ), function( s ) {
    xs <- SETS[[ s ]]; X <- mform( m, xs, "sami" ); mm <- cbind( m[ , .( logP, population, n_filer_c3, exp_c3_nohu ) ], X )
    rhs <- paste( "logP +", paste( xs, collapse = " + " ) )
    rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y ) {
      o <- coeftable( feols( as.formula( paste( "log(", y, ") ~", rhs ) ), mm[ get( y ) > 0 ], vcov = "hetero" ) )
      p <- coeftable( fepois( as.formula( paste( y, "~", rhs ) ), mm, vcov = "hetero" ) )
      q <- coeftable( fepois( as.formula( paste( y, "~", rhs ) ), mm[ population < quantile( population, .95 ) ], vcov = "hetero" ) )
      data.table( set = s, outcome = y, term = rownames( o ), ols = o[ , 1 ], ols_se = o[ , 2 ], pois = p[ , 1 ], pois_se = p[ , 2 ],
                  pois_p = p[ , 4 ], pois_no_top5 = q[ , 1 ], pois_no_top5_se = q[ , 2 ], se_ratio = p[ , 2 ] / o[ , 2 ] ) } ) ) } ) )
  mres <- mres[ term != "(Intercept)" ]
  fwrite( mres, file.path( OD, "d5_metro_ols_pois.csv" ) )
  cat( "\n== D5: metro OLS vs Poisson (per SD; SAMI-form covariates, log P on the right) ==\n" )
  print( r3( mres[ order( set, outcome, match( term, TORD ) ) ] ), nrows = 100 )
}

## ===========================================================================
## D5c (58 K, counties): subsector scaling under OLS / log(n+1) / Poisson with lean county covariates
## ===========================================================================
if ( "D5c" %in% PARTS ) {
  log_msg( "D5c: county subsector models" )
  GL <- c( ALL = "Full sector", ART = "Arts", EDU = "Education", UNI = "Universities", ENV = "Environment/animals",
           HEL = "Health", HOS = "Hospitals", HMS = "Human services", IFA = "International",
           PSB = "Public/societal benefit", REL = "Religion-related", MMB = "Mutual/membership" )
  pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )
  ef <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ),
          col_select = c( "ein", "year", "subsec_group", "rev", "exp", "ind", "geoid" ) ) )[ year == 2022 & subsec_group == "c3" & !is.na( geoid ) ]
  ef <- merge( ef, pce[ , .( year, floor_nom ) ], by = "year" )[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
  bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "ntee3" ),
                colClasses = list( character = c( "EIN", "ntee3" ) ) )[ !is.na( ntee3 ) & ntee3 != "" ]
  bmf <- unique( bmf[ order( EIN, -year ) ], by = "EIN" )[ , .( ein = EIN, ntee_bmf = toupper( substr( ntee3, 1, 3 ) ) ) ]
  ef <- merge( ef, bmf, by = "ein", all.x = TRUE ); rm( bmf ); gc()
  map_ntee <- function( n ) { a <- substr( n, 1, 1 )
    fcase( n %in% NTEE_UNIV, "UNI", n %in% NTEE_HOSP, "HOS", a == "A", "ART", a == "B", "EDU",
           a %in% c( "C", "D" ), "ENV", a %in% c( "E", "F", "G", "H" ), "HEL", a %in% LETTERS[ 9:16 ], "HMS",
           a == "Q", "IFA", a %in% LETTERS[ 18:23 ], "PSB", a == "X", "REL", a == "Y", "MMB", default = "UNU" ) }
  ef[ , grp := fifelse( is.na( ind ) | ind == "UNU", map_ntee( ntee_bmf ), ind ) ]
  ef <- ef[ geoid %in% SPINE$geoid ]
  cn <- rbind( ef[ grp %in% names( GL ), .( n = .N ), by = .( geoid, grp ) ], ef[ , .( grp = "ALL", n = .N ), by = geoid ] )
  cn <- CJ( geoid = SPINE$geoid, grp = names( GL ) )[ cn, on = .( geoid, grp ), n := i.n ][ is.na( n ), n := 0L ]
  cv0 <- UNITS[[ "County (all)" ]]; cv0 <- cv0[ complete.cases( cv0[ , LEAN, with = FALSE ] ) & population > 0 ]
  log_msg( "  counties with every lean covariate: ", nrow( cv0 ), " of ", nrow( UNITS[[ "County (all)" ]] ) )
  EST <- c( ols = "OLS on log(n), zeros dropped", ols1 = "OLS on log(n + 1)", pois = "Poisson (PML), zeros kept" )
  res <- rbindlist( lapply( names( SETS ), function( s ) {
    xs <- SETS[[ s ]]
    cv <- cbind( cv0[ , .( geoid, logP ) ], mform( cv0, xs, "sami" ) )
    x <- merge( cn, cv, by = "geoid" ); rhs <- paste( "logP +", paste( xs, collapse = " + " ) )
    rbindlist( lapply( names( GL ), function( g ) { z <- x[ grp == g ]
      rbindlist( lapply( names( EST ), function( e ) {
        r <- switch( e, ols = feols( as.formula( paste( "log( n ) ~", rhs ) ), z[ n > 0 ], vcov = "hetero" ),
                     ols1 = feols( as.formula( paste( "log( n + 1 ) ~", rhs ) ), z, vcov = "hetero" ),
                     pois = fepois( as.formula( paste( "n ~", rhs ) ), z, vcov = "hetero" ) )
        ct <- coeftable( r )
        data.table( set = s, grp = g, label = GL[[ g ]], est = e, term = rownames( ct ), b = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ],
                    n_used = nobs( r ), counties = nrow( z ), zero_share = mean( z$n == 0 ) ) } ) ) } ) ) } ) )
  fwrite( res, file.path( OD, "d5_county_models.csv" ) )
  cat( "\n== D5c: county scaling exponent by estimator ==\n" )
  print( r3( dcast( res[ term == "logP" ], label + zero_share ~ set + est, value.var = "b" )[ order( zero_share ) ] ) )
  w <- dcast( res[ !term %in% c( "(Intercept)", "logP" ) ], set + grp + term ~ est, value.var = c( "b", "p" ) )
  dis <- w[ , .( flips = sum( sign( b_ols ) != sign( b_pois ) & ( p_ols < .05 | p_pois < .05 ) ),
                 mean_abs_gap = mean( abs( b_ols - b_pois ) ), mean_abs_pois = mean( abs( b_pois ) ) ), by = .( set, grp ) ]
  dis <- merge( dis, unique( res[ , .( grp, zero_share ) ] ), by = "grp" )[ order( set, zero_share ) ]
  print( r3( dis ) ); fwrite( dis, file.path( OD, "d5_disagreement.csv" ) )
  cat( "\n== D5c: foundation assets, OLS vs Poisson, sparse groups ==\n" )
  print( r3( dcast( res[ term == "pf_assets" & grp %in% c( "ALL", "HOS", "UNI", "MMB" ) ], set + label ~ est, value.var = "b" ) ) )
}

## ===========================================================================
## D8 (58 M): endowment, market share, growth kernel
## ===========================================================================
if ( "D8" %in% PARTS ) {
  log_msg( "D8: history controls" )
  sh <- P[ cbsa_type == "metro" & year %in% 2013:2022 & !( bls_delin_flag %in% TRUE ) & bls_pct_npo > 0, .( share = mean( bls_pct_npo ), yrs = .N ), by = cbsa ][ yrs >= 5 ]
  m <- merge( lean_ok( M22 ), sh, by = "cbsa" )[ , bls_share := log( share ) ]
  log_msg( "  metros with market share and every lean covariate: ", nrow( m ) )
  NOE <- setdiff( LEAN, "hist_pre1970" )
  ARMS <- list( lean_noendow = NOE, lean = LEAN, lean_noendow_share = c( NOE, "bls_share" ), lean_share = c( LEAN, "bls_share" ),
                lean_nogrowth = setdiff( LEAN, "lag_g10" ),
                old6 = OLD6, old6_endow = c( OLD6, "hist_pre1970" ), old6_share = c( OLD6, "bls_share" ), old6_both = c( OLD6, "hist_pre1970", "bls_share" ) )
  res <- rbindlist( lapply( c( n_filer_c3 = "n_filer_c3", exp_c3_nohu = "exp_c3_nohu" ), function( y ) rbindlist( lapply( names( ARMS ), function( a )
    fit_cs( m, y, ARMS[[ a ]], "sami" )[ , `:=`( outcome = y, arm = a ) ] ) ) ) )
  fwrite( res, file.path( OD, "d8_models.csv" ) )
  for ( y in c( "n_filer_c3", "exp_c3_nohu" ) ) { cat( "\n== D8: partial r,", y, "==\n" ); print( wide( res[ outcome == y ][ , arm := factor( arm, names( ARMS ) ) ], "arm" ) ) }
  cat( "\n== D8: R2 ==\n" ); print( r3( dcast( unique( res[ , .( outcome, arm = factor( arm, names( ARMS ) ), r2, n ) ] ), arm ~ outcome, value.var = "r2" ) ) )
  X <- mform( m, c( LEAN, "bls_share" ), "sami" )
  cr <- data.table( term = c( LEAN, "bls_share" ), r_endow = sapply( c( LEAN, "bls_share" ), function( v ) cor( X[[ v ]], X$hist_pre1970 ) ),
                    r_share = sapply( c( LEAN, "bls_share" ), function( v ) cor( X[[ v ]], X$bls_share ) ),
                    r_growth = sapply( c( LEAN, "bls_share" ), function( v ) cor( X[[ v ]], X$lag_g10 ) ) )
  print( r3( cr ) ); fwrite( cr, file.path( OD, "d8_correlations.csv" ) )
  AL <- c( lean_noendow = "Lean without endowment", lean = "Lean (with endowment)", lean_noendow_share = "Lean without endowment + share",
           lean_share = "Lean + share", lean_nogrowth = "Lean without growth kernel" )
  dotfig( res[ arm %in% names( AL ) ][ , `:=`( arm = factor( AL[ arm ], AL ), outcome = factor( c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. H&U" )[ outcome ] ) ) ],
          "arm", "d8_lean_history.png", "D8. What the endowment, market share and growth kernel absorb in the lean model",
          sprintf( "Partial r with 95%% interval, SAMI form, %d metros, 2022 (share = mean 2013-2022)", nrow( m ) ), facet = "outcome", h = 8.5, w = 12 )
}

## ===========================================================================
## D9 (58 E): between-state variance; estimator
## ===========================================================================
if ( "D9" %in% PARTS ) {
  log_msg( "D9: geography" )
  m <- lean_ok( M22 )
  X <- mform( m, LEAN, "log", std = FALSE )
  for ( v in TOT ) set( X, j = v, value = resid_logP( X[[ v ]], m$logP, m$year ) )     # totals in SAMI form, rates as is
  X[ , `:=`( y_filers = resid_logP( log( m$n_filer_c3 ), m$logP, m$year ), y_spending = resid_logP( log( m$exp_c3_nohu ), m$logP, m$year ),
             state = m$state_fips, division = m$division ) ]
  bs <- rbindlist( lapply( c( "y_filers", "y_spending", LEAN ), function( v )
    data.table( term = v, between_state = summary( lm( X[[ v ]] ~ factor( X$state ) ) )$r.squared,
                between_division = summary( lm( X[[ v ]] ~ factor( X$division ) ) )$r.squared ) ) )
  cat( "\n== D9: share of variance between states / divisions (2022 metros, model form) ==\n" ); print( r3( bs[ , term := fifelse( term %in% names( LAB ), LAB[ term ], term ) ] ) )
  fwrite( bs, file.path( OD, "d9_between_state_share.csv" ) )
  ESTS <- c( "ols", "div_fe", "state_fe", "state_re" )
  res <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y ) rbindlist( lapply( ESTS, function( e )
    fit_cs( m, y, SETS[[ s ]], "loglog", est = e, df_rule = "places" )[ , `:=`( set = s, outcome = y, estimator = e ) ] ) ) ) ) ) )
  fwrite( res, file.path( OD, "d9_estimators.csv" ) )
  for ( s in names( SETS ) ) { cat( "\n== D9: partial r by estimator,", s, "==\n" ); print( wide( res[ set == s & term != "logP" ][ , estimator := factor( estimator, ESTS ) ], c( "outcome", "estimator" ) ) ) }
}

## ===========================================================================
## D10 (58 N): money measure
## ===========================================================================
if ( "D10" %in% PARTS ) {
  log_msg( "D10: money measure" )
  m <- lean_ok( M22 )
  OUTS <- c( exp_c3 = "Expenses, all", exp_c3_net = "Expenses net, all", rev_c3 = "Revenue, all", rev_c3_net = "Revenue net, all",
             exp_c3_nohu = "Expenses excl. H&U", exp_c3_nohu_net = "Expenses excl. H&U, net", rev_c3_nohu = "Revenue excl. H&U",
             rev_c3_nohu_net = "Revenue excl. H&U, net", n_filer_c3 = "Filers, all", n_filer_c3_nohu = "Filers excl. H&U" )
  res <- rbindlist( lapply( names( SETS ), function( s ) rbindlist( lapply( names( OUTS ), function( o )
    fit_cs( m, o, SETS[[ s ]], "sami" )[ , `:=`( set = s, outcome = o ) ] ) ) ) )
  fwrite( res, file.path( OD, "d10_models.csv" ) )
  for ( s in names( SETS ) ) { cat( "\n== D10: partial r,", s, "==\n" ); print( wide( res[ set == s & outcome %in% c( "exp_c3", "exp_c3_nohu", "exp_c3_nohu_net", "rev_c3_nohu" ) ][ , outcome := factor( outcome, names( OUTS ) ) ], "outcome" ) ) }
  print( unique( res[ set == "lean", .( outcome, n, r2 = round( r2, 3 ) ) ] ) )
  bt <- rbindlist( lapply( names( OUTS ), function( o ) { d <- m[ get( o ) > 0 ]; b <- lm( log( d[[ o ]] ) ~ d$logP )
    data.table( outcome = o, beta = coef( b )[ 2 ], sd_sami = sd( resid( b ) ), total = sum( d[[ o ]] ) ) } ) )
  print( r3( bt ) ); fwrite( bt, file.path( OD, "d10_beta.csv" ) )
  LB <- c( exp_c3 = "Expenses, all 501(c)(3)", exp_c3_nohu = "Expenses excl. H&U", exp_c3_nohu_net = "... and net of pass-through", rev_c3_nohu = "Revenue excl. H&U" )
  dotfig( res[ set == "lean" & outcome %in% names( LB ) ][ , outcome := factor( LB[ outcome ], LB ) ], "outcome", "d10_lean_money.png",
          "D10. The lean model on four money measures", sprintf( "Partial r with 95%% interval, SAMI form, %d metros, 2022 (spending series as corrected 2026-11-17)", nrow( m ) ), h = 7.5 )
}

## ===========================================================================
## D11 (58 C): subsector Poisson models
## ===========================================================================
if ( "D11" %in% PARTS ) {
  log_msg( "D11: subsector models" )
  GRP <- c( ART = "Arts", EDU = "Education", ENV = "Environment/animals", HEL = "Health", HMS = "Human services",
            IFA = "International", PSB = "Public/societal benefit", REL = "Religion-related", MMB = "Mutual/membership",
            HOS = "Hospitals", UNI = "Universities" )
  sub <- fread( file.path( BUILD, "data", "followup", "p3_subsector_metro_year.csv" ), colClasses = list( character = "cbsa" ) )[ year == 2022 ]
  tot <- sub[ grp != "UNU", .( n = sum( n ), exp = sum( exp ) ), by = cbsa ][ , grp := "ALL" ]
  sub <- rbind( sub[ , .( cbsa, grp, n, exp ) ], tot )
  m <- lean_ok( M22 )
  res <- rbindlist( lapply( names( SETS ), function( s ) {
    xs <- SETS[[ s ]]
    x <- merge( sub, cbind( m[ , .( cbsa, logP ) ], mform( m, xs, "log" ) ), by = "cbsa" )
    rbindlist( lapply( c( count = "n", spending = "exp" ), function( yv ) rbindlist( lapply( c( "ALL", names( GRP ) ), function( g ) {
      z <- x[ grp == g ]; r <- fepois( as.formula( paste( yv, "~ logP +", paste( xs, collapse = " + " ) ) ), z, vcov = "hetero" ); ct <- coeftable( r )
      data.table( set = s, grp = g, label = c( ALL = "All 501(c)(3) filers", GRP )[[ g ]], outcome = fifelse( yv == "n", "count", "spending" ),
                  term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], z = ct[ , 3 ], p = ct[ , 4 ],
                  r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + nobs( r ) - nrow( ct ) - 1 ), orgs = sum( z$n ), zeros = sum( z[[ yv ]] == 0 ) ) } ) ) ) ) } ) )
  res <- res[ term != "(Intercept)" ]
  fwrite( res, file.path( OD, "d11_subsector_models.csv" ) )
  for ( s in names( SETS ) ) { cat( "\n== D11: partial r, counts,", s, "==\n" )
    w <- dcast( res[ set == s & outcome == "count" & term != "logP" ], label ~ term, value.var = "r" )
    setcolorder( w, c( "label", intersect( TORD, names( w ) ) ) ); print( r3( w ) )
    print( r3( res[ set == s & outcome == "count" & term == "logP", .( label, logP = est ) ] ) ) }
  agree <- rbindlist( lapply( c( "count", "spending" ), function( o ) merge( res[ outcome == o & grp != "ALL" ], res[ outcome == o & grp == "ALL", .( set, term, s_all = sign( est ) ) ], by = c( "set", "term" ) )[
    , .( outcome = o, same_sign = sum( sign( est ) == s_all ), sig_same = sum( sign( est ) == s_all & p < .05 ),
         sig_opposite = sum( sign( est ) != s_all & p < .05 ), subsectors = .N ), by = .( set, term ) ] ) )
  agree <- agree[ order( outcome, set, match( term, TORD ) ) ]
  cat( "\n== D11: sign agreement with the full sector ==\n" ); print( agree, nrows = 100 )
  fwrite( agree, file.path( OD, "d11_sign_agreement.csv" ) )
}

## ===========================================================================
## D12 (58 P): sector age
## ===========================================================================
if ( "D12" %in% PARTS ) {
  log_msg( "D12: sector age" )
  a <- fread( file.path( BUILD, "data", "catchup", "c12_age_by_metro.csv" ), colClasses = list( character = "cbsa" ) )
  m <- merge( lean_ok( M22 ), a[ , .( cbsa, age_f = mean_age_filers, age_r = median_age_all, age_d = dollar_weighted_age_nohu ) ], by = "cbsa" )
  m <- m[ is.finite( age_f ) & is.finite( age_r ) & is.finite( age_d ) ]
  log_msg( "  metros: ", nrow( m ) )
  NOE <- setdiff( LEAN, "hist_pre1970" ); OLDNOE <- OLD6
  ARMS <- list( lean_noendow = NOE, lean_noendow_age = c( NOE, "age_f" ), lean = LEAN, lean_age = c( LEAN, "age_f" ),
                old6 = OLD6, old6_age = c( OLD6, "age_f" ), old6_endow = c( OLD6, "hist_pre1970" ), old6_age_endow = c( OLD6, "age_f", "hist_pre1970" ) )
  res <- rbindlist( lapply( c( "n_filer_c3", "n_grass", "exp_c3_nohu" ), function( y ) rbindlist( lapply( names( ARMS ), function( ar )
    fit_cs( m, y, ARMS[[ ar ]], "sami" )[ , `:=`( outcome = y, arm = ar ) ] ) ) ) )
  fwrite( res, file.path( OD, "d12_levels.csv" ) )
  for ( y in c( "n_filer_c3", "n_grass", "exp_c3_nohu" ) ) { cat( "\n== D12: partial r,", y, "==\n" ); print( wide( res[ outcome == y ][ , arm := factor( arm, names( ARMS ) ) ], "arm" ) ) }
  cat( "\n== D12: R2 ==\n" ); print( r3( dcast( unique( res[ , .( outcome, arm = factor( arm, names( ARMS ) ), r2 ) ] ), arm ~ outcome, value.var = "r2" ) ) )
  alt <- rbindlist( lapply( c( "age_f", "age_r", "age_d" ), function( v ) rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y )
    fit_cs( m, y, c( LEAN, v ), "sami" )[ term == v ][ , `:=`( outcome = y, measure = v ) ] ) ) ) )
  cat( "\n== D12: alternative age measures in the lean model ==\n" ); print( r3( alt[ , .( outcome, measure, r, t ) ] ) )
  fwrite( alt, file.path( OD, "d12_alt_measures.csv" ) )
  AL <- c( lean_noendow = "Lean without endowment", lean_noendow_age = "Lean without endowment + age", lean = "Lean", lean_age = "Lean + mean filer age" )
  dotfig( res[ arm %in% names( AL ) & outcome != "n_grass" ][ , `:=`( arm = factor( AL[ arm ], AL ), outcome = factor( c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. H&U" )[ outcome ] ) ) ],
          "arm", "d12_lean_age.png", "D12. Sector age in the lean model", sprintf( "Partial r with 95%% interval, SAMI form, %d metros, 2022", nrow( m ) ),
          facet = "outcome", h = 8.5, w = 12 )
}
log_msg( "done: ", OD )
