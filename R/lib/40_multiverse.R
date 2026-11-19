## 40_multiverse.R
## Multiverse configuration, specification grid, and model engine.
## Sourced by 42_timing_test.R and the production runner.
##
## Strata (type N, never pooled in one curve): outcome construct x hospital/
## university treatment x identification. Forks (type E) below. Every
## specification returns one row per estimated term with the estimate, SE,
## t, p, standardized beta (in-sample), n, and R2.

suppressPackageStartupMessages( { library( data.table ); library( fixest ); library( lme4 ) } )
setFixest_notes( FALSE ); setFixest_nthreads( 1 )

## ---- covariate catalog: block, measure, predictor type ------------------------
## type A = intensive (level; residualized on log P in the all-SAMI arm)
## type B = extensive (raw | per capita | log | SAMI)

COVS <- data.table( var = c(
  "race_hhi", "pct_nonwhite", "pct_foreign_born", "gini", "age_sd",
  "pf_assets", "n_pf", "contrib_total", "contrib_per_return", "contrib_share_agi",
  "turnout", "dem_margin",
  "gov_earn_sl", "gov_emp_sl", "gov_emp_share",
  "poverty_rate", "pct_ba", "pcpi", "unemp_rate", "pct_65plus",
  ## built 2026-11-12. Religion (rel_hhi, pct_adherent) and civic capacity (socap_sk2) entered
  ## BLOCKS as add-on blocks on 2026-11-16 (run v8); socap_assn_n (CBP membership
  ## establishments, mostly nonprofits) stays out: it counts the outcome. Urban economy: kitchen only.
  "rel_hhi", "pct_adherent", "socap_assn_n", "socap_sk2",
  "gdp_per_worker", "qcew_priv_wages", "patents_frac",
  ## historical endowment: 501(c)(3) public charities in the 2000 BMF stock
  ## ruled before 1970 (time-invariant; data/cov/cov_endowment.csv, built by
  ## 25_cov_endowment.R; group-exemption artifacts set to NA)
  "hist_pre1970",
  ## population-lag kernels (2026-11-15; data/cov/cov_lag_kernel.csv, built by
  ## 26_cov_lag_kernel.R): G[t] = a G[t-1] + dlogP[t], a = 0.5^(1/half-life), with
  ## 1970-2000 census history. Rates (type A): used as is, residualized on log P in the
  ## all-SAMI arm. (A 2.5-year Pascal pair was tried and dropped the same day: its two
  ## terms are collinear and split the effect; notes/catchup-lag-results.md 2.9.)
  "lag_g5", "lag_g10",
  ## census mail response rate (Rupasingha et al. waves 1997/2005/2009/2014, carried forward
  ## after 2014), the one social-capital component that does not count organizations or
  ## votes (added to BLOCKS 2026-11-16, run v8)
  "socap_respn",
  ## metro household-income Gini from summed county ACS brackets (B19001; data/cov/
  ## cov_gini_metro.csv, built by 27_cov_acs_metro.R, 2010-2024). The panel's
  ## gini is a population-weighted mean of county Ginis and misses between-county inequality.
  ## Congregations: RCMS count (2000/2010/2020, interpolated), extensive like foundation assets.
  ## Both added 2026-11-16 (run v9; notes/need-socap-diversity-results.md)
  "gini_m", "rel_cng",
  ## indirect vs direct government (Lecy & Van Slyke 2013; built 2026-11-16 by 22_cov_government.R
  ## and the CAINC35 lines in 12_cov_bea.R; notes/government-funding-measures.md). Dollar totals,
  ## extensive like foundation assets. BEA transfers to residents (annual 2000-2022, carried to
  ## 2024): Medicaid; Medicare + Medicaid + income maintenance. Census of Governments local
  ## spending (2002/07/12/17/22, interpolated): public welfare; direct general expenditure.
  ## Added in run v10 (93_gov_fork.R)
  "tr_medicaid", "tr_medmaint", "cog_welfare", "cog_dge" ),
  block = c( rep( "diversity", 5 ), rep( "philanthropy", 5 ), rep( "political", 2 ),
             rep( "government", 3 ), rep( "need", 5 ),
             "religion", "religion", "political", "civic",
             rep( "urban_economy", 3 ), "history", rep( "lag", 2 ), "civic",
             "inequality", "congregations",
             rep( "gov_indirect", 3 ), "gov_direct" ),
  type = c( "A", "A", "A", "A", "A", "B", "B", "B", "A", "A", "A", "A", "B", "B", "A",
            "A", "A", "A", "A", "A",
            "A", "A", "B", "A", "A", "B", "B", "B", "A", "A", "A",
            "A", "B",
            "B", "B", "B", "B" ) )

## default measure set per block, and one-at-a-time alternatives
BLOCKS <- list(
  ## default switched from race_hhi to pct_nonwhite on 2026-11-13 (D7 decision; runs
  ## through v4 used race_hhi). Fractionalization is non-monotone in composition: the
  ## border metros (>85% nonwhite) score as the least diverse, and those few metros
  ## drive its partial r (58_outline_analyses.R part U).
  diversity    = list( default = "pct_nonwhite",
                       alt = list( hhi = "race_hhi", foreign_born = "pct_foreign_born",
                                   gini = "gini", age_sd = "age_sd" ) ),
  philanthropy = list( default = "pf_assets",
                       alt = list( n_pf = "n_pf", contrib_total = "contrib_total",
                                   contrib_per_return = "contrib_per_return",
                                   contrib_share_agi = "contrib_share_agi" ) ),
  political    = list( default = "turnout",
                       alt = list( margin = "dem_margin", both = c( "turnout", "dem_margin" ) ) ),
  government   = list( default = "gov_earn_sl",
                       alt = list( gov_emp = "gov_emp_sl", gov_emp_share = "gov_emp_share" ) ),
  need         = list( default = c( "poverty_rate", "pct_ba" ),
                       alt = list( unemp = c( "unemp_rate", "pct_ba" ),
                                   plus_pcpi = c( "poverty_rate", "pct_ba", "pcpi" ),
                                   plus_age = c( "poverty_rate", "pct_ba", "pct_65plus" ) ) ),
  ## add-on block (no default): its alternative ADDS a covariate to the full
  ## default model. Time-invariant, so not run in the within stratum.
  history      = list( default = character( 0 ), alt = list( pre1970 = "hist_pre1970" ) ),
  ## add-on block (2026-11-15): density lags population change (notes/catchup-lag-results.md
  ## section 2.8). Each alternative adds a kernel of past population change to the full
  ## default model: half-life 5 (local projections) or 10 (level fit). Varies over time:
  ## run in every stratum.
  lag          = list( default = character( 0 ), alt = list( g5 = "lag_g5", g10 = "lag_g10" ) ),
  ## add-on blocks (2026-11-16, run v8; 85_civic_fork.R): civic capacity and religion, built
  ## since 11-12 but never in the grid. Each alternative adds one covariate to the full default
  ## model. civic:respn is census mail response (the clean civic measure); civic:sk2 is the
  ## Rupasingha index re-estimated without nonprofit density and turnout, which still contains
  ## association establishments, so it partly counts the outcome (read it as a check).
  ## religion:hhi is denominational concentration, religion:adherent the adherence rate.
  ## All four are observed only in census / survey waves (socap through 2014, RCMS 2000/2010/2020)
  ## and have almost no within-metro variance, so they are not run in the within stratum.
  civic        = list( default = character( 0 ), alt = list( respn = "socap_respn", sk2 = "socap_sk2" ) ),
  religion     = list( default = character( 0 ), alt = list( hhi = "rel_hhi", adherent = "pct_adherent" ) ),
  ## add-on blocks (2026-11-16, run v9; 92_ineq_cong_fork.R), from the measurement checks in
  ## 89_need_socap_diversity.R. inequality:gini adds the metro Gini to the full default model
  ## (poverty stays: Gini is positive given poverty and is not a need measure). It varies
  ## within metros (ACS 5-year), so it runs in every stratum. congregations:cong adds the
  ## congregation count (the one civic item that adds beyond turnout); RCMS waves only, so
  ## not in the within stratum.
  inequality    = list( default = character( 0 ), alt = list( gini = "gini_m" ) ),
  congregations = list( default = character( 0 ), alt = list( cong = "rel_cng" ) ),
  ## add-on blocks (2026-11-16, run v10; 93_gov_fork.R): government delivery mode. Each
  ## alternative adds one measure to the full default model, which keeps state and local
  ## government earnings (direct government). gov_indirect: government money that reaches
  ## residents through third parties -- Medicaid (mostly paid to providers, many of them
  ## nonprofits), Medicare + Medicaid + income maintenance, and local public-welfare spending.
  ## gov_direct:dge adds local direct general expenditure (CoG); given payroll, it reads as
  ## spending beyond in-house labor. (As a replacement for gov_earn_sl it would be a
  ## government-block alternative, which renumbers the base grid.) All vary within metros
  ## (within-metro share of log per-capita variance .17-.37 vs .04 for gov_earn_sl), so they
  ## run in every stratum.
  gov_indirect  = list( default = character( 0 ), alt = list( medicaid = "tr_medicaid", transfers = "tr_medmaint",
                                                              welfare = "cog_welfare" ) ),
  gov_direct    = list( default = character( 0 ), alt = list( dge = "cog_dge" ) ) )

## ---- lean default model (opt-in: Sys.setenv( MV_MODEL = "lean" ) before sourcing) ----
## The 13-covariate model of 94_variables_panel_model.R / paper/02-variables.qmd (2026-11-17):
## one item per construct, diversity and government split into the constructs they contain.
## Every block has a default, so there are no add-ons: drop-one-block covers "without the
## endowment" (an estimand choice) and the alternatives are one-at-a-time measurement swaps.
## The default grid (runs v1-v10) is unchanged when MV_MODEL is unset. Run v11 uses this.
## Note: religion enters as rel_hhi (concentration); fractionalization = 1 - rel_hhi, so its
## partial r has the opposite sign.
BLOCKS_LEAN <- list(
  need          = list( default = c( "poverty_rate", "pct_65plus" ),
                        alt = list( unemp = c( "unemp_rate", "pct_65plus" ),
                                    plus_pcpi = c( "poverty_rate", "pct_65plus", "pcpi" ) ) ),
  heterogeneity = list( default = c( "gini_m", "pct_nonwhite", "rel_hhi", "age_sd" ),
                        alt = list( race_hhi = c( "gini_m", "race_hhi", "rel_hhi", "age_sd" ),
                                    foreign_born = c( "gini_m", "pct_foreign_born", "rel_hhi", "age_sd" ) ) ),
  philanthropy  = list( default = "pf_assets",
                        alt = list( n_pf = "n_pf", contrib_total = "contrib_total",
                                    contrib_per_return = "contrib_per_return",
                                    contrib_share_agi = "contrib_share_agi" ) ),
  education     = list( default = "pct_ba", alt = list() ),
  civic         = list( default = "turnout", alt = list( respn = "socap_respn" ) ),
  growth        = list( default = "lag_g10", alt = list( g5 = "lag_g5" ) ),
  gov_direct    = list( default = "gov_earn_sl", alt = list( gov_emp = "gov_emp_sl", dge = "cog_dge" ) ),
  gov_indirect  = list( default = "tr_medicaid", alt = list( transfers = "tr_medmaint", welfare = "cog_welfare" ) ),
  history       = list( default = "hist_pre1970", alt = list() ) )
MV_MODEL <- Sys.getenv( "MV_MODEL", "default" )
if ( MV_MODEL == "lean" ) BLOCKS <- BLOCKS_LEAN
## time-invariant covariates leave within-metro models: in SAMI form they move within a
## metro only through log P (only reachable in the lean model, where history is a default)
TIME_INVARIANT_VARS <- "hist_pre1970"

## blocks with no default are add-ons: nothing to drop, and their specs are
## appended after the base grid so existing spec_ids do not change
ADDON_BLOCKS <- function() names( BLOCKS )[ vapply( BLOCKS, function( z ) length( z$default ) == 0, logical( 1 ) ) ]
## add-on blocks skipped in the within stratum: time-invariant (history) or with
## essentially no observed within-metro change (civic, religion, congregations)
TIME_INVARIANT_BLOCKS <- c( "history", "civic", "religion", "congregations" )

## the resource ("exp") construct carries a measure fork: expenses vs revenue,
## gross vs net of pass-through grants to other organizations (net series
## start in 2010; see 09_npo_grants.R and the revenue/expense research note)
OUTCOMES <- list(
  count = list( incl = c( "n_filer_c3", "n_filer_c3_hs", "n_bmf_c3pc" ),
                excl = c( "n_filer_c3_nohu" ) ),
  exp   = list( incl = c( "exp_c3", "exp_c3_net", "rev_c3", "rev_c3_net", "exp_c3_hs" ),
                excl = c( "exp_c3_nohu", "exp_c3_nohu_net", "rev_c3_nohu", "rev_c3_nohu_net" ) ) )

FORKS <- list(
  dv_form   = c( "pc", "logpc", "loglog", "sami", "pois_off", "pois_free" ),
  pred_form = c( "raw", "pc", "log", "sami" ),
  rhs_logP  = c( FALSE, TRUE ),
  unit      = c( "metro", "cbsa" ),
  ## "ols" reports two SEs from one fit: heteroskedasticity-robust (cs) or
  ## metro-clustered (panels), plus state-clustered. (Was two forks with
  ## identical coefficients.)
  ## div_fe = Census division FE (9 groups of states; added 2026-11-12 as a
  ## coarser alternative to state FE: fewer metros straddle a division line)
  est       = c( "ols", "state_fe", "state_re", "div_fe" ),
  cs_year   = c( 2022L, 2019L ),
  outlier   = c( "none", "winsor1" ) )

IDENT <- list( cs = "single-year cross-section", pooled = "pooled 2015-2022, year FE",
               within = "two-step within: year-specific SAMIs, metro + year FE, 2010-2022" )

## ---- grid -------------------------------------------------------------------

op_variants <- function() {
  v <- data.table( op = "default", op_block = NA_character_ )
  for ( b in names( BLOCKS ) ) for ( a in names( BLOCKS[[ b ]]$alt ) )
    v <- rbind( v, data.table( op = paste0( b, ":", a ), op_block = b ) )
  v
}
block_sets <- function() {
  b <- setdiff( names( BLOCKS ), ADDON_BLOCKS() )
  data.table( drop = c( "none", b ) )
}

## universe = "principled": the analysis curve (trim rules below)
## universe = "kitchen":    full cross product of every fork
## In both, rows with dup_of != NA are algebraic duplicates (identical
## coefficients on the covariates); they are excluded from the curve and a
## sample is run only as a verification check (FWL).
build_grid <- function( universe = c( "principled", "kitchen" ) ) {
  universe <- match.arg( universe )
  ## strata
  st <- rbindlist( lapply( names( OUTCOMES ), function( k )
          rbindlist( lapply( names( OUTCOMES[[ k ]] ), function( h )
            data.table( construct = k, hu = h, outcome = OUTCOMES[[ k ]][[ h ]] ) ) ) ) )
  ## principled: subsector outcomes (human services) are a separate extension
  if ( universe == "principled" ) st <- st[ !grepl( "_hs$", outcome ) ]
  st <- st[ , .( ident = names( IDENT ) ), by = .( construct, hu, outcome ) ]

  st[ , k := 1L ]
  ## one grid per set of covariate variants; add-on variants are built
  ## separately and appended, so adding an add-on block never renumbers the
  ## existing specifications
  make <- function( ov ) {
    if ( !nrow( ov ) ) return( NULL )
    g <- CJ( dv_form = FORKS$dv_form, pred_form = FORKS$pred_form, rhs_logP = FORKS$rhs_logP,
             unit = FORKS$unit, est = FORKS$est, cs_year = FORKS$cs_year, outlier = FORKS$outlier,
             drop = block_sets()$drop, op = ov$op )
    g <- merge( g, ov, by = "op" )
    g <- g[ is.na( op_block ) | op_block != drop ]                     # can't vary a dropped block
    ## principled: covariate forks one at a time -- alternative measures only in
    ## the full model; leave-one-block-out only with default measures
    if ( universe == "principled" ) g <- g[ drop == "none" | op == "default" ]
    g <- g[ !( dv_form %in% c( "loglog", "pois_free" ) & rhs_logP == FALSE ) ]
    g <- g[ !( dv_form %in% c( "pois_off", "pois_free" ) & est == "state_re" ) ]
    g <- unique( g )
    g[ , k := 1L ]
    full <- merge( st, g, by = "k", allow.cartesian = TRUE )[ , k := NULL ]
    ## identification-specific restrictions
    full <- full[ !( ident != "cs" & cs_year != 2022L ) ]               # year fork only in cs
    full <- full[ !( ident == "within" & est != "ols" ) ]              # within: metro + year FE only
    ## time-invariant add-ons are absorbed by metro FE
    full[ !( ident == "within" & op_block %in% TIME_INVARIANT_BLOCKS ) ]
  }
  ov <- op_variants()
  addon <- ov$op_block %in% ADDON_BLOCKS()
  ## each add-on block is built and appended on its own, in BLOCKS order, so a new
  ## add-on block never interleaves with (and renumbers) an earlier one
  full <- rbind( make( ov[ !addon ] ),
                 rbindlist( lapply( ADDON_BLOCKS(), function( b ) make( ov[ op_block %in% b ] ) ) ) )

  ## algebraic duplicates (exact; identical covariate coefficients)
  full[ , logP_rhs := rhs_logP | dv_form %in% c( "loglog", "pois_free" ) ]
  full[ , dup_of := NA_character_ ]
  ## log(Y/P) + log P  ==  log Y + log P  (log P coefficient shifts by 1)
  full[ outlier == "none" & dv_form == "logpc" & rhs_logP == TRUE, dup_of := "loglog" ]
  ## Poisson offset(log P) + free log P  ==  Poisson with free log P
  full[ outlier == "none" & dv_form == "pois_off" & rhs_logP == TRUE, dup_of := "pois_free" ]
  ## cross-section only (single first stage, no year-specific slopes):
  ## SAMI_Y + log P  ==  log Y + log P   (SAMI note, models 2 = 3)
  full[ is.na( dup_of ) & outlier == "none" & ident == "cs" & dv_form == "sami" & rhs_logP == TRUE, dup_of := "loglog" ]
  ## residualized predictors alongside log P == logged/level predictors (FWL)
  full[ is.na( dup_of ) & outlier == "none" & ident == "cs" & pred_form == "sami" & logP_rhs == TRUE, dup_of := "pred_log" ]
  full[ , logP_rhs := NULL ]
  full[ , spec_id := .I ]
  full[]
}

## ---- data ---------------------------------------------------------------------

## Census divisions by state FIPS (for the div_fe estimator)
DIVISION <- c( "09" = 1, "23" = 1, "25" = 1, "33" = 1, "44" = 1, "50" = 1, "34" = 2, "36" = 2, "42" = 2,
  "17" = 3, "18" = 3, "26" = 3, "39" = 3, "55" = 3, "19" = 4, "20" = 4, "27" = 4, "29" = 4,
  "31" = 4, "38" = 4, "46" = 4, "10" = 5, "11" = 5, "12" = 5, "13" = 5, "24" = 5, "37" = 5,
  "45" = 5, "51" = 5, "54" = 5, "01" = 6, "21" = 6, "28" = 6, "47" = 6, "05" = 7, "22" = 7,
  "40" = 7, "48" = 7, "04" = 8, "08" = 8, "16" = 8, "30" = 8, "32" = 8, "35" = 8, "49" = 8,
  "56" = 8, "02" = 9, "06" = 9, "15" = 9, "41" = 9, "53" = 9 )

load_panel <- function( path = file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) {
  d <- as.data.table( arrow::read_parquet( path ) )
  d[ , logP := log( population ) ]
  d[ , division := unname( DIVISION[ state_fips ] ) ]
  ## hist_pre1970, lag_g5 / lag_g10 and gini_m are merged by 30_assemble_analysis.R
  ## (since Phase 2 of the reproducibility plan; before, they were attached here)
  miss <- setdiff( c( "hist_pre1970", "lag_g5", "lag_g10", "gini_m" ), names( d ) )
  if ( length( miss ) ) stop( basename( path ), " lacks ", paste( miss, collapse = ", " ),
                              ": rebuild it with R/build/E_assemble/30_assemble_analysis.R" )
  d[]
}

## winsor(), resid_logP() (the SAMI first stage), slog() and flog() (log with a
## per-capita floor, run v6 on) are in the package: ?npdensity::transforms.

covars_for <- function( s ) {
  blocks <- setdiff( names( BLOCKS ), s$drop )
  unlist( lapply( blocks, function( b ) {
    if ( !is.na( s$op_block ) && s$op_block == b ) BLOCKS[[ b ]]$alt[[ sub( "^[^:]+:", "", s$op ) ]]
    else BLOCKS[[ b ]]$default } ) )
}

## ---- one specification ---------------------------------------------------------

run_spec <- function( s, panel ) {
  s <- as.list( s )
  xs <- covars_for( s )
  if ( s$ident %in% c( "within", "within_long" ) ) xs <- setdiff( xs, TIME_INVARIANT_VARS )
  ## within_long (2000-2022) is opt-in: not in IDENT, run by 57_run_within_long.R
  yrs <- switch( s$ident, cs = s$cs_year, pooled = 2015:2022, within = 2010:2022, within_long = 2000:2022 )
  d <- panel[ year %in% yrs ]
  if ( s$unit == "metro" ) d <- d[ cbsa_type == "metro" ]
  if ( !"division" %in% names( d ) ) d[ , division := unname( DIVISION[ state_fips ] ) ]
  d <- d[ , c( "cbsa", "year", "state_fips", "division", "logP", "population", s$outcome, xs ), with = FALSE ]
  d <- d[ complete.cases( d ) & population > 0 ]
  y <- d[[ s$outcome ]]

  ## dependent variable
  if ( s$dv_form %in% c( "logpc", "loglog", "sami" ) ) { keep <- y > 0; d <- d[ keep ]; y <- y[ keep ] }
  yv <- switch( s$dv_form,
    pc        = y / d$population,
    logpc     = log( y / d$population ),
    loglog    = log( y ),
    sami      = resid_logP( log( y ), d$logP, d$year ),
    pois_off  = y,
    pois_free = y )
  if ( s$outlier == "winsor1" && !s$dv_form %in% c( "pois_off", "pois_free" ) ) yv <- winsor( yv )

  ## predictors
  X <- list()
  for ( v in xs ) {
    x <- d[[ v ]]; ty <- COVS[ var == v, type ]
    xv <- if ( ty == "B" ) switch( s$pred_form,
            raw = x, pc = x / d$population, log = flog( x, d$population ),
            sami = resid_logP( flog( x, d$population ), d$logP, d$year ) )
          else if ( s$pred_form == "sami" ) resid_logP( x, d$logP, d$year ) else x
    if ( s$outlier == "winsor1" ) xv <- winsor( xv )
    X[[ v ]] <- xv
  }
  m <- data.table( y = yv, as.data.table( X ), logP = d$logP, cbsa = d$cbsa, year = d$year,
                   state = d$state_fips, division = d$division, lpop = log( d$population ) )
  m <- m[ is.finite( y ) & Reduce( `&`, lapply( xs, function( v ) is.finite( m[[ v ]] ) ) ) ]
  if ( nrow( m ) < 30 ) return( NULL )

  ## numerical conditioning: fit on unit-SD predictors, report in original
  ## units (estimate / scale, se / scale). Raw dollar totals (~1e9) otherwise
  ## make variance estimates unstable; t and p are unchanged by scaling.
  sc <- vapply( xs, function( v ) { s0 <- sd( m[[ v ]] ); if ( is.finite( s0 ) && s0 > 0 ) s0 else 1 },
                numeric( 1 ) )
  for ( v in xs ) set( m, j = v, value = m[[ v ]] / sc[[ v ]] )

  use_logP <- s$rhs_logP || s$dv_form %in% c( "loglog", "pois_free" )
  rhs <- paste( c( xs, if ( use_logP ) "logP" ), collapse = " + " )
  fe <- switch( s$ident,
    cs     = switch( s$est, state_fe = "state", div_fe = "division", "0" ),
    pooled = switch( s$est, state_fe = "state + year", div_fe = "division + year", "year" ),
    within = "cbsa + year", within_long = "cbsa + year" )
  ## primary SE: robust (cs) / metro-clustered (panels) / state-clustered (state
  ## and division FE: 9 divisions are too few clusters); every fixest fit also
  ## reports state-clustered SEs (se_state, p_state)
  vc <- switch( s$ident,
    cs     = switch( s$est, ols = "hetero", state_fe = ~state, div_fe = ~state, state_re = NA ),
    pooled = switch( s$est, ols = ~cbsa, state_fe = ~state, div_fe = ~state, state_re = NA ),
    within = ~cbsa, within_long = ~cbsa )

  f <- as.formula( paste( "y ~", rhs, "|", fe ) )
  fit <- tryCatch( {
    if ( s$est == "state_re" ) {
      fr <- as.formula( paste( "y ~", rhs, if ( s$ident == "pooled" ) "+ factor(year)", "+ (1|state)" ) )
      r <- lmer( fr, data = m, REML = FALSE )
      cf <- summary( r )$coefficients
      data.table( term = rownames( cf ), estimate = cf[ , 1 ], se = cf[ , 2 ], stat = cf[ , 3 ],
                  p = 2 * pnorm( -abs( cf[ , 3 ] ) ), se_state = NA_real_, p_state = NA_real_,
                  r2 = NA_real_, n = nrow( m ) )
    } else {
      r <- if ( s$dv_form == "pois_off" ) fepois( f, data = m, offset = ~lpop, vcov = vc )
           else if ( s$dv_form == "pois_free" ) fepois( f, data = m, vcov = vc )
           else feols( f, data = m, vcov = vc )
      ct <- coeftable( r )
      cs2 <- tryCatch( coeftable( summary( r, vcov = ~state ) ), error = function( e ) ct * NA )
      data.table( term = rownames( ct ), estimate = ct[ , 1 ], se = ct[ , 2 ], stat = ct[ , 3 ],
                  p = ct[ , 4 ], se_state = cs2[ rownames( ct ), 2 ], p_state = cs2[ rownames( ct ), 4 ],
                  r2 = tryCatch( as.numeric( r2( r, if ( inherits( r, "fixest" ) && r$method == "fepois" ) "pr2" else "r2" ) ),
                                 error = function( e ) NA_real_ ),
                  n = nobs( r ) )
    }
  }, error = function( e ) NULL )
  if ( is.null( fit ) ) return( NULL )
  fit <- fit[ term %in% c( xs, "logP" ) ]
  ## Poisson coefficients are semi-elasticities: scale by the dispersion of
  ## the log outcome so standardized betas are comparable with log DV forms
  sdy <- switch( s$dv_form,
    pois_off  = sd( log( pmax( m$y, 1e-9 ) ) - m$lpop ),
    pois_free = sd( log( pmax( m$y, 1e-9 ) ) ),
    sd( m$y ) )
  ## std beta from the scaled fit (predictor SD = 1), then back to original units
  fit[ , std_beta := estimate * vapply( term, function( v ) sd( m[[ v ]] ), numeric( 1 ) ) / sdy ]
  k <- ifelse( fit$term %in% xs, sc[ fit$term ], 1 )
  fit[ , `:=`( estimate = estimate / k, se = se / k, se_state = se_state / k ) ]
  fit[ , spec_id := s$spec_id ]
  fit[]
}
