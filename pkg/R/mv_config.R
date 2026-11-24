#' Multiverse specification: configuration
#'
#' `mv_spec_config()` returns the configuration the grid and the model engine read: the
#' covariate catalog, the covariate blocks, the outcome strata, the forks and the
#' identification strategies. Two models are defined:
#'
#' * `"default"`: the block set of runs v1–v10 (five base blocks, the rest add-on blocks
#'   that only add a covariate to the full default model).
#' * `"lean"`: the 13-covariate model of run v11 (`paper/02-variables.qmd`). Every block has
#'   a default, so there are no add-ons.
#'
#' The configuration is a plain list; change it (e.g. add an alternative measure to a block)
#' and pass it to [mv_grid()] and [mv_run_spec()]. Elements:
#' `covs` (data.table: `var`, `block`, `type`; type A = intensive, B = extensive),
#' `blocks` (per block: `default` measures and named `alt` alternatives), `outcomes`,
#' `forks`, `ident`, `time_invariant_vars` (dropped in within models) and
#' `time_invariant_blocks` (add-on blocks not run in the within stratum).
#'
#' `DIVISION` maps state FIPS codes to the nine Census divisions (the `div_fe` estimator).
#'
#' @param model `"default"` or `"lean"`.
#' @return A list.
#' @examples
#' cfg <- mv_spec_config( "lean" )
#' names( cfg$blocks )
#' @export
mv_spec_config <- function( model = c( "default", "lean" ) ) {
  model <- match.arg( model )
  list( model = model, covs = MV_COVS,
        blocks = if ( model == "lean" ) MV_BLOCKS_LEAN else MV_BLOCKS_DEFAULT,
        outcomes = MV_OUTCOMES, forks = MV_FORKS, ident = MV_IDENT,
        time_invariant_vars = "hist_pre1970",
        time_invariant_blocks = c( "history", "civic", "religion", "congregations" ) )
}

## ---- covariate catalog: block, measure, predictor type ------------------------
## type A = intensive (level; residualized on log P in the all-SAMI arm)
## type B = extensive (raw | per capita | log | SAMI)

MV_COVS <- data.table( var = c(
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
MV_BLOCKS_DEFAULT <- list(
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

## ---- lean default model (model = "lean") ----
## The 13-covariate model of 94_variables_panel_model.R / paper/02-variables.qmd (2026-11-17):
## one item per construct, diversity and government split into the constructs they contain.
## Every block has a default, so there are no add-ons: drop-one-block covers "without the
## endowment" (an estimand choice) and the alternatives are one-at-a-time measurement swaps.
## The default grid (runs v1-v10) is model = "default". Run v11 uses this one.
## Note: religion enters as rel_hhi (concentration); fractionalization = 1 - rel_hhi, so its
## partial r has the opposite sign.
MV_BLOCKS_LEAN <- list(
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

## the resource ("exp") construct carries a measure fork: expenses vs revenue,
## gross vs net of pass-through grants to other organizations (net series
## start in 2010; see 09_npo_grants.R and the revenue/expense research note)
MV_OUTCOMES <- list(
  count = list( incl = c( "n_filer_c3", "n_filer_c3_hs", "n_bmf_c3pc" ),
                excl = c( "n_filer_c3_nohu" ) ),
  exp   = list( incl = c( "exp_c3", "exp_c3_net", "rev_c3", "rev_c3_net", "exp_c3_hs" ),
                excl = c( "exp_c3_nohu", "exp_c3_nohu_net", "rev_c3_nohu", "rev_c3_nohu_net" ) ) )

MV_FORKS <- list(
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

MV_IDENT <- list( cs = "single-year cross-section", pooled = "pooled 2015-2022, year FE",
               within = "two-step within: year-specific SAMIs, metro + year FE, 2010-2022" )

## Census divisions by state FIPS (for the div_fe estimator)
#' @rdname mv_spec_config
#' @format NULL
#' @export
DIVISION <- c( "09" = 1, "23" = 1, "25" = 1, "33" = 1, "44" = 1, "50" = 1, "34" = 2, "36" = 2, "42" = 2,
  "17" = 3, "18" = 3, "26" = 3, "39" = 3, "55" = 3, "19" = 4, "20" = 4, "27" = 4, "29" = 4,
  "31" = 4, "38" = 4, "46" = 4, "10" = 5, "11" = 5, "12" = 5, "13" = 5, "24" = 5, "37" = 5,
  "45" = 5, "51" = 5, "54" = 5, "01" = 6, "21" = 6, "28" = 6, "47" = 6, "05" = 7, "22" = 7,
  "40" = 7, "48" = 7, "04" = 8, "08" = 8, "16" = 8, "30" = 8, "32" = 8, "35" = 8, "49" = 8,
  "56" = 8, "02" = 9, "06" = 9, "15" = 9, "41" = 9, "53" = 9 )
