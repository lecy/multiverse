# Multiverse data build

Scripts live here (Dropbox). Every build product, download cache, temp file, and
log lives on **D:/MULTIVERSE** (C: is nearly full). Read-only sources stay in the
Dropbox data hub (`00 - DATA`). Run each script from the `multiverse/` folder:

    "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" R/<script>.R > D:/MULTIVERSE/logs/<script>.log 2>&1

| Order | Script | Builds | Output (D:/MULTIVERSE/…) |
|---|---|---|---|
| 0 | `00_setup.R`, `geo_helpers.R` | paths, constants, geography helpers | — |
| — | `md2qmd.R <note>.md [--render] [--subtitle="..."]` | converts a GitHub-markdown results note in `notes/` to a Quarto `.qmd` beside it (title to YAML, "How to read" blockquote to a callout, the blank lines Pandoc needs before/after lists and tables), optionally renders self-contained HTML. The `.md` stays the source; the `.qmd` is overwritten each run. E.g. `Rscript R/md2qmd.R notes/catchup-lag-results.md --render --subtitle="Catch-up results note"` | `notes/<note>.qmd`, `.html` |
| 1 | `01_geo_spine.R` | 2023 county-equivalent spine (CT planning regions), annual PEP population 2000–2025, CBSA crosswalk | `data/geo`, `data/pop` |
| 2 | `02_npo_bmf_counts.R` | registered-org counts from NCCS BMF snapshots; EIN × year locations (FIPS → other-year FIPS → ZIP modal) | `data/npo/bmf_county_year.csv`, `ein_location_bmf.csv.gz` |
| 3 | `03_npo_core.R` | harmonized CORE 2000–2019 (PZ, CE, PF) | `raw/nccs-core-hrmn`, `data/npo/orgyear_core.parquet` |
| 3b | `03b_npo_pf_soi.R` | IRS SOI 990-PF extracts | `data/npo/orgyear_soi_pf.parquet` |
| 4 | `04_npo_soi.R` | SOI extract panel 2011–2023 | `data/npo/orgyear_soi.parquet` |
| 5 | `05_npo_efile.R [years]` | e-file 990/EZ/PF 2020–2024 via panel990 | `raw/panel990`, `data/npo/orgyear_efile*.parquet` |
| 10 | `10_source_coverage.R` | tables for `DATA-MANIFEST.md` (run after 06): national frame totals by year, records by return type (CORE codes 93 = 990, 92 = EZ; no type before TY2008), share of each year's records e-filed (EIN × period-end match to `efile_v3_1` TY2009–24 headers, cached in `raw/panel990`) | `data/diagnostics/manifest_*.csv` |
| 2b | `02b_npo_bmf_monthly.R` | 2023–24 counts from NCCS monthly BMF (Dec files); **reconstructed registered-org stock 2000–2024** (first/last appearance + ruling year). Invalid NTEE (UNDEFINED/INVALID/Z99/blank, ~25% of monthly-file public charities) backfilled from the EIN's latest valid legacy-snapshot code since 2026-11-17 (same rule as 06; `diagnostics/bmf_monthly_ntee_backfill.csv`, `bmf_ntee_backfill_effect.csv`; restored 2023 `n_bmf_c3pc_hs` from 24.8% to 27.8% of the stock; old outputs in `data/_pre_bmfntee_20261117/`) | `data/npo/bmf_county_year_{monthly,recon}.csv` |
| 7 | `07_npo_bls.R` | BLS QCEW nonprofit MSA data 2013–2022 (employment, payroll, market share, composition-adjusted wage ratio); 990-vs-BLS validation | `data/cov/cbsa/cbsa_bls.csv`, `diagnostics/bls_vs_990*.csv` |
| 8 | `08_npo_entry.R` | new 501(c)(3) registrations by ruling year (Long BMF panel 2000–2021; Dec-2024 BMF 2022–24) | `data/npo/npo_entry_county_year.csv` |
| 9a, 9 | `09a_efile_grants.R`, `09_npo_grants.R` | grants paid (Part IX lines 1–3; EZ line 10), CY/PY totals | `data/npo/orgyear_grants.parquet` |
| 9b | `09b_revexp_analysis.R` | research note: revenue vs expense volatility, pass-through grants, scaling bias | `data/aux_revexp/` |
| 6 | `06_npo_assemble.R` | locate, floor, tabulate, splice, overlap diagnostics; net-of-grants resources (2012+); invalid NTEE codes (e-file "UND", ~16% of filers 2020–24) backfilled from the BMF since 2026-11-17 (`diagnostics/ntee_backfill.csv`; moved 2020+ `*_nohu` spending ~7% and `*_hs` +13%; run v10 predates it) | `data/npo/npo_{county,cbsa}_year.csv`, `data/diagnostics/splice_overlap.csv` |
| 11–17 | `1x_cov_*.R` | covariates: PEP race/age, BEA, SAIPE, LAUS, IRS SOI county, elections, ACS | `data/cov/cov_*.csv` (+ codebooks) |
| 18 | `18_cov_rcms.R` | religion: congregations, adherents, Catholic share, denominational HHI (2000/2010/2020) | `data/cov/cov_rcms.csv`, `cov/cbsa/cbsa_rcms.csv` |
| 19 | `19_cov_socap.R` | Rupasingha social capital 1997–2014; index re-estimated without nonprofit density and turnout | `data/cov/cov_socap.csv` |
| 20 | `20_urban_economy.R [qcew\|bench]` | QCEW private totals 2000–2024; scaling benchmarks, SAMI-on-SAMI, persistence | `data/cov/cov_qcew.csv`, `diagnostics/scaling_benchmarks*.csv`, `sami_*.csv` |
| 21 | `21_patents.R` | patents by inventor county (needs PatentsView files; see header) | `data/cov/cov_patents.csv` |
| 22 | `22_cov_government.R` | direct vs indirect government (`notes/government-funding-measures.md`): QCEW federal/state/local government jobs and wages 2000–2024 (suppressed county cells = state residual allocated by population; the 2000 file omits them); ACS class of worker 2009–2024; Census of Governments local finance 2002/07/12/17/22 (county-area files to 2012, unit files 2017–22; NYC split across boroughs by population). BEA transfer receipts (CAINC35, `tr_*`, 2000–2022) are read by `12_cov_bea.R` | `data/cov/cov_gov.csv`, `diagnostics/cog_*.csv` |
| 23 | `23_cov_compete.R` | market competition / niche quality (after npcompete, recomputed): HHI, normalized HHI, n·HHI, CR1/CR4, Gini, small-org shares, subsector (metro × NTEE major group) HHI means and cross-subsector mix, of revenue and contributions, for c3 / nohu / `local` (community-serving: no health, Q, T, U, B11, uncoded) active filers, 2000–2024. Backfills e-file "UND" NTEE codes from the BMF. Run after 06, before 30. See `notes/market-competition-measures.md` (use `local` nHHI; HHI/CR/Gini are mechanically tied to n) | `data/cov/cbsa/cbsa_compete.csv` (69 `mkt_*` vars), `data/compete/` |
| 60 | `60_metro_reports.R` | 50-metro sidecar reports + variable spaghetti charts | `reports/metro_reports/index.html` |
| 30 | `30_assemble_analysis.R` | county and CBSA analysis panels with derived variables and gap-filled periodic covariates | `data/analysis/{county,cbsa}_year.parquet` |
| 40 | `40_multiverse.R` | fork configuration, grid builder, model engine (sourced). `Sys.setenv( MV_MODEL = "lean" )` before sourcing switches BLOCKS to `BLOCKS_LEAN`, the 13-covariate default of `paper/02-variables.qmd` (no add-on blocks; drop-one-block and one-at-a-time measurement swaps; time-invariant `hist_pre1970` leaves within models). Unset = the six-covariate grid of runs v1–v10, unchanged. Run v11 = `MV_MODEL=lean` + `44_run_universe.R principled 2000 14 principled_v11` (432,432 specs, 87 min, all fit; 2026-11-17) | — |
| 41 | `41_composites.R` | opt-in composite-index fork: 8 additive indices (signed z-score means, fixed weights) as 9 extra `BLOCKS$alt` options; sourced after 40 by script 53 only, so the default grid is unchanged | — |
| 57 | `57_run_within_long.R [test\|run\|summary]` | within-metro specs re-run on 2000–2022 (`ident = "within_long"`, opt-in; skips specs whose data start 2010/2012) and paired with their 2010–2022 twins | `data/spec/run_principled_wl/` |
| 71 | `71_fe_frequency.R` | annual FE vs 5-year waves vs long differences; change-variance diagnostics (signal vs noise by horizon) | `data/collinearity/p4_*` |
| 72 | `72_catchup_lag.R [parts]` | nonprofit growth vs population growth for filers, grassroots (registered minus filers) and expenditures (parts 1–8): horizon elasticities, local-projection impulse responses (half-life of the growth deficit), fast-grower trajectories, predictors of catch-up 2011–23 (Shapley R² by block), growth vs decline and income-loss asymmetry, entries/exits/filing share, BLS market share and wage premium (ceiling test); see `notes/catchup-lag-results.md` (figures in `notes/catchup-figures/`) | `data/catchup/` |
| 73 | `73_pre2000_growth.R [grass]` | is the Sunbelt density gap an old growth lag? 1970/1980/1990 census county counts (Census historical tables, downloaded to `raw/census-hist/`; CT metros dropped) mapped to 2023 CBSAs; 2000 SAMI on decade growth vs the within-metro catch-up rates; convergence 2000–24 where growth slowed | `data/catchup/c9_*` |
| 72/73 `grass` | `72_catchup_lag.R grass`, `73_pre2000_growth.R grass` | robustness: filers / grassroots / pooled registered stock (splice-free) | `data/catchup_grass/` |
| 75 | `75_capacity_grid.R` | "carrying capacity": 1970–2000 growth × BLS nonprofit employment share grid of mean SAMI (2013–22) and of SAMI change; scatter, loess surface, R² decomposition, metros above/below the growth × share expectation | `data/catchup/c10_*` |
| 76 | `76_bls_share_change.R` | regional profile of nonprofit jobs per capita and within-industry nonprofit shares (part 6); how the BLS nonprofit employment share moves within metros (2013–22) and an exact decomposition of the change into establishment-count vs employees-per-establishment gaps (nonprofit vs for-profit); national trends; relation to SAMI change | `data/catchup/c11_*` |
| 77 | `77_nonprofit_age.R` | nonprofit age (2022 BMF ruling year) by state, region and metro for all registered, filers and grassroots; dollar-weighted age of filers; age vs BLS market share; age vs size (log expenditures) | `data/catchup/c12_*` |
| 78 | `78_nonprofit_lifespan.R` | lifespan of public charities ruled 1990-2021 (ruling year to last 990 filing for filers, to last BMF year for non-filers) by region: Kaplan-Meier with delayed entry and censoring, restricted mean and median lifespan with 95% CIs, Cox hazard ratios | `data/catchup/c13_*` |
| 79 | `79_kernel_lag.R` | distributed-lag (partial-adjustment) models of density on population: log N on log P plus geometric, Pascal (hump), two-rate and gain/loss kernels of past population change, half-lives profiled on a grid with metro bootstrap CIs; checks against the local projections, free growth windows, single-lag specs and metro trends. Arg: bootstrap draws (default 300) | `data/catchup/c14_*` |
| 82 | `82_lag_fork.R [build\|test\|run\|merge\|compare]` | population-lag fork (add-on block `lag`): builds kernels of past log population change for all 925 CBSAs from 1970–2024 population (census county counts 1970/1980/1990; Connecticut uses statewide growth), runs the 36,168 lag specs, merges v6 + lag into v7, compares lag terms and covariate shifts against matched default specs | `data/cov/cov_lag_kernel.csv`, `data/spec/run_principled_{lag,v7}/` |
| 80 | `80_interaction_screen.R [slog]` | interaction screen for the five benchmark covariates: pairwise, x², x × log P one at a time (2022 matrix), joint Wald + CV, stability over a 108/72-spec mini-multiverse, with and without Eagle Pass TX (zero foundation assets); see `notes/interaction-screen-results.md` | `data/interactions/` |
| 81 | `81_compare_floor.R [old] [new]` | v5 (log(x + 1)) vs v6 (per-capita floor): same-spec agreement by term, unit, predictor form; five-benchmark summary in both runs; foundation-asset shift by fork | `data/spec/compare_v5_v6_*.csv` |
| 74 | `74_scaling_aggregation.R`, `74b_plot_scaling_aggregation.R` | scaling exponent B by aggregation (county, metro CBSA, all CBSAs, CSA, CSA-else-metro), full sector + NCCS 12 groups, counts and expenses, OLS and Poisson, e-file 2021–23; county models with metro FE (within-metro B, variance left); see `notes/scaling-aggregation-results.md` | `data/scaling_agg/` |
| 53 | `53_run_composites.R [test\|run\|merge]` | runs the composite specs (162,756) and merges them with a base run by fork settings | `data/spec/run_principled_cmp{,_merged}/` |
| 42 | `42_timing_test.R` | universe sizes + 1,000-spec timing test | `data/spec/` |
| 43 | `43_verify_duplicates.R` | checks that trimmed algebraic duplicates reproduce their twins | `data/spec/verify_duplicates.csv` |
| 45 | `45_summarize_run.R <run>` (e.g. `principled_v7`) | design-plan tests for a finished run: sign stability, share significant, marginal η² of each fork, median partial r by DV form, Spearman of t-statistics across DV forms, meta-hypothesis (R² of each covariate on log P vs its DV-form and predictor-form sensitivity). Since 2026-11-15 partial r counts df in places (pooled ÷ 8, within ÷ 13), drops pooled state-RE fits and uses `flog()`, as 58 part A does | `data/spec/run_<run>/summary_*.csv` |
| 83 | `83_sample_exclusions.R [workers]` | are sample exclusions worth a fork? (with `MV_MODEL=lean`: 13-covariate default, all default terms summarized, outputs to `sample_excl_v11/`) 1,470 default-model specs re-fit without DC, AK/HI, the group-exemption metros (Memphis, Mankato), the 5 largest and 20 smallest, covariate outliers (\|z\| > 4), and above 50k / 100k / 250k population; winsor1 twins as the benchmark; paired \|Δr\| and η² | `data/sample_excl/` |
| 86 | `86_sensitivity_tcja_size.R [A] [B] [workers]` | sensitivity checks reported instead of forked. A: every contribution spec (34,914) and its default twin re-fit on a pre-2018 window (cs 2017, pooled 2015–17, within 2010–17); law effect = change in the contribution spec minus change in the twin. B: 567 default-model metro specs fit separately on metros < 250k and >= 250k ; with `MV_MODEL=lean` part A runs on the v11 grid with all 13 default terms as benchmarks (outputs `data/sensitivity_v11/`) | `data/sensitivity/` |
| 87 | `87_poverty_size_figure.R [fit] [workers]` | figure for the poverty x size heterogeneity (D2 Evidence 6). Main figure (`fig_poverty_size`): top = component-plus-residual plot of filer SAMI vs poverty on all 387 metros (spec 5704, lowess overall and per size bin), bottom = B. Bin figure (`fig_poverty_size_bins`): top = metros split at 250k and each half at its median (four bins of 96–97); default-model metro specs (OLS and division FE) re-fit in each bin (cached in `c_bins.csv`, `fit` re-runs; an earlier rolling-window version is in `c_rolling.csv`). B: added-variable plot for the filer spec nearest both size-group medians, fit within each group; checked against run_spec. Coefficient figure (`fig_poverty_size_coef`): top = poverty coefficient of the same spec for filers, grassroots (registered, not filing) and spending, re-fit with Gaussian kernel weights in log population (bandwidth 0.5, curves to 2.5M) with the 250k split fits as dashed lines; bottom = full-sample added-variable plot with lowess and the split slopes. Window + coefficient figure (`fig_poverty_size_window_coef`): A = rolling-window partial r (cs 2022) for filers, grassroots (fit here, `c_rolling_grass.csv`) and spending; B = poverty coefficient per 10 points with 95% CI in the four size bins (halves and all-metro estimates also in `c_group_coef.csv`); C = the split added-variable plots | `data/sensitivity/fig_poverty_size{,_bins,_coef,_window_coef}.{png,pdf}` and the `c_*.csv` tables; copies in `notes/` |
| 88 | `88_covariate_size_figures.R [fit] [workers]` | the 87 window + coefficient figure for poverty (reproduces 87 exactly), % BA and foundation assets: A = rolling-window partial r (cs 2022; filers, grassroots, spending; `c_rolling_terms.csv`), B = coefficient with 95% CI in four size bins, C = split added-variable plots for filers (grassroots for foundation assets). Spec per covariate nearest both 250k-half medians (`c_rep_specs.csv`); foundation assets use the SAMI predictor form because no spec is near both medians (the size pattern depends on the predictor form) | `data/sensitivity/fig_<term>_size_window_coef.{png,pdf}`, `c_group_coef_terms.csv`; copies in `notes/` |
| 91 | `91_government_measures.R` | first look at the government measures: scaling exponents, correlations, and the benchmark model (2012/2022, metros/all CBSAs, OLS/state FE) with each direct, indirect and mode measure in place of S&L earnings, alone and in pairs | `data/government/` |
| 90 | `90_size_moderation_screen.R [workers]` | curved size moderation (with `MV_MODEL=lean`: on the 13-covariate default, outputs to `size_moderation_v11/`): each benchmark gets x × c and x × c² (c = centred log population) in all 3,636 default-model cs and pooled specs (filers, grassroots, spending; run_spec's design, state RE and within left out); joint Wald p, squared-term t/p, and t of the marginal effect at 100k / 250k / 1M / 4M | `data/interactions/size_moderation/` (`screen_specs.csv`, summaries, `fig_size_moderation.png`; copy in `notes/`) |
| 92 | `92_ineq_cong_fork.R [test\|run\|merge\|compare]` | inequality and congregations add-on blocks (metro Gini `gini_m` added alongside poverty, every stratum; RCMS congregation count `rel_cng`, type B, no within): 34,584 specs, v8 + new → v9, added-term stability and how the default covariates move | `data/spec/run_principled_{ineq_cong,v9}/` (`ineq_cong_{term,shift,correlations}.csv`) |
| 93 | `93_gov_fork.R [test\|run\|merge\|compare]` | government delivery-mode add-on blocks, each adding one dollar-total measure (type B) to the full default model, which keeps S&L earnings, in every stratum: `gov_indirect` (medicaid = BEA Medicaid `tr_medicaid`; transfers = Medicare + Medicaid + income maintenance `tr_medmaint`; welfare = CoG local public welfare `cog_welfare`) and `gov_direct` (dge = CoG local direct general expenditure `cog_dge`): 72,336 specs, v9 + new → v10, added-term stability and how the default covariates move | `data/spec/run_principled_{gov,v10}/` (`gov_{term,shift,correlations}.csv`) |
| 94 | `94_variables_panel_model.R [build\|describe\|models\|index\|lean\|figs]` | key variables for a unified panel model, by theory family (demand: need + heterogeneity; supply: wealth, education, social capital; niche: growth, government, history). build: 387 metros 2010–2022 in model form (totals `flog` + SAMI, rates as is); describe: three correlation matrices (2022 and pooled), VIFs within family, PC1 share and Cronbach's α per construct, within-metro variance shares, contaminated civic measures vs the outcomes; models: demand / supply / niche / combined for filer, grassroots (registered − filers) and spending (excl. H/U) SAMIs in cs 2022, pooled 2010–22 (year FE + year-specific log P slopes, metro-clustered) and within (metro + year FE), with drop-one absorber diagnostics; index: dominant item vs PC1 vs standardized mean per construct; lean: the recommended 13-covariate set and variants. Results: `notes/variables-panel-model-results.md`; section `paper/02-variables.qmd` | `data/variables/` (`cor_*`, `vif_2022.csv`, `constructs_pc1_alpha.csv`, `block_*.csv`, `item_vs_index*.csv`, `lean_*.csv`); figures `paper/figures/var_*.png` |
| 95 | `95_compete_niche.R [data\|describe\|addone\|pairs\|change\|figs]` | market-competition measures (from 23) added one at a time to the 94 lean model as niche terms: cs 2022, pooled, within, and a 2012 measure -> 2012-22 change test; grassroots is the clean outcome (HHI >= 1/n ties filers, spending is the revenue divided). Verdict: not in the default model; nHHI on grassroots as an extension. Rebuilds outcomes after the NTEE fix (spending partial r move <= .04). See `notes/variables-panel-model-results.md` section 8 | `data/variables/compete/`, `paper/figures/var_compete_addone.png` |
| 96 | `96_lean_card_evidence.R [D1 D2 D2size D3 D4 D5 D5c D8 D9 D10 D11 D12]` | single-model evidence tables of the design-decision cards refit with the 13-covariate lean default (BLOCKS_LEAN, taken directly; MV_MODEL not used), each mirroring its 58 part (or 52 / 86 B / 90) and also refit with the old six covariates on the same sample. D2 rebuilds all 13 terms from county data for six units (Gini from summed ACS brackets, religious HHI from summed RCMS 2020 group adherents, age SD from summed age bins, growth kernel from summed county population 1970–2022, endowment from 2000 BMF rulings by county); D5c refits the county subsector OLS / log(n+1) / Poisson models with lean county covariates. Results: `notes/lean-card-evidence.md` | `data/outline/v11/single/` (`d1_*` … `d12_*`, `d2_unit_covariates_*.csv`); figures `paper/figures/d*_lean_*.png` |
| 97 | `97_v11_decision_summary.R` | run v11 evidence for the design-decision cards (`paper/04-design-decisions.qmd`): eta^2 of each decision x 13 covariates (counts, spending), option medians, stability (robust vs state-clustered significance), matched drop-one-block shifts (endowment, growth kernel), Spearman of covariate t across estimators, v10 vs v11 benchmark medians ; with `MV_SPLIT=1` filers / grassroots (104) / spending instead of pooled counts, outputs `v11/mv_split/` | `data/outline/v11/mv/` |
| 98 | `98_decision_matrix.R [run]` | compact summary exhibit replacing the per-benchmark specification curves: covariates x decisions; top row = every spec's partial r (colored by significance) with (median) (% agree) (% sig); option means as shapes, blue where eta^2 >= .06; Impact column = mean ; with `MV_SPLIT=1` one matrix per population (filers, grassroots, spending), outputs `matrix_split/` |delta r| and % verdicts changed in matched pairs, # covariates affected. Horizontal and vertical layouts; vertical splits into pages of <= 7 covariates | `data/outline/<v>/matrix/`, `paper/figures/decision_matrix_<v>_*.png` |
| 99 | `99_interaction_screen_v11.R [workers]` | script 80's interaction screen (parts 1–3) in the 13-covariate lean default: 78 pairs, 13 squares, 13 x × log P one at a time (2022 metros, HC1, with/without square controls, BH over 104 and over the old 20); joint Wald / adj R² / 10-fold CV for the old 10 pairs + 5 squares, BH survivors, all x × log P, all 91 pairs + squares, with main-effect sign checks; 180-spec mini-multiverse (108 count, 72 spending) for every term. Arm `old6` refits the old six-covariate model's 20 terms on the same 385 metros. ≤ 6 workers. Results: `notes/interaction-screen-v11-results.md` | `data/interactions/v11/`; figure `paper/figures/d6_lean_interactions.png` |
| 100 | `100_d12_change_lean.R` | D12 Evidence 2 (58 part P change model) in the lean default: change in filer / grassroots SAMI from a start window to 2022–24 on the start SAMI, in-window growth and the 13 terms measured in the start year (2010–12 main; 2000–02 with 12 terms, gini_m starts 2010; 2012–14 check), ± mean filer age (2022) or start-year age of the registered stock (BMF). Growth kernel at the start year (disjoint from in-window growth; arm with the 2022 kernel for contrast). Replicates the card (six 2022 covariates, 363 metros) exactly. Results: `notes/lean-card-evidence.md` | `data/outline/v11/single/d12_change_lean*.csv` |
| 101 | `101_lean_open_checks.R [F] [M] [W] [workers]` | the last six-covariate checks refit on the 13-covariate default (v11 grid specs). F: revenue floor, 93 default 2022 metro specs with filers above the $50k real floor vs all filers. M: the same specs for filers, registrations and spending on all vs single-state metros (and single-division for division FE). W: every within spec except contribution measures and net outcomes, 2010–22 vs 2000–22; both windows use the county-weighted Gini because gini_m starts in 2010, with a check against v11 | `data/outline/v11/open_checks/` |
| 102 | `102_card_figures.R` | card figures of `paper/04-design-decisions.qmd` in the decision-matrix layout (plus D3: identification × population with min–max dumbbells, from 103), read from 96's single-model tables: covariates as rows, per outcome an "all options" column (median, range of estimates, envelope of 95% intervals; blue = every option significant with one sign) and a "by option" column (shapes, blue when p < .05); D1, D2, D4, D8, D9, D10, D12, and D11 as a subsector × covariate matrix | `paper/figures/d*_card.png` |
| 103 | `103_ident_by_population.R [workers]` | D3 Evidence 1 by outcome population instead of pooled counts: median partial r and share significant with the median sign by identification (cs / pooled / within) for filers, registered stock, spending (v11 default specs) and grassroots (registered-stock specs refit with n_bmf_c3pc - n_filer_c3); within - cross-section | `data/outline/v11/mv/ident_by_population*.csv` |
| 104 | `104_fit_grassroots_v11.R [workers]` | grassroots as a multiverse outcome: every v11 registered-stock spec refit with n_bmf_c3pc − n_filer_c3 (spec_id + 1e7, outcome n_grass); read by 97 and 98 with `MV_SPLIT=1` (filers / grassroots / spending groups instead of pooled counts) | `data/spec/run_principled_v11_grass/` |
| 89 | `89_need_socap_diversity.R build\|describe\|horse [workers]\|horse_summary\|figs` | measurement checks on the need and civic blocks and on diversity vs participation. build: ACS 5-year 2010–24 county tables (describe/horse use 2015–22; build also writes `data/cov/cov_gini_metro.csv` for the inequality block) summed to CBSAs (metro grouped Gini from B19001, national-quintile income diversity, deep/near poverty, SNAP, rent burden, homeownership, same house); describe: correlations, factor structure, benchmark partial r, diversity vs turnout / census response; horse: 1,836 default specs (cs 2022/2019, pooled; no within) re-fit with 9 need and 7 civic alternative sets (31,212 fits). Results: `notes/need-socap-diversity-results.md` | `data/need_socap/` (`acs_need_cbsa.csv`, `horse_*.csv`, figures); copies in `notes/need-socap-figures/` |
| 85 | `85_civic_fork.R [test\|run\|merge\|compare]` | civic and religion add-on blocks (census response, re-estimated social-capital index, denominational HHI, adherence): 66,000 specs (no within), v7 + civic → v8, added-term stability and how the default covariates move | `data/spec/run_principled_{civic,v8}/` |
| 84 | `84_tcja_contrib.R` | 2018 tax law (TCJA) and the contribution covariates: national itemizer series, metro rank stability across 2017–18, partial r of each contribution measure in yearly 2010–22 cross-sections, within-metro models with a post-2018 slope ; with `MV_MODEL=lean` the 13-covariate default (run v11) | `data/tcja/`; lean: `data/tcja_v11/` |
| 50 | `50_followups_jeremy.R [parts]` | follow-ups: FE geography, SAMI persistence, subsector SAMIs, economy SAMIs, pass-through scaling, demeaned scatterplots. Part 3 fills invalid e-file NTEE from the EIN's most recent valid BMF code (rule of 06/23 since 2026-11-17; old outputs in `data/followup/pre_ntee_20261117/`) | `data/followup/` |
| 51 | `51_run_divfe.R [test\|run\|merge\|compare]` | ran the div_fe estimator specs and merged them with v2 into v3 (historical: its merge predates the history block) | `data/spec/run_principled_{divfe,v3}/` |
| 52 | `52_endowment_change.R` | SAMI as % deviation; risers/fallers 2000-24; convergence, hybrid (CRE) and growth-curve models; pre-1970 endowment | `data/followup/p7-p9_*` |
| 55 | `55_endowment_fork.R [build\|run\|merge\|compare]` | builds the pre-1970 endowment covariate; runs the `history:pre1970` specs; v3 + endowment → v4 with canonical spec_ids | `data/cov/cov_endowment.csv`, `data/spec/run_principled_{endow,v4}/` |
| 56 | `56_drop_religion_mutual.R` | test of dropping NTEE X (religion-related) and Y (mutual benefit) filers (with `MV_MODEL=lean`: 13-covariate baseline and matched specs, outputs `p10_v11_*`) | `data/followup/p10_*` |
| 70 | `70_collinearity_composites.R [parts]` | correlation structure, VIFs, EFA (psych), additive/factor composites, panel models items vs composites with block Wald tests; see `notes/collinearity-composites-results.md` | `data/collinearity/` |

## Universes (`build_grid( "principled" | "kitchen" )`)

- As of 2026-11-16 (latest): run v10 (`data/spec/run_principled_v10/`, `93_gov_fork.R run` + `merge`) is current.
  It is v9 plus 72,336 specs from two add-on blocks, `gov_indirect` (medicaid, transfers, welfare) and
  `gov_direct` (dge), run in every stratum: 605,352 in all, all fit. v9 spec_ids are unchanged and the v9 coefficient rows
  are identical. Results: `93_gov_fork.R compare` and `notes/government-funding-measures.md` ("run v10").
  Rerun on v10 the same day:
  - **45 and 58:** `45_summarize_run.R principled_v10`, then 58 with `MV_RUN=run_principled_v10`: parts A–U and W, `V all`, `V all exp` → `data/outline/v10/`. 71 of 74 tables match v9 exactly. Part A's decision importance adds Indirect government and Direct spending rows, and the explainer spec curve grows from 48,200 to 55,912 specs.
  - **Composites:** not refit. Their 162,756 specs keep their ids and never carry add-on blocks. `53 merge run_principled_v10` and `54 run_principled_v10` produced 768,108 specs. The old v8 merge is kept as `run_principled_cmp_merged_v8`.
  - **57:** run on the new merge. It adds 5,040 within-long specs (inequality, which is a no-op since `gini_m` starts in 2010, plus the four government alternatives), for 32,256 in all. Old rows are identical.
  - **58 part F:** rerun after the merge.

  Note: spec_ids are not contiguous
  (algebraic duplicates are dropped after numbering), so v9 ids run to 628,320 and the new ones are 628,321–713,504.
- As of 2026-11-16 (later): run v9 (`data/spec/run_principled_v9/`, `92_ineq_cong_fork.R run` + `merge`).
  It is v8 plus 34,584 specs from two add-on blocks, `inequality` (gini = metro Gini from summed county ACS
  brackets, run in every stratum) and `congregations` (cong = RCMS congregation count, type B; cs and pooled
  only): 533,016 in all, all fit; v8 spec_ids unchanged. Results: `92_ineq_cong_fork.R compare` and
  `notes/need-socap-diversity-results.md` section 5. 45 and every part of 58 were rerun on v9
  (`45_summarize_run.R principled_v9`; `MV_RUN=run_principled_v9`, parts A–U and W, then `V all` and `V all exp`;
  `data/outline/v9/`; part A adds Inequality and Congregations rows, part D's covariate-set strip adds the two
  blocks). Shared v8 rows are identical. The composite merge (53/54) and 57 still sit on v8.
- As of 2026-11-16: run v8 (`data/spec/run_principled_v8/`, `85_civic_fork.R run` + `merge`). It is v7
  plus 66,000 specs from two add-on blocks, `civic` (respn = census mail response; sk2 = re-estimated
  social-capital index, which still counts association establishments) and `religion` (hhi, adherent),
  cross-section and pooled only (498,432 in all; v7 spec_ids unchanged). Results: `85_civic_fork.R compare`
  (`v8/civic_{term,shift,correlations}.csv`) and the outline's D7 card, Evidence 5. 45 and every part of 58
  were rerun on v8 (`MV_RUN=run_principled_v8`, `data/outline/v8/`; part A adds Civic and Religion rows).
  The composite-index run was rebuilt the same day on the floor and the % nonwhite default and merged with v8
  (`53_run_composites.R run` + `merge D:/MULTIVERSE/data/spec/run_principled_v8`; `54` against v8), and 57
  was rerun on it. The pre-v5 composite runs and the pre-v6 within-long run are kept as
  `run_principled_cmp{,_merged}_pre_v5_slog/` and `run_principled_wl_pre_v6_slog/`.
- As of 2026-11-15 (later): run v7 (`data/spec/run_principled_v7/`, `82_lag_fork.R run` + `merge`) is v6 plus
  the 36,168 `lag` specs (432,432 in all; v6 spec_ids unchanged). Read it in 58 with `MV_RUN=run_principled_v7`.
  Results: `notes/catchup-lag-results.md` section 2.9.
  The outline was rerun on the final v7 on 2026-11-15 (`data/outline/v7/`; logs `outline_v7_{a,v,vexp,xs}.log`):
  parts A D L M N S W, V all (counts and spending), B C E G H I J K L M N P R U. From then on 58 logs extensive
  covariates with `flog()` (as in 40), so its single-model foundation-asset numbers differ from `outline/v5/`.
  Part A adds a `Lag` row (default vs + lag kernel), and part D's covariate strip labels `lag:*` specs. Part F,
  and the figure at the end of U, read the 11-12 composite run `run_principled_cmp_merged` (pre-v5, `slog`), so
  they were not rerun (U's tables are written before it stops).
  `45_summarize_run.R` was rerun on v6 and v7 the same day (first runs since v4). The meta-hypothesis holds for
  predictor form (Spearman of R² on log P with predictor-form sensitivity .42–.68 in all 12 strata) and fails for
  DV form (−.40 to +.15, as in v4). Sample exclusions (83) need no fork: case exclusions move partial r less than
  winsorizing; only a population threshold matters, and it changes the estimand (D2 card in the outline).
- **principled** (130,200 specs after removing duplicates): covariate forks one at a time
  (alternative measures only in the full model; leave-one-block-out only with
  defaults); human-services outcomes left to a subsector extension; OLS reports
  robust/metro-clustered and state-clustered SEs from one fit.
  As of 2026-11-15: run v6 (`data/spec/run_principled_v6/`, `44_run_universe.R principled 2000 14 principled_v6`)
  is current. It is v5 with logged and SAMI-form extensive predictors floored per capita (`flog()`; see
  "Predictors" below); `81_compare_floor.R` compares the two. Read it in 58 with `MV_RUN=run_principled_v6`.
  As of 2026-11-13: run v5 (`data/spec/run_principled_v5/`, one pass of `44_run_universe.R principled 2000 14 principled_v5`): `pct_nonwhite` is the diversity default and `race_hhi` an alternative (`diversity:hhi`), and the panel
  carries the Bedford VA 2016 election fix. Its `diversity:hhi` specs reproduce v4's defaults exactly in the 2022
  cross-section. `58_outline_analyses.R` reads it with `MV_RUN=run_principled_v5` (outputs in `data/outline/v5/`).
  With that switch the cross-sectional parts (B, C, E, G-N, P, R) also use % nonwhite as the diversity control
  (`divx()`; the column keeps the name `race_hhi` inside each part). Part I panel A still reads run v1 (racial HHI);
  parts F and U compare diversity measures and are not switched.
  As of 2026-11-12: 396,264 specs after duplicates (run v4 in `data/spec/run_principled_v4/`),
  with the resource-measure outcome fork, the `div_fe` estimator (Census-division FE,
  SEs clustered by state) and the `history` block.
- **kitchen** (703,080 after duplicates in the first grid; 1,848,132 with the
  forks above): every fork crossed.
- **Add-on blocks** (no default, e.g. `history`): their alternative adds a covariate
  to the full default model; nothing to drop. Their specs are appended after the
  base grid, so adding one never renumbers existing spec_ids. `history:pre1970`
  adds the pre-1970 endowment (`hist_pre1970`, time-invariant: not run in the
  within stratum); `load_panel()` merges it from `data/cov/cov_endowment.csv`.
  `lag` (added 2026-11-15) adds a kernel of past log population change,
  G[t] = a·G[t−1] + Δlog P[t] with a = 0.5^(1/half-life): `lag:g5` (5-year half-life,
  the local-projection estimate) or `lag:g10` (10-year, the level fit). A third,
  `lag:pascal`, was run and dropped (collinear terms). The kernels vary over time,
  so they also run in the within stratum (36,168 specs).
  Type A: used as is, residualized on log P in the all-SAMI arm. `load_panel()`
  merges them from `data/cov/cov_lag_kernel.csv` (built by `82_lag_fork.R build`).
  Each add-on block's specs are built and appended separately, in `BLOCKS` order,
  so the `history` spec_ids did not change. See `notes/catchup-lag-results.md` 2.8.
- **Duplicates** (`dup_of` not NA) give identical covariate coefficients and are not
  run in the curve: log(Y/P)+log P ≡ log Y+log P; Poisson offset + log P ≡ free
  Poisson; in cross-sections SAMI_Y + log P ≡ log Y + log P and residualized
  predictors + log P ≡ logged predictors. Only unwinsorized specs qualify.
- Predictors are fit on unit-SD scale and reported in original units (raw dollar
  totals otherwise destabilize variance estimates).
- **Logged extensive predictors** (`log` and `sami` predictor forms; also the `log`
  base of the composites in 41) use `flog(x, pop)` from run v6 on: per-capita values
  below the 1st percentile of positive values in the estimation sample are raised to
  it before logging. Runs v1–v5 used `slog()` = log(x + 1), which put Eagle Pass, TX
  (zero foundation assets every year) ~11 SD below the metro mean and left 89 CBSAs
  with zero or near-zero assets as leverage points. Foundation assets and foundation
  counts are the only covariates with zeros. Since 2026-11-16 every script uses the floor:
  50, 52, 56, 70, 71, 72 and 76 were switched and re-run (pre-change outputs in
  `D:/MULTIVERSE/data/_pre_flog_20261116/`, with `diff_files.csv` / `diff_cells.csv`); only
  80 and 81 keep `slog()` on purpose, to compare the two.

## Source decisions worth knowing

- **Population** is rebuilt from Census intercensal/postcensal estimates. The older
  `00 - DATA/population` files interpolate between censuses and mishandle CT.
- **CORE**: the catalog's PZ/CE `marts/…-V1` files store money fields as
  denormalized doubles (~1e-308) and are unusable; the unversioned harmonized
  files are used. CORE PF (V0) is nearly empty for TY2016–2018.
- **Foundations**: CORE 2000–2015 → interpolated 2016–2018 (no complete source; the
  IRS published no 990-PF extracts for filing years 2017–2019) → SOI extracts
  2019–2020 → e-file 2021–2024.
- **990/EZ splice**: CORE 2000–2010 → SOI 2011–2019 → e-file 2020–2024 (SOI's 2020
  EZ extract is short).
- **Locations**: SOI and e-file use current geocodes (block FIPS; lat/long → 2023
  county by point-in-polygon). CORE and SOI-PF use the BMF snapshot county for
  the same EIN-year.
- **BMF stock**: the 2016–2018 legacy snapshots omit most newly ruled orgs (the
  2019 file adds ~214k EINs ruled 2016–2019), and the 2019+ files code
  foundation status as "4.0" (previously read as non-foundation). `n_bmf_*` is
  now the reconstructed stock; raw snapshot counts are `n_bmf_*_snap`.
- **Pass-through**: `exp_c3_net` / `rev_c3_net` subtract grants to domestic
  organizations and governments (Part IX line 1; EZ line 10). From 2012 only
  (CORE 2010 and SOI 2011 grant fields are incomplete); SOI-era 990-EZ grants
  are unknown and counted as zero.
- **Social capital**: the published Rupasingha index includes NCCS nonprofit
  density and turnout; use `socap_sk2` (re-estimated without them) or
  `socap_assn_n`.
- **Patents**: PatentsView bulk files require a USPTO.gov login (Open Data Portal,
  dataset PVGPATDIS, 2026-04 release, grants through 2025). The five tables
  (`g_patent`, `g_application`, `g_inventor_disambiguated`,
  `g_location_disambiguated`, `g_assignee_disambiguated`) are unzipped in
  `D:/MULTIVERSE/raw/patents/`. The DataLumos mirror (223582-V1.zip, same
  folder) lacks the inventor and location tables. Application years 2021+ are
  truncated by grant lag (`patents_truncated`).
- R scripts must be written with an editor, not a Bash heredoc (heredocs here
  collapse `\\` in regexes).
