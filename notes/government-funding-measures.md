# Direct vs indirect government: data and measurement proposal

Written 2026-11-16. The first-wave measures were built the same day and are in the analysis panel; no multiverse run uses them yet. See "Built: first wave" below.

## Built: first wave (2026-11-16)

**Scripts**
- `R/build/D_cov/22_cov_government.R` → `data/cov/cov_gov.csv`
- `R/build/D_cov/12_cov_bea.R` now also reads CAINC35 (`tr_*` columns)
- `R/build/E_assemble/30_assemble_analysis.R`: interpolation of the new series and the derived ratios (`derive_gov()`)
- `R/notes/91_government_measures.R` → `data/government/`

The rebuilt panels keep every existing column identical, cell by cell (backup: `data/analysis/pre_gov_20261116/`).

| Construct | Panel variables | Years | Checks |
|---|---|---|---|
| A2 direct: government jobs | `qcew_{fgov,sgov,lgov}_{emp,wages}`, `qcew_*_imputed`; `qcew_lgov_share` | 2000–2024 | County sums equal QCEW's US totals. About 300 counties a year have a suppressed local-government cell; these are imputed as the state total minus disclosed counties, allocated by population. QCEW's own local-government total jumps 4% from 2000 to 2001 (source coverage). |
| A3 direct: local spending | `cog_dge`, `cog_curop`, `cog_capital`, `cog_assist`, `cog_interest`, `cog_educ_curop`, `cog_health_curop`, `cog_fed_igr`, `cog_state_igr` | census years 2002–2022, interpolated, carried to 2024, missing 2000–01 | 2017 local direct general expenditure = $1,641.7bn, matching Census table 1 exactly. The 2012 county-area file and the 2012 unit file agree (log r = .9996). |
| A4 direct: residents working for government | `acs_cow_*`; `acs_pct_gov`, `acs_pct_lgov` | 2009–2024 | Rio Arriba NM 2018 interpolated. `acs_cow_npo` (residents working for nonprofits) is outcome-adjacent: never a predictor. |
| B1 indirect: transfers to persons | `tr_medicare`, `tr_medicaid`, `tr_income_maint`, `tr_snap`, …; `tr_medmaint` = Medicare + Medicaid + income maintenance | 2000–2022, carried to 2024 | County sums equal BEA's US totals. `tr_nonprofit` (receipts of nonprofit institutions) is descriptive only. |
| B2 indirect: local welfare spending | `cog_welfare` | census years | — |
| C2 mode: labor intensity | `gov_labor_intensity` = QCEW local wages / CoG local current operations | 2002–2024 | — |
| C3 mode: outsourcing share | `gov_indirect_share` = `tr_medmaint` / (`tr_medmaint` + state and local wages) | 2000–2024 | — |
| C1 mode: delivery residual | residual of log local government jobs on log local current operations, computed in `91` (not stored) | — | — |

**Things that differ from the proposal**
- **Salaries (Z00) are dropped from the 2022 public-use file**, so labor intensity uses QCEW wages for every year. `cog_salaries` is kept for 2002–2017 only.
- **The 2022 file folds equipment purchases (G) into F and welfare vendor payments (E74/E75) into E79.** The aggregates above use sums that are stable across these changes.
- **Cities spanning several counties stay in their home county** (only New York City is split). Use CBSA totals, which are unaffected.

### First results (`91_government_measures.R`, 2022 metros unless noted)

Partial r of each government measure, entered alone in place of S&L earnings in the benchmark model:

| Measure | Counts, OLS | Counts, state FE | Spending, OLS | Spending, state FE | Counts, 2012 OLS |
|---|---|---|---|---|---|
| A1 S&L earnings (current default) | .33 | .40 | .21 | .36 | .28 |
| A2 local government jobs | .09 | .00 | .07 | .13 | .07 |
| A2b state + local jobs | .27 | .50 | .18 | .43 | .26 |
| A3 local direct general expenditure | .17 | .17 | .12 | .10 | .14 |
| A4 % residents in government | .12 | −.03 | .00 | −.06 | −.01 |
| B1 Medicare + Medicaid + income maintenance | .31 | .24 | .32 | .31 | .24 |
| B1b Medicaid | .41 | .52 | .34 | .50 | .38 |
| B2 local welfare spending | .22 | .35 | .17 | .04 | .14 |
| C1 delivery residual | −.13 | −.21 | −.09 | .01 | −.10 |
| C2 labor intensity | −.03 | −.29 | −.03 | −.03 | −.01 |
| C3 outsourcing share | −.12 | −.29 | −.05 | −.22 | −.12 |

**Reading**
- **Indirect government is the strongest new signal.** Medicaid receipts per capita (SAMI) have partial r .25–.52 in every sample, estimator and year (.34–.52 in metros), mostly larger than the current S&L-earnings default. Direct and indirect measures are nearly uncorrelated (A3 vs B1 r = .09), so they enter separately. In A + B pairs, both A1 and B1 stay positive (counts .30 and .29).
- **Direct government is positive, not negative.** S&L earnings, CoG spending and state + local jobs all have positive partial r. That is the opposite of the simple government-failure prediction.
- **Delivery mode leans toward interdependence.** Where local government employs more people than its spending predicts (C1), there are slightly fewer nonprofits (−.09 to −.21; ~0 for spending with state FE). The effect is small. C3 is mostly the inverse of S&L wages (r = −.85 with A1), so it does not measure mode cleanly; use C1 instead.
- **Adding transfers moves the benchmarks.**
  - Poverty's partial r turns ≤ 0: counts OLS −.03 → −.15; spending with state FE .33 → .16.
  - Turnout drops: counts with state FE .44 → .30; spending OLS .06 → −.03.
  - Transfers correlate .40 with poverty and −.51 with % BA. Medicare also tracks age, and the benchmark model has no age term.
  - The current default A1 already absorbs much of % BA (counts OLS .52 → .24).
- **Caveat on Medicaid:** BEA records it by beneficiary residence, but it pays providers, so dense nonprofit health sectors could raise it (reverse causality). Spending excludes hospitals and universities but still includes clinics and nursing homes.

**Suggested next step for the multiverse:** an add-on block `gov_indirect` (default none; alternatives B1, B1b, B2) at the end of `BLOCKS`. For the `government` block, add alternatives A3 (CoG spending) and A2b. Both need a new run, so they are not added here. *(Done as run v10, below. A3 went in as an add-on instead. A2b was left out: BEA S&L employment, `government:gov_emp`, is already an alternative.)*

### In the multiverse: run v10 (`93_gov_fork.R`, 2026-11-16)

**Blocks.** There are two add-on blocks at the end of `BLOCKS`. Each alternative adds one dollar-total measure (type B: raw, per capita, log or SAMI) to the full default model, which keeps S&L earnings.
- `gov_indirect`:
  - medicaid = B1b (`tr_medicaid`)
  - transfers = B1 (`tr_medmaint`)
  - welfare = B2 (`cog_welfare`)
- `gov_direct`: dge = A3 (`cog_dge`). Given payroll, A3 reads as spending beyond in-house labor. As a replacement for S&L earnings it would be a `government`-block alternative, and that would renumber the base grid.
- All four vary within metros: within-metro shares of log per-capita variance are .17–.37, against .04 for S&L earnings. So they run in every stratum.

**Run.**
- 72,336 new specs (18,084 per alternative), 9.9 minutes on 12 workers.
- v10 = v9 + new = 605,352 specs, all fit.
- v9 spec_ids are unchanged, and the v9 coefficient rows are identical in v10 (estimates and SEs).
- Outputs are in `data/spec/run_principled_v10/`: `gov_term.csv`, `gov_shift.csv`, `gov_correlations.csv`.

**Added term.** Median partial r across specs, with the share of specs with the median's sign in parentheses:

| | Counts, cs/pooled | Spending, cs/pooled | Within metros (counts / spending) |
|---|---|---|---|
| Medicaid | .10–.13 (72–78%); p < .05 in 66–69% | .13–.15 (87–95%); p < .05 in 70–79% | ≈ 0 / .05–.07 |
| Medicare + Medicaid + income maintenance | .09–.10 (68–71%) | .10–.11 (74–86%) | .02–.03 / .05–.06 |
| Local public welfare | .04–.09 (68–94%) | .05–.07 (83–89%) | ≈ 0 / ≈ 0 |
| Local direct general expenditure | .01–.04 (52–64%) | −.04 to .01 | .04–.06 / .03–.05 |

- **Medicaid is the strongest of the four, and it adds to S&L earnings rather than replacing them.** The add-on partial r is smaller than the .34–.52 in `91`, for two reasons:
  - S&L earnings stay in the model.
  - The multiverse medians run over every DV and predictor form, including raw and per-capita forms.
- **Within metros it is near zero for counts.** It is .05–.07 for spending, where 89–97% of specs share the sign. That is weak evidence on the reverse-causality worry: if providers drove Medicaid up, within-metro spending changes would track it.
- **Direct spending beyond payroll adds nothing in cross-section.** That fits neither the failure story (which predicts negative) nor a strong interdependence story (which predicts positive).

**How the defaults move.**
- **Overall:** the median absolute change in a default covariate's partial r is ≤ .02. Paired specs correlate .80–.99.
- **Largest shifts, all in the direction `91` showed:**
  - Poverty for spending falls .08 → .03 (−.04 to −.05) with transfers or Medicaid; its sign flips in 7–18% of specs.
  - Poverty for counts goes from ~0 to −.02/−.03.
  - % BA rises .02–.04 with Medicaid.
  - Turnout falls .02–.03.
  - S&L earnings for spending fall .02–.03; the sign flips in 11–16% of specs.
- Welfare and direct general expenditure move nothing (≤ .01).
- **Correlations, 2022 metros, log per capita:**
  - Medicaid: poverty .34, % BA −.39, S&L earnings .07.
  - Transfers: % 65+ .38 (Medicare), poverty .43, % BA −.56.

## Mechanism check: do the government terms work through grants? (`R/notes/105_govgrant_mechanism.R`, 2026-11-17)

**Question.** In the 13-covariate model, Medicaid transfers (.24) and S&L government earnings (.23) have positive partial r for filer density. Two readings fit that sign:
- **Interdependence:** government funds nonprofits.
- **Something else:** for example, demand for services.

Line 1e can tell them apart, but only when used as an *outcome*, never as a predictor. The model is the lean single model of `96_lean_card_evidence.R`: 13 covariates with log P on the right, standardized, partial r, 385 metros. Hospitals and universities are excluded.

**Part B: population split, 2022 (fully observed).** Log count of full-990 filers with any government grants vs. with none. This is the sharpest test.

| Covariate | With grants: OLS / division FE / state FE | Without grants: OLS / division FE / state FE |
|---|---|---|
| Medicaid transfers | **.29 / .17 / .04** | .08 / .03 / .04 |
| S&L govt earnings | **.23 / .14 / .16** | .12 / .12 / .11 |
| Pre-1970 endowment | .06 / .07 / .19 | **.36 / .29 / .30** |
| Growth kernel | **−.23 / −.27 / −.15** | .03 / .00 / .06 |
| % BA | .33 / .33 / .25 | .27 / .24 / .19 |
| Foundation assets | .15 / .29 / .30 | .23 / .24 / .23 |
| Poverty rate | −.14 / −.07 / −.03 | −.21 / −.17 / −.18 |
| % nonwhite | −.21 / −.17 / −.14 | −.12 / −.11 / −.11 |

**Part A: how dependent are a metro's nonprofits?** Two outcomes, in 2022 and averaged over 2015–2018:
- **Incidence:** the logit share of 990 filers with any government grants. Medicaid has partial r .16–.23 under OLS, .12–.14 with division FE and .01–.05 with state FE. S&L earnings is .05–.12. The growth kernel (−.19 to −.28) and the endowment (−.11 to −.23) are negative under every estimator. R² is .28–.33 under OLS and .56–.60 with state FE.
- **Intensity:** the log of government grants ÷ revenue. Neither government term predicts it: Medicaid is .00–.12, S&L earnings −.05 to .04. The terms that do are % 65+ (.10–.25), % nonwhite (.09–.18), age SD (−.15 to −.17) and foundation assets (−.08 to −.12). R² is .20–.26 under OLS and .39–.44 with state FE.

**Reading**
- **The government terms work through government-funded nonprofits.** Medicaid's density association sits almost entirely in filers that receive government grants: .29 against .08. S&L earnings tilts the same way, .23 against .12, and this holds within states. That is the interdependence (third-party government) reading. The gap-filling reading predicts the opposite: more nonprofits where government is absent.
- **Medicaid's part is a state-policy pattern.** With state FE, its association falls to .04 for both groups, as its density effect does in D9 (.24 → .06). States that buy services through nonprofits have more government-funded nonprofits. Within a state, metros with more Medicaid do not. S&L earnings, which varies within states, keeps its tilt under state FE.
- **Government funding affects how many organizations receive grants, not how much of their revenue grants supply.** Where government is larger, more nonprofits get government grants. But the grant share of revenue is set by the age profile, diversity and private philanthropy, not by the government terms.
- **The two history terms split the sector.** The pre-1970 endowment predicts filers *without* government grants (.36 against .06): old, privately funded institutions. The Sunbelt growth deficit sits in government-funded filers (−.23 against .03). Fast-growing metros lag in organizations that depend on public money, not in privately funded ones, which suggests catch-up waits on public contracting.
- **Limits.** These are cross-sections, not causal estimates. "With grants" is any line-1e amount above zero, and EZs are excluded because they have no line 1e. 2022 is fully observed. In the 2015–2018 window, 7–11% of dollars are imputed and incidence uses e-filed returns only. Including hospitals and universities leaves intensity unchanged in substance (Medicaid .00–.06).

Outputs are in `D:/MULTIVERSE/data/govgrants/`: `mechanism_dependence.csv` and `mechanism_split.csv`.

## Why this matters

Lecy & Van Slyke (JPART 2013, 23(1)) tested two theories that predict the same sign for "government size":

- **Government failure:** nonprofits fill gaps where the state does not provide.
- **Interdependence (third-party government):** the state funds nonprofits to deliver services.

They can be told apart only if government is split by **mode of delivery**:

- **Direct (make):** government spends and employs, and delivers services itself.
- **Indirect (buy):** government pays others through grants, contracts and vouchers. Much of this money lands in the nonprofit sector.

The 2013 paper's scatterplots used four measures:

| Panel | Mode it measures |
|---|---|
| % employed by government | direct |
| Direct benefit payments | indirect, through vouchers and transfers to persons |
| Federal grant dollars | indirect, as federal money entering the area |
| Poverty, inequality and income | need and demand, not government |

## What the multiverse has now

There is a single `government` block (`R/lib/40_multiverse.R`):

| | Variable | Source | Years |
|---|---|---|---|
| default | `gov_earn_sl`: state and local government earnings | BEA CAINC5N | 2001–2024 |
| alt | `gov_emp_sl`: state and local government jobs | BEA CAEMP25N | 2001–2022 |
| alt | `gov_emp_share`: state and local government jobs / all jobs | BEA | 2001–2022 |

- All three measure **direct** government only, and only through labor.
- There is no spending measure and no indirect measure.
- The pipeline also carries `gov_earn_federal_civ` and `gov_earn_military` (`12_cov_bea.R`), but no run uses them.

## Available data, 2000–2024

| # | Source | What it measures | Mode | Geography | Years | Status |
|---|---|---|---|---|---|---|
| 1 | BEA CAINC5N / CAEMP25N | Government earnings and jobs, split federal civilian / military / state and local | direct | county | 2001–2024 (jobs to 2022 locally) | **in pipeline** |
| 2 | BLS QCEW, ownership codes 1/2/3 | Federal, state and local government employment and wages, by place of work | direct | county | 2000–2024 | **already on disk** (`raw/cov/qcew/qcew_YYYY_10.csv` holds codes 0/1/2/3/5); only code 5 is read now |
| 3 | ACS class of worker (B24080) | Residents working for local, state or federal government | direct | county / CBSA | 5-year from 2009; decennial SF3 2000 | ACS cache exists; table not pulled |
| 4 | Census of Governments + Annual Survey of State & Local Government Finances, individual unit files | Revenue and expenditure for every local government, by function and by character/object (salaries and wages, current operations, capital, assistance and subsidies, vendor payments) | direct and indirect | each unit carries a county code; aggregate to county and CBSA | Census years 2002, 2007, 2012, 2017, 2022 are complete; other years are samples | download needed |
| 5 | Annual Survey of Public Employment & Payroll (ASPEP), individual unit files | Local government full-time-equivalent employees and payroll, by function (welfare, health, hospitals, housing, parks, libraries) | direct | unit → county | annual; complete in census years; 2023 out | download needed |
| 6 | Government Finance Database (Pierson, Hand & Thompson 2015) | Source 4, cleaned and harmonized, with county, municipal, school-district and special-district panels | as 4 | unit | 1967–2015 (ICPSR 37641) | optional shortcut for 2000–2015 |
| 7 | Lincoln Institute Fiscally Standardized Cities (FiSC) | Spending of the city plus overlying county, school and special districts, made comparable | direct | ~200 central cities | 1977–2022 | metro cores only |
| 8 | BEA CAINC35 | Personal current transfer receipts: Medicare, Medicaid, SNAP, SSI, Social Security and others | indirect (vouchers) | county | 1969–2024 | download needed (CAINC30 is on disk) |
| 9 | Consolidated Federal Funds Report (CFFR) | Federal grants, direct payments, procurement and salaries | both | county | FY1983–2010, then discontinued | the 2013 paper's likely source |
| 10 | USAspending.gov | Federal prime awards by place of performance and recipient type (nonprofit flag) | indirect | county / zip | FY2001+, reliable from FY2008 (FFATA) | download needed; state pass-through subawards are poorly covered |
| 11 | Federal Audit Clearinghouse (Single Audit) | Federal awards spent by each nonprofit auditee, flagged direct vs **passed through from a state** | indirect | auditee address / EIN | 1997–2015 (Census archive); 2016+ (fac.gov API) | download needed; threshold $500k–$750k |
| 12 | Form 990 Part VIII line 1e, government grants received | Government grants into local nonprofits | indirect | org → CBSA | see next section | partly available |

## Line 1e coverage, checked today

From CORE 501(c)(3) PZ files (`F9_08_REV_CONTR_GOVT_GRANT`):

| Tax year | Field filled | Filers > 0 | Gov grants | Share of revenue |
|---|---|---|---|---|
| 2005 | 72% | 30% | $113 bn | 9.2% |
| 2010 | 100% | 25% | $152 bn | 9.1% |
| 2013 | 61% | 32% | $52 bn | 2.8%, incomplete |
| 2016 | column absent | — | — | — |

- **2000–2010:** usable, and the 9% share matches published national totals.
- **2011–2019:** missing. The IRS SOI 990 extracts have no line-1e field (checked in the 2012–2024 crosswalk and raw headers), and CORE 2014+ is built from them. The only fill is the e-file XML, which covers e-filers only (roughly 60–70% of 990s, skewed large) until the 2020 mandate.
- **2020–2024:** available in e-file. `panel990` table P08 is not cached yet.
- **Not counted:** Medicaid and Medicare reimbursements are program-service revenue, not line 1e, so line 1e understates government money by roughly 3×.

## Line 1e built for 2011–2024 (`R/build/B_npo/24_npo_govgrants.R`, 2026-11-17)

The 2011–2019 gap is now filled: e-filed returns are observed, and paper filers are imputed. Outputs are in `D:/MULTIVERSE/data/govgrants/`. Nothing in the analysis panel or the multiverse uses them; use the series descriptively (see Cautions).

**Data.** The e-file revenue table (`F9-P08-T00-REVENUE`) for TY2011–2024 was downloaded with `panel990::download_tables()` into `raw/panel990/<year>/`, about 460 MB; the download log is in `raw/panel990/logs/`.
- A blank line 1e on an e-filed 990 is a zero.
- Duplicate returns are dropped by the panel990 rule: non-group, non-partial, amended, then the latest timestamp.
- The 990-EZ has no line 1e in any year.

**Frame.** Active 501(c)(3) filers, defined as in `06_npo_assemble.R`: SOI for 2011–2019 and e-file for 2020–2024, max(revenue, expenses) ≥ $50k in 2010 dollars, the record's own geocode, and invalid NTEE codes backfilled from the BMF. Full 990s are matched to e-file on EIN × tax-period month.

**Imputation.** Government grants = share × total contributions. Line 1h, total contributions, contains line 1e, and SOI carries line 1h for paper filers. The share comes from:
1. **The same EIN's nearest e-filed year,** within 4 years. A donor year across the COVID boundary counts 5 years farther. Filers of $25M or more (2022 $) use their own history at any distance.
2. **Otherwise, a cell ratio:** Σ government grants / Σ contributions of observed filers in the same year × NTEE major group × size band. Hospitals and universities are their own groups. The ratio is taken by state if the cell has ≥ 30 filers, else by census region, else nationally.

The same method imputes paper 990s and all EZs; EZs are kept as their own component.

**Validation.** 20% of observed active filers under $1M, the paper-filer size range, were held out in 2011–2019 and imputed without their own year.

| | Ratio of imputed to true dollars | r |
|---|---|---|
| Organizations imputed from their own year (132k) | 1.006 | .93 (organization level) |
| ... donor 1 / 2 / 3 / 4 years away | 1.00 / 0.99 / 1.10 / 1.16 | .94 / .90 / .81 / .78 |
| Organizations imputed from a cell ratio (6k) | 1.17 | .47 (organization level) |
| Metro totals, 2011–2018 | 0.99–1.02 | .95–.997 |
| Metro totals, 2019 | 1.07 | .996 |

The 2019 excess is COVID relief: some held-out organizations borrow 2020-era shares from donor years.

**National totals, active located 501(c)(3) filers, 2022 $bn.** "Imputed" is the share of dollars imputed. "Unlocated" adds about $9–10bn a year in 2011–2019, mostly foreign-address filers (EIN prefix 98).

| Year | Gov. grants | Observed | Imputed paper 990 | Imputed EZ | Share of revenue | Excl. hospitals and universities | Imputed |
|---|---|---|---|---|---|---|---|
| 2010 (CORE) | 194.4 | — | — | — | 9.2% | — | — |
| 2011 | 191.9 | 153.1 | 38.1 | 0.7 | 9.4% | 16.7% | 20% |
| 2015 | 194.1 | 172.1 | 21.4 | 0.6 | 8.2% | 14.5% | 11% |
| 2018 | 211.6 | 197.2 | 13.7 | 0.6 | 7.9% | 13.8% | 7% |
| 2019 | 254.5 | 231.3 | 22.6 | 0.6 | 9.2% | 15.1% | 9% |
| 2020 | 342.8 | 342.3 | 0 | 0.6 | 11.2% | 17.4% | 0% |
| 2022 | 314.6 | 314.0 | 0 | 0.6 | 10.3% | 17.3% | 0% |
| 2024 | 312.9 | 312.3 | 0 | 0.6 | 9.1% | 15.6% | 0% |

**Reading and cautions**
- **The 2010 → 2011 seam is smooth:** CORE gives 9.16% of revenue, this build 9.36%.
- **COVID relief inflates TY2019–2021.** PPP loan forgiveness, the Provider Relief Fund and ESSER were commonly reported on line 1e. 77% of TY2019 dollars sit in tax periods ending after March 2020, mostly June fiscal years. The flag is `covid` (tax periods ending 2020-04 to 2022-06), and aggregates carry `covid_dollars`. For trends, compare 2018 with 2022–2024.
- **Imputation matters most early on.** The median metro has 17% of its 2011 dollars imputed, 7.5% in 2015 and 2.5% in 2019. Single large paper filers dominate some metros: Jacksonville is 82% imputed in 2015. Filter on `gg_imputed_share`, or start in 2015.
- **Paper filers are contribution-heavy.** Contributions are 52–85% of their revenue, against 19% for e-filers. So they hold about 20% of 2011 government-grant dollars with about 10% of revenue. The CORE seam suggests the level is right.
- **It is stable across metros.** Each metro's government share of revenue (excluding hospitals and universities) correlates .82 between 2015 and 2022. The 2022 median metro is 18%; the 10th–90th percentiles are 9% and 37%.
- **E-file EZs with a blank contributions field** (about 20% of e-file EZs, 2020–24) are counted as zero. EZ dollars are small in any case, about $0.6bn a year.

**Files** (`D:/MULTIVERSE/data/govgrants/`)
- `orgyear_govgrants.parquet`: one row per active c3 filer-year. Includes `status` (observed / paper990 / ez), `method`, `own_dist`, `covid`, `gg` and `gg_obs`.
- `govgrants_{county,cbsa}_year.csv` (2022 $):
  - `gg_c3` = `gg_c3_obs` + `gg_c3_imp990` + `gg_c3_impez`;
  - `gg_c3_nohu` and `gg_c3_nohu_obs`;
  - `rev_c3`, `rev_c3_nohu` and `con_c3`;
  - `gg_imputed_share` and `covid_dollars`;
  - `n_c3`, `n_c3_observed` and `n_gg_pos_obs`;
  - CBSA only: `gg_share_rev` and `gg_share_rev_nohu`.
- `govgrants_national.csv`, `coverage_by_year.csv`, `imputation_methods.csv`, and `validation_{org,own_distance,geo}.csv`.

## Proposed schema

There are three constructs, each with one main measure and alternatives. All are per capita or in SAMI form, like the other type-B covariates.

### A. Direct government size

- **A1 (default, keep):** state and local government earnings (BEA). This preserves comparability with runs v1–v8.
- **A2:** state and local government employment from QCEW. State (code 2) and local (code 3) can enter separately, and a "local only" variant avoids the state-capital problem.
- **A3:** local government direct expenditure per capita (Census of Governments), using census years 2002–2022 interpolated. Run the annual-survey years as a sensitivity check.
- **A4:** share of residents working for government (ACS). This is the 2013 paper's measure.

### B. Indirect government size

- **B1 (default):** transfers to persons per capita (BEA CAINC35). The main variant is Medicaid + Medicare + income maintenance; alternatives are all transfers, or Medicaid alone. Annual, county-level and not derived from nonprofit filings, so it is the cleanest exogenous indirect measure. It corresponds to the 2013 paper's "direct benefit payments".
- **B2:** local government payments to private providers (Census of Governments). These are public-welfare vendor payments plus current operations minus salaries and wages, i.e. purchased services. Function codes need checking against the Census classification manual.
- **B3:** federal grants and assistance entering the area. Use CFFR through FY2010 and USAspending from FY2008, with the overlap used to check the splice. This corresponds to "federal grant dollars".
- **B4:** federal awards spent by local nonprofits (Single Audit), including state pass-through. This is the closest to a direct count of contracting with nonprofits.

### C. Mode of delivery (make vs buy)

This is the construct the theories actually disagree about.

- **C1, delivery SAMI:** the residual of log public employment on log public spending, `resid( log( gov_emp ) ~ log( gov_exp ) )`. Positive means the area delivers more directly than its spending predicts. This formalizes "direct government employees compared to overall government spending per capita". It uses the same residual logic as the population SAMIs.
- **C2, labor intensity:** local government salaries and wages / current operations (Census of Governments). A low value means more is purchased.
- **C3, outsourcing share:** indirect $ / (direct payroll + indirect $). For example, B1 / (A1 + B1), or B4 / (state and local payroll + B4).

### How the theories map onto the schema

| Theory | Direct size (A) | Indirect size (B) | Mode (C1: direct-delivery residual) |
|---|---|---|---|
| Government failure | − | 0 | − |
| Interdependence | 0 / + | + | − (more outsourcing → more nonprofits) |
| Crowd-in from total spending | + | + | 0 |

A and B entered together separate the theories, which a single `government` term cannot.

## Fitting this into the multiverse

1. **Keep the `government` block for A,** with A1 as default and A2–A4 as alternatives. Changing a non-add-on block renumbers spec_ids, so add the alternatives in the same rebuild as B.
2. **Add an add-on block `gov_indirect` at the end of `BLOCKS`.** Its default is `character(0)`, so existing specs keep their ids. Alternatives are B1, B2 and B3, plus C1 as a replacement form.
3. **Sample windows differ.** B1 covers every year, B2/A3 need interpolation between census years, and B3 has a splice in 2008–2010. The 2019 and 2022 cross-sections are covered by everything except 990 line 1e.

## Cautions

- **990 line 1e is mechanically tied to the outcome.** Government grants are part of nonprofit revenue, and the line is counted from the same filers as the count and spending outcomes. It is the same problem as the Rupasingha `sk` index. Use line 1e for descriptive government-dependence shares or as an outcome, not as a predictor. B4 has a milder version of the problem.
- **State government is not geographic.** State spending and employment are statewide. State employment in a capital metro reflects administration, not services delivered there. State-level measures are absorbed by state fixed effects, so they identify only in specs without them. Local-only variants avoid this.
- **Overlapping local governments** (county, city, school and special districts) must all be summed to the county area. Use FiSC to check the central cities.
- **Census-year interpolation** has the same issue as the 2016–2018 foundation interpolation. Fork on whether to use only census years.
- **USAspending prime awards miss state pass-through.** Most federal human-services money reaches nonprofits through states (TANF, SSBG, Title IV-E, CDBG), so B3 measures where federal money arrives, not who delivers. B4 (Single Audit) captures pass-through.

## Suggested first build (about a day)

These are cheap and cover 2000–2024 annually:

1. QCEW ownership codes 1/2/3, from files already on disk (A2).
2. BEA CAINC35 transfers (B1).
3. ACS B24080 government workers (A4).
4. C1 from A2 and local direct expenditure, once the Census of Governments individual unit files for 2002, 2007, 2012, 2017 and 2022 are downloaded (A3/B2).

The second wave would be USAspending/CFFR (B3) and the Single Audit (B4). Line 1e from e-file P08 is built for 2011–2024, with paper filers imputed (see above).

## Sources

- [Lecy & Van Slyke 2013 (JPART)](https://intersector.com/resource/nonprofit-sector-growth-density-testing-theories-government-support/)
- [2022 Census of Governments finance files](https://www.census.gov/data/datasets/2022/econ/local/public-use-datasets.html)
- [Annual Survey of State & Local Government Finances](https://www.census.gov/programs-surveys/gov-finances/about.html)
- [ASPEP](https://www.census.gov/programs-surveys/apes/about.html)
- [Government Finance Database (ICPSR 37641)](https://www.icpsr.umich.edu/web/ICPSR/studies/37641)
- [Fiscally Standardized Cities](https://www.lincolninst.edu/data/fiscally-standardized-cities/)
- [CRS R44027: tracking federal awards, covering USAspending and CFFR](https://www.everycrsreport.com/files/2025-04-22_R44027_a6070ba9ac3f414e157a2b1dd12d67a47e3efbfc.html)
- [Federal Audit Clearinghouse data](https://fac.gov/data)
- Line-1e check script: scratchpad `core_govgrant_check.R`. Downloaded CORE files are in `D:/MULTIVERSE/tmp/core_govgrant/` (850 MB; safe to delete).
