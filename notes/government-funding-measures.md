# Direct vs indirect government: data and measurement proposal

Written 2026-11-16. The first-wave measures were built the same day and are in the analysis panel; no multiverse run uses them yet. See "Built: first wave" below.

## Built: first wave (2026-11-16)

**Scripts**
- `R/22_cov_government.R` → `data/cov/cov_gov.csv`
- `R/12_cov_bea.R` now also reads CAINC35 (`tr_*` columns)
- `R/30_assemble_analysis.R`: interpolation of the new series and the derived ratios (`derive_gov()`)
- `R/91_government_measures.R` → `data/government/`

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

There is a single `government` block (`R/40_multiverse.R`):

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

The second wave would be USAspending/CFFR (B3), the Single Audit (B4), and e-file P08 for line 1e from 2020 on.

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
