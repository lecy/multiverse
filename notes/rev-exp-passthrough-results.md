# Measuring nonprofit sector size: revenue vs expenses, and pass-through grants

First results for the research note (built 2026-11-12). Code: `R/09_npo_grants.R`, `R/09b_revexp_analysis.R`; all tables are in `D:/MULTIVERSE/data/aux_revexp/`. Figures are in `rev-exp-figures/`.

**Sample.** Active 501(c)(3) public charities: 990 and 990-EZ filers with max(revenue, expenses) ≥ $50k in 2010 dollars. Sources follow the main splice: CORE 2000–2010, SOI 2011–2019, e-file 2020–2024. Year-over-year pairs are formed only within a source era, so pairs that cross the 2011 and 2020 seams are excluded.

## 1. Revenue is lumpier than expenses

**Consecutive filings, same organization** (4.95M year pairs, 2001–2024):

| | Revenue | Expenses |
|---|---|---|
| Median absolute % change from prior year | 15.8% | 12.7% |
| 10th–90th percentile of % change | −32% to +61% | −22% to +53% |
| Share of org-years with a change of more than ±20% | **42.6%** | **35.8%** |
| … of which increases / declines | 26.5% / 16.1% | — |

**How often do the two move together?**
- When revenue changes by more than 20%, expenses also change by more than 20% in 58% of those years. Expenses move by that much *in the same direction* only 47% of the time.
- When expenses change by more than 20%, revenue does too in 69% of cases.
- Correlation of the % changes: Spearman 0.44; Pearson (winsorized at 1%/99%) 0.39.

**By size class** (prior-year expenses):

| Size | Revenue > ±20% | Expenses > ±20% | Spearman |
|---|---|---|---|
| < $100k | 52% | 53% | 0.26 |
| $100k–1M | 46% | 38% | 0.47 |
| $1M–10M | 35% | 25% | 0.50 |
| $10M+ | 22% | 15% | 0.58 |

The gap is widest for mid-sized organizations. Among the smallest filers expenses are about as volatile as revenue.

**Within-organization dispersion** (335k orgs with at least 5 annual changes):
- Median SD of % change is 0.36 for revenue and 0.29 for expenses, a ratio of 1.16.
- Revenue is the more volatile series for 63% of organizations, and for 72% of those with $10M+ in expenses.
- The median within-org correlation of the two changes is 0.48.

**Same return, current vs prior year** (e-file 990 Part I, 1.15M returns, 2020–2024). This version is free of matching and restatement issues:
- Revenue changes by more than ±20% in 51% of returns; expenses in 41%.
- Expenses move by more than 20% in the same direction in 49% of the years revenue does.
- Spearman correlation 0.43.

**Over time.** The shares are stable from 2001 to 2019: about 40% for revenue and 33% for expenses. They jump in 2021–2022, to 56% vs 46% and then 50% vs 46%, during the pandemic relief money and its unwinding.

**Aggregated to metros, the gap narrows but persists:**
- SD of the annual log change in metro totals: revenue 0.135, expenses 0.118.
- Metro-years with a change above 20%: revenue 7.1%, expenses 5.3%.
- Year-to-year persistence of metro SAMIs is slightly higher for expenses: r = 0.989 vs 0.987, and 0.983 vs 0.975 excluding hospitals and universities.

## 2. Pass-through grants

Pass-through here means grants paid to domestic organizations and governments: Form 990 Part IX line 1, or line 10 for 990-EZ filers. Grants to individuals leave the sector, so including them gives an upper bound. Coverage starts in 2012, because CORE 2010 and SOI 2011 record grants for too few filers. The SOI-era 990-EZ extract has no grants line, so those filers count as zero.

| | 2012 | 2016 | 2019 | 2022 | 2024 |
|---|---|---|---|---|---|
| Share of active charities paying grants to organizations | 19.1% | 20.3% | 20.6% | 21.7% | 22.3% |
| Share paying any grants | 30.6% | 32.0% | 32.7% | 31.2% | 32.0% |
| Grants to organizations ($bn, 2022 dollars) | 68 | 92 | 122 | 173 | 210 |
| Revenue reduction after netting org grants | 3.9% | 4.3% | 4.9% | 5.6% | 5.8% |
| Expense reduction after netting org grants | 4.2% | 4.5% | 5.2% | 5.9% | 6.2% |
| Expense reduction after netting all grants (upper bound) | 8.4% | 9.2% | 9.7% | 10.7% | 10.8% |
| Combined expense reduction, charities + foundations (foundation grants also netted) | 7.1% | — | 8.6% | 9.5% | 9.6% |

**Where pass-through money sits:**
- It is extremely concentrated: the top 1% of grantmaking charities pay 85–89% of all grants to organizations.
- By subsector (2015–2024), philanthropy, voluntarism and grantmaking (NTEE T, including federated funds and donor-advised-fund sponsors) passes through 71% of its expenses. Community improvement (S) passes through 37%, food and nutrition (K) 45%, and religion-related (X) 17%.
- Pass-through is about 1.6% of expenses in health (E) and about 4.7% in education (B).
- The adjustment therefore matters most for measures that include grantmaking intermediaries.

## 3. Does skipping the adjustment bias scaling estimates?

β is the slope of log(total) on log(metro population), estimated across about 387 metros each year from 2012 to 2024.

| Measure | β gross − β net (mean; range) | Typical SE of β |
|---|---|---|
| Expenses, all 501(c)(3) | +0.004 (0.001 to 0.006) | 0.044 |
| Revenue, all 501(c)(3) | +0.004 (0.002 to 0.005) | 0.043 |
| Expenses, excl. hospitals & universities | +0.011 (0.007 to 0.014) | 0.033 |
| Revenue, excl. hospitals & universities | +0.010 (0.007 to 0.012) | 0.033 |

- **Direction:** the bias is positive in every year and series. Pass-through intermediaries are concentrated in large metros: the pass-through share of expenses rises by about 0.2–0.6 percentage points per log unit of population (significant at 5% only in 2022). Gross totals therefore overstate how superlinear sector resources are.
- **Size:** the bias is small, about a tenth to a third of one standard error. Gross and net β agree to two decimals (about 1.24–1.29). Netting moves sector *levels* by 4–11%, but barely moves the cross-metro *slope*.
- **Implication for the note:** the pass-through argument is strongest as a claim about sector size and about comparisons of intermediary-heavy places. It is weak as a claim about scaling exponents.

## 4. In the multiverse

- The resource construct now carries a measure fork: {expenses, revenue} × {gross, net of grants to organizations}, both including and excluding hospitals and universities.
- The universe grows to 286,440 specifications.
- Net series start in 2012, so within-metro net specifications use 2012–2022 rather than 2010–2022.
- **Run results** (`D:/MULTIVERSE/data/spec/run_principled/`; comparison tables in `spec/compare_resource_measure*.csv`):
  - **Cross-sections and pooled:** the measure fork barely matters. It explains on average 0.2–0.9% of the variation in covariate partial correlations, against 15–17% for the DV transform and 22–44% for the estimator.
    - The largest effects are on % BA and poverty (eta² about 0.03 and 0.01), almost all from net vs gross.
    - For example, % BA has r = 0.24 on gross expenses and 0.21 on net (excl. hospitals and universities).
    - Revenue vs expenses hardly moves anything.
  - **Within metros:** the fork explains 7–9% of the variation (half revenue vs expenses, half net vs gross). Part of this comes from the shorter net window (2012–2022). Within-metro covariate effects are near zero for every measure.
  - **Reading:** at the metro level, the choice of resource measure is a level and measurement issue more than a source of different substantive conclusions. The note's case rests mainly on parts 1 and 2.

## Open items

- Pre-2012 pass-through: CORE 2000–2009 has no Part IX split (the old Form 990 line 22 was "grants and allocations" combined). A pre-2008 total-grants field could extend the series as a cruder version.
- SOI-era 990-EZ grants are unknown. In e-file years, EZ grants are a small share of the total; this should be quantified for the note.
- A metro-level comparison of 990 expenses with BLS nonprofit payroll (where the money is spent) is in `diagnostics/bls_vs_990*.csv`. It is a natural companion section on headquarters attribution.
