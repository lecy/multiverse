# Need, social capital, and diversity: measurement checks (2026-11-16)

Script: `R/89_need_socap_diversity.R` (modes build, describe, horse, horse_summary, figs).
Outputs: `D:/MULTIVERSE/data/need_socap/`. Figures: `notes/need-socap-figures/`.

New metro measures (ACS 5-year 2015–2022, county tables summed to 2023 CBSAs):

- **Metro Gini (`gini_m`).** A grouped Gini from the 16 income brackets in B19001. It correlates .87–.92 with the Census-published metro Gini.
  - The panel's `gini` and `med_hh_income` are population-weighted means of county values. They therefore leave out inequality between counties.
- **Income-quintile diversity (`econ_div`).** One minus the HHI of household shares in the five *national* income quintiles (cutoffs from B19080), rescaled so that 1 means the national mix.
- **Shares of households in the US top and bottom quintiles** (`hh_high`, `hh_low`).
- **Need rates:**
  - deep poverty (below 50% of the poverty line) and near poverty (below 200%), both from C17002;
  - SNAP households (B22003);
  - rent burden of 30% or more (B25070).
- **Attachment rates:** homeownership (B25003) and living in the same house a year ago (B07003).
- **Indices (PC1 within each year):**
  - need index: poverty, deep and near poverty, SNAP, unemployment, rent burden, and −log median income;
  - civic index: turnout, census response, homeownership, same house, and log congregations per capita.

Evidence comes from two sources:

- **Benchmark:** the default model in sami/sami form, cross-section OLS.
- **Horse race:** 1,836 default specs (filers, registered organizations, and spending without hospitals and universities; cs 2022, cs 2019 and pooled; every DV form, predictor form, unit and estimator; no winsorizing). Each spec is refit with the need block or the political block swapped, 31,212 fits in all.

## 1. Need: poverty or inequality?

- **The old slide plots are mostly about size.** In raw counts, Gini (r = .40) and median income (.57) look strongly positive. That is because large metros are more unequal and richer: Gini correlates .42 with log population, and median income .48.
  - Against the filer SAMI, the Gini correlation drops to .14. Poverty (−.42) and income (.40) hold up.
  - See `fig_ns_raw_vs_sami`.
- **The need measures are one dimension, and poverty already is that dimension.**
  - PC1 explains 61% of the variance in the seven need items.
  - The need index correlates .94 with poverty.
  - Near poverty, deep poverty and the index perform like poverty in the horse race (median partial r for filers: −.03, −.07, −.04, against −.02 for poverty), with no fit gain (ΔR² ≈ 0).
  - In the added-variable plot, the index has partial r −.05 once poverty is in the model.
  - **An index buys nothing over poverty.**
- **Inequality is not a need measure. It is a separate, positive construct.**

  | | Gini alone | Gini with poverty | Gini sig. at 5% (with poverty) |
  |---|---|---|---|
  | filers | +.08 | +.13 | 81% of specs |
  | registered | +.10 | +.10 | 67% |
  | spending | +.11 | +.09 | 60% |

  - Adding Gini also makes the poverty term more negative for filers (−.02 → −.09).
  - Gini takes .07–.10 off % BA, because educated metros are more unequal.
  - The fit gain is small (R² +.002 to +.005).
  - The county-weighted Gini behaves the same way (+.12 for filers).
  - Gini survives a control for the top-quintile share (`pov_gini_top`: Gini +.13; top share about 0). So it is dispersion, not just the affluent tail. In the single benchmark fit the top share looked like the driver, but it did not hold across specs.
- **Income-quintile diversity (`econ_div`) is null** in every outcome (median |r| < .02). How *spread across the national income distribution* a metro is says nothing about density. *Inequality within* the metro does.
- **Reading.** Poverty is the need measure. Gini enters as heterogeneity: Weisbrod-style demand heterogeneity, or co-location of affluent donors with need. It belongs in its own block, not as a substitute for poverty.
  - Candidate fork: `need:pov_gini`, which adds `gini_m` to the default. It needs `gini_m` from 2000 for the within stratum. The SF3 2000 brackets exist, and 2009–2014 would need an ACS fetch.

## 2. Social capital

- **What loads on what** (size-adjusted items, 3-factor varimax; `fig_ns_civic_factors`). Three factors, not one:
  1. *Organized interests:* Rupasingha business, professional, political, civic and fitness associations.
  2. *Participation and attachment:* turnout .64, census response .52, homeownership .72, same house .46, bowling and golf .55–.57.
  3. *Congregations:* congregations per capita .99, adherence .43, religious associations .57.
  - The Rupasingha index mixes factors 1 and 2. It contains association counts, which overlap the outcome (sk2 vs filer SAMI r = .47).
- **Turnout still dominates once census response is in.**
  - Turnout is unchanged: filers .15 → .16, registered .23 → .25, spending .07 → .07.
  - Census response is *negative* given turnout (filers −.09, registered −.12; 52% and 80% significant). Alone it is also negative (−.08 and −.11).
  - Census response correlates .50 with turnout but carries a compliance or homogeneity component (it is higher in white, older, homeowning metros), and that component is not positive for density.
  - Added-variable plots: census response | turnout, −.16; turnout | census response, +.12.
- **The civic index adds nothing beyond turnout** (partial r .01; ΔR² ≈ 0). Alone it is weaker than turnout, and much weaker for registered organizations (ΔR² −.02).
- **Congregations per capita is the one civic item that adds:**
  - turnout + log congregations: congregations +.12 / +.21 / +.09, significant in 74% / 97% / 53% of specs;
  - R² for registered organizations rises +.025;
  - turnout gets slightly stronger.
- **Attachment items:**
  - Homeownership is negative given turnout (−.05 / −.08 / −.11).
  - Same house a year ago is positive for filers (+.12) and absorbs part of turnout (.15 → .09).
- **What is still missing:**
  - *Civic participation:* volunteering and civic-organization measures from the Chetty et al. Social Capital Atlas (county; its civic-organization count partly overlaps the outcome); midterm or local turnout instead of presidential.
  - *Attachment and cohesion:* tenure length (B25038), commute time.
  - *Field-specific need:* uninsured rate, food insecurity, homelessness counts, matched to subsector outcomes.

## 3. Diversity and civic participation (2019, 387 metros; `fig_ns_diversity`, `diversity_civic.csv`, `diversity_joint.csv`)

Partial r after controls (size, education, poverty, income, % 65+, division FE); in parentheses, the value with % nonwhite also controlled:

| diversity | turnout | census response |
|---|---|---|
| racial (1 − HHI) | −.15 | −.24 |
| % nonwhite | −.38 | −.28 |
| % foreign-born | −.54 (−.42) | −.13 (+.06) |
| religious (1 − HHI) | +.12 (+.06) | −.07 (−.12) |
| age (1 − HHI) | +.38 (+.37) | −.01 |
| Gini | −.13 (−.08) | −.13 (−.09) |
| income-quintile diversity | +.12 | .00 |

- **Racial composition, not diversity, carries the racial association.**
  - With % nonwhite in the joint model, racial fractionalization is not separately estimable from composition.
  - With fractionalization instead, its coefficient is −.13 for turnout (p < .1) and −.21 for census response.
  - Foreign-born share is the strongest single correlate of turnout. That is largely eligibility: the denominator is the voting-age population, not citizens.
- **Age diversity is the strongest *positive* correlate of turnout** (standardized β .30–.31 in the joint model, net of % 65+). It is unrelated to census response.
- **Religious diversity is weak and changes sign between the two outcomes.**
- **Economic inequality goes with lower participation; economic spread goes with higher turnout.** Gini β −.20 to −.22 for turnout, p < .01. Quintile diversity β +.08 to +.10, p < .001.
- **This is consistent with "hunkering down" for race and inequality, but not for age or religion.** The two participation measures diverge: turnout responds to age and economic structure, census response to race.

## 4. Index vs dominant item: how to show it

`fig_ns_index_vs_item` is a grid of added-variable plots.

- **How to read a panel.** Each panel plots a candidate (an index or a second measure) against the filer SAMI. Both are taken net of the block's dominant item and the rest of the default model.
  - A flat line means the dominant item suffices.
  - A sloped line means the candidate carries independent information.
  - Labeled metros are where the candidate and the dominant item disagree most.
- **Results:**
  - The need index is flat given poverty.
  - Gini is not flat given poverty.
  - The civic index is flat given turnout.
  - Census response slopes *down* given turnout.
- **Companion figure.** `fig_ns_horse` shows the same comparisons across the sub-universe: median and 10th–90th percentile partial r for every alternative set and outcome.

## 5. In the multiverse: run v9 (`R/92_ineq_cong_fork.R`)

Two add-on blocks; each adds one covariate to the full default model.

- **`inequality:gini`** adds `gini_m`, with poverty kept. The ACS series was extended to 2010–2024 so the block also runs in the within stratum; 11% of `gini_m`'s variance is within-metro.
- **`congregations:cong`** adds the RCMS congregation count as an extensive (type B) covariate. Within-metro variance is 0.8%, so it has no within stratum.

The new blocks add 34,584 specs, all of which fit. v9 has 533,016 specs and every v8 spec_id is unchanged.

**Gini** (median partial r, share significant at 5%):

| | cs | pooled | within |
|---|---|---|---|
| filers, excl. H/U | .13 (82%) | .13 (82%) | .03 (7%) |
| filers + registered, incl. | .11 (75%) | .11 (71%) | .03 (18%) |
| spending, excl. H/U | .09 (55%) | .11 (75%) | .02 (4%) |
| spending, incl. | .07 (42%) | .10 (70%) | .05 (37%) |

- **Sign stability is .94–1.00 in cs and pooled.** Within metros, changes in inequality don't move density.
- **Gini moves the defaults more than any add-on so far.**
  - % BA drops .07–.09 (e.g. counts cs .29 → .19). Educated metros are more unequal.
  - Poverty moves .05–.08 more negative: counts −.00 → −.06; spending +.07 → ±.01, with sign flips in 14–28% of pairs.
  - Foundation assets drop .02–.03.
  - Median |Δr| across all default terms is .015–.017 (max .10), with no sign flips in the within stratum.

**Congregations** (median partial r):

- Counts .07–.10, sign stability .71–.79; spending .03–.05.
- The result depends on the predictor form:
  - SAMI form: positive (.09–.18, significant in 59–93% of specs), and strongest for registered organizations (.18, 93%).
  - Per-capita form: also positive.
  - Raw and log counts: near zero or slightly negative. The count is dominated by size (log congregations per capita correlates −.49 with log population).
- It barely moves the defaults (median |Δr| .005, max .02).

**Decision importance** (58 part A on v9, η² of default vs default + the add-on, per benchmark term):

| | % BA | poverty | % nonwhite | foundation assets | turnout |
|---|---|---|---|---|---|
| Inequality, counts | .16 | .17 | .01 | .01 | .00 |
| Inequality, spending | .22 | .35 | .01 | .02 | .00 |
| Congregations, counts | .01 | .03 | .06 | .01 | .00 |
| Congregations, spending | .00 | .01 | .01 | .00 | .00 |

- **Adding the Gini is a consequential choice for % BA and poverty.** For poverty it is on the order of the population-lag block (η² .29–.30).
- **Congregations is not consequential.** Its largest η² is .06, for % nonwhite in counts.
- Rows shared with v8 are unchanged. Outputs are in `D:/MULTIVERSE/data/outline/v9/`.

## Caveats

- Sections 1–4 are cross-sectional and use 2015–2022. Only the v9 Gini block (section 5) uses the 2010–2024 series and runs in the within stratum.
- Census response is the 2014 wave carried forward. Rupasingha category counts are from 2014.
- Turnout uses the voting-age population, so foreign-born and noncitizen shares lower it mechanically.
- The grouped Gini ignores inequality within brackets (it runs about .01 below the published Gini). It also uses 2023 delineations while the ACS metro tables use 2020 ones.
