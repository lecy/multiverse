# Market competition (niche quality) measures

Built 2026-11-17 by `R/build/C_npo_panel/23_cov_compete.R`. The measures are in the CBSA analysis panel (`data/analysis/cbsa_year.parquet`, 69 `mkt_*` columns, 2000–2024; the pre-merge panel is backed up in `data/analysis/pre_compete_20261117/`). They are not in the multiverse grid yet.

**Idea.** When a few organizations hold most of a metro's resources, smaller organizations face a thinner niche, and growth of the sector as a whole may be held back. The measures follow the Nonprofit Open Data Collective's [npcompete](https://github.com/Nonprofit-Open-Data-Collective/npcompete) package (HHI, normalized HHI, concentration ratios, Gini). They are recomputed here because the package is pre-release and has several errors:

- Density denominators are summed over organization rows, not over counties.
- CR comes back on mixed 0–1 and 0–100 scales.
- The normalized-HHI correction is applied once per organization.
- The fundraising-efficiency function does not run.

## Construction

- **Organizations.** The DV universe: active 501(c)(3) filers, with max(revenue, expenses) of at least $50k in 2010 dollars. They are located and spliced as in `06_npo_assemble.R` (CORE 2000–10, SOI 2011–19, e-file 2020–24).
- **Resources.** Total revenue (`rev`) and contributions (`con`, Part VIII 1h, which includes government grants), both in 2022 dollars.
- **Markets.**
  - The whole metro.
  - Metro × NTEE major group (10 NCCS groups, with hospitals and universities split out).
- **Variable names.** `mkt_<metric>_<rev|con>_<c3|nohu|local>`.

| metric | definition |
|---|---|
| `hhi` | Σ s², on a 0–1 scale; at least 1/n by construction |
| `nhhi` | (HHI − 1/n) / (1 − 1/n) |
| `xs` | n·HHI = 1 + CV² of the shares |
| `cr1`, `cr4` | share of revenue held by the largest 1 and the largest 4 organizations |
| `gini` | Gini of organization resources, with the n/(n−1) correction |
| `small` | share of resources held by organizations under $1M (2022 dollars) |
| `shhi`, `shhio` | subsector HHIs averaged with resource weights (`shhi`) or organization weights (`shhio`) |
| `scr4o` | subsector CR4, averaged with organization weights |
| `mix` | HHI of metro resources across the NTEE major groups |
| `mkt_small_n_<univ>` | share of active filers under $1M (a count measure) |

## Which universe: use `local`

- In `c3` and `nohu`, the largest organization in many metros is a national or regional entity that books its revenue at headquarters:
  - Kaiser Foundation Hospitals: $71B in Oakland, coded E31, so it is not caught by the hospital codes.
  - UPMC in Pittsburgh: NTEE "UND".
  - Cleveland Clinic (E03).
  - Fidelity Charitable in Boston.
  - Battelle in Columbus.
- So whole-metro HHI in those universes mostly records where headquarters are.
- `local` drops the following from `nohu`:
  - health (E–H)
  - international (Q)
  - research institutes (U)
  - grantmakers and donor-advised funds (T)
  - university support organizations (B11)
  - uncoded organizations
  - Fidelity Charitable (dropped by EIN)
- `local` and `nohu` revenue HHI correlate only .39 across metros (2022), so the universe choice matters more than the metric.
- Some national headquarters remain (Feeding America in Chicago, Red Cross in DC, ALSAC in Memphis). They are small shares of those large metros.
- The most concentrated `local` metros in 2022 are small metros and college towns with one dominant regional organization: Sioux Falls (Good Samaritan Society), Daphne–Fairhope, McAllen (IDEA charter schools), Corvallis, Napa.

## Mechanical links to the DV: read before modeling

All figures below are for metros in 2022, `local` revenue, with log n = log active filers (nohu).

| measure | r with log n | r with filer SAMI | r with spending SAMI |
|---|---|---|---|
| log HHI | −.68 | −.29 | −.03 |
| nHHI | −.38 | −.21 | .07 |
| CR4 | −.68 | −.34 | −.06 |
| Gini | .59 | .13 | .32 |
| log xs | .70 | .19 | .30 |
| log shhio (subsector, org-weighted) | −.84 | −.33 | −.12 |

- HHI, CR and the subsector means fall with n by construction. `xs` and Gini *rise* with n, because the size distribution is heavy-tailed: more organizations means a longer upper tail. So `xs` does not remove size.
- `nhhi` is the least tied to the count. Use it as the primary concentration covariate.
- Gini measures something different: inequality across the whole size distribution, not dominance at the top. It correlates only .29 with nHHI. It is a candidate second measure.
- For a growth test, use concentration at t (or a baseline year) to predict later change in density. That avoids most of the simultaneity.
- Within-metro variance is small: 10–18% of the total for most measures, 2010–2022.

## Trends and splices (metro medians, local revenue)

- Gini rises from .77 (2000) to .81 (2024): resources are increasingly unequal within metros.
- HHI falls from .068 to .054 as metro sector size grows.
- Breaks at the source splices:
  - 2010→2011: HHI drops about 12%, because SOI covers more small filers than CORE.
  - 2020: HHI rises and the small-org share drops (.747 → .714), because 990-EZ e-filers are missing in TY2020. This is the same undercount as in the filer counts.
- Use year fixed effects. Treat within-metro changes across 2010/11 and 2019/20 with care.

## Data defect found while building this (fixed in the DV 2026-11-17)

- E-file records code NTEE as "UND" for about 16% of active 501(c)(3) filers in 2020–24. About 90% of these can be recovered from the BMF.
- `06_npo_assemble.R` used the raw code, so for 2020–24:
  - About 590 hospitals and universities a year were not excluded from the `nohu` series.
  - Human-services counts (`*_hs`) were 12–13% low.
- 06 now backfills the codes with the same rule as 23. 06 and 30 were rerun; backups are in `npo/pre_ntee_20261117/` and `analysis/pre_ntee_20261117/`. Only seven columns changed. National totals, 2022 (new ÷ old):

| column | new ÷ old | notes |
|---|---|---|
| `n_filer_c3_nohu` | .998 | SAMI r(old, new) = 1.000 |
| `exp_c3_nohu` | .934 | median metro unchanged; 10% of metros move ≥ 9%; Winchester VA moves 1.6 log points; SAMI r = .972 |
| `rev_c3_nohu` | .935 | |
| `*_nohu_net` | .93 | |
| `n_filer_c3_hs` | 1.14 | SAMI r = .988 |
| `exp_c3_hs` | 1.13 | |

  Before 2020, the changes are under 1%.
- **Multiverse run v10 predates the fix.** 275,160 of its 605,352 specs use the five `nohu` outcomes. The four spending and revenue outcomes moved; filer counts did not.

## Files (D:/MULTIVERSE/data/)

- `cov/cbsa/cbsa_compete.csv`: CBSA × year measures, read by `30_assemble_analysis.R`.
- `compete/compete_subsector.csv.gz`: every metro × NTEE-group market, with n, total and all the metrics.
- `compete/codebook_compete.csv`
- `compete/largest_org_by_metro_year.csv.gz`
- `compete/diag_*.csv`: medians by year, mechanical correlations, within-metro shares, the correlation matrix, and 2022 metro extremes.
