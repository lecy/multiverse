# Collinearity and composite covariates

Nov 12, 2026 · script `R/70_collinearity_composites.R` · outputs `D:/MULTIVERSE/data/collinearity/`

Metros only: 386 metros, 2010–2022, complete on all 31 items. "Log form" means extensive (type B) variables are logged and intensive ones are levels, with incomes logged. "SAMI form" means each item is residualized on log P year by year. "Within" means SAMI forms with metro and year means removed.

## 1. Correlation structure

**Metro scaling is the dominant latent factor, and it only touches the extensive variables.** These are their correlations with log P in log form (pooled 2015–2022):

| QCEW wages | gov emp | assoc. estabs | congregations | gov earnings | contributions | # foundations | PF grants | PF assets |
|---|---|---|---|---|---|---|---|---|
| .97 | .95 | .95 | .94 | .94 | .93 | .89 | .80 | .80 |

Among themselves these variables correlate at .85–.99. After the SAMI transformation, most cross-block pairs fall to |r| < .1. For example, government vs congregations goes from .89 to –.01. Intensive variables correlate only moderately with log P: % foreign-born .48, race HHI –.47, Dem margin .44, median household income .41, % BA .38.

**Pairs that stay high after removing size are within-block duplicates.** SAMI form, pooled:

| pair | r |
|---|---|
| log contributions vs log contributions per return | .98 (the same variable once size is removed) |
| PF assets vs PF grants | .92 |
| gov earnings vs gov employment | .92 |
| gov employment vs gov employment share | .91 |
| Rupasingha sk2 vs association establishments | .86 |
| contributions per return vs share of AGI | .82 |
| poverty vs median household income | –.80 |
| per capita income vs median household income | .78 |

The cross-block latent structure is weaker. The strongest pairs involve % nonwhite: with social capital –.66, with turnout –.65, and with % foreign-born .66.

**VIFs.** The default ("full") principled model has one measure per block:

| arm | max VIF | condition number |
|---|---|---|
| log + log P on the RHS | gov earnings 13.2, log P 13.1 (all others < 3.5) | 9.3 |
| SAMI | 2.2 | 2.8 |
| within | 1.1 | 1.5 |

All 30 items entered together give a SAMI-arm max VIF of 78 (the contributions pair), a median of 6.0 and a condition number of 47. The within version has a max of 28 and a median of 1.7.

**The within-metro problem is lack of variation, not collinearity.** Within-metro share of SAMI-form variance, 2010–2022:

- race HHI 0.7%, % BA 0.8%, % foreign-born 0.6%, congregations 0.8%, sk2 0.1%, gov earnings 1.4%, turnout 3.3%, PF assets 6.7%
- the most within variation: population growth 23%, unemployment 19%, contribution share of AGI 13%
- Rupasingha measures are flat after 2014, religion is interpolated from 2010 to 2020, and turnout between elections, so most of the remaining within variation is interpolation.

## 2. Factor structure

Method: SAMI forms pooled over 2015–2022, psych `fa(fm = "ml", rotate = "oblimin")`, with the number of factors from parallel analysis.

**Census/demographic (KMO .65).** Parallel analysis suggests 5 factors; the 4-factor solution is the interpretable one.

| factor | items (loadings) | α |
|---|---|---|
| affluence | median HH income .93, per capita income .91, poverty –.75, % BA .60 | .88 |
| diversity | % nonwhite .95, % foreign-born .76, race HHI –.65 (unemployment .45) | .78 |
| age structure | age SD .88, % 65+ .69 | .77 |
| inequality | Gini .97 (single item) | — |

Five-year population growth loads on nothing (h² = .07). We have no "stability" measure. Homeownership or tenure (ACS B25003 / B07003) would be needed for one.

**Philanthropy (KMO .72).** Two clean factors, correlated .33:
- foundation wealth: assets .98, grants .95, number of foundations .71 (α .91)
- individual giving: contributions .96, per return .99, share of AGI .87 (α .95)

In log form the first factor is size (log P loads 1.05), so all foundation measures plus log contributions form a single "big metro" factor. That is the scaling story.

**Civic/political, joint EFA of every item:**
- Turnout loads with civic engagement: census mail response and sk2, the latter through response.
- Dem margin loads with religion. Congregation density .45, % adherent 1.00 and Dem margin –.38 form a religious-conservative culture axis.
- Religious HHI is its own factor.
- Association establishments load with sk2. They are CBP membership organizations, largely nonprofits, so they mechanically overlap the DV.

## 3. Panel models: default vs all items vs composites

**Setup:**
- DV: year-specific SAMI. Predictors: SAMI forms at unit SD, so coefficients are per SD.
- pooled: 2015–2022 with year FE, metro-clustered SEs.
- within: 2010–2022 with metro + year FE.
- Composites are unit-weighted means of signed z-scores, with weights fixed on 2015–2022. Ten composites: affluence, diversity, age, inequality, religious homogeneity, foundations, individual giving, civic (turnout + census response), religious-conservative, and government (earnings, employment, share).
- ML regression factor scores were also run. They are not recommended: when one loading is near 1, the weights collapse onto that item (MHI .83, % nonwhite .99, sk2 .98).

**Fit and SEs** (pooled R² / median SE per SD):

| | default (6) | composites (10) | all items (25) | items + assoc (26) |
|---|---|---|---|---|
| n_filer_c3 | .626 / .017 | .681 / .016 | .790 / .019 | .822 / .020 |
| n_bmf_c3pc | .506 / .013 | .586 / .013 | .711 / .015 | .734 / .015 |
| exp_c3_nohu | .382 / .034 | .408 / .036 | .497 / .050 | .516 / .055 |
| within R², n_filer | .024 | .041 | .077 | .111 |

**Cancel-out effect.** In the all-items model, whole blocks are jointly significant even though most of their items are not:

| block | items p < .05 | block Wald p | composite estimate (SE), p |
|---|---|---|---|
| foundations | 1 of 3 | < .001 | .123 (.016), < .001 |
| individual giving | 0 of 3 | .12 | –.041 (.016), .012 |
| diversity | 1 of 3 | < .001 | –.087 (.015), < .001 |

Blocks in this table are for n_filer_c3, pooled. Expenditures show the same pattern: foundations 1 of 3 items, Wald p .013, composite .168 (.032). The contribution pair gets SEs of .07–.14 per SD against .016 for the composite.

**Composite estimates** (pooled, n_filer_c3, per SD):

| composite | estimate | | composite | estimate |
|---|---|---|---|---|
| affluence | +.201 | | inequality | +.049 |
| foundations | +.123 | | individual giving | –.041 |
| government | +.099 | | religious homogeneity | –.049 |
| diversity | –.087 | | civic | –.018 (ns) |
| age structure | +.040 | | religious-conservative | –.001 (ns) |

- Counts and expenditures agree in sign on everything except age structure (null for expenditures).
- Cross-section 2019 ≈ pooled.
- The turnout effect in the default model (+.073) does not survive as "civic" once affluence is in.
- Individual giving is negative net of affluence and foundations. This is suspect because of the TCJA break (below).

**Within models: composites do not help.** Within R² stays at .01–.10, composite SEs are no smaller, and few terms are significant. Exceptions are inequality, individual giving and foundations for counts, but the effects are tiny. The binding constraint is identifying variation, not collinearity.

## Implemented (Nov 12): composite fork

`R/41_composites.R` and `R/53_run_composites.R` add the composites to the principled universe as nine extra measure options:

| block | options |
|---|---|
| diversity | `index` |
| philanthropy | `pf_index`, `giving_index`, `both_index` |
| political | `civic_index` (turnout + census response), `culture_index` (civic + religious-conservative) |
| government | `index` |
| need | `index` (affluence), `index_age` (affluence + age structure) |

- Type-B items follow each spec's predictor form; the SAMI arm uses the log base and then residualizes the composite.
- z constants are fixed on metros pooled 2015–2022.
- The FWL duplicate check reproduces twins to 1e-12.
- 162,756 new specifications. The default grid is unchanged because 41 is opt-in.

## Multiverse results (run finished Nov 12, 22:17)

Rebuilt 2026-11-16 on base run v8, with the per-capita floor and % nonwhite as the diversity default.

All 162,756 composite specs fit, with 0 errors. Merged with v8 into `run_principled_cmp_merged/` (661,188 specs). Summary: `R/54_summarize_composites.R` → `run_principled_cmp/summary_*.csv`. Shareable page: https://claude.ai/artifact/TANCnJHvdQMpR2YGxoFiLS (local copy `notes/composites-results-page.html`).

Full model, all specifications. "Sig." is p < .05 with the median's sign; "|t| wins" is the share of cross-section/pooled pairs where the index beats its default twin.

| option | median partial r | sign stable | sig. | vs default |
|---|---|---|---|---|
| foundation index | .100 | 95% | 73% | assets .095 / 93% / 72%; \|t\| wins 70–71% |
| diversity index | –.024 | 70% | 30% | % nonwhite –.026 / 75% / 32% |
| affluence index | .080 | 95% | 68% | % BA .165 / 98% / 92%; \|t\| wins 5% (within: 51%) |
| civic index | .034 | 88% | 34% | turnout .052 / 93% / 49% |
| government index | –.001 | 51% | 13% | S&L earnings .025 / 69% / 36% |
| giving index | .014 | 61% | 31% | |
| religious-conservative index | –.004 | 55% | 16% | |

Spill-over onto the other blocks is a median |Δ partial r| ≤ .02, and ≤ 7% of their coefficients lose significance.

Reading: in the multiverse, composites are a measurement choice, not a collinearity fix. They help where items share one construct (foundations; the diversity index only ties % nonwhite) and dilute blocks that one item carries (% BA, turnout, government earnings).

## Time spacing for within models (`R/71_fe_frequency.R`, outputs `data/collinearity/p4_*`)

**5-year waves don't add within variance.** FE within variance is how far metros drift from their own mean over the window, which depends on the window's length, not how often it is sampled.

Balanced panel, 383 metros, 2010–2022. Estimates under annual FE, 5-year waves (2012/17/22 or 2010/15/20), stacked 5-year differences and the 2012→2022 long difference are mostly similar:
- Within R² rises a little with spacing (filer counts, composites: .041 annual → .061–.069 waves → .099 long difference).
- SEs grow with the smaller N, so median |t| doesn't improve.
- The two wave choices disagree on some terms (affluence +.019 vs –.016), so few waves make the estimates fragile; 2020 is also a splice and COVID year.
- Only annual first differences diverge: they attenuate toward zero (affluence .000, foundations .001, turnout .003). That is classic measurement-error attenuation in short differences.

**Annual changes are mostly noise for model-based or sampled series.** Shown as var(Δ5)/var(Δ1) and the autocorrelation of annual changes; a steady trend gives 25, a random walk 5, pure noise about 1.

| series | var(Δ5)/var(Δ1) | annual-change autocorrelation | reading |
|---|---|---|---|
| SAIPE poverty and income | 1.5–1.6 | –.42 | annual change is mostly noise |
| IRS contributions | 2.1 | –.31 | annual change is mostly noise |
| foundation grants | 2.9 | –.18 | annual change is mostly noise |
| filer counts (outcome) | 2.0 | –.20 | annual change is mostly noise |
| PEP race and age | 13–19 | .5–.7 | slow, real trends |
| turnout, religion | 15–21 | .71–.95 | interpolated straight lines, not observed change |

Spacing alone doesn't remove noise; averaging adjacent years would.

**The lever is window length.** Within share for the covariates available since 2000, by window:

| series | 2010–22 | 2000–22 |
|---|---|---|
| race HHI | 0.7% | 2.1% |
| % 65+ | 1.3% | 4.1% |
| age SD | 1.8% | 5.1% |
| foundation assets | 6.7% | 11.7% |
| filer counts | 1.2% | 2.7% |
| BMF counts | 2.8% | 5.8% |

Within SDs are 40–80% larger, so SEs should fall by about a third. Costs:
- the CORE→SOI splice at 2011
- ACS measures need a 2000 census anchor
- IRS contributions start in 2010 in the panel

## Within-metro over 2000–2022 (`R/57_run_within_long.R`, `run_principled_wl/`)

**Setup.** 27,216 `within_long` specs (all OK). These are the within specs, default grid plus composite options, re-run on 2000–2022. 23,472 specs whose data start in 2010/2012 are skipped, since they would reproduce their twins: the net outcomes and the contribution and giving options. The engine change is a `within_long` case in `run_spec()` only; IDENT and the default grid are unchanged.

**Result: more data, no more precision.** Paired against the 2010–2022 twins, with 1.77x the metro-years:

| | median SE ratio | median \|t\| (13 → 23 yrs) | share p < .05 | same sign | partial r correlation |
|---|---|---|---|---|---|
| counts | 1.12 | 2.02 → 1.86 | 52% → 47% | 84% | .83 |
| expenditures | 0.93 | 1.08 → 1.09 | 21% → 21% | 76% | .76 |

The outcome's within variance grows as much as the covariates' does. That includes the CORE→SOI splice, and metro-clustered SEs count smooth trends as little new information.

**Terms that gain:**
- counts: affluence index, % 65+, # foundations, religious-conservative index
- expenditures: % BA, Gini, per capita income

**Terms that lose:**
- counts: % nonwhite and the diversity index (effect roughly halves), unemployment, Gini, % foreign-born, civic index

**Caveats:**
- The religious-conservative gain is probably an artifact. RCMS 2000 lacks Black Protestant counts, so the 2000→2010 adherence change is partly a measurement change.
- % BA, % foreign-born and Gini are straight-line interpolations between the 2000 census and 2010 ACS.
- Census response is interpolated from 1997 to 2000, then carried flat after 2014.

**Reading:** keep `within_long` as a robustness stratum, not a replacement. It changes the estimand (adds the 2000s, crosses the 2011 splice) without buying precision.

## 4. What this means for the multiverse

1. **The principled universe already avoids within-block collinearity**, because each block enters one measure at a time. The default model is not collinear in the SAMI or within arms. The only real collinearity is metro scaling in the log + log P arm (government earnings VIF 13), and the SAMI arm removes it by construction. The VIF gap between arms is itself a predictor of DV-form sensitivity (design §2 meta-hypothesis).
2. **Composites as an operationalization fork.** Add an additive composite as one more `BLOCKS$alt` option per block. Composites beat any single item on fit, while a kitchen-sink item set loses interpretability (VIF 78).
3. **Block-level inference.** Report block Wald tests or block partial R² wherever a spec enters more than one item per block, e.g. `political:both`.
4. **Regroup blocks:**
   - turnout → civic engagement, with census response
   - Dem margin → religious-conservative culture, with congregation density and adherence
   - religious HHI → its own diversity-type item
   - philanthropy splits into foundations and individual giving; do not pool them
5. **Data cautions:**
   - Association establishments and sk2 overlap the DV; keep them kitchen-only, like the published Rupasingha `sk`. Census response is the clean civic indicator.
   - Contributions: the TCJA break in 2018 cut the itemizer share from 21% to 6% and median contributions per return from $1,227 to $804. Post-2018 within variation is a tax-law artifact. `l_contrib_total` and `l_contrib_per_return` must never enter together.
   - The CDC SVI is available for counties but does not fit here. Its percentile ranks don't aggregate to CBSAs, and it fuses poverty, minority status and age into one index, which erases the constructs we want to keep apart. Our own indices are preferable.
