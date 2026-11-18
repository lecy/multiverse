# Card evidence refit with the lean default model

2026-11-17. Script: `R/paper/96_lean_card_evidence.R` (log `D:/MULTIVERSE/logs/96.log`). Outputs:
`D:/MULTIVERSE/data/outline/v11/single/*.csv`. Figures: `paper/figures/d1_lean_outcomes.png`,
`d2_lean_units.png`, `d2_lean_size.png`, `d4_lean_dv_form.png`, `d8_lean_history.png`,
`d10_lean_money.png`, `d12_lean_age.png`.

**Lean model** (13 terms, BLOCKS_LEAN defaults): % BA, foundation assets, % nonwhite, turnout,
poverty (the five benchmarks), S&L government earnings, Medicaid transfers, the 10-year growth
kernel, the pre-1970 endowment, % 65+, metro Gini, religious fractionalization (1 − rel_hhi), age SD.
Totals (foundation assets, S&L earnings, Medicaid, endowment) enter as `flog(x, pop)` with log P on
the right (log-log) or residualized on log P (SAMI form); rates as is. Coefficients per SD; partial
r = t / √(t² + df), df counted in places.

**Conventions.** Each table mirrors the 58 part (or script) that produced the old one: same year,
sample rule, standard errors and option construction. Sample: 385 metros (Memphis and Mankato have
no endowment). Every table was also refit with the **old six covariates on the same 385-metro
sample** ("old6, current data" below). Comparing the card with old6 shows how much comes from the
data and the sample (spending was corrected on 2026-11-17, and two metros were dropped). Comparing
old6 with lean shows how much comes from the model. Where the two old columns agree to ±.02, only
the card value is shown.

**Headline.** Three results move in every card:

1. **Poverty turns clearly negative for filers.** It is −.29 in the lean model against −.10 in the
   old one. In D1 poverty no longer flips sign between populations, and in D2 the size reversal is
   gone.
2. **Foundation assets roughly halve** (filers .41 → .21; spending .26 → .11). Other lean terms
   absorb that share, and no single one does it. Dropping the endowment alone restores only .28.
3. **Turnout goes to zero for filers and spending** (.09 → .01; .07 → .04) but stays clear for
   grassroots (.24).

% BA gets stronger (.24 → .37). % nonwhite weakens a little (−.26 → −.20). S&L earnings drop by a
third (.33 → .23). The new terms that matter are the endowment (.32 filers), Medicaid (.24 filers,
.18 spending), religious fractionalization (.51 for grassroots) and age SD (.15).

---

## D1 Evidence 2 — the same model, four outcome populations (log-log, 2022)

Partial r. Old = card (old6 on current data in brackets where it differs by > .02).

| Covariate | Filers old → lean | Grassroots old → lean | Registered old → lean | Spending old → lean |
| --- | --- | --- | --- | --- |
| % BA | .24 → **.37** | .15 → .14 | .20 → .24 | .20 [.21] → .26 |
| Foundation assets | .41 → **.21** | .32 → **.12** | .39 → .18 | .26 [.27] → **.11** |
| % nonwhite | −.26 → −.20 | −.02 → **+.13** | −.08 → +.05 | −.15 [−.13] → −.08 |
| Turnout | .09 → **.01** | .30 → .24 | .27 → .21 | .07 [.08] → .04 |
| Poverty rate | −.10 → **−.29** | .14 [.13] → **−.06** | .08 → **−.14** | .07 [.08] → −.04 |
| S&L govt earnings | .33 → .23 | .15 [.16] → .08 | .21 → .13 | .21 [.23] → .10 |
| Medicaid transfers | — → .24 | — → .00 | — → .06 | — → .18 |
| Growth kernel | — → −.05 | — → −.08 | — → −.10 | — → −.05 |
| Pre-1970 endowment | — → .32 | — → .26 | — → .30 | — → .16 |
| % 65+ | — → −.04 | — → −.01 | — → −.02 | — → −.06 |
| Gini (metro) | — → .13 | — → .15 | — → .16 | — → .08 |
| Religious fractionalization | — → .12 | — → **.51** | — → .48 | — → −.01 |
| Age SD | — → .15 | — → .07 | — → .10 | — → .13 |

R² (filers / grassroots / registered / spending): .959/.969/.972/.873 → .976/.981/.984/.895.

**What changed.** Poverty no longer changes sign between filers and grassroots: it is negative for
all four populations, and most strongly for filers (−.29). % nonwhite now splits instead: negative
for filers (−.20) and positive for grassroots (+.13). Turnout is now a grassroots-only covariate
(.24 vs .01; the old ratio was 3×). Religious fractionalization is the strongest grassroots
predictor (.51) and does nothing for spending. % BA and foundation assets still agree in sign
everywhere.

## D2 Evidence 2 — the same model at six units (log-log, 2022)

All 13 terms could be built at every unit from county data, so **no term is missing at any unit**:

- **Gini:** grouped Gini from summed ACS 2018–22 B19001 brackets (89's estimator; the county Gini
  uses the county's own brackets).
- **Religious fractionalization:** summed RCMS 2020 group adherents, harmonized as in 18.
- **Age SD:** summed age bins.
- **Growth kernel:** summed county population (1970/80/90 census, 2000–22 PEP) with 82's kernel.
  Connecticut legacy counties are moved to planning regions; 82 used the state ratio for them.
- **Endowment:** 2000 BMF rulings by county. The group-exemption rule (> 60% pre-1970) is applied
  per unit.

Check against the metro panel: correlation 1.000 for all 13. The maximum absolute difference is
.002 (turnout, growth kernel). Rows lost to missing terms:

- **Counties:** 16 endowment artifacts, 3 with no RCMS adherents, and 1 with no turnout or poverty
  (3,124 of 3,144 usable).
- **Metros and CSAs:** 2 (Memphis, Mankato).

`d2_missing_terms.csv` has the counts.

**Filer counts**, partial r, old (card) → lean:

| Covariate | County | Metro | Metro + micro | Micro only | CSA or metro | Counties within metros (metro FE) |
| --- | --- | --- | --- | --- | --- | --- |
| % BA | .32 → .30 | .24 → **.37** | .30 → .29 | .30 → .28 | .23 → **.36** | .43 → .41 |
| Foundation assets | .25 → .17 | .41 → **.21** | .31 → .18 | .29 → .17 | .41 → **.23** | .31 → .24 |
| % nonwhite | −.12 → −.04 | −.26 → −.20 | −.10 → −.07 | −.06 → −.04 | −.25 → −.13 | −.07 → −.07 |
| Turnout | .11 → .04 | .09 → .01 | .20 → .10 | .25 → .13 | .15 → .08 | .17 → .07 |
| Poverty rate | .02 → −.06 | −.10 → **−.29** | −.12 → −.21 | −.13 → −.19 | −.15 → **−.34** | **.27 → .12** |
| S&L govt earnings | .31 → .21 | .33 → .23 | .21 → .11 | .17 → .08 | .33 → .16 | .19 → .13 |
| Medicaid | .10 | .24 | .17 | .13 | .25 | .04 |
| Growth kernel | −.02 | −.05 | −.03 | −.02 | −.15 | −.07 |
| Endowment | .16 | .32 | .23 | .21 | .34 | .30 |
| % 65+ | .12 | −.04 | .02 | .10 | .17 | .17 |
| Gini | .02 | .13 | .08 | .06 | .06 | .04 |
| Religious fract. | .12 | .12 | .10 | .13 | .11 | .17 |
| Age SD | .02 | .15 | .06 | .05 | −.08 | −.03 |

(Old6 on current data reproduces the card column by column to ±.01.)

**Spending excl. H&U**, lean (old6 on current data in brackets):

| Covariate | County | Metro | Metro + micro | Micro only | CSA or metro | Within metros |
| --- | --- | --- | --- | --- | --- | --- |
| % BA | .22 [.25] | .26 [.21] | .19 [.22] | .19 [.23] | .26 [.13] | .30 [.31] |
| Foundation assets | .07 [.13] | .11 [.27] | .09 [.21] | .08 [.20] | .11 [.25] | .07 [.16] |
| % nonwhite | −.04 [−.09] | −.08 [−.13] | −.13 [−.15] | −.14 [−.17] | .04 [−.06] | .05 [.03] |
| Turnout | .05 [.08] | .04 [.08] | .04 [.08] | .07 [.09] | .11 [.16] | .01 [.11] |
| Poverty rate | .07 [.15] | −.04 [.08] | −.04 [.04] | −.05 [.05] | −.06 [.00] | .20 [.32] |
| S&L govt earnings | .14 [.23] | .10 [.23] | .12 [.22] | .13 [.23] | .14 [.30] | .10 [.16] |
| Medicaid | .11 | .18 | .10 | .09 | .18 | .10 |
| Endowment | .13 | .16 | .17 | .18 | .17 | .21 |

**What changed.** The poverty reversal survives within metros but is weaker: +.27 → +.12 for counts
and +.32 → +.20 for spending. Across market units poverty is now firmly negative (−.19 to −.34
instead of −.10 to −.15). Foundation assets are still larger at the market level (metros and CSAs
.21–.23 vs .17 elsewhere), but the gap shrinks from .12–.16 to .04–.06. The endowment carries the
market-level pattern instead (.32–.34 for metros and CSAs vs .16–.23). % BA now peaks at metros and
CSAs (.36–.37) rather than at counties. Turnout is near zero at the market level and survives only
in micro areas (.13). β (bivariate) is unchanged: filers 0.97–0.99 (1.10 within metros), spending
1.25–1.42 (1.60 within metros).

## D2 size check — lean SAMI spec, metros < 250k (193) vs ≥ 250k (192), 2022

This is a single specification, so the old card (medians over 567 specs) is quoted only for
direction. Partial r, small / large (z of the difference):

| Benchmark | Old card, filers (multiverse median) | Old6, filers, this spec | Lean, filers | Lean, grassroots | Lean, spending |
| --- | --- | --- | --- | --- | --- |
| Poverty | −.11 to −.14 / +.08 to +.13 | −.15 / .00 (z 1.5) | **−.33 / −.22 (z 1.2)** | −.06 / −.05 (0.1) | −.09 / +.05 (1.3) |
| % BA | .35–.37 / .40–.46 | .28 / .18 (−0.9) | .43 / .29 (−1.6) | .24 / .03 (**−2.0**) | .24 / .27 (0.4) |
| Foundation assets | .16–.24 / .25–.28 | .39 / .45 (0.7) | .22 / .25 (0.4) | .19 / .04 (−1.4) | .03 / .20 (1.6) |
| % nonwhite | −.15 to −.18 / −.16 to −.21 | −.25 / −.26 (−0.2) | −.20 / −.20 (0.0) | .10 / .15 (0.5) | −.07 / −.09 (−0.2) |
| Turnout | .06–.07 / .15–.16 | .05 / .19 (1.3) | −.08 / .16 (**2.4**) | .19 / .26 (0.7) | .06 / .03 (−0.2) |

Other lean terms beyond |z| = 1.8: Gini for grassroots (.05 / .30, z 2.5), religious
fractionalization for filers and grassroots (z 1.8, 1.9) and age SD (z −1.9 for both).

**Quadratic size interaction** (x × c + x × c², c = centred log P), lean SAMI spec. t of the effect at
100k / 250k / 1M / 4M; joint Wald p of the two size terms; t of x × c²:

| Focal, outcome | Joint p | t(x × c²) | 100k | 250k | 1M | 4M | Old6 same spec: joint p; 100k / 250k / 1M / 4M |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Poverty, filers | **.018** | −2.6 | **−5.4** | **−4.8** | **−2.5** | **−3.0** | .038; −3.1 / −0.9 / +0.3 / −1.2 |
| Poverty, grassroots | .20 | −1.5 | −2.1 | −0.5 | +0.7 | 0.0 | .22; 0.0 / 3.0 / 2.0 / 0.4 |
| Poverty, spending | .15 | −1.8 | −1.4 | −0.4 | −0.5 | −1.9 | .20; 0.3 / 2.0 / 1.0 / −1.0 |
| Foundation assets, filers | .38 | −0.9 | 2.1 | 4.4 | 2.9 | 0.6 | .90; 4.3 / 8.1 / 6.0 / 2.8 |
| Foundation assets, spending | **.015** | −2.8 | −0.6 | 3.1 | 3.0 | −0.4 | .013; 1.4 / 5.9 / 5.3 / 1.3 |
| % BA, filers | .47 | 1.2 | 7.4 | 7.1 | 5.3 | 5.6 | .049; 4.2 / 3.9 / 3.2 / 4.9 |

Figure: `paper/figures/d2_lean_size.png`.

**What changed.** **The poverty reversal is gone in the lean model.** Poverty is negative for filers
at every size, significant at all four points, and stronger in small metros (−.33) than large ones
(−.22). The curvature stays concave (joint p .018), but it now bends a negative effect toward zero
instead of carrying it across zero. Drop-one checks find no single absorber; the change is joint:

- Poverty correlates .34 with Medicaid and .36 with Gini (SAMI form).
- Dropping Medicaid or Gini moves poverty in large metros only to −.16 to −.22.

For the card, "report poverty × size as heterogeneity" loses its main case. Foundation assets for
spending still peak at mid-size (concave, p .015), as before. The one new split worth a line is
filer turnout: negative in small metros and positive in large ones (z 2.4).

## D3 Evidence 4 — hybrid (CRE) between vs within, 2010–2022

lmer: year-specific SAMI on within-metro deviations and metro means of every covariate + year FE +
(1 | metro). Totals are in SAMI form by year. The endowment enters as a metro mean only. Effects are
in SD of the SAMI per SD of the term, as in 52. 385 metros, 5,005 metro-years. ICC .94 (filers), .91
(grassroots), .86 (spending).

| Covariate | Filers between (old6) | Filers within (old6) | Spending between (old6) | Spending within (old6) | Grassroots between | Grassroots within |
| --- | --- | --- | --- | --- | --- | --- |
| % BA | **.39** (.23) | .025 (.017) | **.42** (.26) | .023 (.013) | .11 | .025 |
| Foundation assets | .16 (.34) | .004 (.006) | .13 (.29) | −.002 (−.001) | .13 | −.001 |
| % nonwhite | −.23 (−.27) | −.023 (−.026) | −.20 (−.22) | −.027 (−.018) | .07 | −.041 |
| Turnout | −.01 (.08) | .012 (.024) | .02 (.10) | .018 (.034) | .28 | .026 |
| Poverty rate | **−.21** (−.12) | .007 (.008) | .00 (.11) | −.003 (−.001) | −.05 | .022 |
| S&L govt earnings | .20 (.29) | −.001 (.000) | .15 (.23) | .002 (.006) | .14 | −.029 |
| Medicaid | .15 | .003 | .19 | .025 | .02 | .002 |
| Growth kernel | .00 | −.027 | −.07 | −.022 | .01 | −.048 |
| Endowment (between only) | .23 | — | .13 | — | .32 | — |
| % 65+ | −.06 | .001 | −.03 | −.013 | .01 | −.054 |
| Gini | .08 | .004 | .10 | .006 | .06 | −.011 |
| Religious fract. | .08 | −.002 | .06 | −.007 | .32 | .016 |
| Age SD | .22 | −.016 | .15 | −.019 | .09 | .064 |

The t-statistics of the within terms are often large: growth kernel −11.0 for filers and −12.1 for
grassroots, S&L −10.9 for grassroots. That is because SEs are model-based on 5,000 metro-years. The
effects themselves are small.

**What changed.** The sentence holds with new numbers. Between-metro effects of the benchmarks run
from .13 to .42 SD per SD for filers and spending (old .14–.35). The exceptions are turnout (~0 for
both) and poverty for spending (.00). % BA is the largest. Within effects stay at .03 SD or less for filers and spending. For grassroots the largest
within effect is .06 (age SD, % 65+), which old6 also shows (.057, % nonwhite). Suggested wording:
"between .13–.42 SD; within .03 SD or less (.06 for grassroots)".

## D4 Evidence 2 — DV form (385 metros, 2022)

Partial r: log-log / log per capita (no log P, per capita totals) / linear per capita (no log P).

| Covariate | Counts, old (card) | Counts, lean | Spending, old (card) [old6 current] | Spending, lean |
| --- | --- | --- | --- | --- |
| % BA | .24 / .16 / .31 | .37 / .32 / .30 | .20 / .27 / .24 [.21 / .28 / .25] | .26 / .29 / .20 |
| Foundation assets | .41 / .37 / .21 | .21 / .16 / .12 | .26 / .32 / .17 [.27 / .33 / .17] | .11 / .18 / .15 |
| S&L govt earnings | .33 / .38 / .20 | .23 / .27 / .15 | .21 / .15 / .03 [.23 / .17 / .06] | .10 / .04 / .02 |
| % nonwhite | −.26 / −.43 / −.30 | −.20 / −.28 / −.14 | −.15 / −.02 / .05 [−.13 / −.01 / .08] | −.08 / .00 / .07 |
| Turnout | .09 / .08 / .06 | .01 / .00 / .02 | .07 / .08 / .06 [.08 / .09 / .09] | .04 / .04 / .01 |
| Poverty rate | −.10 / −.06 / −.05 | −.29 / −.23 / −.18 | .07 / .04 / −.02 | −.04 / −.10 / −.09 |
| Medicaid | — | .24 / .20 / .19 | — | .18 / .22 / .13 |
| Growth kernel | — | −.05 / −.03 / −.08 | — | −.05 / −.06 / −.16 |
| Endowment | — | .32 / .38 / .34 | — | .16 / .09 / −.03 |
| % 65+ | — | −.04 / −.03 / .03 | — | −.06 / −.08 / −.01 |
| Gini | — | .13 / .09 / .03 | — | .08 / .12 / .04 |
| Religious fract. | — | .12 / .09 / −.01 | — | −.01 / .03 / −.02 |
| Age SD | — | .15 / .16 / .08 | — | .13 / .11 / .11 |

Log-log = SAMI = log per capita + log P exactly, as before.

**What changed.** The per capita forms still move results, but less for counts:

- **% nonwhite, counts:** strengthens to −.28 in log per capita (old −.43).
- **% nonwhite, spending:** still vanishes and turns positive (−.08 → .00 → .07).
- **S&L earnings, spending:** still falls to ~0 in linear per capita (.10 → .02).
- **Endowment, spending:** a new casualty: .16 under log-log, −.03 under linear per capita.

Covariates' correlation with log P as entered in the per capita model (`d4_size_cor.csv`):
endowment per capita −.25, Gini .42, age SD −.33, growth kernel .22. Without log P on the right,
these new terms carry size.

## D5 Evidence 4 — metro OLS vs Poisson PML (log P on the right), 2022

Coefficients per SD (SAMI-form covariates).

| | Filers OLS | Filers Poisson | Spending OLS | Spending Poisson | Spending Poisson, top 5% dropped | SE ratio Poisson/OLS, spending |
| --- | --- | --- | --- | --- | --- | --- |
| log P (β) | 0.987 | 0.997 | 1.283 | 1.221 | 1.291 | 1.41 |
| % BA | .156 | .149 | .281 | .453 | **.070** | 2.50 |
| Foundation assets | .059 | .045 | .089 | .117 | .231 | 2.91 |
| % nonwhite | −.065 | −.045 | −.062 | .039 | −.009 | 2.03 |
| Turnout | .003 | .029 | .030 | .048 | .154 | 1.97 |
| Poverty | −.092 | −.066 | −.031 | −.002 | −.039 | 1.67 |
| S&L earnings | .056 | .037 | .063 | .068 | .174 | 1.80 |
| Medicaid | .064 | .042 | .121 | .065 | .040 | 1.79 |
| Growth kernel | −.015 | .000 | −.036 | .042 | −.120 | 1.83 |
| Endowment | .101 | .126 | .133 | .253 | .043 | 1.74 |
| Age SD | .067 | .027 | .109 | .246 | .103 | 1.86 |

Old (card): filers β 0.99 vs 1.02, covariates within .06. Spending Poisson SEs 1.7–3.2× OLS; dropping
the top 5% moves % BA .29 → .00 and turnout .09 → .22. Old6 on current data: % BA .315 → .004,
turnout .098 → .201, SE ratio 1.71–3.29.

**What changed.** Same story, slightly tighter:

- **Filers:** OLS and Poisson agree within .04 on every lean term (β 0.99 vs 1.00).
- **Spending SEs:** Poisson SEs are 1.7–2.9× larger.
- **Top 5% dropped:** Poisson spending results still hinge on the largest metros. % BA falls .45 →
  .07, the endowment .25 → .04 and age SD .25 → .10, while turnout (.05 → .15), S&L (.07 → .17)
  and foundation assets (.12 → .23) rise.

## D5 Evidence 1–3 — county subsector models (lean county covariates exist, so refit)

3,124 counties with all 13 terms (SAMI form, standardized). Evidence 1 (bivariate OLS − Poisson gap
vs zero share, from R/74) has no covariates and is unchanged.

**Evidence 2**, scaling exponent β: OLS (zeros dropped) / OLS log(n + 1) / Poisson.

| Group (zero share) | Old (card) | Lean |
| --- | --- | --- |
| Full sector (2%) | 0.97 / 0.93 / 1.03 | 0.98 / 0.93 / 1.00 |
| Human services (6%) | 0.92 / 0.85 / 0.98 | 0.93 / 0.86 / 0.95 |
| Health (25%) | 0.77 / 0.67 / 1.05 | 0.77 / 0.68 / 0.99 |
| Arts (30%) | 0.75 / 0.66 / 1.09 | 0.76 / 0.67 / 1.01 |
| Hospitals (57%) | 0.34 / 0.26 / 0.88 | 0.35 / 0.27 / 0.81 |
| Universities (72%) | 0.42 / 0.25 / 1.05 | 0.39 / 0.25 / 1.00 |
| Mutual/membership (90%) | 0.22 / 0.09 / 1.11 | 0.19 / 0.10 / 1.06 |

All 12 groups, lean Poisson: 0.81 (hospitals) to 1.27 (international); old 0.88–1.26.

**Evidence 3.**

- **Foundation assets, sparse groups:** OLS gives −.01 to −.03 SD (old .04–.06) and Poisson .06–.17
  (old .30–.40).
- **Mean absolute OLS–Poisson gap:** .05 for the full sector (old .07) up to .09–.14 for the sparse
  groups (old up to .17), again about as large as the Poisson coefficients themselves (.08–.16).
- **Significant sign disagreements** between OLS and Poisson: 0–4 per group (old 0–2), because
  there are now 13 terms per group.

**What changed.** Nothing qualitative. Poisson β stays near 1 (0.81–1.27) while OLS β collapses with
zeros. Absolute covariate effects are smaller with 13 controls, so "Poisson finds .30–.40" becomes
".06–.17".

## D8 Evidence 1–2 — endowment, market share, growth kernel (360 metros, SAMI form, 2022)

**Evidence 1.**

| | Lean without endowment | Lean (with endowment) | Lean without endowment + share | Lean + share | Lean without growth kernel |
| --- | --- | --- | --- | --- | --- |
| R², filers | .773 | .792 | .784 | .801 | .790 |
| R², spending | .496 | .508 | .557 | .565 | .507 |
| Endowment r (filers / spending) | — | .29 / .15 | — | .28 / .13 | .37 / .21 |
| Market share r | — | — | .23 / .33 | .22 / .32 | — |
| Growth kernel r | −.25 / −.15 | −.09 / −.05 | −.18 / −.05 | −.05 / .02 | — |

Old (card): R² filers .65 / .74 / .72 / .77 (default / + endowment / + share / + both), spending
.36 / .42 / .50 / .51. Endowment r .45 / .32; share .45 / .47; both .36 / .16 and .33 / .40. Old6 on
current data: filers .653 / .740 / .719 / .766 and spending **.394 / .456 / .524 / .540**. The
spending R² rise is the 2026-11-17 series correction; the partial r match the card.

**Evidence 2 — what each addition absorbs** (filers; spending in brackets):

| Covariate | Lean − endowment | Lean | Lean + share | Lean − growth kernel |
| --- | --- | --- | --- | --- |
| % BA | .41 [.32] | .38 [.29] | .34 [.19] | .36 [.27] |
| Foundation assets | .27 [.16] | .20 [.11] | .20 [.10] | .21 [.12] |
| % nonwhite | −.19 [−.10] | −.18 [−.09] | −.15 [−.04] | −.20 [−.10] |
| Turnout | −.05 [.02] | .01 [.05] | .00 [.05] | .02 [.06] |
| Poverty | −.31 [−.07] | −.31 [−.06] | −.31 [−.06] | −.30 [−.06] |
| S&L earnings | .28 [.15] | .22 [.11] | .23 [.12] | .22 [.11] |
| Medicaid | .31 [.20] | .28 [.18] | .22 [.07] | .31 [.21] |
| Growth kernel | −.25 [−.15] | −.09 [−.05] | −.05 [.02] | — |
| Religious fract. | .17 [.00] | .11 [−.04] | .10 [−.06] | .10 [−.04] |
| Age SD | .13 [.19] | .09 [.18] | .09 [.15] | .10 [.18] |
| Gini | .09 [.07] | .13 [.09] | .15 [.14] | .14 [.10] |

Old card (filers, default → + endowment → + share): foundation assets .40 → .23 → .34; S&L .33 →
.20 → .30; % BA .26 → .34; poverty −.10 → −.18; % nonwhite −.25 → −.15. Share absorbs turnout (.13
→ .08) and % nonwhite.

**What changed.**

- **Endowment is weaker as a predictor:** partial r .29 vs .45 for filers, .15 vs .32 for spending.
  It adds .02 R² instead of .09.
- **Endowment absorbs less from philanthropy and government:** foundation assets .27 → .20 and S&L
  .28 → .22, against .40 → .23 and .33 → .20 before. The other lean terms already took much of that.
- **Endowment's main new absorption is the growth kernel** (−.25 → −.09 filers; SAMI-form
  correlation −.62). Fast-growing metros have little pre-1970 stock.
- **Dropping the growth kernel** raises the endowment from .29 to .37 and changes little else.
- **Market share** absorbs Medicaid instead of turnout (filers .28 → .22, spending .18 → .07) and
  some % BA for spending (.29 → .19). Turnout has nothing left to absorb.
- **Overlap:** share and endowment correlate .50, and adding share leaves the endowment almost
  intact (.29 → .28; old .45 → .36).

## D9 Evidence 1 — share of variance between states / divisions (385 metros, 2022, model form)

| Term | Between states | Between divisions |
| --- | --- | --- |
| Turnout | .65 | .29 |
| % nonwhite | .64 | .32 |
| Religious fractionalization | .58 | .25 |
| Growth kernel | .52 | .33 |
| Medicaid (SAMI form) | .50 | .24 |
| Endowment (SAMI form) | .46 | .24 |
| Poverty | .39 | .18 |
| % 65+ | .37 | .11 |
| Gini | .34 | .12 |
| S&L earnings (SAMI form) | .31 | .14 |
| Age SD | .21 | .04 |
| % BA | .21 | .11 |
| Foundation assets (SAMI form) | .19 | .08 |
| *Filer SAMI* | .39 | .22 |
| *Spending SAMI* | .27 | .17 |

The six old rows match the card (±.01). The η² columns come from run v11 (main session).

## D9 Evidence 2 — single-model partial r by estimator (log-log with log P, 2022)

OLS / division FE / state FE / state random intercepts. FE models use state-clustered SEs; df counted
in metros as in 58 part A.

| Covariate | Filers, old6 (current data) | Filers, lean | Spending, old6 | Spending, lean |
| --- | --- | --- | --- | --- |
| % BA | .24 / .28 / .27 / .30 | .37 / .33 / .26 / .37 | .21 / .29 / .22 / .21 | .26 / .29 / .28 / .26 |
| Foundation assets | .41 / .39 / .35 / .48 | .21 / .26 / .24 / .28 | .27 / .23 / .21 / .31 | .11 / .11 / .09 / .15 |
| % nonwhite | −.26 / −.16 / −.13 / −.21 | −.20 / −.18 / −.20 / −.20 | −.13 / −.09 / −.03 / −.10 | −.08 / −.08 / −.05 / −.09 |
| Turnout | .09 / .08 / .16 / .18 | .01 / .03 / .09 / .05 | .08 / .05 / .09 / .10 | .04 / .01 / .01 / .03 |
| Poverty | −.10 / −.02 / .00 / −.06 | −.29 / −.24 / −.18 / −.27 | .08 / .12 / .09 / .09 | −.04 / .00 / −.01 / −.03 |
| S&L earnings | .33 / .18 / .15 / .24 | .23 / .16 / .15 / .19 | .23 / .12 / .15 / .18 | .10 / .07 / .14 / .09 |
| Medicaid | — | .24 / .15 / **.06** / .16 | — | .18 / .19 / .16 / .17 |
| Growth kernel | — | −.05 / −.11 / −.04 / −.04 | — | −.05 / .06 / .09 / −.02 |
| Endowment | — | .32 / .34 / .34 / .35 | — | .16 / .21 / .26 / .18 |
| % 65+ | — | −.04 / −.06 / −.14 / −.10 | — | −.06 / −.12 / −.10 / −.08 |
| Gini | — | .13 / .07 / .08 / .12 | — | .08 / .06 / .04 / .07 |
| Religious fract. | — | .12 / .10 / **.00** / .09 | — | −.01 / −.03 / −.08 / −.03 |
| Age SD | — | .15 / .18 / .21 / .25 | — | .13 / .12 / .12 / .14 |

The old card's Evidence 2 table is a multiverse median under racial HHI, so it is not comparable
cell by cell; the old6 single model above is the reference.

**What changed.** The lean model is more stable across estimators for the benchmarks:

- **% nonwhite:** −.18 to −.20 under all four estimators, against −.13 to −.26.
- **Poverty:** stays negative under state FE (−.18; old6 .00).
- **Turnout:** still gains within states (.01 → .09), but from zero.
- **Most estimator-sensitive terms are now new ones.** Both are state-patterned:
  - Medicaid for filers: .24 under OLS, .06 under state FE. Medicaid generosity is set by states.
  - Religious fractionalization: .12 → .00.

## D10 Evidence 2 — money measure (SAMI form, 385 metros, 2022; corrected spending series)

Old (card) [old6 on current data] → lean. Columns: expenses, all / expenses excl. H&U / excl. H&U net
of pass-through / revenue excl. H&U.

| Covariate | Old (card) [old6 current] | Lean |
| --- | --- | --- |
| % BA | .12 / .20 / .15 / .21 [.12 / .21 / .16 / .22] | .19 / .26 / .24 / .24 |
| S&L govt earnings | .12 / .21 / .20 / .22 [.13 / .23 / .21 / .23] | **−.02 / .10** / .08 / .10 |
| % nonwhite | −.16 / −.15 / −.13 / −.14 [−.16 / −.13 / −.12 / −.12] | −.07 / −.08 / −.07 / −.07 |
| Foundation assets | .24 / .26 / .25 / .27 [.24 / .27 / .26 / .28] | .05 / .11 / .11 / .13 |
| Turnout | .09 / .07 / .09 / .07 [.09 / .08 / .10 / .08] | .05 / .04 / .04 / .05 |
| Poverty rate | .10 / .07 / .05 / .05 [.10 / .08 / .06 / .06] | −.04 / −.04 / −.06 / −.05 |
| Medicaid | — | .18 / .18 / .20 / .16 |
| Growth kernel | — | −.12 / −.05 / −.05 / −.03 |
| Endowment | — | .13 / .16 / .15 / .17 |
| % 65+ | — | −.05 / −.06 / −.04 / −.07 |
| Gini | — | .05 / .08 / .08 / .09 |
| Religious fract. | — | **.12 / −.01** / −.03 / −.02 |
| Age SD | — | .06 / .13 / .13 / .11 |

R² (lean): .41 / .48 / .46 / .49. β (spending excl. H&U) 1.283; net 1.269; all 1.275.

**What changed.** The data correction moves old6 by ≤ .02, so the changes are the model's.

- **The H&U exclusion still matters most.**
  - S&L earnings goes from −.02 to .10 (old .12 → .21).
  - % BA goes from .19 to .26 (old .12 → .20).
  - A new exclusion-dependent term appears: religious fractionalization, .12 with H&U and −.01
    without.
- **Netting pass-through** now lowers % BA only .26 → .24 (old .20 → .15). Revenue and expenses
  agree within .03.
- **Poverty** is slightly negative for every money measure (old +.05 to +.10).

## D11 Evidence 3 — subsector Poisson models of 2022 counts, log P + lean set (per SD)

Partial r, lean (old card value in brackets for the benchmarks):

| Subsector | % BA | Foundation assets | % nonwhite | Turnout | Poverty | S&L | Medicaid | Endowment | Growth |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Full sector | .27 [.19] | .12 [.20] | −.14 [−.14] | .07 [.12] | **−.20** [.01] | .12 [.22] | .15 | .29 | .00 |
| Human services | .17 [.11] | .12 [.22] | −.22 [−.22] | .12 [.17] | −.21 [.01] | .15 [.28] | .18 | .22 | −.11 |
| Education | .35 [.40] | .15 [.20] | .05 [.02] | .08 [.10] | −.08 [−.02] | .15 [.24] | .05 | .24 | .06 |
| Public/societal benefit | .24 [.16] | .04 [.12] | −.03 [−.05] | .06 [.10] | −.05 [.09] | .00 [.07] | .01 | .19 | −.02 |
| Health | .19 [.12] | .05 [.15] | −.21 [−.22] | .08 [.11] | −.07 [.08] | .10 [.21] | .10 | .23 | −.04 |
| Arts | .27 [.26] | .24 [.28] | −.11 [−.12] | −.04 [−.01] | −.19 [.00] | .16 [.26] | .15 | .24 | −.07 |
| Religion-related | −.01 [.08] | .08 [.08] | −.06 [−.03] | −.09 [−.07] | −.06 [−.02] | −.08 [−.13] | .01 | .09 | .11 |
| Environment/animals | .11 [.16] | .06 [.12] | .12 [.10] | .22 [.23] | −.21 [−.15] | .23 [.26] | .15 | .29 | .27 |
| International | .19 [.16] | .08 [.07] | .05 [.04] | .00 [.01] | −.15 [−.06] | −.02 [.00] | .04 | .14 | .17 |
| Hospitals | .04 [−.04] | −.04 [.10] | −.27 [−.29] | −.03 [.01] | .00 [.11] | −.01 [.07] | .01 | .15 | −.05 |
| Universities | .10 [.04] | .06 [.13] | −.11 [−.17] | −.04 [−.02] | −.02 [.02] | .11 [.19] | .01 | .09 | −.13 |
| Mutual/membership | .05 [.05] | −.02 [.04] | −.10 [−.12] | −.03 [.00] | −.08 [−.03] | .06 [.13] | .03 | .04 | −.06 |

Religious fractionalization is .36 for religion-related organizations (.13 for the full sector); the
other new terms are in `d11_subsector_models.csv`.

**Sign agreement with the full sector**, counts (subsectors of 11 sharing the sign; old in brackets):

| % BA | Foundation assets | % nonwhite | Turnout | Poverty | S&L |
| --- | --- | --- | --- | --- | --- |
| 10 [10] | 9 [11] | 8 [8] | 5 [8] | **10 [6]** | 8 [10] |

New terms:

- **11 of 11:** Medicaid and the endowment (8 of them significant).
- **10:** Gini.
- **9:** age SD.
- **8:** religious fractionalization.
- **7:** growth kernel (3 significantly opposite: environment, international and religion-related
  grow with growth).
- **4:** % 65+.

**What changed.**

- **Poverty becomes one of the most consistent covariates:** negative in 10 of 11 subsectors (old
  6 of 11). The full-sector value is −.20 against .01.
- **Foundation assets lose universality:** 9 of 11 (old 11), with hospitals and mutual/membership
  now slightly negative.
- **Turnout is the least consistent benchmark** (5 of 11).
- **The endowment and Medicaid are the new universal terms.**
- **Religion-related still scales fastest:** log P coefficient 0.90 vs 0.42 for the full sector (old
  1.25 vs 0.61).

## D12 Evidence 1 — sector age (367 metros, SAMI form, 2022)

The lean model already contains the endowment, so the old "+ endowment" arm is now the baseline.
Filer SAMI:

| | Lean − endowment | Lean − endowment + mean filer age | Lean | Lean + mean filer age |
| --- | --- | --- | --- | --- |
| R² | .773 | .773 | .792 | .794 |
| Mean filer age, r | — | −.00 | — | **−.10** (t −1.9) |
| Endowment, r | — | — | .29 | .31 |
| Foundation assets | .27 | .26 | .20 | .20 |
| S&L earnings | .28 | .29 | .22 | .24 |
| % BA | .41 | .40 | .38 | .37 |
| % nonwhite | −.18 | −.19 | −.18 | −.19 |
| Growth kernel | −.25 | −.20 | −.09 | −.12 |

Old (card), default / + age / + endowment / + age + endowment: R² .65 / .68 / .74 / .74; age .26 /
−.01; endowment .45 / .42; foundation assets .40 / .35 / .23 / .23; S&L .33 / .27 / .20 / .20; % BA
.25 / .31 / .34 / .33; % nonwhite −.26 / −.17 / −.15 / −.15. Old6 on the current 367 metros
reproduces this: age .26; endowment .45 → .42.

Other outcomes, mean filer age r in lean + age (in lean − endowment + age):

- **Grassroots:** −.21 (−.12); old −.17 with the endowment.
- **Spending:** .06 (.10); old .25 → .12.

Alternative age measures in the lean model, filers: mean filer age −.10, median age of all
registered .07, dollar-weighted age excl. H&U .03.

**What changed.** Age no longer stands in for anything. Even without the endowment, the lean model
leaves age nothing to explain: −.00 against .26. The growth kernel, Medicaid and the heterogeneity
terms already carry what age picked up. Age moves only the growth kernel (−.25 → −.20). With the
endowment in, age turns weakly negative for filers and clearly negative for grassroots (−.21), so
"older sectors are denser" does not hold net of history. The card's conclusion ("do not add age")
gets stronger.

## D12 Evidence 2 — change model with start-of-window covariates

Script: `R/notes/100_d12_change_lean.R` (log `D:/MULTIVERSE/logs/100.log`). Outputs: `d12_change_lean.csv`
(every coefficient), `d12_change_lean_summary.csv`, `d12_change_lean_cor.csv`.

**Model.** Change in the filer (or grassroots) SAMI between the start window and 2022–24, regressed on:

- the start-window SAMI;
- in-window growth, log(P2024 / P at the start);
- the lean covariates measured in the start year, residualized on that year's log P and standardized.

HC1 SEs; partial r. Mean filer age is the card's 2022 measure (c12). Sample: 367 metros (Memphis
and Mankato have no endowment).

**Windows.**

- **Main lean version: 2010–12 → 2022–24, covariates 2010.** All 13 terms exist in 2010.
- **The card's window: 2000–02 → 2022–24, covariates 2000.** 12 terms: gini_m starts in 2010.
  Every other term exists in 2000, including the kernel (from 1970–2000 census history) and the
  endowment (2000 BMF). Adding the 2010 Gini as a proxy changes nothing (filers R² .231 → .385, age
  −.43).
- **Check: 2012–14 → 2022–24.** The start window clears the 2011 auto-revocations, which purged
  registered non-filers (grassroots).

**Growth kernel vs in-window growth.** The old model held 2000–2024 growth fixed and had no kernel.
The 2022 kernel is mostly in-window growth (r .95 with 2010–24 growth, .98 with 2000–24), and
putting it in splits the growth effect: 2000 window, filers + age, kernel −.09 and growth −.07. So
the kernel is measured at the start year. Its inputs (growth up to the start) are then disjoint from
in-window growth, though growth persists (r .80).

**Replication.** The card's model (six covariates in 2022, % nonwhite) on its 363-metro sample
reproduces the card exactly: R² .217 → .399, age −.45, growth −.28 → −.47, turnout .09 → −.02;
grassroots .438 → .455, age −.17. (58 P's `race_hhi` column holds % nonwhite from run v5 on.)

**Filer SAMI change**, without → with mean filer age (2022):

| | Card (6 terms in 2022, 2000 window) | Old6 in 2000 | Lean in 2000 (12 terms) | **Lean in 2010 (13 terms)** | Lean in 2012 |
| --- | --- | --- | --- | --- | --- |
| R² | .22 → .40 | .14 → .35 | .23 → .38 | **.16 → .33** | .16 → .28 |
| Mean filer age, r | −.45 | −.46 | −.44 | **−.41** | −.33 |
| In-window growth | −.28 → −.47 | −.21 → −.42 | −.30 → −.40 | **−.32 → −.41** | −.31 → −.37 |
| Growth kernel (start year) | — | — | .24 → .14 | **.20 → .10** | .18 → .09 |
| Turnout | .09 → −.02 | .01 → −.04 | −.02 → −.09 | **−.01 → −.07** | .01 → −.04 |
| Start SAMI | −.37 → −.22 | −.29 → −.17 | −.25 → −.19 | −.06 → −.06 | −.03 → −.04 |
| Endowment | — | — | −.07 → .05 | −.04 → .07 | .00 → .10 |
| Age of the registered stock in the start year, r (R²) | — | −.18 (.18) | −.11 (.24) | **−.06 (.17)** | −.01 (.16) |

Main window, the other term that moves by more than .05 when age is added is S&L (.02 → .08; %
nonwhite −.09 → −.13 is just under). Every lean term other than growth and the kernel stays within
±.10 in the base model.
In the 2000 window, poverty also moves (−.06 → −.14).

**Grassroots SAMI change.** Age is weak in every version:

- **Main window:** lean R² .406 → .416, age −.14; growth −.22 → −.24.
- **2000 window:** lean .439 → .458, age −.17. The card had .44 → .45 and −.17.
- **Start-year stock age:** −.16, R² .453. It takes most of the endowment's share (endowment −.28 → −.11; r .57 between the two).

**What changed.**

- **The age result survives the lean model.** In the main window adding mean filer age still doubles
  R² (.16 → .33), age is −.41, and growth strengthens (−.32 → −.41).
- **The age effect is the 2022 measurement, not sector age at the start.**
  - Mean filer age in 2022 correlates −.65 with 2010–24 growth: metros that added filers have young
    filers because of that entry.
  - The age of the registered stock at the start of the window barely predicts the change: −.06
    (2010), −.11 (2000), −.01 (2012), adding at most .01 to R².
  - This measure is all registered public charities, not filers. It correlates .65 with 2022 filer age.
  - So the change-model age coefficient is largely the outcome measured a second way. It supports
    "do not add age" rather than "age predicts catch-up".
- **The kernel term is new and positive** (.20 holding in-window growth fixed). Metros that grew fast
  before the window gained filer SAMI during it: catch-up after the lag. In-window growth stays
  negative.
- **Turnout has nothing to lose.** It starts at −.01 in the lean model (old .09), so "turnout drops
  to zero with age" no longer applies.
- **The shorter window lowers R²** (.16 vs .23 for the 2000 window) because the outcome varies less
  (SD .08 vs .13). The start SAMI loses its pull: −.06 vs −.25.

## D11 rerun after the NTEE backfill (2026-11-17)

**Backfill check.** `50_followups_jeremy.R` part 3 already fills unknown e-file industry from the BMF.
The unknown rows are exactly the invalid NTEE rows (`ind` UNU or NA ⇔ ntee3 UND / INV / Z99 / blank:
163,055 of 968,908 filer-years 2021–23, 16.8%). So the UND problem fixed in 06/23 had not reached the
subsector file.

**The one difference.** Part 3 took the latest non-empty BMF code even when it was invalid, and
`map_ntee()` sends "UND" to PSB and "INV" to HMS. 1,533 filer-years were affected. Part 3 now skips
invalid BMF codes (`bad_ntee()`, as in 06/23) and was rerun. Backups:
`D:/MULTIVERSE/data/followup/pre_ntee_20261117/` and `D:/MULTIVERSE/data/outline/v11/single/pre_ntee_d11/`.

**Effect, 2022.**

- **Counts:** every named group's national count changes by ≤ .03%; unknown falls 3,506 → 3,461.
- **Spending:** education +.07%, unknown −.7%, the rest ≤ .01%.
- **Unknown after backfill:** 1.238% → 1.224% of filer-years.

**D11 rerun.** Partial r and coefficients change by ≤ .004 (no subsector coefficient moves more than
.05). Sign agreement is identical for counts and spending, lean and old6. The D11 Evidence 3 table
above stands.

Spending sign agreement (subsectors of 11 sharing the full sector's sign, lean; old6 in brackets),
unchanged by the rerun:

| % BA | Foundation assets | % nonwhite | Turnout | Poverty | S&L | Medicaid | Endowment | Growth | % 65+ | Gini | Religious fract. | Age SD |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 10 [11] | 8 [9] | 6 [7] | 6 [8] | 7 [5] | 5 [8] | 7 | 10 | 7 | 7 | 5 | 7 | 11 |

---

## Not rebuilt

- **D5 Evidence 1** (gap vs zero share): bivariate, unchanged.
- **D3 Evidence 1/2/5, D9 η² columns, D8 Evidence 3:** multiverse quantities (run v11, main
  session).
- **D2 size check:** done for the single lean SAMI spec only, not over the 567 / 3,636-spec
  multiverse that the old card summarizes. Treat the old card's percentages as multiverse
  statistics, and these as one specification.
- **D11:** subsector counts and spending come from `p3_subsector_metro_year.csv` (followup), as in
  58 part C. That file was rebuilt with the NTEE rule of 06/23 on 2026-11-17 (section above); the
  D11 numbers did not change.
