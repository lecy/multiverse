# Multiverse Analysis Design: Nonprofit Density

Nov 3, 2026 · @Jesse Lecy

## 1. Partition the forks: non-equivalent vs. arbitrary

Not all forks belong in one specification curve. Del Giudice and Gangestad's distinction applies: some choices change *what is being estimated* (type N); others are defensible alternatives for the same estimand (type E). Pooling both into one curve produces a spread that is uninterpretable.

Treat these as top-level strata, each with its own multiverse underneath:

- **Outcome construct**: count of orgs vs. total expenditures (revenue and employment as optional extras). These answer different questions — entry and prevalence vs. sector scale — so analyze them as parallel universes and compare across them.
- **Identification**: cross-section (one year, or pooled with year FE) vs. panel within-unit FE. Lecy and Van Slyke (2012) saw poverty and income flip sign between pooled and FE models. That is not specification noise; it is a different estimand.
- **Hospital and university exclusion** for the expenditure DV. The top revenue quintile holds roughly 87% of sector revenue, so the expenditure universe is really a hospitals-and-universities universe unless they are cut. Make this a stratum, not a fork.

Everything below sits inside those strata.

## 2. The DV functional-form fork — and what it actually is

The candidate DV forms are nested constraints on one scaling model:

```latex
\log Y = \alpha + \beta \log N + X\gamma + \varepsilon
```

| Form | What it imposes |
| --- | --- |
| Per capita (Y/N) | β = 1, linear in Y |
| Log per capita | β = 1, log-linear |
| Log count with log pop as covariate (one-stage) | β estimated jointly with γ |
| Two-stage SAMI (residual of log Y \~ log N, then regress on X) | β estimated *without* X; residuals then modeled |
| Poisson/NB with log N offset vs. free coefficient | β = 1 vs. estimated, count-appropriate |

Two consequences follow.

1. "Per capita vs. SAMI" is really "is β constrained to 1." The gap between them will be proportional to |β̂ − 1| — likely small for counts (near-linear), potentially large for expenditures (superlinear).
2. One-stage and two-stage SAMI coincide by Frisch–Waugh–Lovell only if the predictors are *also* residualized on log N. The predictor-form fork is therefore entangled with the DV-form fork, and the clean design crosses them.

**Predictor form for extensive variables** (foundation assets, total giving, government wages, total crimes): raw total / per capita / log / SAMI-residualized. Intensive variables (Gini, poverty rate, % BA, turnout) stay as-is, though even shares scale with size (education share rises with metro population), so a "residualize everything" arm is defensible.

**Meta-hypothesis for goal 2.** A covariate's sensitivity to DV form should be predicted by its correlation with log population. Compute that correlation before running anything and test whether it rank-orders the observed sensitivity. That turns "results vary" into an explanation.

## 3. Covariate forks: constructs vs. operationalizations

Run these as two distinct dimensions, because they diagnose different things.

**Block inclusion** (omitted-variable sensitivity). Five theory blocks: government failure / diversity, philanthropy, political culture, government size, demographics and need. Include all 2^k subsets, or at minimum leave-one-block-out. Kim's (2015) Gini result surviving median-income controls is the kind of thing this tests.

**Operationalization within block** (measurement sensitivity):

| Block | Candidate measures | Likely source |
| --- | --- | --- |
| Diversity | Racial HHI; % non-white; % foreign-born; religious HHI; Gini; age dispersion | ACS; RCMS/ARDA |
| Philanthropy | Foundation assets per capita; number of foundations; itemized contributions per return; contributions as share of AGI | Candid; IRS SOI county |
| Political culture | Presidential turnout; partisan margin; Rupasingha social capital index; Elazar typology (state-level, random-intercept specs only) | MIT Election Lab; Penn State |
| Government size | Government wages; government employment share; direct expenditures; federal grants to county | BEA; Census of Governments |
| Demographics / need | Median vs. per capita income; poverty vs. unemployment; % BA; crime per capita; % 65+ | ACS; BLS; FBI UCR |

Racial diversity is the obvious candidate for instability: Corbin (1999) found a positive effect, Kim (2015) a negative one, and the sign may hinge entirely on HHI vs. share-based measures. Turnout is the obvious candidate for stability — significant in all ten of Kim's subsectors.

## 4. Remaining arbitrary forks

| Fork | Options |
| --- | --- |
| Unit of analysis | County / MSA / CBSA incl. micropolitan / commuting zone |
| Sample | Minimum population threshold; rural inclusion; drop DC and Alaska |
| Organization filter | BMF-registered vs. 990 filers; $25k vs. $50k revenue threshold; all 501(c)(3) vs. human services only |
| Outlier handling | None / winsorize top 1% / trim / log(1+x) |
| Estimator | OLS robust SE / state random intercept / state FE / SEs clustered by state |
| Year | Single year vs. pooled with year FE |

## 5. Analysis plan

**Goal 1 — which covariates are specification-sensitive.** For each covariate, across the within-stratum universe:

- Sign stability (% of specifications sharing the median sign), % significant at p < .05, and the interquartile range of the standardized coefficient.
- Young and Holsteen's (2017) robustness ratio and *model influence*: an ANOVA-style decomposition of coefficient variance across fork dimensions. This identifies which fork drives the instability for each covariate — more useful than the specification curve alone.

**Goal 2 — how much results depend on the DV form.**

- Standardize coefficients (or use t-statistics), since per capita and log/SAMI forms are on different scales. Fully standardized betas or partial R² are the safest common currency.
- Split the specification curve by DV form; compute the Spearman correlation of covariate t-statistics across forms; build a DV-form × covariate matrix of sign-stability.
- Report β̂ itself as a result, by outcome construct and subsector — it is what makes the per capita arm deviate.

**Pre-registration.** Before running anything, write down the expected sensitives (racial diversity, income and poverty, extensive philanthropy measures, anything correlated with log N) and the expected stables (turnout; government size for hospitals).

**Reporting.** The full cross product runs to tens of thousands of models before covariate subsets. That is fine computationally, but for the paper define a *principled* universe (every fork has a written rationale) and a *kitchen-sink* universe, and show the first.

- [ ] Draft the R scaffolding: specification grid via `expand.grid()`, a model-builder function, and the variance-decomposition step, run against a mock dataset before data assembly.
