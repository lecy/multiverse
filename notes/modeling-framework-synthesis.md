# How to model nonprofit density: synthesis of results so far

Nov 14, 2026. Synthesis of `Multiverse Analysis Design Nonprofit Density.md`, `R/README.md`, the four results notes in `notes/` (scaling aggregation, revenue/expenses and pass-through, collinearity and composites, catch-up and lags) and the v5 multiverse outputs (`D:/MULTIVERSE/data/outline/v5/`, from `R/58_outline_analyses.R` with `MV_RUN=run_principled_v5`). Nothing was re-run for this note.

## The overall story

"Nonprofit density" isn't one quantity. Most of the disagreement in the literature comes from choices that change what is being estimated, not from noise between equally good specifications. Three choices matter most:

1. how you adjust for city size,
2. which organizations you count,
3. whether you compare places with each other or a place with itself over time.

Once you fix those three on purpose, the covariate story is fairly stable. It also says something uncomfortable for demand-side theories: **density behaves like an inherited stock.**

- Filer density correlates .94 with itself 20 years later, which is more persistent than size-adjusted GDP (.78).
- The convergence half-life is about 240 years.
- Within-metro variation is about 1% of the total.
- The strongest single predictor is the stock of pre-1970 nonprofits (r ≈ .48–.52).

Cross-sectional covariates mostly describe where old, institution-heavy sectors are, not current demand. Most of the modeling choices below follow from that.

## Key findings

**1. Size adjustment: constrain the exponent or not, and the answer is not.**
- **Counts scale one-for-one with population.** The exponent is ≈ 0.99 at every geography and with both OLS and Poisson. Per capita and SAMI rank metros almost identically (ρ = .999).
- **Spending grows faster than population.** The exponent is 1.27–1.29 for metros and 1.46 for counties. Per capita then misranks large metros badly:

  | Metro | Percentile, per capita | Percentile, SAMI |
  |---|---|---|
  | Houston | 54 | 12 |
  | Miami | 50 | 11 |
  | Dallas | 66 | 20 |

  The ten largest metros average the 77th percentile per capita against the 39th by SAMI (`d4_tilt.csv`, `d4_big_metros.csv`).
- **Three of the DV forms are the same model.** Log-log with log P on the right-hand side, SAMI, and log per capita plus log P give identical coefficients, by the Frisch–Waugh–Lovell theorem (FWL; `d4_forms.csv`). The DV-form fork really reduces to "is the exponent fixed at 1?"

**2. Predictor form is the biggest lever for size-dependent ("extensive") covariates, and the reason is known.**
- Totals like foundation assets and government earnings correlate .75–.94 with log population.
- Regressing a SAMI outcome on logged predictors that weren't size-adjusted shrinks each coefficient by exactly 1 − R²(x, log P) (`fact6_size_attenuation.csv`). Government earnings keeps only 12% of its effect, and foundation assets 44%.
- Raw totals are also extremely skewed: for foundation assets, the top 5% of metros hold 42% of the total (`d6_mechanism.csv`).
- This confirms the design doc's prediction: a covariate's correlation with log P predicts how sensitive it is to form. Turnout (r ≈ 0 with log P) is unaffected.
- **Rule:** adjust both sides for size, or put log P on the right-hand side. Never pair a SAMI outcome with size-unadjusted predictors.

**3. Which organizations you count defines the construct.**
- Filers and grassroots (registered but not filing) correlate only .67. The pooled registered stock is essentially the grassroots count (.97).
- The two groups move in opposite directions with the economy:
  - income growth: partial r +.35 for filers, −.07 for grassroots
  - wage growth: +.28 for filers, −.17 for grassroots
  - poverty: −.97 for filers, +1.20 for grassroots (log-log coefficients, `d1_outcome_models.csv`)
- Pooling them cancels these effects out. Model them separately.

**4. For spending, the hospital/university cut and the money measure play very different roles.**
- Excluding hospitals and universities changes results enough that it has to be its own stratum.
- Revenue vs expenses and gross vs net of pass-through grants barely move the slopes: the exponent shifts by +.004 to +.011, about a tenth to a third of a standard error.
- Netting out pass-through grants moves sector levels by 4–11%, so it matters for size claims, not for scaling.
- Use expenses, which are less lumpy than revenue: 36% vs 43% of org-years change by more than 20%.

**5. Within-metro models are underpowered by construction.**
- Only 1.2% of filer SAMI variance is within metros. For % nonwhite it is 0.2%, and for % BA 0.8% (`d3_within_share.csv`). Much of the remaining covariate variation is interpolation between censuses.
- Annual changes are mostly noise. Composite indices don't help, and extending the window to 2000–22 adds data without adding precision.
- A null within-metro estimate is not evidence of no effect. Treat within-metro models as a separate question (how places adjust), not as a stricter test of the cross-sectional one.

**6. State fixed effects absorb what you may want to study.**
- 65% of turnout variance and 64% of % nonwhite variance lies between states (`d9_between_state_share.csv`).
- That is why the estimator decision explains 43% of turnout's variation across specifications.
- Choose the geographic fixed effect deliberately. Census-division FE is the middle ground.

**7. Robust and unstable covariates (across the whole principled universe, `exhibit_spec_summary.csv`).**

| Covariate | Counts: median partial r | Same sign | Significant | Spending: median r |
|---|---|---|---|---|
| % BA | .27 | 98% | 93% | .24 |
| Foundation assets | .16 | 93% | 73% | .11 |
| Turnout | .18 | 100% | 88% | .08 |
| % nonwhite | −.03 | 63% | 39% | −.02 |
| Poverty | .01 | 55% | 24% | .06 |

- This matches the pre-registered expectations: turnout stable, diversity and poverty unstable.
- Diversity is mostly a measurement issue. The choice of measure explains 66% of its variation (racial HHI vs % nonwhite; v5 now defaults to % nonwhite; `decision_importance.csv`).
- Under all the recommended choices together (54 specs, `summary_table.md`), diversity sharpens to −.17 and foundation assets to .41, while turnout weakens to .10.

**8. History absorbs part of what looks like resources.**
- Adding pre-1970 endowment raises R² from .65 to .75 (`d8_models.csv`).
- It cuts the partial r of foundation assets from .41 to .23 and of government earnings from .34 to .19.
- Sector age adds nothing once endowment is in the model (`d12_levels.csv`).
- The Sunbelt gap combines growth history with institutional ownership: in the South and West, hospitals and colleges are more often for-profit or public. Nonprofit employment share sets the level (R² .35 for filers), and growth history adds nothing once share is known.

**9. Dynamics differ within and across metros.**
- Within a metro, a growth surge leaves a deficit that closes with a half-life of about 5 years for filers and 9 for grassroots.
- Across metros, the deficit never closes.
- Catch-up comes from existing organizations crossing the $50k filing floor, not from new registrations.
- Responses are asymmetric: organizations persist after population loss, and filer density drops at once after an income loss.

**10. Geography.**
- Metro CBSAs and CSAs are interchangeable.
- County spending exponents are inflated, because headquarters budgets are credited to the core county's population.
- County OLS on logs drops zeros. For example, 58% of counties have no hospital filer, which flattens the hospital exponent to 0.36. Use Poisson there.
- A county model with metro fixed effects measures how nonprofits concentrate within a metro, which is a different quantity from metro density.

## A repeatable framework

**Step 0. Name the question, since each has its own model.**

| Question | Unit and identification |
|---|---|
| (a) Why are some places denser? | Metro cross-section or pooled with year FE |
| (b) How do places adjust? | Within-metro local projections over multi-year horizons |
| (c) Where do nonprofits locate inside a labor market? | County with metro FE |

Never pool estimates across these questions.

**Step 1. Build the outcome.**
- Filers at or above $50k in real 2010 dollars, and grassroots, modeled separately. The registered stock serves as a splice-free robustness check.
- Spending: expenses excluding hospitals and universities, in real dollars.
- Fixed 2023 geography, with population rebuilt from Census estimates.

**Step 2. Adjust for size.**
- Estimate the exponent for each outcome and year, and report it with the SAMI standard deviation (≈ .39 for counts, .73 for spending excluding hospitals and universities; `d10_beta.csv`).
- Default model: log Y on log P plus covariates. Add a Poisson twin with a free log P coefficient wherever zeros exist.
- No per capita for money measures.

**Step 3. Choose covariates.**
- One measure per construct block:
  - need: % BA, poverty
  - foundations
  - individual giving, kept separate and flagged for the 2018 TCJA break
  - civic: turnout and census response
  - diversity: % nonwhite
  - government: state and local earnings
- Log extensive covariates and keep log P on the right-hand side; enter intensive ones as levels.
- Publish each covariate's correlation with log P as a sensitivity diagnostic.
- Treat historical endowment and nonprofit market share as a separate stratum, since they are partly lagged versions of the outcome.

**Step 4. Estimate.**
- OLS with metro-clustered SEs, alongside division-FE and state-FE versions.
- No state random intercepts on pooled panels.
- Within models only with a power diagnostic (the within share of variance) and multi-year horizons.

**Step 5. Report in a common currency.**
- Partial r, with degrees of freedom counted in independent places rather than place-years.
- Sign stability, share significant, and an η² decomposition for each decision.
- Separate specification curves for each outcome/identification stratum.
- Lead with the recommended core, then the principled universe.

**Step 6. Check the known data hazards.**
- Source splices at 2011 and 2020; TY2020 counts run about 9% low.
- The 2016–18 BMF snapshot gap.
- The TCJA break in contributions data.
- Interpolated covariates (ACS 2001–09, religion, turnout between elections).
- Group-exemption artifacts (Memphis, Mankato).
- The published Rupasingha social-capital index contains nonprofit density, so it is circular; use the re-estimated version (`socap_sk2`).

If others follow Steps 0–5, the exponent, partial r and stability metrics are directly comparable across studies.

## Update, Nov 15, 2026 (runs v6–v7)

- **Zero foundation assets.** Logged totals now use a per-capita floor (`flog()`, run v6). Under log(x + 1), Eagle Pass, TX sat about 11 SD below the mean. In single 2022 models the floor raises foundation assets' partial r by .10–.15; in the multiverse its median rises about .02–.03 (`81_compare_floor.R`).
- **Growth history.** Run v7 adds a population-lag kernel (5- or 10-year half-life) as an add-on block. The kernel is negative in 100% of cross-section and pooled specs. Net of it, poverty turns negative for counts, % BA strengthens and foundation assets weaken (`82_lag_fork.R`; catch-up note 2.9).
- **Design-plan meta-hypothesis** (`45_summarize_run.R`, v6 and v7). A covariate's R² on log P predicts its predictor-form sensitivity (Spearman .42–.68 in all 12 strata), not its DV-form sensitivity (−.40 to +.15). Finding 2 above is the right reading.
- **Sample exclusions** (`83_sample_exclusions.R`). Dropping DC, AK/HI, Memphis and Mankato, the size extremes or covariate outliers moves partial r less than winsorizing does (median |Δr| ≤ .016), so no fork is needed. A population threshold does matter: among metros of 250k or more, poverty turns positive for filers. That threshold changes the estimand, like the unit choice.
- **TCJA** (`84_tcja_contrib.R`). Metro rankings of contributions survive 2018 (rank r .966), but contribution partial r jumps +.03 to +.06 after it, and within-metro estimates partly identify off the break. Every multiverse cross-section is post-TCJA.
- **Sensitivity checks, not forks** (`86_sensitivity_tcja_size.R`, 2026-11-16).
  - TCJA in the multiverse: re-fitting all 34,914 contribution specs on pre-2018 windows, net of their default twins, moves contribution terms by .03–.04 in cross-sections and ≤ .014 in pooled specs. Benchmarks move ≤ .024. Within-metro contribution estimates are unstable in any window.
  - City size: only poverty differs. For filers it is −.11 to −.14 in metros under 250k and +.08 to +.13 above, beyond 2 SE in 56–65% of specs; no other benchmark differs consistently.
  - Both are reported as notes in the outline's "Sensitivity analyses" section.
- **Interactions** (`80_interaction_screen.R`). They are second-order. Turnout is concave, and turnout and % BA matter more in diverse metros. The x × log P terms are unstable.
- **Curved size moderation** (`90_size_moderation_screen.R`, 2026-11-16). Each benchmark was given x × c and x × c² terms (c = centred log population) in all 3,636 default-model cs and pooled specs (filers, grassroots, spending).
  - Poverty is the one consistent case. The squared term is negative (concave) in 91–99% of specs for every outcome. For filers, the joint size test is significant in 92% of specs, in every identification and predictor form. The effect is negative at 100k (median t −2.0), positive by 1M (+1.3), and about 0 at 4M. Grassroots and spending are about 0 at 100k and positive at 250k–1M.
  - Foundation assets have no consistent curvature: the squared term is negative in only 42–54% of specs, and its sign depends on the predictor form (raw totals convex, log and SAMI concave for spending). In the SAMI form, size moderation for filers is significant in only 10% of specs.
  - Size moderation in general is common: % BA (stronger in smaller metros for filers) and turnout also pass the joint test in 40–70% of specs. What is distinctive about poverty is the sign change for filers.
  - Read curvature from the squared term, not from the marginal-effect t: t falls at the ends because few metros lie there.
  - Figures for the paper: `87`/`88` (`fig_<term>_size_window_coef`) and `fig_size_moderation.png`.

## Still open

- Nothing here is causal.
- Hospital ownership and state contracting data (AHA, CMS, Medicaid) would be needed to test the institutional explanation of the regional gap.
- The individual-giving result is suspect until the TCJA break is handled: restrict pooled and within contribution specs to 2010–17, or add a post-2018 slope (see the Nov 15 update).
- Census response, religion and the re-estimated social-capital index are built but not in the multiverse blocks.
- The composite-index run (`run_principled_cmp_merged`) predates v5 and uses log(x + 1).
