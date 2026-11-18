# Key variables for a unified panel model: results

Nov 17, 2026 · script `R/94_variables_panel_model.R` (parts build, describe, models, index, lean, figs) · outputs `D:/MULTIVERSE/data/variables/` · figures `paper/figures/var_*.png` · draft section `paper/02-variables.qmd` (replaces outline card D7).

## Setup

- **Sample.** 387 metros, 2010–2022 (5,031 metro-years). Cross-section = 2022. Models with the pre-1970 endowment have 385 metros (Memphis and Mankato are set to missing).
- **Outcomes.** Year-specific SAMIs (residual of log Y on log P, year by year): filers `n_filer_c3`; grassroots `n_bmf_c3pc − n_filer_c3` (no metro-year ≤ 0); spending without hospitals and universities `exp_c3_nohu`.
- **Covariate form.** Dollar totals and counts: `flog(x, pop)` then `resid_logP()` (SAMI form). Rates as is; incomes, GDP per worker and contributions per return logged. Racial and religious HHI enter as fractionalization (1 − HHI). Political division = 1 − |Dem margin|.
- **Models.** Log P on the right in every model (cs: log P; pooled: a log P slope for each year, `| year[logP]`; within: log P + metro and year FE), which makes rates size-adjusted by FWL. Coefficients per SD. Cross-section: robust SEs. Pooled 2010–2022: year FE, metro-clustered SEs. Within: metro + year FE, metro-clustered SEs. Partial r = t/√(t² + df), df = metros − K − 1.
- **Families.** Demand = need (poverty, unemployment, log median HH income, % 65+) and heterogeneity (Gini, % nonwhite, racial fractionalization, % foreign-born, religious fractionalization, age SD, political division). Supply = wealth (log per capita income, foundation assets, number of foundations, foundation grants, itemized contributions, contributions per return, GDP per worker, private wage bill), education (% BA), social capital (turnout, census mail response, religious adherence, congregations). Niche = growth (5- and 10-year kernels, 10-year growth, 1970–2000 growth), government (S&L earnings and jobs, Medicaid, Medicare + Medicaid + income maintenance, local welfare, local direct general expenditure), history (pre-1970 endowment, BLS nonprofit share of private jobs).

### Placement calls (variables that fit two families)

| Variable | Could be | Placed in | Why |
|---|---|---|---|
| Per capita income | need (income deficit) / supply (wealth) | supply | Positive given poverty (combined .16 filers cs); poverty carries the need side. It correlates .80 with median HH income and −.65 with poverty. |
| Median HH income | need / supply | need (dropped) | −.79 with poverty; the need index is poverty plus income. |
| Gini | need (deficit) / heterogeneity | heterogeneity | Only .22 with poverty; positive given poverty; loads on the diversity PC (.34), not need. |
| % 65+ | need / heterogeneity (age) | need | Level of a dependent population; age SD carries age heterogeneity (r .65). |
| Government size | demand (government failure) / niche | niche | Size and mode of public funding defines space for providers; it enters as two measures (direct, indirect). |
| Medicaid | need (tracks poverty .32) / niche | niche (indirect government) | Money paid to providers; nearly uncorrelated with S&L earnings (.08). |
| Congregations, religious fractionalization | social capital / heterogeneity | congregations: supply; fractionalization: demand | Different constructs (r .31). |

## 1. Correlation matrices (2022, model form)

Files: `cor_{demand,supply,niche}_2022.csv` (and `_pooled.csv`, 2010–2022); figures `var_cor_{demand,supply,niche}.png`. Each matrix leads with log P and the filer SAMI.

**Correlation with the three outcomes, 2022** (filers / grassroots / spending):

| Demand | r | Supply | r | Niche | r |
|---|---|---|---|---|---|
| Poverty | −.39 / −.17 / −.18 | Per capita income | .55 / .31 / .36 | Growth kernel 10-yr | −.27 / −.26 / −.27 |
| Unemployment | −.37 / −.30 / −.19 | Foundation assets | .57 / .44 / .42 | Growth 1970–2000 | −.29 / −.25 / −.29 |
| Median HH income | .41 / .16 / .25 | No. of foundations | .67 / .49 / .47 | S&L earnings | .37 / .24 / .32 |
| % 65+ | .09 / .12 / .02 | Foundation grants | .57 / .42 / .40 | S&L jobs | .29 / .23 / .27 |
| Gini | .17 / .19 / .20 | Contributions | .29 / .23 / .16 | Medicaid | .03 / −.03 / .09 |
| % nonwhite | −.42 / −.26 / −.26 | Private wage bill | .63 / .36 / .49 | Medicare+Medicaid+IM | −.16 / −.08 / −.03 |
| Racial fractionalization | −.16 / .07 / −.15 | % BA | .59 / .40 / .44 | Local welfare | .21 / .00 / .18 |
| % foreign-born | −.13 / −.26 / −.05 | Turnout | .48 / .41 / .31 | Local DGE | .14 / −.03 / .11 |
| Religious fractionalization | .22 / .49 / .11 | Census response | .28 / .10 / .23 | Pre-1970 endowment | .62 / .55 / .49 |
| Age SD | .06 / .11 / .04 | Religious adherence | −.28 / −.27 / −.20 | BLS nonprofit share | .56 / .32 / .57 |
| Political division | −.04 / .08 / .04 | Congregations | −.07 / .14 / −.06 | | |

The three outcomes correlate .67 (filers–grassroots), .71 (filers–spending), .41 (grassroots–spending).

**Redundant pairs, |r| > .7 (2022):**

| Pair | r | Family |
|---|---|---|
| growth kernel 5-yr vs 10-yr growth | .99 | niche |
| kernel 5-yr vs kernel 10-yr | .98 | niche |
| kernel 10-yr vs 10-yr growth | .95 | niche |
| foundation assets vs foundation grants | .93 | supply |
| S&L earnings vs S&L jobs | .92 | niche |
| contributions (SAMI) vs contributions per return (log) | .90 | supply |
| kernel 10-yr vs 1970–2000 growth | .83 | niche |
| Medicaid vs Medicare + Medicaid + income maintenance | .82 | niche |
| median HH income vs per capita income | .80 | demand × supply |
| poverty vs median HH income | −.79 | demand |
| kernel 5-yr vs 1970–2000 growth | .73 | niche |
| % nonwhite vs % foreign-born | .71 | demand |

Next tier (.6–.7): % nonwhite vs racial fractionalization .69; foundation assets vs number of foundations .69; % BA vs per capita income .67 and vs median HH income .67; % 65+ vs age SD .65; poverty vs per capita income −.65; per capita income vs GDP per worker .64; % nonwhite vs turnout −.61.

Cross-family pairs above .5: pre-1970 endowment vs growth kernel −.59 and vs 1970–2000 growth −.58; Medicaid vs growth kernel −.52; poverty vs turnout −.55; % nonwhite vs census response −.54; turnout vs census response .52.

**Size.** In SAMI form the totals are orthogonal to log P by construction. Rates correlate with log P at: % foreign-born .49, median HH income .48, racial fractionalization .47, per capita income .43, Gini .42, % BA .40, % nonwhite .38, age SD −.32, political division .27, poverty −.23, % 65+ −.21, growth kernel .22.

**VIFs within family** (2022, covariates residualized on log P; `vif_2022.csv`):

| Family | all items: max VIF | candidate set: max | candidate set in combined model: max |
|---|---|---|---|
| Demand (11 items) | poverty 5.7, % nonwhite 5.3, median HH income 4.9 | % nonwhite 2.9 | % nonwhite 4.7 |
| Supply (13 items) | contributions 54.6, per return 48.8, grants 7.9, assets 7.7 | per capita income 2.8 | % BA 5.4 |
| Niche (12 items, BLS share out) | 5-yr kernel 212, 10-yr kernel 153, 10-yr growth 52, 1970–2000 growth 11.9, S&L earnings 11.3, S&L jobs 10.0 | kernel 4.4 | 1970–2000 growth 6.4 |
| Lean set (13) | | | % BA 4.5 (all others ≤ 3.0) |

Collinearity sits inside constructs (twins), not across families. One item per construct brings every VIF under 5.5.

**Within-metro share of variance, 2010–2022** (two-way demeaned / total; `within_share.csv`): filer SAMI 1.2%, grassroots 4.9%, spending 7.2%. Covariates: 1970–2000 growth 0, census response 0.003%, % nonwhite 0.2%, endowment 0.4% (only through log P), % foreign-born 0.5%, % BA 0.8%, congregations 0.8%, S&L earnings 1.4%, BLS share 1.8% (10 years), growth kernel 1.9%, turnout 3.0%, Medicaid 5.3%, foundation assets 6.7%, poverty 8.0%, Gini 9.1%, unemployment 10.6%.

## 2. Constructs: PC1 and Cronbach's alpha

Items signed so higher = more of the construct; z-scores; pooled 2010–2022 (2022 in parentheses where it differs). `constructs_pc1_alpha.csv`.

| Construct | Items | PC1 share | α | Reading |
|---|---|---|---|---|
| Need | poverty, unemployment, −log MHI, % 65+ | .54 (.50) | .53 (.56); .79 (.73) without % 65+ | Poverty and income define it; % 65+ loads −.17 (.01) |
| Diversity | % nonwhite, racial frac., % foreign-born, religious frac., age SD, Gini, political division | .39 | .39 | Not one construct: religious frac. (−.21) and age SD (−.24) load against it |
| Wealth/philanthropy | foundation assets, no. of foundations, grants, contributions, log PCPI, log GDP/worker, private wage bill | .54 (.58) | .85 (.87) | One factor once size is removed; contributions (.30) and GDP/worker (.26) load least |
| Education/affluence | % BA, log PCPI, log MHI, −poverty | .76 | .89 | Coherent, but three of four items belong to other constructs |
| Social capital | turnout, census response, adherence, congregations | .41 | .33 (.22) | Two opposed poles: participation vs religion (adherence −.49, congregations −.44) |
| Government | S&L earnings, S&L jobs, Medicaid, Medicare+Medicaid+IM, local welfare, local DGE | .36 | .57 | Direct (earnings, jobs .62) and indirect (Medicaid −.21, transfers −.38) load on opposite ends; PC2 .32 |
| Growth | kernel 10-yr, kernel 5-yr, 10-yr growth, 1970–2000 growth | .89 | .96 | One construct; any item is the index |
| History | endowment, BLS nonprofit share | .73 | .63 (r = .47) | Two items; the share is partly the outcome |

## 3. Block models and the combined model

Candidate sets (one or two items per construct, twins removed):
- Demand (9): poverty, unemployment, % 65+, Gini, % nonwhite, % foreign-born, religious fractionalization, age SD, political division.
- Supply (8): log per capita income, foundation assets, contributions, % BA, turnout, census response, religious adherence, congregations.
- Niche (7): growth kernel 10-yr, 1970–2000 growth, S&L earnings, Medicaid, local welfare, local DGE, endowment.
- Combined: all 24.

Files: `block_models.csv`, `block_vs_combined.csv`, `block_models_fit.csv`, `drop_one.csv`, `absorbers_top3.csv`; figure `var_block_vs_combined.png`.

**Fit (R²)**

| Model | cs filers | cs grassroots | cs spending | pooled filers | pooled grassroots | pooled spending | within R², filers / grassroots / spending |
|---|---|---|---|---|---|---|---|
| Demand | .479 | .436 | .252 | .495 | .377 | .297 | .037 / .059 / .015 |
| Supply | .655 | .473 | .342 | .645 | .463 | .398 | .062 / .062 / .032 |
| Niche | .467 | .366 | .283 | .474 | .407 | .318 | .045 / .114 / .018 |
| Combined | .821 | .683 | .464 | .810 | .677 | .529 | .089 / .187 / .040 |

R² here includes log P (whose partial contribution to a SAMI outcome is ~0 in cs). Supply alone explains the most; the three families together add .17 (filers) to .21 (grassroots) over supply. Cross-section and pooled agree closely: the pooled panel is the cross-section repeated.

**Partial r, own family → combined** (cs 2022 / pooled 2010–2022; filers, grassroots, spending):

| Term | cs: own → combined | pooled: own → combined |
|---|---|---|
| Poverty | −.35 → −.12; −.28 → −.11; −.14 → .01 | −.41 → −.10; −.33 → −.11; −.23 → .05 |
| Unemployment | −.10 → −.03; −.08 → .01; −.04 → .03 | −.21 → −.07; −.12 → .04; −.11 → −.01 |
| % 65+ | −.14 → −.07; −.04 → .01; −.14 → −.06 | −.19 → −.11; −.04 → −.05; −.16 → −.05 |
| Gini | .43 → .01; .38 → .07; .35 → .05 | .40 → .02; .31 → .04; .39 → .14 |
| % nonwhite | −.31 → −.22; .01 → .14; −.26 → −.09 | −.29 → −.25; −.01 → .09; −.26 → −.10 |
| % foreign-born | .12 → −.01; −.13 → −.08; .12 → .06 | .14 → .03; −.07 → −.05; .12 → .03 |
| Religious frac. | .14 → .09; .44 → .32; .08 → −.01 | .14 → .09; .41 → .30; .10 → .03 |
| Age SD | .11 → .18; .09 → .08; .10 → .14 | .10 → .27; .05 → .11; .05 → .16 |
| Political division | .09 → −.08; .13 → .01; .13 → .01 | .13 → −.07; .18 → .03; .16 → −.02 |
| Per capita income | .34 → .16; .09 → .01; .16 → .04 | .36 → .21; .12 → .06; .19 → .09 |
| Foundation assets | .27 → .15; .25 → .10; .19 → .08 | .29 → .18; .27 → .10; .24 → .09 |
| Contributions | −.25 → .05; −.04 → .10; −.18 → .04 | −.25 → .03; −.06 → .06; −.21 → .01 |
| % BA | .44 → .30; .24 → .11; .34 → .20 | .48 → .40; .22 → .17; .43 → .25 |
| Turnout | .17 → .04; .34 → .27; .03 → .04 | .21 → .07; .43 → .36; .07 → .05 |
| Census response | .01 → −.17; −.22 → −.20; .06 → −.02 | .03 → −.18; −.19 → −.22; .10 → .03 |
| Religious adherence | −.13 → .00; −.24 → −.05; −.06 → −.03 | −.11 → .01; −.25 → −.08; −.08 → −.04 |
| Congregations | .31 → .02; .38 → .09; .16 → .01 | .30 → .04; .34 → .13; .20 → .00 |
| Growth kernel 10-yr | .06 → −.06; −.08 → −.13; .05 → .01 | .01 → −.02; −.14 → −.18; .04 → .05 |
| 1970–2000 growth | −.03 → .00; .09 → .02; −.07 → −.10 | .00 → .01; .18 → .17; −.08 → −.11 |
| S&L earnings | .22 → .23; .15 → .18; .19 → .09 | .25 → .25; .17 → .15; .24 → .14 |
| Medicaid | −.14 → .18; −.19 → .00; −.04 → .12 | −.16 → .21; −.19 → .03; −.04 → .18 |
| Local welfare | .22 → .18; −.01 → −.05; .14 → .07 | .17 → .21; −.03 → −.04; .13 → .11 |
| Local DGE | −.10 → −.10; −.17 → −.13; −.07 → −.07 | −.07 → −.11; −.12 → −.09; −.10 → −.10 |
| Pre-1970 endowment | .51 → .32; .40 → .29; .33 → .13 | .53 → .37; .47 → .40; .36 → .14 |

**Status counts** over 24 terms × 6 cells (cs and pooled × 3 outcomes; "significant" = |t| ≥ 1.96): survives 42, absorbed 44, null in both 31, flips 19, emerges 8.

- **Survive everywhere:** % BA (6 of 6), endowment (6 of 6). S&L earnings survives in 5 of 6 (absorbed for spending cs, .19 → .09).
- **Absorbed:** Gini (.35–.43 → .01–.07 in 5 of 6 cells; survives only pooled spending .14); congregations (.16–.38 → .00–.13); turnout for filers (.17 → .04, by % BA); per capita income for grassroots and spending; foundation assets for grassroots and spending (they keep .08–.10); unemployment and % 65+ mostly.
- **Flip:** Medicaid (−.14 to −.19 alone → +.12 to +.21 combined, every outcome but grassroots; it was confounded with low education and fast growth, r −.37 with % BA and −.52 with the kernel); contributions (−.25 → +.05 for filers); census response for filers (.01 → −.17); % foreign-born for filers (+.12 → −.01); political division (+.09 to +.18 → −.08 to +.03); poverty for spending (−.14 → +.01).
- **Emerge:** age SD (.10 → .27 pooled filers; its correlation with % BA, −.43, hid it).

**Main absorbers** (`absorbers_top3.csv`, the dropped covariate that moves a term furthest back toward its own-family value): % BA absorbs Gini (filers pooled: .02 → .20 without % BA), turnout (.07 → .24) and unemployment; per capita income absorbs poverty (−.10 → −.26); the endowment absorbs foundation assets (.18 → .26) and religious fractionalization; religious fractionalization absorbs congregations (grassroots cs .09 → .25); % nonwhite absorbs the census-response sign (−.18 → −.09).

**Within (metro + year FE), combined model, partial r** (filers / grassroots / spending): growth kernel −.16 / −.22 / −.03; unemployment −.12 / −.21 / −.02; % BA .16 / .20 / .04; turnout .12 / .17 / .07; contributions .15 / −.02 / .04 (partly the TCJA break); local DGE .12 / −.04 / .05; S&L earnings −.08 / −.26 / −.06; age SD −.07 / .20 / .00; poverty .10 / .20 / .03; Medicaid .00 / −.01 / .12; foundation assets .06 / .00 / −.03; % nonwhite −.08 / −.10 / −.03. Endowment and 1970–2000 growth leave the within model. Within R² is .04–.19. Signs that differ from cs: S&L earnings (+.23 cs → −.08 within for filers, −.26 for grassroots), poverty (−.12 → +.10), age SD for filers (+.18 → −.07).

## 4. Item vs index, per construct

Base = combined model with each construct carried by its dominant item, plus % 65+ and Gini. One construct at a time is swapped for its PC1 score, its standardized mean, or each alternative item. Weights and z constants fixed on pooled 2010–2022. `item_vs_index.csv`, `item_vs_index_summary.csv`; figure `var_item_vs_index.png`.

Partial r, cs 2022 (filers / grassroots / spending); mean |r| over 6 cells (cs + pooled); cells with p < .05:

| Construct | Version | cs partial r | mean \|r\| | sig. cells | sign agreement |
|---|---|---|---|---|---|
| Need | **poverty** | −.27 / .02 / −.06 | .119 | 2 | 4 of 6 |
| | PC1 | −.29 / .03 / −.04 | .114 | 2 | 4 of 6 |
| | standardized mean | −.14 / .01 / −.03 | .059 | 2 | 4 of 6 |
| Diversity | **% nonwhite** | −.14 / .09 / −.04 | .092 | 2 | 4 of 6 |
| | PC1 | −.11 / .05 / −.02 | .061 | 2 | 4 of 6 |
| | standardized mean | −.02 / .28 / .02 | .099 | 2 | 4 of 6 |
| | alt: age SD | .15 / .10 / .15 | .127 | 4 | 6 of 6 |
| | alt: Gini | .09 / .14 / .09 | .115 | 3 | 6 of 6 |
| | alt: religious frac. | .08 / .47 / −.03 | .188 | 2 | 5 of 6 |
| | alt: racial frac. | −.03 / .37 / −.10 | .162 | 3 | 4 of 6 |
| Wealth | **foundation assets** | .21 / .11 / .10 | .145 | 6 | 6 of 6 |
| | PC1 | .26 / .10 / .15 | .168 | 4 | 6 of 6 |
| | standardized mean | .25 / .09 / .14 | .156 | 4 | 6 of 6 |
| | alt: no. of foundations | .26 / .15 / .13 | .188 | 5 | 6 of 6 |
| | alt: contributions | .15 / .14 / .06 | .086 | 2 | 5 of 6 |
| | alt: GDP per worker | −.06 / −.10 / −.04 | .068 | 0 | 6 of 6 (negative) |
| Education | **% BA** | .22 / .10 / .19 | .176 | 4 | 6 of 6 |
| | PC1 (affluence) | .21 / −.03 / .18 | .148 | 4 | 5 of 6 |
| Social capital | **turnout** | .10 / .30 / .08 | .177 | 3 | 6 of 6 |
| | PC1 | −.06 / −.02 / .05 | .054 | 1 | 3 of 6 |
| | standardized mean | .01 / .09 / .02 | .035 | 0 | 5 of 6 |
| | alt: census response | −.14 / −.16 / .03 | .115 | 4 | 4 of 6 |
| Government | **S&L earnings** | .23 / .00 / .10 | .133 | 3 | 6 of 6 |
| | PC1 | .09 / .01 / .01 | .050 | 1 | 6 of 6 |
| | standardized mean | .29 / −.16 / .18 | .208 | 5 | 4 of 6 |
| | alt: Medicaid | .27 / −.09 / .19 | .192 | 4 | 4 of 6 |
| Growth | **kernel 10-yr** | −.15 / −.01 / −.12 | .100 | 4 | 5 of 6 |
| | PC1 | −.16 / −.02 / −.14 | .110 | 4 | 5 of 6 |
| | alt: 1970–2000 growth | −.06 / −.02 / −.15 | .090 | 2 | 5 of 6 |
| History | **endowment** | .37 / .33 / .18 | .317 | 6 | 6 of 6 |
| | PC1 (with BLS share) | .39 / .18 / .28 | .308 | 6 | 6 of 6 |
| | alt: BLS share | .28 / −.02 / .22 | .182 | 4 | 5 of 6 |

Recommendations:
- **Need → poverty.** PC1 = poverty (loadings .61 poverty, .61 −MHI); no gain. % 65+ does not belong (α rises to .79 without it).
- **Diversity → separate items, not an index** (α .39). % nonwhite for composition; age SD and religious fractionalization as separate heterogeneity terms; Gini as economic heterogeneity. The standardized mean flips the grassroots sign (+.28) because it is mostly religious fractionalization.
- **Wealth → foundation assets.** Index gains .04–.05 for filers and spending, loses on grassroots and significance (4 vs 6 cells), and folds in per capita income and post-TCJA contributions. Number of foundations is the strongest single item but counts private foundations, which share registration propensity with the outcome. Report PC1 as the robustness check.
- **Education → % BA.** The affluence index is coherent (α .89) but borrows income and poverty from need and wealth, and loses grassroots.
- **Social capital → turnout.** α .33: participation and religion are opposed poles. Index collapses to ~0.
- **Government → two items, S&L earnings (direct) and Medicaid (indirect).** PC1 averages opposed loadings (.09 / .01 / .01). The standardized mean is strong only because it carries Medicaid.
- **Growth → kernel 10-yr.** Index ≈ item (α .96). The kernel is time-varying and has a half-life interpretation.
- **History → endowment.** The PC1 with the BLS share lifts spending (.18 → .28) only because the share counts hospital and university jobs.

## 5. Lean set and its models

LEAN (13): poverty, % 65+, Gini, % nonwhite, religious fractionalization, age SD, foundation assets, % BA, turnout, growth kernel 10-yr, S&L earnings, Medicaid, pre-1970 endowment. Max VIF 4.5 (% BA). `lean_models.csv`, `lean_vif_2022.csv`.

Partial r (filers / grassroots / spending):

| Term | cs 2022 | pooled 2010–22 | within | no endowment, cs |
|---|---|---|---|---|
| Poverty | −.28 / −.06 / −.04 | −.29 / −.06 / −.05 | .04 / .13 / −.02 | −.29 / −.07 / −.05 |
| % 65+ | −.03 / −.01 / −.05 | −.06 / .00 / −.04 | .00 / −.13 / −.01 | −.03 / −.01 / −.05 |
| Gini | .13 / .15 / .07 | .14 / .08 / .14 | .07 / −.10 / .02 | .07 / .11 / .04 |
| % nonwhite | −.20 / .12 / −.09 | −.19 / .12 / −.13 | −.09 / −.11 / −.04 | −.21 / .11 / −.10 |
| Religious frac. | .12 / .52 / .00 | .14 / .52 / .09 | −.04 / .09 / −.09 | .20 / .53 / .05 |
| Age SD | .15 / .07 / .12 | .18 / .06 / .10 | −.10 / .19 / −.03 | .17 / .10 / .13 |
| Foundation assets | .22 / .12 / .10 | .22 / .13 / .13 | .06 / −.02 / −.02 | .29 / .19 / .15 |
| % BA | .37 / .14 / .26 | .46 / .16 / .31 | .19 / .15 / .07 | .41 / .19 / .29 |
| Turnout | .01 / .24 / .03 | .03 / .32 / .04 | .11 / .15 / .09 | −.05 / .19 / .00 |
| Growth kernel 10-yr | −.05 / −.07 / −.05 | .00 / .01 / −.07 | −.20 / −.26 / −.03 | −.22 / −.25 / −.15 |
| S&L earnings | .23 / .08 / .09 | .24 / .13 / .11 | −.02 / −.32 / .00 | .29 / .14 / .12 |
| Medicaid | .25 / .01 / .16 | .26 / .04 / .23 | .03 / −.01 / .15 | .27 / .03 / .18 |
| Endowment | .32 / .26 / .15 | .34 / .34 / .15 | — | — |

Coefficients per SD, cs 2022 (log points of density): % BA .171 / .050 / .306; endowment .100 / .069 / .131; foundation assets .060 / .024 / .084; S&L earnings .056 / .017 / .061; Medicaid .065 / .002 / .116; poverty −.094 / −.016 / −.038; % nonwhite −.070 / .033 / −.080; religious frac. .028 / .092 / .001; age SD .068 / .026 / .122; Gini .040 / .037 / .061.

Fit: cs R² .791 / .620 / .451; pooled .783 / .618 / .512; within R² .067 / .120 / .026. Without the endowment cs R² falls to .765 / .578 / .437.

Variants:
- **+ per capita income:** poverty for filers −.28 → −.12, Gini .13 → .05, foundation assets .22 → .16; income itself .17 / −.02 / .06. Poverty's sign and size depend on whether income is in the model.
- **No Gini:** % BA rises .37 → .48 (filers cs), poverty −.28 → −.25.
- **Wealth index instead of foundation assets:** index .27 / .12 / .15; poverty moves to −.17 (the index carries per capita income).
- **+ unemployment:** −.07 / −.11 / .04 cs; −.14 / −.21 / −.06 within. Its use is within-metro.
- **No endowment:** growth kernel goes from −.05 to −.22 (filers cs). The endowment and growth history are the same Sunbelt fact in levels (r −.59); the kernel keeps its within-metro effect (−.20) either way.

## 6. Contaminated and outcome-adjacent measures (2022, r with filer SAMI)

- Rupasingha `sk` .69; re-estimated `socap_sk2` .47 (grassroots .39); association establishments (SAMI) .54 (grassroots .56). Census mail response .28 and turnout .48 for comparison. All three contain organization counts: excluded.
- BLS nonprofit share of private jobs: .56 filers, .57 spending; includes hospitals and universities; 2013–2022 only (334–370 metros); within share 1.8%. Excluded from the model (D8, Fact 5).
- Pre-1970 endowment .62: the outcome's own past (D8), kept as an estimand choice and reported with and without.

## 7. Consistency with the outline

- **D7 (replaced).** Agrees: % nonwhite over fractionalization; foundations and individual giving apart; one item where one item carries the block (% BA, turnout). New: the wealth construct is coherent in SAMI form (α .85) but the item is preferred; government must be split, not indexed; growth is one construct.
- **D8.** Agrees on endowment as an estimand choice and on excluding market share. Qualifies "include growth history in any current-conditions model": with the endowment in, the kernel's level effect is ~0 (−.05); it matters in levels only without the endowment (−.22) and within metros (−.20). Keep it for the within model and as the time-varying growth term.
- **D12.** Consistent: no sector-age term; the endowment covers history.
- **Benchmarks.** Poverty's filer partial r here (−.28) is more negative than the multiverse median (≈0) because the lean model includes the endowment and Gini, both of which push poverty negative (D8, v9). Poverty reverses with metro size (negative under ~250k, positive above; `R/86`, `R/90`): reported as heterogeneity.

## Caveats

- Interpolated covariates: Gini (ACS 5-year), congregations and adherence (RCMS 2010/2020), CoG welfare and DGE (census years), census response (frozen after 2014), turnout (observed in election years only). Their within estimates are mostly interpolation.
- Contributions span the 2018 TCJA break inside the 2010–2022 window.
- Medicaid is recorded by beneficiary residence but pays providers; dense health nonprofit sectors could raise it.
- Political division uses the presidential margin, interpolated between elections.

## 8. Market competition measures (added 2026-11-17, `R/95_compete_niche.R`)

Twelve concentration measures from `23_cov_compete.R` (local universe unless noted: no health, Q, T, U, B11, uncoded) were added one at a time to the 13-covariate lean set, on the common sample of 385 metros, 2010-2022. Outcomes were rebuilt from the current panel, after the 2026-11-17 NTEE backfill that moved spending (r old vs new spending SAMI, 2022 = .972). Refitting the lean model on the corrected spending series moves no spending partial r by more than .04, so sections 1-7 stand.

Outputs: `D:/MULTIVERSE/data/variables/compete/` (`correlations_2022.csv`, `matrix_2022.csv`, `addone_summary.csv`, `addone_models.csv`, `pairs_models.csv`, `change_2012_2022.csv`); figure `paper/figures/var_compete_addone.png`.

**Mechanical links.** HHI >= 1/n, with n the number of filers, so log HHI and CR4 correlate -.68 with log filers; normalized HHI -.38, CR1 -.40. The spending SAMI is the revenue being divided. Grassroots appear in no measure: the clean outcome.

**Redundancy.** Log HHI vs CR4 .96; normalized HHI vs CR1 .95; HHI vs subsector HHI .89. Gini vs n x HHI .85; Gini vs small-org revenue share -.82. Local and whole-sector (excl. H/U) normalized HHI correlate only .35. Measures are nearly orthogonal to the lean set given log P (largest |r| .17-.37, VIF 1.05-1.35).

**Grassroots (partial r, cs 2022 / pooled / within; 2012 measure -> 2012-22 change):**
- normalized HHI -.17 / -.07 / -.04; -.11 (p .04)
- log HHI -.19 / -.13 / -.03; -.07 (n.s.)
- CR1 -.15 / -.07 / -.03; -.09 (p .07)
- CR4 -.21 / -.16 / -.06; -.08 (n.s.)
- normalized HHI of contributions -.15 / -.03 / .02; -.15 (p .005)
- revenue Gini -.15 / -.05 / -.07; -.05 (n.s.)
- HHI + CR4 together: CR4 -.13, HHI .04.

**Filers (mechanical):** dominance measures -.06 to -.22 cs; n x HHI and Gini positive (.18, .07). **Spending (mechanical):** dominance .18-.47 and small-org shares -.27 to -.29, in levels and within metros; whole-sector normalized HHI .47 (headquarters revenue).

**Fit.** Delta R2 <= .015 for filers and grassroots; up to .15 for spending (whole-sector nHHI), all mechanical. No lean coefficient moves more than .03 for filers or grassroots (up to .10 for spending: age SD with the small-org share).

**Decision.** Not in the default model: partly the outcome for filers and spending (same status as the BLS share), and small, cross-sectional and adding nothing on grassroots. Keep one extension: a niche-crowding test on grassroots with normalized HHI (local revenue or contributions) dated before the outcome.
