# Data Construction Plan: Nonprofit Density Multiverse

Nov 12, 2026 · draft for discussion

This plan supports the *Multiverse Analysis Design* (Nov 3) and the SAMI note (`sami-nonprofit-density.qmd`). It covers what to build, from which sources, over what years, how the sources fit together, and the full list of forks the data must support.

---

## 0. Decisions this plan makes (and the ones it leaves open)

| Decision | Recommendation | Why |
|---|---|---|
| Panel window | **2000–2024** (25 years). Fall back to 2001 if CORE 2000 fails the validation checks in §7 | 2000 is a census year, so the panel starts from observed values: 2000 SF3 (education, foreign-born, Gini), RCMS 2000, and the intercensal population base. Older CORE files are less stable, and nothing before 2000 is needed. |
| Unit spine | **County × year, built on 2020 county boundaries**, then aggregated to 2023 CBSA delineations | All sources are county-native or can be geocoded to county. A fixed delineation is required by the SAMI note's panel section, because boundary changes would otherwise appear as population growth. |
| Nonprofit financials | **Three eras**: NCCS harmonized CORE PZ (2000–2010) → SOI extract panel (2011–2020) → **NCCS e-file via panel990** (2021–2024; 990 + 990-EZ + 990-PF, near-complete) | Adjacent sources overlap (CORE/SOI 2011–2019; SOI/e-file 2021–2023), so each splice can be validated rather than assumed. |
| Registered-org counts | **NCCS BMF snapshots** (local, 2000–2024) | This is the "BMF-registered" arm of the organization-filter fork. |
| Entry flows | **Long BMF Panel** (local; cohorts through 2022), extended to 2023–2024 using ruling dates in the 2024 BMF | It is a cohort panel of *new* 501(c)(3)s, so it gives births but not stocks. It is not a substitute for a density measure. |
| Bettencourt measures | **Yes, as a separate module and an optional sixth block**, not part of the principled universe (§5) | Strong value as scaling benchmarks and as a SAMI-on-SAMI question. They are poor controls because they are jointly determined with income and with university and hospital activity. |

Open for you to decide: (a) metro-only (~390) vs. CBSA including micropolitan (~930) as the default unit; (b) whether commuting zones are worth building as a unit fork, given that they need a separate 2020 CZ delineation.

---

## 1. Geography spine (build first; everything else hangs on it)

**`geo_county_spine`**: one row per county FIPS (2020 vintage) × year, 2000–2024.

Boundary events to harmonize onto 2020 counties:
- **Connecticut**: the 2022 switch to 9 planning regions. Keep the 8 legacy counties and map post-2022 data back using the Census CT crosswalk. PEP and ACS from 2022 on are released by planning region only.
- **Virginia**: Bedford city merged into Bedford County in 2013; Clifton Forge in 2001.
- **South Dakota**: Shannon County became Oglala Lakota (46113 → 46102) in 2015.
- **Alaska**: borough splits and merges in 2008, 2013, and 2019. Alaska is in the "drop" sample fork anyway.
- **Colorado**: Broomfield was created in 2001; attribute 2000 data by tract share.

`population/county-harmonization-2000-2020.R` already does part of this. Extend it rather than rewrite it.

**Aggregation crosswalks** (each one a unit fork):
- `county → CBSA 2023` (`population/cbsa2fipsxw.csv`, already local), with flags for metro vs. micro and central vs. outlying.
- `county → CBSA 2013` (alternative delineation vintage; a robustness fork for delineation sensitivity).
- `county → CSA`, and `county → commuting zone` (USDA ERS 2000 CZ; optional 2020 CZ from Fowler & Jensen).
- Keep **county** as a unit in its own right. Kim (2015) and Lecy & Van Slyke (2012) both worked at the county level.

**Organization geolocation rule** (must be the same across CORE, SOI, and BMF):
- *Primary*: filing-year ZIP → county using a HUD ZIP–county crosswalk of the matching year, with residential-address weights. The location can change year to year, so it captures organizations that move.
- *Alternative*: the current geocoded address (`CENSUS_BLOCK_FIPS` in the SOI frame; lat/long in NCCS BMF), held fixed for the organization's whole history.
- Check this. The current `build-metro-data.R` uses `CENSUS_CBSA_FIPS`, which appears to come from the *current* geocode. That is fine for a cross-section but slightly overstates stability in a panel. The choice becomes a fork (§9.6).

---

## 2. Nonprofit outcomes

### 2.1 Sources and coverage

| Source | Years | Content | Location |
|---|---|---|---|
| NCCS BMF snapshots | 2000–2024 used | All registered exempt orgs: subsection, NTEE, ruling date, ZIP, income/asset codes | `00 - DATA/00-NCCS/bmf/` |
| NCCS harmonized CORE 501(c)(3) PZ | used for 2000–2010; 2011–2019 kept for splice checks | 990 + 990-EZ filers: revenue, expenses, contributions, assets, ZIP | `nccsdata.s3.amazonaws.com/harmonized/core/501c3-pz/` |
| NCCS harmonized CORE 501(c)(E) (non-c3) | same | Other exempt orgs; needed for the "all 501(c)" arm | same bucket; locate the path |
| NCCS CORE private foundations (PF) | 2000–2010 | 990-PF assets and grants paid, used for the philanthropy block | path still to find; my guess at the bucket path returned 403 |
| SOI extract panel (990 / EZ / PF) | used for TY 2011–2020; 2021–2023 kept for splice checks | Same fields, harmonized, with NTEEV2 and CBSA/block geocodes | `00 - DATA/SOI/02_sample-framework/…AUGMENTED.CSV`, `01_data/04_final/` |
| NCCS e-file tables via **panel990** | TY 2021–2024 (near-complete universe) | 990, 990-EZ, and 990-PF; headers, Part I totals, Part VIII/IX; Schedule data available for later extensions | <https://nccs.urban.org/nccs/datasets/efile/> |
| Long BMF Panel | 1995–2022 rule-year cohorts | New 501(c)(3) entrants, annual BMF presence, CORE revenue/assets by fiscal year | `00 - DATA/BMF/` |

### 2.2 Splice: CORE (2000–2010) → SOI (2011–2020) → e-file (2021–2024)

There are two splice points, and both have overlap years.

| Splice | Overlap | What can differ | Main risk |
|---|---|---|---|
| CORE → SOI | 2011–2019 | NCCS cleaning rules, deduplication, and which returns count as "in scope"; the definition of tax year | Level shifts that differ by metro |
| SOI → e-file | 2021–2023 | Both come from IRS filings, so totals should nearly match. Differences come from amended or duplicate returns, and from paper filers still present in SOI | Duplicate EIN-years in e-file |

1. Build county-year and CBSA-year aggregates from **every** source in each overlap window.
2. For each aggregate (count, expenditure, revenue, contributions, foundation assets), report:
   - national totals by source and year;
   - the CBSA-level correlation in logs;
   - the median SOI/CORE and e-file/SOI ratios, and their spread across metros;
   - the share of EINs found in both sources.

   `SOI/CORE-VS-SOI-PANEL-VALIDATION.R` already starts this for 2015 and 2018.
3. **E-file deduplication rule**: keep one return per EIN × tax year. Prefer the original over an amended return unless the amended one is the only full return, and prefer the latest submission when there are several. Apply the same rule wherever SOI has duplicates.
4. **Tax-year rule**: define the year by the start of the tax period (`TAXYEAR`) in every source, and check that CORE's `F9_00_TAX_YEAR` agrees.
5. **Splice years (default)**: 2010→2011 and 2020→2021. If the overlap shows a better-behaved year, move the splice there.
6. Keep the **source-pure** series as forks: CORE-only (2000–2019), SOI-only (2011–2023), e-file-only (2021–2024). A spliced series that behaves differently from every source-pure series is a red flag.
7. How splice breaks enter the models is covered in §2.6.

### 2.3 Regulatory breaks that every long nonprofit series must handle

These are not noise. In a fixed-effects design they will be read as real change unless handled.

| Break | Year | Affects | Handling |
|---|---|---|---|
| 990-N threshold raised from $25k to $50k gross receipts | TY 2008 (phased 2008–2010) | Filer counts: orgs with $25–50k leave the 990/EZ files | **Impose a real-dollar floor across all years** ($50k in 2010 dollars as the default; $25k nominal as a fork for ≥2010 only) |
| 990-EZ ceiling raised from $100k to $200k receipts | TY 2008–2010 | PC vs. EZ split | Always use **PZ** (990 + EZ together), never PC alone |
| PPA automatic revocations | 2010–2011 (and ongoing) | BMF counts drop by about 275k | BMF count arm: flag 2010–2012; prefer *filer* counts for within-metro designs; build the BMF series both with and without auto-revoked EINs (the SOI frame has `REVOKED_YEAR_X`) |
| Form 1023-EZ introduced | July 2014 | Entry flows jump ~80% | Entry outcome: include a 2014+ indicator, or run separately before and after |
| Redesigned Form 990 | TY 2008 | Some line definitions; expense and revenue totals are stable | Use only total revenue, total expenses, total contributions, total assets |
| COVID | 2020–2021 | Revenue, contributions (PPP loans booked as contributions or grants) | Sensitivity fork: drop 2020–2021 |
| Mandatory e-filing | TY 2020+ | Coverage improves in SOI; minimal effect on CORE | Covered by the splice diagnostics |

### 2.4 Completeness by tax year

SOI `pz_taxyear_2023.csv` is about 74% the size of 2022, and 2024 is a stub. That is why 2021 onward comes from e-file, which is near-complete through 2024. Two checks are still worth making:
- **Late fiscal years in TY2024**: organizations whose fiscal year ends in mid-2025 file by May 2026 with an extension. Compare the 2024 filer count with 2023 by fiscal-year-end month; a shortfall concentrated in late months means 2024 needs a completeness flag.
- **The existing `metro-npo-pop.csv`** uses SOI for 2023 and should be rebuilt from the spliced series.

### 2.5 Outcome variables to construct (CBSA × year; also county × year)

Each variable is built for four organization filters: **all 501(c)**, **501(c)(3)**, **501(c)(3) excluding hospitals and universities** (NTEE E20–E24, B40–B43, B50), and **human services** (NTEE I–P). All dollars are deflated to 2022 dollars with the PCE price index, with nominal values kept alongside.

| Variable | Definition | Universe |
|---|---|---|
| `n_bmf` | Registered orgs in the BMF snapshot nearest Dec of the year | Stock, registered |
| `n_filer` | Distinct EINs filing 990/EZ for the tax year, above the real floor | Stock, active |
| `n_filer_nofloor` | Same, no floor (valid only within 2010+) | Fork |
| `n_entry` | New rulings in the year (Long BMF Panel cohort) | Flow |
| `exp`, `rev`, `con`, `assets` | Sums over filers, negatives set to 0, `rev ≥ con` repaired as in `prelim-sami-plots.R` | Scale |
| `emp` (optional) | Employees from 990 Part I (990 filers only, 2008+) | Scale; optional |
| `share_top1_exp` | Largest org's share of metro expenditure | Diagnostic for concentration and outliers |

### 2.6 Modeling the breaks: construction first, model terms only where construction can't fix it

**Plain period dummies are mostly redundant.**
- Any break that shifts every metro by the same proportion is already absorbed by year fixed effects. A dummy for 2010–2012 in a model with year FE is perfectly collinear with them.
- Year-by-year SAMIs are mean-zero each year by construction, so a nationwide level shift disappears from the outcome before any model is run.
- In single-year cross-sections, a break only affects which year is chosen. It does not bias the estimate.

**What actually biases estimates is *differential exposure*.** For example, the $50k filing threshold removes more organizations from metros whose sector is made up of many small organizations. Those metros are also poorer, smaller, and more rural. In a within-metro model, the break then appears as a change in density that lines up with poverty and income, and the coefficients on those variables absorb it. The same logic applies to auto-revocation (dormant small organizations), the 1023-EZ (entry concentrated in some places), TCJA (itemizer share tracks income), and each source splice (coverage differences by metro).

**Ways to handle it.** Each option is a level of a break-handling fork (B1 in §9.6).

| Option | What it does | Cost |
|---|---|---|
| **B1a. Construction + year FE** (default) | Consistent real-dollar floor; filer counts rather than BMF counts in within designs; year FE or year-specific SAMIs | Leaves differential exposure in place, if there is any |
| **B1b. Exposure × post-period terms** | Add `exposure_i × 1[t ≥ event]`, where exposure is measured *before* the event and held fixed (table below). The coefficient is also a diagnostic: near zero means the break was neutral across metros | One extra term per event; exposure has to be defined in advance |
| **B1c. Metro × era fixed effects** | Each metro gets its own intercept per source era (CORE / SOI / e-file), so only within-era change is used | Most robust to splice shifts, but throws away change *across* eras, which is much of a 25-year panel's within-metro signal |
| **B1d. Ratio splicing** (data fork, not a model term) | Rescale each metro's earlier-era series by its own overlap-year ratio, the way price indexes are chain-linked | Assumes the metro's ratio is stable over time; check that in the overlap years |
| **B1e. Drop break windows** | Exclude 2008–2011 and 2020–2021, or estimate each era separately and compare | Fewer years; no adjustment needed |

**Real shocks are not measurement artifacts.** The Great Recession, TCJA, and COVID changed the nonprofit sector itself. Year FE handle their national component, and dropping those years is a sensitivity fork. By default they are not "controlled away" with metro-specific terms. TCJA is also a natural experiment in its own right (exposure = itemizer share in 2015–2017), which could be a sidecar paper rather than part of the multiverse.

| Event | Year | Kind | Outcomes affected | Exposure measure for B1b |
|---|---|---|---|---|
| 990-N threshold $25k → $50k | 2008–2010 | Artifact | `n_filer` (only if no floor is set) | Share of a metro's filers with $25–50k gross receipts, 2005–2007 |
| PPA auto-revocation | 2010–2011 | Artifact | `n_bmf` | Share of BMF orgs with no 990/EZ/N filing, 2007–2009 |
| 1023-EZ | 2014+ | Partly real | `n_entry`, small-org `n_filer` | Pre-2014 share of new rulings with receipts under $50k |
| TCJA | 2018+ | Real | Giving covariates, `con` | Itemizer share of returns, 2015–2017 |
| COVID / PPP | 2020–2021 | Real | `rev`, `con`, `exp` | None; use the drop-years fork |
| CORE → SOI splice | 2011 | Artifact | All financial series | Metro's SOI/CORE ratio, 2011–2013 |
| SOI → e-file splice | 2021 | Artifact (small) | All financial series | Metro's e-file/SOI ratio, 2021–2023 |

### 2.7 BLS nonprofit employment and wages (QCEW research data)

**Location:** `00 - URBAN/00-DATA/bls/`, prepared by `BLS-DATA-PREP.R`.

**What's there:** BLS matches QCEW establishments to IRS 501(c)(3) status. For each geography and industry it reports nonprofit and total-private establishments, employment, total wages, average annual and weekly wage, the nonprofit share of employment (`percent_npo`), and the nonprofit/private wage ratio (`wage_ratio_npo`).

| Years | Geography | Coverage (total private) | Industry detail |
|---|---|---|---|
| 2007–2012 | US and states only | All states | 2- and 3-digit NAICS |
| 2013–2017 | US, state, MSA, county | MSAs: 344–380; counties: about 1,630–1,690 (the rest are suppressed) | About 45 industries with nonprofit values at MSA level |
| 2018–2022 | same | MSAs: 383–386 of about 390; counties: about 1,850–1,910 | About 108 industries |

Check whether BLS has released 2023 or later.

**Why it is worth adding.**
1. **A third outcome construct: nonprofit employment and payroll.** Add `npo_emp` and `npo_wages` to stratum S1. Their scaling exponent sits alongside β_N and β_E, and they offer the cleanest "sector scale" measure, because they are not dominated by the expenses of a few very large filers.
2. **The sector is counted where people work.** QCEW counts each establishment in its own location, while 990 data puts all of an organization's spending at its headquarters. Comparing `exp` (990) with `npo_wages` (BLS) by metro measures the **headquarters-attribution error** in the 990 outcomes. That error is largest for multi-site hospital and university systems, so it also tests whether excluding hospitals and universities fixes the problem.
3. **Cross-sector market share** (`percent_npo`): the nonprofit share of private employment. It is a ratio of two size-dependent totals, so check its relationship to log P before treating it as size-neutral. It is a natural intensive outcome, and in health (NAICS 62), education (61), social assistance (624), and arts (71), where not suppressed, it measures nonprofit market share within an industry.
4. **Wage premium** (`wage_ratio_npo`). This connects to the labor-market-power paper. Be careful with the raw ratio: it is driven by industry mix, because nonprofit employment is concentrated in health care. Build a **composition-adjusted ratio**, weighting each industry's wage ratio by fixed national employment shares, wherever industry cells are not suppressed.

**Limitations and how they shape its use.**
- **Short window**: metro data start in 2013. These outcomes go into the cross-section and **2013–2022 short-panel** strata only, never the 2000–2024 panel.
- **Use the MSA files directly.** Do not aggregate the county files: county suppression is not random (small counties drop out), so county sums would be biased. That makes the unit metro-only, and puts it in the U1 = MSA cell.
- **Delineation vintage**: QCEW MSA codes (e.g., `C1018`) follow the OMB definitions in force for each year (2013 vs. 2018 vintages). Map them to 2023 CBSAs and flag metros whose definition changed.
- **Covers 501(c)(3) only**, so it pairs with the O1 = 501(c)(3) filter.
- **Role in the model**: use it as an outcome or for validation, not as a predictor of density. The wage premium and market share are determined together with the nonprofit count.

---

## 3. Population

`population/cbsa_population_2000_2025.csv` and `county_population_2000_2025.csv` already exist on 2023 delineations. Add:
- **Alternative population bases**, as the SAMI note recommends (Recommendation 7): adults 18+, households (from PEP characteristics and ACS), and **daytime / commuting-adjusted population** (ACS commuting flows, 2009+).
- A `pop_interp` flag. PEP vintages differ, so use intercensal series for 2000–2010 and 2010–2020, and the latest postcensal vintage (V2025) for 2021–2024.

---

## 4. Covariates by block

Frequency codes: **A** = annual, **D** = decennial or periodic (needs interpolation, which is a fork), **X** = cross-section only (usable only in cross-section strata).

**Window: 2000–2024 for every series.** As of late 2026 these releases cover 2024: SAIPE 2024, ACS 5-year 2020–2024, BEA county income and GDP 2024, PEP V2025, LAUS 2024, and the 2024 presidential returns. Where a table below lists an earlier end year, extend it to 2024. Series with no 2024 release are carried forward and flagged: Census of Governments 2022, RCMS 2020, Rupasingha (last release), and UCR (stops at 2020 by design).

### 4.1 Diversity / government failure

| Measure | Source | Years | Freq | Note |
|---|---|---|---|---|
| Racial/ethnic HHI (6 groups: NH white, NH Black, NH AIAN, NH Asian/PI, NH 2+, Hispanic) | PEP county characteristics (cc-est) | 2000–2023 | A | Prefer this over ACS; it is annual and has no sampling error |
| % non-white (incl. Hispanic) | PEP cc-est | 2000–2023 | A | |
| % foreign-born | Census 2000 SF3; ACS 5-yr 2009+ | 2000, 2009–2022 | D | Interpolate 2001–2008 |
| Religious HHI; adherents per capita | RCMS (ARDA) | 2000, 2010, 2020 | D | 1990 RCMS coverage differs, so do not use it in the panel. 2000 and 2010 files are already in `replication/…/Data` |
| Income Gini | Census 2000 SF3 (computed from brackets); ACS B19083 5-yr 2010+ | 2000, 2010–2022 | D | |
| Age dispersion (SD of age; HHI of 5-yr bins) | PEP cc-est | 2000–2023 | A | |

### 4.2 Philanthropy

| Measure | Source | Years | Freq | Note |
|---|---|---|---|---|
| Itemized charitable contributions per return; contributions / AGI; share of returns itemizing contributions | IRS SOI County Income Data | verify start (contributions may begin in county files around TY2010; ZIP files earlier) | A | The 2018 TCJA itemization collapse is a structural break: flag it and include a fork that drops 2018+ |
| Foundation count; foundation assets | CORE PF (2000–2010) → SOI PF extracts (2011–2020) → e-file 990-PF via panel990 (2021–2024) | Local SOI PF extracts cover 2012–2016 and 2020–2024; **2017–2019 are missing**, so download them | A | Foundation location ≠ where grants go. Uses the same splice and deduplication rules as §2.2 |
| Foundation grants received by metro orgs | 990-PF Part XV e-file (`00 - DATA/grants/`) | ~2011+ with partial e-file coverage; complete 2020+ | X | Kitchen-sink or cross-section only |
| Contributions received by metro nonprofits (`con`) | §2 | | A | This is part of the DV side. Do **not** use it as a predictor of `n` or `exp`: it is mechanically related to the outcome |

### 4.3 Political culture

| Measure | Source | Years | Freq | Note |
|---|---|---|---|---|
| Presidential turnout (votes / VAP 18+) | MIT Election Lab county returns; PEP 18+ | 2000–2024 every 4 years | D | Fork: carry forward vs. linear interpolation vs. election years only |
| Partisan margin (D–R share) | MIT Election Lab | same | D | |
| Social capital index | Rupasingha–Goetz (Penn State NERCRD) | 1990, 1997, 2005, 2009, 2014 | D | Confirm the latest year |
| Economic connectedness; civic-organization density | Opportunity Insights Social Capital Atlas | 2022 | X | Its "civic organizations" measure partly *is* nonprofit density. Treat it as a validation correlate, not a predictor |
| Elazar typology | State-level, time-invariant | — | X | Usable only in random-intercept specifications (per the design) |

### 4.4 Government size

| Measure | Source | Years | Freq | Note |
|---|---|---|---|---|
| State + local government wages; government employment share | BEA CAINC5N / CAEMP25N | 2001–2023 | A | 2000 comes from the SIC-based table; flag it |
| Local government direct expenditure | Census of Governments (finance) | 2002, 2007, 2012, 2017, 2022 | D | Aggregate local units to county area |
| Federal grants to local governments and nonprofits | Consolidated Federal Funds Report (FY1983–2010) + USAspending (FY2008+) | 2000–2024 | A | Splice break at 2008–2010; overlap years allow a check |

### 4.5 Demographics and need

| Measure | Source | Years | Freq | Note |
|---|---|---|---|---|
| Per capita personal income | BEA CAINC1 | 1969–2023 | A | |
| Median household income | SAIPE | 1997–2023 | A | Preferred over ACS for annual data |
| Poverty rate | SAIPE | 1997–2023 | A | |
| Unemployment rate | BLS LAUS | 1990–2023 | A | |
| % BA+ (age 25+) | Census 2000 SF3; ACS 5-yr 2009+ | 2000, 2009–2022 | D | |
| % age 65+; % under 18 | PEP cc-est | 2000–2023 | A | |
| Violent and property crime per capita | Kaplan concatenated UCR offenses | 2000–2020 | A | Agency coverage gaps, and the 2021 NIBRS transition is a series break. **Kitchen-sink only** |
| Regional price parity | BEA RPP (MSA) | 2008–2023 | A | Optional deflator fork for cross-section dollar measures |

---

## 5. Bettencourt-style urban measures (innovation, productivity)

### 5.1 Should they be included? Yes, in three specific roles, and kept out of the principled covariate set.

**Role 1: Scaling benchmarks (strongest case).** Estimate β for nonprofit counts and expenditures **in the same sample and years** as the canonical urban quantities. Bettencourt et al. (2007) report roughly: new patents ≈ 1.27, private R&D employment ≈ 1.34, GDP ≈ 1.15, total wages ≈ 1.12, total employment ≈ 1.01, serious crime ≈ 1.16, gas stations ≈ 0.77. Placing β_N (≈ 0.9, infrastructure-like) and β_E (superlinear, output-like) on that spectrum is a finding in itself. It also **validates the pipeline**: if the build cannot reproduce β_patents ≈ 1.2–1.3, something is wrong with the geography.

**Role 2: SAMI-on-SAMI association (a substantive question).** Do metros that out-innovate their size also out-organize it? Bettencourt et al. (2010) found that SAMIs for income, GDP, and patents are correlated and highly persistent. The nonprofit version asks whether civic capacity travels with innovative capacity (Florida's creative-class argument, Glaeser on human and social capital) or substitutes for it. Run it as the between regression (time-averaged SAMIs) and the two-step within model from the SAMI note, with lags of 2–5 years.

**Role 3: An optional sixth "urban economy" block in the kitchen-sink universe.** This shows whether the five theory blocks are robust to controlling for economic dynamism.

**Why they stay out of the principled universe:**
- *Bad-control risk.* GDP and wages are jointly determined with income, education, and government size, so they absorb the "need" and "resources" mechanisms the paper is testing.
- *Mechanical overlap.* Universities and hospitals produce patents, R&D employment, and a large share of nonprofit expenditure. Patents assigned to universities and nonprofits are literally nonprofit output. **Mitigation**: split patents by assignee type (PatentsView `assignee_type`; university and nonprofit assignees separate), and run the innovation analysis inside the hospital/university-excluded stratum.
- *Zeros and heavy tails in micropolitan areas.* Restrict this module to metros, or use Poisson with a log P offset.

### 5.2 Measures and sources

| Measure | Source | Years | Construction |
|---|---|---|---|
| Utility patents by inventor location (total; corporate-assigned; university/nonprofit-assigned) | USPTO PatentsView (`g_inventor_disambiguated`, `g_location`, `g_assignee`) | 1976–2024 | Fractional counting by inventor; application year; lat/long → county → CBSA |
| Real GDP; GDP per worker | BEA CAGDP county | 2001–2023 | Productivity = GDP / employment, plus the Lobo et al. (2013) TFP proxy built from wage and employment SAMIs |
| Total wages; employment; establishments | BLS QCEW; Census CBP | 1990–2023 | Private-sector only (exclude NAICS 813 and government) |
| Firm / establishment births | Census BDS (metro) | 1978–2022 | Business dynamism; a natural counterpart to `n_entry` |
| Business applications | Census BFS (county) | verify county start year | Optional |
| University R&D expenditure | NSF HERD (institution → county) | 2000–2023 | Separates the university channel |
| Creative-class / STEM occupation share | ACS 5-yr | 2009–2022 | Optional, D-frequency |
| Serious crime | §4.5 | | Already in the need block |

---

## 6. Assembly, file layout, and documentation

```
multiverse/
  R/
    00_geo_spine.R            # county spine, boundary fixes, crosswalks
    01_npo_bmf_counts.R       # BMF snapshots -> county-year counts
    02_npo_core.R             # harmonized CORE PZ download + aggregate
    03_npo_soi.R              # SOI frame -> county-year aggregates
    03b_npo_efile.R           # panel990 e-file 2021-2024 (990, EZ, PF), dedup
    04_npo_splice.R           # overlap diagnostics at both seams, splice, source flags
    05_npo_entry.R            # long BMF panel cohorts
    06_npo_bls.R              # QCEW nonprofit MSA files -> CBSA23, comp-adjusted wage ratio
    07_break_exposure.R       # pre-event exposure measures for B1b (section 2.6)
    10_pop.R                  # population bases
    11_block_diversity.R  ... 15_block_need.R
    20_urban_economy.R        # Bettencourt module
    30_interpolate.R          # D-frequency handling (all methods kept)
    40_aggregate_units.R      # county -> CBSA23 / CBSA13 / CSA / CZ
    50_validate.R             # checks in section 7
    60_metro_reports.qmd      # parameterized sidecar report
  data-raw/                   # pointers / download caches (not edited)
  data/
    county_year.parquet       # wide master, one row per county-year
    metro_year_<unit>.parquet # one per unit fork
    long_metro_year.parquet   # variable, value, source, interp_flag, break_flag
  codebook/
    codebook.csv              # name, block, type (A/B/C), source, years, freq, units
```

- **Every variable carries its predictor type** (A intensive / B extensive / C population) in the codebook. This drives which transforms in §9 apply to it. The SAMI note's test (R² on log P) is computed in `50_validate.R` and stored next to the type, so the empirical classification can override the conceptual one.
- **Interpolation never overwrites.** Keep `x_obs`, `x_lin`, `x_locf`, and an `x_interp` flag, so interpolation method is a fork and not a hidden choice.
- **Keep aggregation additive.** Store numerators and denominators at the county level (for example, poverty count and poverty universe), not rates, so that every unit fork aggregates correctly. Medians (household income) cannot be aggregated; for CBSA rows, use direct ACS/SAIPE CBSA estimates where they exist, or a population-weighted county median, and flag the result.
- Use a fixed seed and a written log for every sample and selection rule.

---

## 7. Validation (before any modeling)

1. **Coverage**: county-years with zero filers; CBSAs present in every year; EIN match rates CORE↔SOI.
2. **Splice diagnostics** at both seams (§2.2), with figures. **Break-exposure diagnostics** (§2.6): correlate each pre-event exposure measure with the covariates. Where they are uncorrelated, B1a is enough.
2b. **990 vs. BLS**: by metro for 2013–2022, compare `exp` with `npo_wages` and `n_filer` with BLS establishments, and correlate their SAMIs. Report which metros have the largest headquarters-attribution gaps.
3. **Break checks**: national totals by year for every nonprofit series, with the §2.3 break years marked.
4. **Reproduce known numbers**: the 2022 β_N and β_E in the SAMI note; the Lecy & Van Slyke (2012) 2000 and 2010 county results using the legacy files in `replication/`; and Bettencourt β for patents and GDP.
5. **Size dependence table**: R² of every covariate on log P, by year. This is the input to the design's goal-2 meta-hypothesis, and it should be written down *before* any model is run, as part of the pre-registration.
6. **Persistence**: year-to-year correlation of the SAMI for each outcome. This decides how much within-metro variance exists, and so whether the FE stratum is worth running at all.

---

## 8. Sidecar descriptive reports: ~50 metros

### 8.1 Sample (48 + named cases)

Stratify on **size** (4 bands: >2M, 500k–2M, 150k–500k, metro <150k), crossed with **Census region** (4). That gives 16 cells, and **3 metros per cell**: the CBSAs at the 10th, 50th, and 90th percentiles of the cell's 2000–2024 mean count SAMI. This guarantees the sample spans both size and the residual, so the reports show what *is* and *is not* explained by scale.

Add 2–4 **named cases** used in the text (e.g., Minneapolis–St. Paul, Las Vegas) and any metro flagged in validation, such as a delineation change or a splice anomaly. Record the selection in `data/metro_report_sample.csv` together with the rule.

### 8.2 Contents of each metro report (one page, small multiples, 2000–2024)

- **Panel row 1, scale**: population; `n_filer` and `n_bmf`; `exp` (real); BLS `npo_emp`, plus `npo_wages` drawn against `exp` for 2013–2022 (the gap between them is headquarters attribution); nonprofit share of private employment.
- **Panel row 2, density measures**: per capita vs. SAMI for counts and for expenditure. This is the core picture: SAMIs are flat while per-capita series drift with size.
- **Panel rows 3–4, covariates**: one panel per block's default measure (racial HHI, itemized contributions per return, turnout, government wage share, per capita income, poverty, % BA, % 65+), plus patents and GDP per worker from the urban-economy module.
- Each covariate panel shows the metro's series against the **median of its size band**, so a reader sees deviation from size-peers rather than raw level.
- **Visual flags**: interpolated years drawn hollow or dashed; vertical rules at the two splice years (2011, 2021) and at the §2.3 break years.
- A header strip with β-based expected vs. actual counts in the latest year, and the metro's SAMI rank.

### 8.3 Companion variable-centric view

For each variable, show one small-multiples page with all 50 metros (spaghetti chart with a highlighted median). This view makes splice and series breaks obvious, because they appear in every line at once.

### 8.4 Implementation

Build one parameterized Quarto file (`60_metro_reports.qmd`, parameter `cbsacode`) and render it in a loop to HTML, with an index page. Reuse the `theme_memo` palette from the SAMI note so figures match the paper.

---

## 9. Complete list of multiverse manipulations

The design separates **strata** (type N: they change the estimand, so they are analyzed side by side and never pooled) from **forks** (type E: defensible alternatives for the same estimand). The data must support every option below.

### 9.1 Strata (type N)

| Stratum | Levels |
|---|---|
| S1. Outcome construct | Count (`n_filer`; `n_bmf` as alternative) · Expenditure (`exp`) · **BLS employment and payroll** (`npo_emp`, `npo_wages`; MSA, 2013–2022) · *optional:* revenue, contributions, entry (`n_entry`), nonprofit employment share (`percent_npo`), composition-adjusted wage premium |
| S2. Identification | Cross-section (single year) · Pooled with year FE · Between (time-averaged SAMIs) · Two-step within (year-specific SAMIs + metro FE) · One-step FE on log Y (β_t) · Mundlak/CRE |
| S3. Hospital/university exclusion | Included · Excluded (expenditure strata; also run for counts for symmetry) |

### 9.2 Dependent-variable form (crossed with predictor form; see §9.4)

| Fork | Options |
|---|---|
| D1. DV functional form | Per capita Y/P · log(Y/P) · log Y with log P covariate (one-stage) · two-stage SAMI · Poisson/NB with log P offset · Poisson/NB with free log P |
| D2. Scaling first stage (SAMI arms only) | Linear in log P · quadratic in log P · spline in log P · estimated on full sample vs. on analysis sample |
| D3. Population base | Total · adults 18+ · households · daytime/commuting-adjusted |
| D4. SAMI timing (panel) | Year-by-year first stage · pooled first stage with year FE |
| D5. Count source | `n_filer` (real floor) · `n_filer_nofloor` (2010+) · `n_bmf` · `n_bmf` excluding revoked |
| D6. Financial series source | Spliced CORE→SOI→e-file (2000–2024) · ratio-spliced (B1d) · CORE-only (2000–2019) · SOI-only (2011–2023) · e-file-only (2021–2024) |
| D7. Dollar adjustment (expenditure) | PCE real · nominal with year FE · RPP-adjusted (cross-section, 2008+) |

### 9.3 Organization filter

| Fork | Options |
|---|---|
| O1. Legal form | All 501(c) · 501(c)(3) only |
| O2. Subsector | All · human services (NTEE I–P) · *optional:* each of the 10 NTEE major groups (as in Kim 2015) |
| O3. Size floor | $50k real · $25k nominal (2010+) · none · $100k real (excludes the smallest active orgs) |
| O4. Org location rule | Filing-year ZIP · current geocode |

### 9.4 Predictor transformation, by variable

Generic transforms:
- **Type B (extensive)**: T1 raw total · T2 per capita · T3 log with log P on the RHS · T4 SAMI (residual of log X on log P, by year).
- **Type A (intensive)**: T5 level · T6 level residualized on log P · T7 z-score within year.
- **All variables**: outlier handling (none / winsorize 1% / trim 1%), and timing (contemporaneous / lag 1 / lag 3 / 3-year moving average).

The design entangles predictor form with DV form. The clean cross is D1 × {T1–T4} for Type B variables, which lets the paper show the FWL equivalences and the model (1) attenuation result empirically.

| Block | Variable | Type | Measure options (operationalization) | Transform options | Gap handling (D-freq) |
|---|---|---|---|---|---|
| Population | log P | C | total / 18+ / households / daytime | linear · quadratic · spline (D2) | — |
| Diversity | Racial diversity | A | HHI · % non-white · % non-Hispanic white (reversed) · entropy index | T5 · T6 · T7 | — |
| | Foreign-born | A | % foreign-born · % non-citizen | T5 · T6 | linear · LOCF · observed years only |
| | Religious diversity | A | religious HHI · adherence rate · % evangelical | T5 · T6 | linear · LOCF · census years |
| | Income inequality | A | Gini · 90/10 ratio · top-quintile share | T5 · T6 | linear · LOCF · observed years |
| | Age dispersion | A | SD of age · age-bin HHI | T5 · T6 | — |
| Philanthropy | Foundation resources | B | assets · number of foundations · grants paid | T1 · T2 · T3 · T4 | — |
| | Individual giving | B / A | total itemized contributions (B) · contributions per return (A) · contributions / AGI (A) · % itemizers claiming (A) | T1–T4 for B; T5–T6 for A | pre/post-TCJA split |
| Political culture | Turnout | A | turnout / VAP · turnout / CVAP | T5 · T6 | LOCF · linear · election years only |
| | Partisanship | A | D–R margin · \|margin\| (competitiveness) | T5 · T6 | same |
| | Social capital | A | Rupasingha index · its components (associations, census response) · Elazar (state RE only) | T5 · T6 | linear · LOCF |
| Government size | Government wages / employment | B / A | total wages (B) · government share of employment (A) | T1–T4 · T5–T6 | — |
| | Local expenditure | B | direct expenditure · own-source revenue | T1–T4 | linear · LOCF (5-year CoG) |
| | Federal grants | B | to local governments · to nonprofits · total | T1–T4 | — |
| Demographics / need | Income | A | per capita personal income · median household income | T5 · T6 · log level | — |
| | Need | A | poverty rate · unemployment rate · child poverty | T5 · T6 | — |
| | Education | A | % BA+ · % graduate degree | T5 · T6 | linear · LOCF |
| | Age structure | A | % 65+ · % under 18 · old-age dependency ratio | T5 · T6 | — |
| | Crime (kitchen sink) | B | violent · property · total offenses | T1–T4 | ≤2020 only |
| Urban economy (kitchen sink and sidecar) | Innovation | B | patents (all · corporate · excl. university/nonprofit) · university R&D | T1–T4; Poisson for zeros | — |
| | Output / productivity | B / A | GDP (B) · GDP per worker (A) · TFP proxy (A) | T1–T4 · T5–T6 | — |
| | Dynamism | B / A | establishment births (B) · birth rate (A) | T1–T4 · T5–T6 | — |

### 9.5 Covariate set

| Fork | Options |
|---|---|
| C1. Block inclusion | All 2⁵ subsets of the five theory blocks (minimum: full model + leave-one-block-out). Urban economy is a sixth block in the kitchen-sink universe only |
| C2. Operationalization | Default measure per block; vary **one block at a time** (principled universe) or the full cross product (kitchen sink) |
| C3. Extra terms | none · X × log P interactions (mechanism arm) · lagged DV (dynamic arm, within stratum only) |

### 9.6 Sample, unit, estimator, time

| Fork | Options |
|---|---|
| U1. Unit | County · MSA (metro only) · CBSA (metro + micro) · CSA · commuting zone |
| U2. Delineation vintage | 2023 · 2013 |
| P1. Population threshold | none · ≥50k · ≥100k |
| P2. Exclusions | none · drop DC · drop AK and HI · drop top 5 and bottom 20 by population (SAMI note, Recommendation 7) · drop Puerto Rico (default: always) |
| E1. Estimator | OLS with HC2 · state random intercept · state FE · SEs clustered by state (cross-section) / by metro (panel) |
| E2. Weights | unweighted · population-weighted |
| Y1. Year | single cross-section (2000, 2010, 2019, 2024) · pooled 2000–2024 · pooled excluding 2020–21 · pre/post 2014 (entry) |
| Y2. Lag structure (within stratum) | contemporaneous · lag 1 · lag 3 · lag 5 · distributed lag |
| B1. Break handling (panel strata only; §2.6) | B1a construction + year FE · B1b + exposure × post terms · B1c metro × era FE · B1e drop 2008–2011 and 2020–2021 (B1d ratio splicing is under D6) |

### 9.7 Universe sizes (for the pre-registration)

- **Principled universe per stratum** (illustrative): D1 (6) × predictor-form pairing (4) × C1 leave-one-out (6) × C2 one-at-a-time (~15) × U1 (2: MSA, CBSA) × O1/O2 (3) × E1 (3) × Y1 (2), which is about 78k specifications. Every fork has a written rationale.
- **Kitchen sink**: add the full C2 cross product, the outlier, timing, interpolation, delineation, and weights forks. That is roughly 10⁷–10⁸, so sample specifications uniformly at random rather than enumerating them.
- The Young–Holsteen variance decomposition works on either universe. Report it for the principled universe and show the kitchen-sink universe in the appendix.

**Why 78k is less than it sounds.**
- *Compute is not the constraint.* An OLS fit on about 390 metros takes milliseconds, so the full set runs in minutes. What is hard is reading the results, which is why the analysis reports summaries (sign stability, variance decomposition) rather than individual specifications.
- *Many cells are identical by algebra.* By Frisch–Waugh–Lovell, the one-stage log model and the all-SAMI model (with intensive variables residualized too) give the same coefficients (examples 0 and 2 below). Those pairs are kept as **verification specifications**: if they don't match to machine precision, the pipeline has a bug. They should not be counted twice in the specification curve.
- *Some forks only apply in certain cells.* The operationalization fork does nothing when its block is dropped, and Poisson DVs don't pair with per-capita predictors. After removing duplicates and empty cells, the number of distinct estimates is probably around a third of the product. Compute the exact count from the grid before pre-registering.

### 9.8 Worked examples

**Baseline (example 0).** Count stratum, cross-section, 2022, metros only (≈390), 501(c)(3) above the $50k real floor, hospitals and universities included, OLS with HC2 errors. Default measure for each block: racial HHI · foundation assets · turnout · state and local government wages · poverty · % BA.

```
log(N) ~ log(P) + raceHHI + log(fdn_assets) + turnout + log(gov_wages) + poverty + pctBA
```

Each example below changes **one** fork from the baseline. That one-at-a-time structure is what the principled universe crosses.

| # | Fork changed | Specification | What it shows |
|---|---|---|---|
| 1 | D1 DV form + predictor form | `N/P ~ raceHHI + fdn_assets/P + turnout + gov_wages/P + poverty + pctBA` | The literature's per-capita convention. It differs from #0 roughly in proportion to \|β̂ − 1\| |
| 2 | Predictor form → all-SAMI (intensive variables residualized on log P too) | `SAMI_N ~ raceHHI~ + SAMI_fdn + turnout~ + SAMI_gov + poverty~ + pctBA~` | **Verification**: coefficients must equal #0 exactly (FWL) |
| 3 | Same as #2, but intensive variables left raw | `SAMI_N ~ raceHHI + SAMI_fdn + turnout + SAMI_gov + poverty + pctBA` | Intensive coefficients shrink by (1 − R² on log P). A direct test of the SAMI note's attenuation result |
| 4 | Model (1): SAMI outcome, raw totals, no log P | `SAMI_N ~ raceHHI + fdn_assets + turnout + gov_wages + poverty + pctBA` | The mistake the SAMI note warns about. Kept to show how far off it is |
| 5 | D1 → Poisson with an offset (β fixed at 1) | `N ~ Poisson, offset(log P) + raceHHI + log(fdn_assets) + …` | Count-appropriate model under the per-capita constraint |
| 6 | D1 → Poisson with β estimated | `N ~ Poisson, log(P) + raceHHI + log(fdn_assets) + …` | Compare with #5 to isolate the β = 1 constraint inside the count model |
| 7 | C2: diversity measure | Same as #0 with `pct_nonwhite` in place of `raceHHI` | The Corbin vs. Kim sign question |
| 8 | C2: philanthropy measure | Same as #0 with `log(itemized_contrib)` in place of `log(fdn_assets)` | Whether "philanthropy" means institutional or individual giving |
| 9 | C1: drop a block | Same as #0 without the philanthropy terms | How much the other blocks rely on philanthropy being controlled |
| 10 | U1: unit | Same as #0 on CBSAs including micropolitan areas (≈930) | Whether the small-place sublinearity changes the coefficients |
| 11 | E1: estimator | Same as #0 with a state random intercept | Whether results depend on between-state variation |
| 12 | D2: scaling fit | Same as #0 plus `I(log(P)^2)` | Whether results come from misfit at the extremes of the size distribution |

**Panel examples (identification stratum; never pooled with the cross-section curve):**

| # | Specification | Settings |
|---|---|---|
| 13 | Two-step within: `SAMI_N[it] ~ metro FE + year FE + SAMI_fdn[i,t-3] + raceHHI[it] + turnout[it] + SAMI_gov[it] + poverty[it] + pctBA[it]` | Year-by-year SAMIs; 2000–2024; spliced financials; turnout carried forward between elections; errors clustered by metro; B1a |
| 14 | Same as #13 with metro × era FE | B1c: only change within the CORE, SOI, and e-file eras is used |
| 15 | Same as #13 plus `small_org_share_0507 × post2008` and `itemizer_share_1517 × post2018` | B1b: exposure terms for differential breaks |
| 16 | Between: metro means of all SAMIs, 2000–2024 | The persistent dimension; the SAMI note's comparison of between and within estimates |
| 17 | One-step FE: `log N[it] ~ metro FE + year FE + log P[it] + …` | Recovers the temporal exponent β_t. Compare it with the cross-sectional β |

**Expenditure stratum** (a separate curve): repeat examples 0–17 with `exp` in place of `N`, with hospitals and universities excluded and, separately, included.

---

## 10. Work order

1. **Geography spine and crosswalks** (§1). Everything depends on this.
2. **Nonprofit outcomes**: BMF counts → CORE 2000–2019 download and aggregation → SOI 2011–2023 aggregation → e-file 2021–2024 via panel990 (990, EZ, PF) → splice diagnostics at both seams → entry series → BLS QCEW nonprofit MSA files mapped to 2023 CBSAs (§2). *Checkpoint: splice report and BLS-vs-990 comparison.*
3. **Population bases** (§3).
4. **Annual covariates** (PEP cc-est, SAIPE, BEA, LAUS, MIT Election Lab, IRS SOI county).
5. **Periodic covariates and interpolation** (ACS / 2000 SF3, RCMS, Census of Governments, Rupasingha).
6. **Urban-economy module** (PatentsView is the largest single download; start it early in the background).
7. **Validation** (§7). *Checkpoint: size-dependence table and reproduced β values go into the pre-registration.*
8. **Sidecar metro reports** (§8).
9. Spec-grid scaffolding against the real data. The design's mock-data step can run in parallel with steps 1–6.

Steps 4–6 are independent once step 1 exists, so they can be split across parallel sessions or assistants.
