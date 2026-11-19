# Corporate headquarters measures: construction, validation, and what they proxy

Nov 18, 2026 · build step `R/build/D_cov/28_cov_corporate_hq.R` → `data/cov/cov_corphq.csv`, `data/cov/cbsa/cbsa_corphq.csv`, `data/cov/cov_corphq_codebook.csv` (+ firm-year file `sec_hq_firm_year.csv.gz`) · per-100k rates in step 30 · analysis `R/notes/106_corporate_hq_correlates.R` → `$MV_BUILD/data/diagnostics/corphq_*.csv` · sources `inputs/sec-edgar/SOURCE.md`, `inputs/census-cbp/SOURCE.md`.

## Measures

| Variable | What | Years |
|---|---|---|
| `hq_pub` | SEC registrants whose latest annual report that year (10-K family incl. 10-KSB, 20-F, 40-F; no amendments) gives a US business address in the county | 2000–2024 |
| `hq_pub_large` | The subset with large-accelerated-filer status (FSDS `afs = 1-LAF`, public float ≥ $700M) | 2011–2024 |
| `cbp_hq_est` | CBP establishments in NAICS 551114 (corporate, subsidiary and regional managing offices) | 2000–2023 |
| `cbp_hq_est100` | 551114 establishments with 100+ employees | 2000–2023 |
| `cbp_hq_emp` | 551114 employment, suppressed cells imputed | 2000–2023 |
| `*_per100k` | `hq_pub`, `hq_pub_large`, `cbp_hq_est` per 100k residents (step 30) | |

Flags: `cbp_hq_emp_imp` and `cbp_hq_est100_imp` are the imputed or allocated parts. `cbp_hq_nosmall = 1` from 2017, when CBP stops publishing cells under 3 establishments; those cells count as 0.

### Rules

- **One location per CIK-year.** The latest annual report filed in the calendar year sets the location.
- **Address source.**
  - From 2009Q2 the address comes from FSDS `sub.txt`.
  - Otherwise it comes from the filing's SEC header (`.hdr.sgml`, falling back to the full `.txt`).
  - 251,867 annual filings; every one has an address record.
  - 7 headers still failed after four retries (SEC 502/504 errors) and were dropped, one in each of 2003Q3, 2005Q4, 2006Q4, 2012Q3, 2017Q1, 2019Q2 and 2023Q1.
- **Exclusions.**
  - Non-US addresses.
  - SIC 6189 (asset-backed), 6221 (commodity pools), 6770 (blank checks/SPACs), 8888 (foreign governments) and 9995 (non-operating shells). These remove roughly 500–2,300 US CIK-years a year; the peak is 2005–07, from asset-backed securitization trusts.
- **Geocoding: ZIP to county.**
  - The 2010 ZCTA relationship file first (largest population part), then the 2020 file, then the modal BMF county for ZIPs that are not ZCTAs.
  - Then Connecticut planning regions and 2023 county recodes.
  - 461 of 184k firm-years are not geocoded.
- **CBP adjustments.**
  - From 2017, withheld size classes are allocated from national withheld counts, raked so the counties do not exceed the national total.
  - Suppressed employment is imputed from class counts times national mean employment per establishment, clamped to the flag range.
  - Counties are moved to 2023 geography.

## Validation

**Top metros, 2023 (hq_pub / large / CBP establishments).**

| Metro | hq_pub | large | CBP est. |
|---|---|---|---|
| New York | 741 | 224 | 2,335 |
| Boston | 321 | 108 | 805 |
| San Francisco | 315 | 144 | 837 |
| Los Angeles | 298 | 89 | 1,652 |
| Houston | 218 | 75 | 994 |
| Dallas | 204 | 84 | 1,491 |
| Miami | 185 | 38 | 618 |
| Chicago | 170 | 87 | 1,380 |
| Washington | 151 | 68 | 1,222 |

New York, Houston, Dallas, Chicago and the Bay Area (San Francisco plus San Jose's 130) are all at the top, as expected.

**Trends.**
- Public-company HQs fall from 11,064 (2000) to 5,071 (2020), then rise a little to 5,432 (2023). This matches the known halving in the number of US-listed companies, followed by the 2021 IPO wave.
- Large filers rise from 1,270 (2011) to about 2,000.
- CBP 551114 establishments rise from 36.5k (2000) to 47.7k (2017), then drop to 40.3k (2023). Part of that drop is the 2017 small-cell change.
- 551114 employment grows steadily, from 2.70M to 3.69M.

**EDGAR vs CBP across metros.**
- Log counts correlate 0.85–0.89 every year.
- Large filers vs CBP 100+ establishments correlate 0.86–0.89.
- Per-100k densities correlate only 0.06–0.46 (Pearson) and about 0.4 (Spearman). This is driven by registered-agent addresses: Carson City NV had 36 public "HQs" in 2010, 65 per 100k, and Reno and Las Vegas are also high. A small firm that gives its registered agent's address as its business address is counted there. Use `hq_pub_large`, or the SAMI (log) form, when outliers matter.

## Is corporate HQ a distinct variable or a proxy?

**Method.**
- Metros only.
- Every HQ measure is in SAMI form: `flog(x, pop)`, residualized on log P by year.
- Comparison variables are in model form.
- Correlations are computed three ways:
  - between metros: metro means;
  - within: metro-demeaned;
  - two-way: metro- and year-demeaned.

### Between metros: a moderate "affluent economic centre" proxy, but mostly distinct

Correlations with size-adjusted public-company HQs (`hq_pub`) and CBP managing offices (`cbp_hq_est`):

| Construct | Variable | hq_pub | cbp_hq_est |
|---|---|---|---|
| Foundations | Private foundations (SAMI) | .54 | .44 |
| | Foundation grants (SAMI) | .47 | .51 |
| | Foundation assets (SAMI) | .45 | .47 |
| Economy / innovation | Real GDP (SAMI) | .49 | .47 |
| | Patents (SAMI) | .50 | .23 |
| Income | Per-capita income | .47 | .31 |
| | Median HH income | .38 | .14 |
| | Poverty rate | −.44 | −.28 |
| Education | Share BA | .35 | .19 |
| Social capital | Rupasingha index | .27 | .49 |
| | Associations (SAMI) | .15 | .50 |
| | Turnout | .31 | .32 |
| Generosity | Itemized contributions (SAMI) | .31 | .24 |
| | Contributions / AGI | .06 | .10 |
| Inequality | Gini | .09 | .01 |
| Market concentration | Nonprofit revenue CR4 | −.18 | −.21 |
| Nonprofit outcome | 501(c)(3) filers (SAMI) | .44 | .46 |

- **Foundations, GDP and income.** HQ density is clearly tied to these (r ≈ .45–.55).
- **Social capital.** The CBP measure also tracks social capital; the EDGAR measure does not.
- **Generosity, inequality and market concentration.** HQ density is nearly unrelated to generosity as a share of income and to inequality, and weakly negative with nonprofit market concentration.
- **No single proxy.** No comparison variable reaches .55.
- **R² on the 13 lean-model covariates.** These explain 27% (`hq_pub`), 27% (`hq_pub_large`) and 42% (`cbp_hq_est`) of the size-adjusted cross-metro variation. Most of what distinguishes HQ metros is not in the current model.
- **The two sources agree only moderately once size is removed.** `hq_pub` vs `cbp_hq_est100` r = .42. Large filers vs all public filers r = .69.

### Within metros: no signal

- **Little within variation.** Only 15–29% of each HQ measure's SAMI variance is within metros.
- **Within correlations are near zero.** Within-metro and two-way correlations with every comparison variable are |r| ≤ .09. The few that reach .13 are large filers with GDP and pay.
- **The same holds on 2000–2016.** On the pre-2017 CBP series the largest |r| is .11.
- **The two sources disagree within metros.** EDGAR vs CBP within-metro r = .03–.04. Year-to-year change in a metro's HQ count is mostly churn and measurement noise, not a shared signal.
- **The lean covariates do not explain it either.** They explain 2–6% of the two-way variation.

### Answer

- **Between metros.** Corporate HQ concentration is a partial proxy for economic weight and wealth: GDP, income, patents and, above all, foundations. It is not a proxy for generosity, inequality or market concentration. It keeps a large distinct component, roughly 60–70% of its size-adjusted variance after the lean covariates.
- **Within metros.** It carries no usable signal. In fixed-effects models it would add noise, not identification.
- **How to use it.** Use it as a cross-sectional (between-metro) covariate, preferably `hq_pub_large` or CBP 100+ establishments, which are less exposed to shells and registered agents. Its overlap with foundation assets (.45) is worth noting if both enter the model.
