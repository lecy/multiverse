# County unemployment (BLS LAUS, republished by USDA ERS)

| Field | Value |
|---|---|
| Publisher | USDA Economic Research Service, County-level Data Sets (unemployment and median household income); underlying data from BLS Local Area Unemployment Statistics |
| File | `ers_unemployment_2000_23.csv` |
| Original URL | `https://www.ers.usda.gov/media/5497/unemployment-and-median-household-income-for-the-united-states-states-and-counties-2000-23.csv?v=78722` |
| Years used | 2000–2023 (labor force, employed, unemployed) |
| Obtained | Downloaded by script with a browser user agent (`fetch_ua()`), cached in `$MV_BUILD/raw/cov/laus/` (17.6 MB) |
| Read by | `R/build/D_cov/14_cov_laus.R` |
| Licence | U.S. government work; listed as CC0 on catalog.data.gov |
| Redistribute in release | Yes |
| Cite as | USDA, Economic Research Service, County-level Data Sets: Unemployment and median household income, 2000–23. |
| Frozen copy | `inputs/ers-laus/` |

**Drift risk:** high. The URL carries a media id (`5497`) and a cache-busting
`?v=78722`, both of which change when ERS posts the 2000–24 update. The header of 14
says 2024 "is added automatically", but no 2024 URL pattern exists in the code.
