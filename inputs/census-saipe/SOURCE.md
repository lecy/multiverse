# Census SAIPE: county poverty and median household income

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau, Small Area Income and Poverty Estimates (SAIPE) |
| Files | `saipe_county_<year>.csv`, one per year (25 files) |
| Original URL | Census API `https://api.census.gov/data/timeseries/poverty/saipe?get=NAME,SAEPOVALL_PT,SAEPOVU_ALL,SAEPOVRTALL_PT,SAEMHI_PT&for=county:*&time=<year>` (key required; `census_key()` in `R/lib/config.R`) |
| Years used | 2000–2024 |
| Obtained | API, cached in `$MV_BUILD/raw/cov/saipe/` |
| Read by | `R/build/D_cov/13_cov_saipe.R` |
| Licence | U.S. government work, public domain. API terms require the notice: "This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau." |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Census Bureau, Small Area Income and Poverty Estimates, 2000–2024. |
| Frozen copy | `inputs/census-saipe/` |

**Note:** a failed year returns NULL without stopping the script (13, line 27). Check
that all 25 cached files are present before freezing.
