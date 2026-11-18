# Census ACS 5-year and Census 2000 SF3

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau, American Community Survey (5-year) and Decennial Census 2000 Summary File 3 |
| Tables | County: B15002 (education), B05002 (foreign born), B19083 (Gini), B24080 (class of worker), B19001, C17002, B22003, B25070, B25003, B07003. US: B19080. CBSA: B19083. SF3 2000: P037, P021, P052 |
| Original URL | Census API through `tidycensus` 2.0.0 (`get_acs(geography = "county", survey = "acs5")`, `get_decennial(sumfile = "sf3", year = 2000)`); key from `census_key()` |
| Years used | ACS end years 2009–2024 (2010–2024 for the metro-Gini tables); SF3 2000 |
| Obtained | API, cached in `$MV_BUILD/raw/cov/acs/` (`acs5_<y>.csv`, `acs5_cow_<y>.csv`, `sf3_2000.csv`; 32 files) and `$MV_BUILD/raw/cov/acs89/` (45 files) |
| Read by | `R/build/D_cov/17_cov_acs.R` (education, foreign born, Gini); `R/build/D_cov/22_cov_government.R` (B24080); `R/build/D_cov/27_cov_acs_metro.R` (bracket tables summed to CBSAs, `gini_m`) |
| Licence | U.S. government work, public domain. API notice: "This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau." |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Census Bureau, American Community Survey 5-year estimates, 2005–2009 through 2020–2024; 2000 Census Summary File 3. |
| Frozen copy | `inputs/census-acs/` (both cache folders) |
