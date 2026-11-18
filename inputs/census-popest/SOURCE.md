# Census PEP county population totals and CT crosswalk

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau, Population Estimates Program (PEP) |
| Files | `co-est00int-tot.csv` (2000–09 intercensal); `sub-est00int.csv` (2000–09 subcounty, CT towns); `sub-est2020int.csv` (2010–19 subcounty intercensal); `co-est2025-alldata.csv` (Vintage 2025, 2020–25); `ct_cou_to_cousub_crosswalk.txt` (CT counties to towns / planning regions, 2022) |
| Original URLs | `https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv`; `…/datasets/2000-2010/intercensal/cities/sub-est00int.csv`; `…/datasets/2010-2020/intercensal/cities/sub-est2020int.csv`; `…/datasets/2020-2025/counties/totals/co-est2025-alldata.csv`; `https://www2.census.gov/geo/docs/reference/ct_change/ct_cou_to_cousub_crosswalk.txt` |
| Years used | July 1 population, 2000–2025 |
| Obtained | Downloaded by script (`fetch()`), cached in `$MV_BUILD/raw/census-popest/` |
| Read by | `R/build/A_geo/01_geo_spine.R` |
| Licence | U.S. government work, public domain (17 U.S.C. §105) |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Census Bureau, Population Estimates Program, county intercensal estimates 2000–2010 and 2010–2020, and Vintage 2025 county estimates. |
| Frozen copy | `inputs/census-popest/` (files above, sha256 in `MANIFEST.csv`) |

**Drift risk:** the Vintage 2025 file name is hard-coded. The next vintage
(`co-est2026-alldata.csv`) replaces it, and Census revises earlier postcensal years
with each vintage. Rebuild from the frozen copy, not the live URL.

**Diagnostic file from the data hub.** The folder also holds
`hub/population/cbsa_population_2000_2025.csv` (`$MV_INPUTS/population/`), an older
interpolated CBSA population series. Step 01 reads it only to write
`data/diagnostics/pop_vs_interpolated.csv`, which compares it with the PEP-based CBSA
totals; no other output depends on it. It was frozen on 2026-11-18 after the clean-room
run found it undeclared.

The same cache folder also holds `fred_pcepi.csv`, which is documented separately in
[`../bea-pcepi/SOURCE.md`](../bea-pcepi/SOURCE.md).
