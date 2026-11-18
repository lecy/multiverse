# Census historical county population estimates, 1970–1999

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau, Population Estimates Program (historical tables) |
| Files | `e7079co.txt` (1970–79), `e8089co.txt` (1980–89), `99c8_00.txt` (1990–99) |
| Original URLs | `https://www2.census.gov/programs-surveys/popest/tables/1900-1980/counties/totals/e7079co.txt`; `https://www2.census.gov/programs-surveys/popest/tables/1980-1990/counties/totals/e8089co.txt`; `https://www2.census.gov/programs-surveys/popest/tables/1990-2000/counties/totals/99c8_00.txt` (all three found at these paths on 2026-11-17; the code records no URL) |
| Years used | 1970–1999, for the population-growth kernels `lag_g5`, `lag_g10` and the pre-2000 growth notes |
| Obtained | **Manual download** to `$MV_BUILD/raw/census-hist/` (no downloader in the code) |
| Read by | `R/build/D_cov/26_cov_lag_kernel.R`; also `R/notes/73_pre2000_growth.R` |
| Licence | U.S. government work, public domain |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Census Bureau, county population estimates 1970–1979, 1980–1989, 1990–1999 (historical tables). |
| Frozen copy | `inputs/census-hist/` |

**To do:** add a `fetch()` step for these three URLs to 26, so the input no longer
depends on a manual download. Connecticut metros are dropped from the 1970–1990
history (DATA-MANIFEST.md 6.18).
