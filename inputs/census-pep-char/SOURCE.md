# Census PEP county characteristics (race, ethnicity, age)

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau, Population Estimates Program |
| Files | `co-est00int-alldata-SS.csv`, one per state (2000–09); `cc-est2020int-alldata.csv` and `cc-est2020int-agesex-all.csv` (2010–19); `cc-est2025-alldata.csv` and `cc-est2025-agesex-all.csv` (2020–24) |
| Original URLs | `https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-alldata-%s.csv`; `…/datasets/2010-2020/intercensal/county/asrh/cc-est2020int-{alldata,agesex-all}.csv`; `…/datasets/2020-2025/counties/asrh/cc-est2025-{alldata,agesex-all}.csv` |
| Years used | 2000–2024 |
| Obtained | Downloaded by script, cached in `$MV_BUILD/raw/cov/pep_char/` (55 files, 421 MB) |
| Read by | `R/build/D_cov/11_cov_pep_char.R` |
| Licence | U.S. government work, public domain |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Census Bureau, Population Estimates Program, county characteristics (intercensal 2000–2010 and 2010–2020; Vintage 2025). |
| Frozen copy | `inputs/census-pep-char/` |

**Drift risk:** the Vintage 2025 names (`cc-est2025-*`) are hard-coded and will be
replaced by the next vintage.
