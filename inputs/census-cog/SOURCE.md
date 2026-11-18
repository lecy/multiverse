# Census of Governments: local government finances

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau, Census of Governments, Finance |
| Files | 2002: `02countyarea.zip`, `02countyareagid.zip`; 2007 and 2012: `county_area_finances_2007.zip`, `county_area_finances_2012.zip`; individual-unit files `2012_Individual_Unit_file.zip`, `2017_Individual_Unit_File.zip`, `2022_Individual_Unit_File.zip` |
| Original URLs | `https://www2.census.gov/programs-surveys/gov-finances/datasets/2002/public-use-datasets/02countyarea.zip` and `…/02countyareagid.zip`; `…/gov-finances/datasets/historical/county_area_finances_{2007,2012}.zip`; `…/gov-finances/tables/2012/summary-tables/2012_Individual_Unit_file.zip`; `…/tables/2017/2017_Individual_Unit_File.zip`; `…/tables/2022/2022_Individual_Unit_File.zip` |
| Years used | 2002, 2007, 2012, 2017, 2022 (interpolated between census years in 30) |
| Obtained | Downloaded by script, cached and unzipped in `$MV_BUILD/raw/cov/cog/` (44 files, 769 MB) |
| Read by | `R/build/D_cov/22_cov_government.R` |
| Licence | U.S. government work, public domain |
| Redistribute in release | Yes (the zips only; the unzipped copies can be rebuilt) |
| Cite as | Source: U.S. Census Bureau, Census of Governments: Finance, 2002, 2007, 2012, 2017, 2022. |
| Frozen copy | `inputs/census-cog/` |

**Drift risk:** the 2012 individual-unit file sits under `tables/…/summary-tables/`,
an unusual path for a dataset that Census may move. The 2022 public-use file drops
salaries and folds some categories together (DATA-MANIFEST.md 6.19).
