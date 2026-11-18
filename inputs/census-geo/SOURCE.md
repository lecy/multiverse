# Geography: TIGER county boundaries and the OMB CBSA delineation

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau (TIGER/Line cartographic boundaries); Office of Management and Budget (CBSA delineation, published by Census) |
| Files | `counties_2023_cb.rds` and `ct_planning_regions_2023.rds` (written by `tigris::counties(year = 2023, cb = TRUE)`); `cbsa2fipsxw.csv` (2023 CBSA-to-county crosswalk) |
| Original URLs | TIGER: through `tigris` 2.3.0 (`https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip`). CBSA crosswalk: data-hub copy `<MV_INPUTS>/population/cbsa2fipsxw.csv`; the public version is the OMB July 2023 delineation file, distributed by Census (https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html) |
| Years used | 2023 delineation, fixed for every year |
| Obtained | TIGER: R package, cached as RDS in `$MV_BUILD/data/geo/` (downloaded only if the RDS is missing). Crosswalk: data-hub file |
| Read by | `R/lib/geo_helpers.R`; `R/build/A_geo/01_geo_spine.R` |
| Licence | U.S. government works, public domain |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Census Bureau, 2023 Cartographic Boundary Files; OMB Bulletin 23-01 metropolitan and micropolitan statistical area delineations (July 2023). |
| Frozen copy | `inputs/census-geo/` |

**Open question:** `cbsa2fipsxw.csv` in the data hub carries no download date or
note of which OMB file it was converted from. Before freezing, compare it with the
July 2023 delineation file and record the match here.

The hub folder also holds `cbsa_population_2000_2025.csv`, the old interpolated CBSA
population. 01 reads it only for a comparison diagnostic; it does not enter the
panel, so it is not part of the release.
