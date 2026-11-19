## geo_helpers.R
## Place organization records on 2023 county-equivalents. The helpers live in the
## package (pkg/R/geo.R, ?npdensity::geo): ct_to_planning_region, ct_pr_from_latlon,
## county_from_latlon, county_from_zip, county_from_block, ct_legacy_to_pr,
## not_in_spine, recode_county. They read the lookups in BUILD/data/geo, set by
## 00_setup.R as options( npdensity.geo_dir ).
## Requires data/geo/ct_town_lookup.csv and county_spine.csv (01_geo_spine.R).
## Some steps (04, 11, 12, 13) read the lookup tables directly; they are bound here.
.ct      <- geo_lookup( "ct" )
.ct_city <- unique( .ct[ , .( town_name, pr ) ], by = "town_name" )
.ct_dom  <- unique( .ct[ , .( legacy_county, dominant_pr ) ] )
.spine   <- geo_lookup( "spine" )
