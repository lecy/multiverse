## geo_helpers.R
## Place organization records on 2023 county-equivalents.
## Requires data/geo/ct_town_lookup.csv and county_spine.csv (01_geo_spine.R).

.ct <- fread( file.path( OUT$geo, "ct_town_lookup.csv" ), colClasses = "character" )
.ct_city <- unique( .ct[ , .( town_name, pr ) ], by = "town_name" )
.ct_dom  <- unique( .ct[ , .( legacy_county, dominant_pr ) ] )
.spine   <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )

## renames and merges onto 2023 county-equivalents (see COUNTY_RECODE);
## Valdez-Cordova (02261) goes to Chugach (02063), the larger successor
recode_county <- function( geoid ) {
  m <- match( geoid, COUNTY_RECODE$from )
  out <- ifelse( is.na( m ), geoid, COUNTY_RECODE$to[ m ] )
  out[ out %in% "02261" ] <- "02063"
  out
}

## Connecticut: legacy county codes (09001-09015) -> planning regions
## (09110-09190) by city name = town name; else the legacy county's
## dominant planning region. Codes already on planning regions pass through.
ct_to_planning_region <- function( geoid, city ) {
  is_legacy <- !is.na( geoid ) & geoid %in% .ct_dom$legacy_county
  if ( !any( is_legacy ) ) return( geoid )
  by_city <- .ct_city$pr[ match( toupper( trimws( city[ is_legacy ] ) ), .ct_city$town_name ) ]
  by_cty  <- .ct_dom$dominant_pr[ match( geoid[ is_legacy ], .ct_dom$legacy_county ) ]
  geoid[ is_legacy ] <- ifelse( is.na( by_city ), by_cty, by_city )
  geoid
}

## Connecticut planning region from coordinates (exact, when lat/long exist)
.ct_pr_poly <- NULL
ct_pr_from_latlon <- function( lat, lon ) {
  out <- rep( NA_character_, length( lat ) )
  ok <- !is.na( lat ) & !is.na( lon )
  if ( !any( ok ) ) return( out )
  if ( is.null( .ct_pr_poly ) ) {
    f <- file.path( OUT$geo, "ct_planning_regions_2023.rds" )
    if ( !file.exists( f ) ) {
      options( tigris_use_cache = FALSE )
      p <- tigris::counties( state = "09", year = 2023, cb = TRUE, progress_bar = FALSE )
      saveRDS( sf::st_transform( p[ , "GEOID" ], 4326 ), f )
    }
    .ct_pr_poly <<- readRDS( f )
  }
  pts <- sf::st_as_sf( data.frame( lon = lon[ ok ], lat = lat[ ok ] ),
                       coords = c( "lon", "lat" ), crs = 4326 )
  hit <- sf::st_intersects( pts, .ct_pr_poly )
  out[ ok ] <- vapply( hit, function( i ) if ( length( i ) ) .ct_pr_poly$GEOID[ i[1] ] else NA_character_,
                       character( 1 ) )
  out
}

## 2023 county-equivalent from coordinates (point-in-polygon; CT -> planning
## regions automatically). Used for the e-file / unified-BMF geocodes, which
## carry lat/long and a county *name* but no FIPS.
.cty_poly <- NULL
county_from_latlon <- function( lat, lon ) {
  out <- rep( NA_character_, length( lat ) )
  ok <- which( !is.na( lat ) & !is.na( lon ) )
  if ( !length( ok ) ) return( out )
  if ( is.null( .cty_poly ) ) {
    f <- file.path( OUT$geo, "counties_2023_cb.rds" )
    if ( !file.exists( f ) ) {
      options( tigris_use_cache = FALSE )
      p <- tigris::counties( year = 2023, cb = TRUE, progress_bar = FALSE )
      saveRDS( sf::st_transform( p[ , "GEOID" ], 4326 ), f )
    }
    .cty_poly <<- readRDS( f )
  }
  sf::sf_use_s2( FALSE )
  pts <- sf::st_as_sf( data.frame( lon = lon[ ok ], lat = lat[ ok ] ),
                       coords = c( "lon", "lat" ), crs = 4326 )
  hit <- sf::st_intersects( pts, .cty_poly )
  out[ ok ] <- vapply( hit, function( i ) if ( length( i ) ) .cty_poly$GEOID[ i[1] ] else NA_character_,
                       character( 1 ) )
  out
}

## ZIP -> county fallback learned from geocoded BMF records (02_npo_bmf_counts.R)
county_from_zip <- function( zip5 ) {
  f <- file.path( OUT$geo, "zip_county_modal_bmf.csv" )
  if ( !file.exists( f ) ) return( rep( NA_character_, length( zip5 ) ) )
  z <- fread( f, colClasses = "character" )
  z$geoid_z[ match( pad( substr( zip5, 1, 5 ), 5 ), z$zip5 ) ]
}

## county from a 15-digit census block code, through the same fixes
county_from_block <- function( block, city ) {
  g <- ifelse( !is.na( block ) & nchar( block ) >= 5,
               substr( pad( block, 15 ), 1, 5 ), NA_character_ )
  recode_county( ct_to_planning_region( g, city ) )
}

## Move county-level covariates published on legacy CT counties (09001-09015)
## onto planning regions (09110-09190). Overlap weights = 2019 town
## populations. type = "total": values are split across planning regions by
## each legacy county's population share; type = "rate": a planning region
## gets the population-weighted mean of the legacy counties it overlaps.
## d must have columns geoid, year and the listed value columns.
ct_legacy_to_pr <- function( d, cols, type = c( "total", "rate" ) ) {
  type <- match.arg( type )
  d <- as.data.table( d )
  w <- .ct[ , .( pop = sum( as.numeric( pop2019 ), na.rm = TRUE ) ), by = .( legacy_county, pr ) ]
  w[ , share_of_legacy := pop / sum( pop ), by = legacy_county ]
  w[ , share_of_pr := pop / sum( pop ), by = pr ]
  leg <- d[ geoid %in% w$legacy_county ]
  if ( !nrow( leg ) ) return( d )
  m <- merge( leg, w, by.x = "geoid", by.y = "legacy_county", allow.cartesian = TRUE )
  wt <- if ( type == "total" ) "share_of_legacy" else "share_of_pr"
  agg <- m[ , lapply( .SD, function( x ) sum( x * get( wt ), na.rm = TRUE ) /
                        ( if ( type == "rate" ) sum( get( wt )[ !is.na( x ) ] ) else 1 ) ),
            by = .( geoid = pr, year ), .SDcols = cols ]
  rbind( d[ !geoid %in% w$legacy_county ], agg, fill = TRUE )
}

## flag codes that are not 2023 county-equivalents
not_in_spine <- function( geoid ) !is.na( geoid ) & !geoid %in% .spine$geoid
