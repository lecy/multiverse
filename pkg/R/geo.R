#' Place records on 2023 county-equivalents
#'
#' Helpers that move organization records and county covariates onto the 2023
#' county-equivalents of the build, with Connecticut on its planning regions.
#'
#' They read the lookup files that build step 01 (`01_geo_spine.R`) writes to the geo
#' folder: `ct_town_lookup.csv`, `county_spine.csv`, and, when present,
#' `zip_county_modal_bmf.csv` (step 02) and the cached boundary files. The folder comes
#' from `geo_dir`, which defaults to the option `npdensity.geo_dir` (the build sets it to
#' `BUILD/data/geo` in `R/lib/00_setup.R`). Lookups are read once per folder and kept.
#'
#' * `ct_to_planning_region()`: Connecticut legacy county codes (09001–09015) to
#'   planning regions (09110–09190), by city name = town name, else the legacy county's
#'   dominant planning region. Other codes pass through.
#' * `ct_pr_from_latlon()`, `county_from_latlon()`: point in polygon (needs `sf`; the
#'   boundaries are downloaded once with `tigris` and cached in the geo folder).
#' * `county_from_zip()`: ZIP to county, from the modal county of geocoded BMF records.
#' * `county_from_block()`: county from a 15-digit census block code, through
#'   [recode_county()] and the Connecticut fix.
#' * `ct_legacy_to_pr()`: county covariates published on the legacy Connecticut
#'   counties onto planning regions, weighted by 2019 town populations. `type = "total"`
#'   splits values by each legacy county's population share; `type = "rate"` gives a
#'   planning region the population-weighted mean of the legacy counties it overlaps.
#'   `d` has columns `geoid`, `year` and `cols`.
#' * `not_in_spine()`: flags codes that are not 2023 county-equivalents.
#' * `geo_lookup()`: the lookup tables themselves (`"ct"`, `"spine"`).
#'
#' @param geoid Character vector of 5-digit county codes.
#' @param city City names (matched to Connecticut town names).
#' @param lat,lon Coordinates (WGS 84).
#' @param zip5 ZIP codes.
#' @param block 15-digit census block codes.
#' @param d A data.frame or data.table with `geoid`, `year` and `cols`.
#' @param cols Value columns of `d`.
#' @param type `"total"` or `"rate"`.
#' @param name Lookup name.
#' @param geo_dir The geo folder of a build.
#' @name geo
NULL

.geo <- new.env( parent = emptyenv() )

.geo_dir <- function( geo_dir ) {
  if ( is.null( geo_dir ) || !nzchar( geo_dir ) )
    stop( "no geo folder: set options( npdensity.geo_dir = <BUILD>/data/geo ) or pass geo_dir" )
  geo_dir
}

#' @rdname geo
#' @export
geo_lookup <- function( name = c( "ct", "spine" ), geo_dir = getOption( "npdensity.geo_dir" ) ) {
  name <- match.arg( name )
  f <- file.path( .geo_dir( geo_dir ), c( ct = "ct_town_lookup.csv", spine = "county_spine.csv" )[[ name ]] )
  key <- paste( name, normalizePath( f, winslash = "/", mustWork = FALSE ) )
  if ( is.null( .geo[[ key ]] ) ) {
    if ( !file.exists( f ) ) stop( "missing ", f, ": run R/build/A_geo/01_geo_spine.R first" )
    .geo[[ key ]] <- data.table::fread( f, colClasses = "character" )
  }
  .geo[[ key ]]
}

#' @rdname geo
#' @export
ct_to_planning_region <- function( geoid, city, geo_dir = getOption( "npdensity.geo_dir" ) ) {
  ct <- geo_lookup( "ct", geo_dir )
  ct_dom <- unique( ct[ , .( legacy_county, dominant_pr ) ] )
  is_legacy <- !is.na( geoid ) & geoid %in% ct_dom$legacy_county
  if ( !any( is_legacy ) ) return( geoid )
  ct_city <- unique( ct[ , .( town_name, pr ) ], by = "town_name" )
  by_city <- ct_city$pr[ match( toupper( trimws( city[ is_legacy ] ) ), ct_city$town_name ) ]
  by_cty  <- ct_dom$dominant_pr[ match( geoid[ is_legacy ], ct_dom$legacy_county ) ]
  geoid[ is_legacy ] <- ifelse( is.na( by_city ), by_cty, by_city )
  geoid
}

## boundary polygons, cached as rds in the geo folder and in memory
.geo_poly <- function( file, state, geo_dir ) {
  f <- file.path( .geo_dir( geo_dir ), file )
  key <- paste( "poly", normalizePath( f, winslash = "/", mustWork = FALSE ) )
  if ( is.null( .geo[[ key ]] ) ) {
    for ( p in c( "sf", if ( !file.exists( f ) ) "tigris" ) )
      if ( !requireNamespace( p, quietly = TRUE ) ) stop( "package ", p, " is needed" )
    if ( !file.exists( f ) ) {
      old <- options( tigris_use_cache = FALSE ); on.exit( options( old ) )
      p <- if ( is.null( state ) ) tigris::counties( year = 2023, cb = TRUE, progress_bar = FALSE )
           else tigris::counties( state = state, year = 2023, cb = TRUE, progress_bar = FALSE )
      saveRDS( sf::st_transform( p[ , "GEOID" ], 4326 ), f )
    }
    .geo[[ key ]] <- readRDS( f )
  }
  .geo[[ key ]]
}

.geo_hit <- function( lat, lon, poly ) {
  pts <- sf::st_as_sf( data.frame( lon = lon, lat = lat ), coords = c( "lon", "lat" ), crs = 4326 )
  hit <- sf::st_intersects( pts, poly )
  vapply( hit, function( i ) if ( length( i ) ) poly$GEOID[ i[1] ] else NA_character_, character( 1 ) )
}

#' @rdname geo
#' @export
ct_pr_from_latlon <- function( lat, lon, geo_dir = getOption( "npdensity.geo_dir" ) ) {
  out <- rep( NA_character_, length( lat ) )
  ok <- !is.na( lat ) & !is.na( lon )
  if ( !any( ok ) ) return( out )
  poly <- .geo_poly( "ct_planning_regions_2023.rds", "09", geo_dir )
  out[ ok ] <- .geo_hit( lat[ ok ], lon[ ok ], poly )
  out
}

#' @rdname geo
#' @export
county_from_latlon <- function( lat, lon, geo_dir = getOption( "npdensity.geo_dir" ) ) {
  out <- rep( NA_character_, length( lat ) )
  ok <- which( !is.na( lat ) & !is.na( lon ) )
  if ( !length( ok ) ) return( out )
  poly <- .geo_poly( "counties_2023_cb.rds", NULL, geo_dir )
  sf::sf_use_s2( FALSE )
  out[ ok ] <- .geo_hit( lat[ ok ], lon[ ok ], poly )
  out
}

#' @rdname geo
#' @export
county_from_zip <- function( zip5, geo_dir = getOption( "npdensity.geo_dir" ) ) {
  f <- file.path( .geo_dir( geo_dir ), "zip_county_modal_bmf.csv" )
  if ( !file.exists( f ) ) return( rep( NA_character_, length( zip5 ) ) )
  z <- data.table::fread( f, colClasses = "character" )
  z$geoid_z[ match( pad( substr( zip5, 1, 5 ), 5 ), z$zip5 ) ]
}

#' @rdname geo
#' @export
county_from_block <- function( block, city, geo_dir = getOption( "npdensity.geo_dir" ) ) {
  g <- ifelse( !is.na( block ) & nchar( block ) >= 5, substr( pad( block, 15 ), 1, 5 ), NA_character_ )
  recode_county( ct_to_planning_region( g, city, geo_dir ) )
}

#' @rdname geo
#' @export
ct_legacy_to_pr <- function( d, cols, type = c( "total", "rate" ), geo_dir = getOption( "npdensity.geo_dir" ) ) {
  type <- match.arg( type )
  d <- data.table::as.data.table( d )
  ct <- geo_lookup( "ct", geo_dir )
  w <- ct[ , .( pop = sum( as.numeric( pop2019 ), na.rm = TRUE ) ), by = .( legacy_county, pr ) ]
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

#' @rdname geo
#' @export
not_in_spine <- function( geoid, geo_dir = getOption( "npdensity.geo_dir" ) )
  !is.na( geoid ) & !geoid %in% geo_lookup( "spine", geo_dir )$geoid
