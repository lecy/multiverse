## 13_cov_saipe.R
## SAIPE county poverty and median household income, 2000-2024
## (Census API timeseries/poverty/saipe). Block: demographics / need.
##
## Output: D:/MULTIVERSE/data/cov/cov_saipe.csv (+ codebook_saipe.csv)
##   pov_n, pov_universe        poverty count and universe (all ages)
##   pov_rate                   100 * pov_n / pov_universe (recomputed)
##   pov_rate_pub               SAIPE published rate (as released)
##   mhi                        median household income (nominal $)
##   mhi_ct_approx              TRUE where mhi is a population-weighted
##                              average of legacy CT counties (not exact)

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )
source( "R/lib/13_16_cov_helpers.R" )

dir <- file.path( OUT$raw_cov, "saipe" )
dir.create( dir, recursive = TRUE, showWarnings = FALSE )
key <- census_key()

get_year <- function( y ) {
  f <- file.path( dir, sprintf( "saipe_county_%d.csv", y ) )
  if ( file.exists( f ) ) return( fread( f, colClasses = "character" ) )
  u <- paste0( "https://api.census.gov/data/timeseries/poverty/saipe?get=",
               "NAME,SAEPOVALL_PT,SAEPOVU_ALL,SAEPOVRTALL_PT,SAEMHI_PT",
               "&for=county:*&time=", y, "&key=", key )
  r <- tryCatch( jsonlite::fromJSON( u ), error = function( e ) NULL )
  if ( is.null( r ) ) { log_msg( "SAIPE ", y, " unavailable" ); return( NULL ) }
  d <- as.data.table( r[ -1, , drop = FALSE ] )
  setnames( d, r[ 1, ] )
  fwrite( d, f )
  d
}

d <- rbindlist( lapply( YEARS, get_year ), fill = TRUE )
num <- function( x ) suppressWarnings( as.numeric( x ) )
d <- d[ , .( geoid = paste0( state, county ), year = as.integer( time ),
             pov_n = num( SAEPOVALL_PT ), pov_universe = num( SAEPOVU_ALL ),
             pov_rate_pub = num( SAEPOVRTALL_PT ), mhi = num( SAEMHI_PT ) ) ]
d <- d[ substr( geoid, 1, 2 ) != "72" ]

ct_legacy_years <- d[ geoid %in% .ct$legacy_county, unique( year ) ]
d <- harmonize_counties( d, counts = c( "pov_n", "pov_universe" ),
                         rates = c( "pov_rate_pub", "mhi" ), w = "pov_universe" )
d[ , pov_rate := 100 * pov_n / pov_universe ]
d[ , mhi_ct_approx := geoid %in% .ct$pr & year %in% ct_legacy_years ]
setorder( d, geoid, year )

validate_cov( d, "SAIPE" )
cat( "rows with missing pov_rate or mhi:\n" ); print( d[ is.na( pov_rate ) | is.na( mhi ), .N, by = geoid ] )
print( d[ , .( us_pov_rate = round( 100 * sum( pov_n, na.rm = TRUE ) / sum( pov_universe, na.rm = TRUE ), 1 ),
               median_of_county_mhi = median( mhi, na.rm = TRUE ),
               max_abs_rate_gap = round( max( abs( pov_rate - pov_rate_pub ), na.rm = TRUE ), 2 ) ),
          keyby = year ] )

fwrite( d, file.path( OUT$cov, "cov_saipe.csv" ) )
fwrite( data.table(
  variable = c( "pov_n", "pov_universe", "pov_rate", "pov_rate_pub", "mhi", "mhi_ct_approx" ),
  definition = c( "Persons in poverty, all ages (SAIPE point estimate)",
                  "Poverty universe, all ages (SAEPOVU_ALL)",
                  "Poverty rate, percent = 100 * pov_n / pov_universe",
                  "SAIPE published poverty rate, percent",
                  "Median household income, nominal dollars",
                  "TRUE = CT planning-region mhi approximated as population-weighted mean of legacy counties" ),
  source = "Census SAIPE, API timeseries/poverty/saipe",
  years = paste( range( d$year ), collapse = "-" ),
  type = c( "B", "B", "A", "A", "A", "flag" ) ),
  file.path( OUT$cov, "codebook_saipe.csv" ) )
log_msg( "wrote cov_saipe.csv: ", nrow( d ), " rows" )
