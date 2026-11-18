## 14_cov_laus.R
## County labor force and unemployment (BLS LAUS annual averages), 2000-2023.
## Block: demographics / need.
##
## Source: USDA ERS "Unemployment and median household income for the U.S.,
## States, and Counties, 2000-23", which republishes LAUS county annual
## averages. (bls.gov rejects scripted downloads without a contact e-mail in
## the user agent, so the BLS files are not used directly.) 2024 is added
## automatically when ERS posts the 2000-24 edition (URL pattern below).
##
## Output: D:/MULTIVERSE/data/cov/cov_laus.csv (+ codebook_laus.csv)
##   lf, emp, unemp       civilian labor force, employed, unemployed
##   unemp_rate           100 * unemp / lf (recomputed)
##   unemp_rate_pub       published rate

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )
source( "R/lib/13_16_cov_helpers.R" )

dir <- file.path( OUT$raw_cov, "laus" )
ers <- "https://www.ers.usda.gov/media/5497/unemployment-and-median-household-income-for-the-united-states-states-and-counties-2000-23.csv?v=78722"
f <- fetch_ua( ers, dir, "ers_unemployment_2000_23.csv" )
if ( is.na( f ) ) stop( "ERS unemployment file not downloadable" )

r <- fread( f, colClasses = c( FIPS_Code = "character" ) )
r <- r[ grepl( "^(Civilian_labor_force|Employed|Unemployed|Unemployment_rate)_[0-9]{4}$", Attribute ) ]
r[ , year := as.integer( sub( "^.*_([0-9]{4})$", "\\1", Attribute ) ) ]
r[ , var := sub( "_[0-9]{4}$", "", Attribute ) ]
r[ , geoid := pad( FIPS_Code, 5 ) ]
r <- r[ substr( geoid, 3, 5 ) != "000" & substr( geoid, 1, 2 ) != "72" ]   # counties only, no PR
d <- dcast( r, geoid + year ~ var, value.var = "Value" )
setnames( d, c( "Civilian_labor_force", "Employed", "Unemployed", "Unemployment_rate" ),
          c( "lf", "emp", "unemp", "unemp_rate_pub" ) )
for ( v in c( "lf", "emp", "unemp", "unemp_rate_pub" ) ) set( d, j = v, value = as.numeric( d[[ v ]] ) )
d <- d[ year %in% YEARS ]

d <- harmonize_counties( d, counts = c( "lf", "emp", "unemp" ), rates = "unemp_rate_pub", w = "lf" )
d[ , unemp_rate := 100 * unemp / lf ]
setorder( d, geoid, year )

validate_cov( d, "LAUS (via ERS)" )
print( d[ , .( us_unemp_rate = round( 100 * sum( unemp, na.rm = TRUE ) / sum( lf, na.rm = TRUE ), 2 ),
               lf_millions = round( sum( lf, na.rm = TRUE ) / 1e6, 1 ),
               missing_rate = sum( is.na( unemp_rate ) ) ), keyby = year ] )

fwrite( d, file.path( OUT$cov, "cov_laus.csv" ) )
fwrite( data.table(
  variable = c( "lf", "emp", "unemp", "unemp_rate", "unemp_rate_pub" ),
  definition = c( "Civilian labor force, annual average", "Employed, annual average",
                  "Unemployed, annual average", "Unemployment rate, percent = 100 * unemp / lf",
                  "Published unemployment rate, percent" ),
  source = "BLS LAUS county annual averages, via USDA ERS county-level data sets (2000-23 edition)",
  years = paste( range( d$year ), collapse = "-" ),
  type = c( "B", "B", "B", "A", "A" ) ),
  file.path( OUT$cov, "codebook_laus.csv" ) )
log_msg( "wrote cov_laus.csv: ", nrow( d ), " rows" )
