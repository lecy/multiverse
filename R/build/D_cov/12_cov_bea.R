## 12_cov_bea.R
## BEA regional county covariates, 2000-2024: personal income and population
## (CAINC1), earnings and government earnings (CAINC5N 2001-2024; CAINC5S SIC
## for 2000), employment and government employment (CAEMP25N 2001-2022;
## CAEMP25S for 2000), GDP (CAGDP1 2001-2024), personal current transfer
## receipts (CAINC35 2000-2022: Social Security, Medicare, Medicaid, income
## maintenance; added 2026-11-16 for the indirect-government measures).
##
## Everything is stored as a total (dollars or persons/jobs) so CBSA
## aggregation is exact; per-capita and shares are recomputed downstream
## (e.g. pcpi = personal_income / bea_pop, gov_sl_share = gov_earn_sl / earn_total).
##
## Geography onto 2023 county-equivalents:
##  - Virginia BEA combination areas (519xx) -> component counties/cities
##  - Hawaii 15901 (Maui + Kalawao), Wisconsin 55901 (Shawano + Menominee)
##  - Alaska legacy areas (02261 Valdez-Cordova, 02232, 02280, 02201, 02231,
##    02010) -> 2023 boroughs/census areas
##  - totals of combined areas split by component population share (Census
##    county population, same year); per-capita values are therefore equal
##    across components
##  - Connecticut: planning regions (09110-09190) used where BEA publishes
##    them for a year/variable; otherwise legacy counties moved to planning
##    regions with ct_legacy_to_pr(..., "total")
##
## Output: D:/MULTIVERSE/data/cov/cov_bea.csv, cov_bea_codebook.csv

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )

BEA_DIR <- file.path( OUT$raw_cov, "bea" )
dir.create( BEA_DIR, recursive = TRUE, showWarnings = FALSE )

tables <- c( "CAINC1", "CAINC5N", "CAINC5S", "CAEMP25N", "CAEMP25S", "CAGDP1", "CAINC35" )
for ( t in tables )
  fetch( sprintf( "https://apps.bea.gov/regional/zip/%s.zip", t ), BEA_DIR )

## ---- variable map: table, line, variable, scale ---------------------------

VMAP <- rbindlist( list(
  list( "CAINC1",   "1",    "personal_income",      1000, "Personal income" ),
  list( "CAINC1",   "2",    "bea_pop",              1,    "Population (BEA, midyear)" ),
  list( "CAINC5N",  "35",   "earn_total",           1000, "Earnings by place of work" ),
  list( "CAINC5N",  "50",   "wages_total",          1000, "Wages and salaries" ),
  list( "CAINC5N",  "2000", "gov_earn_total",       1000, "Earnings: government and government enterprises" ),
  list( "CAINC5N",  "2001", "gov_earn_federal_civ", 1000, "Earnings: federal civilian government" ),
  list( "CAINC5N",  "2002", "gov_earn_military",    1000, "Earnings: military" ),
  list( "CAINC5N",  "2010", "gov_earn_sl",          1000, "Earnings: state and local government" ),
  list( "CAINC5S",  "35",   "earn_total",           1000, "Earnings by place of work (SIC, 2000 only)" ),
  list( "CAINC5S",  "50",   "wages_total",          1000, "Wages and salaries (SIC, 2000 only)" ),
  list( "CAINC5S",  "900",  "gov_earn_total",       1000, "Earnings: government (SIC, 2000 only)" ),
  list( "CAINC5S",  "910",  "gov_earn_federal_civ", 1000, "Earnings: federal civilian (SIC, 2000 only)" ),
  list( "CAINC5S",  "920",  "gov_earn_military",    1000, "Earnings: military (SIC, 2000 only)" ),
  list( "CAINC5S",  "930",  "gov_earn_sl",          1000, "Earnings: state and local (SIC, 2000 only)" ),
  list( "CAEMP25N", "10",   "emp_total",            1,    "Total employment (jobs)" ),
  list( "CAEMP25N", "2000", "gov_emp_total",        1,    "Employment: government and government enterprises" ),
  list( "CAEMP25N", "2010", "gov_emp_sl",           1,    "Employment: state and local government" ),
  list( "CAEMP25S", "10",   "emp_total",            1,    "Total employment (SIC, 2000 only)" ),
  list( "CAEMP25S", "900",  "gov_emp_total",        1,    "Employment: government (SIC, 2000 only)" ),
  list( "CAEMP25S", "930",  "gov_emp_sl",           1,    "Employment: state and local (SIC, 2000 only)" ),
  list( "CAGDP1",   "1",    "gdp_real",             1000, "Real GDP (chained 2017 dollars)" ),
  list( "CAGDP1",   "3",    "gdp_nominal",          1000, "Current-dollar GDP" ),
  ## personal current transfer receipts (22_cov_government.R; BEA publishes
  ## CAINC35 through 2022). Line 3000 (receipts of nonprofit institutions)
  ## is allocated from nonprofit activity: descriptive only, never a predictor.
  list( "CAINC35",  "1000", "tr_total",             1000, "Personal current transfer receipts" ),
  list( "CAINC35",  "2000", "tr_gov",               1000, "Transfer receipts of individuals from governments" ),
  list( "CAINC35",  "2110", "tr_socsec",            1000, "Social Security benefits" ),
  list( "CAINC35",  "2200", "tr_medical",           1000, "Medical benefits (Medicare + Medicaid + military)" ),
  list( "CAINC35",  "2210", "tr_medicare",          1000, "Medicare benefits" ),
  list( "CAINC35",  "2220", "tr_medicaid",          1000, "Public assistance medical care (mostly Medicaid)" ),
  list( "CAINC35",  "2300", "tr_income_maint",      1000, "Income maintenance (SSI, EITC, SNAP, other)" ),
  list( "CAINC35",  "2330", "tr_snap",              1000, "SNAP benefits" ),
  list( "CAINC35",  "2400", "tr_unemp",             1000, "Unemployment insurance compensation" ),
  list( "CAINC35",  "2500", "tr_veterans",          1000, "Veterans' benefits" ),
  list( "CAINC35",  "2600", "tr_education",         1000, "Education and training assistance" ),
  list( "CAINC35",  "3000", "tr_nonprofit",         1000, "Transfer receipts of nonprofit institutions (descriptive only)" ) ) )
setnames( VMAP, c( "table", "line", "var", "scale", "label" ) )
## SIC tables supply 2000 only; NAICS/other tables supply 2001+ (CAINC1: all)
VMAP[ , years := fifelse( table %in% c( "CAINC5S", "CAEMP25S" ), "2000",
                 fifelse( table == "CAINC1", "2000-2024", "2001+" ) ) ]

read_bea <- function( t ) {
  z <- file.path( BEA_DIR, paste0( t, ".zip" ) )
  f <- grep( "ALL_AREAS", unzip( z, list = TRUE )$Name, value = TRUE )
  tmpd <- tmpfile(); dir.create( tmpd )
  unzip( z, files = f, exdir = tmpd )
  d <- fread( file.path( tmpd, f ), colClasses = "character", encoding = "Latin-1", fill = TRUE )
  unlink( tmpd, recursive = TRUE )
  d[ , GeoFIPS := trimws( gsub( '"', "", GeoFIPS ) ) ]
  d[ , LineCode := trimws( LineCode ) ]
  d <- d[ grepl( "^[0-9]{5}$", GeoFIPS ) & !grepl( "000$", GeoFIPS ) ]   # counties only
  v <- VMAP[ table == t ]
  d <- d[ LineCode %in% v$line ]
  ycols <- intersect( as.character( 2000:2024 ), names( d ) )
  if ( t %in% c( "CAINC5S", "CAEMP25S" ) ) ycols <- "2000"
  if ( t %in% c( "CAINC5N", "CAEMP25N", "CAGDP1" ) ) ycols <- setdiff( ycols, "2000" )
  m <- melt( d[ , c( "GeoFIPS", "GeoName", "LineCode", ycols ), with = FALSE ],
             id.vars = c( "GeoFIPS", "GeoName", "LineCode" ), variable.name = "year", value.name = "raw" )
  m[ , year := as.integer( as.character( year ) ) ]
  m[ , value := suppressWarnings( as.numeric( raw ) ) ]
  m <- merge( m, v[ , .( LineCode = line, var, scale ) ], by = "LineCode" )
  m[ , value := value * scale ]
  m[ , .( bea_fips = GeoFIPS, bea_name = trimws( GeoName ), year, var, value, raw = trimws( raw ) ) ]
}

raw <- rbindlist( lapply( tables, read_bea ) )
log_msg( "BEA rows: ", format( nrow( raw ), big.mark = "," ) )
supp <- raw[ is.na( value ), .N, by = .( var, raw ) ][ order( var, -N ) ]
fwrite( supp, file.path( OUT$diag, "bea_suppression_codes.csv" ) )

## ---- geography map: bea_fips -> 2023 geoid(s) --------------------------

spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
codes <- unique( raw[ !is.na( value ), .( bea_fips, bea_name ) ] )

## Virginia combination areas: first token = county, later tokens = cities
va_spine <- spine[ substr( geoid, 1, 2 ) == "51" ]
va_spine[ , nm := tolower( county_name ) ]
va <- codes[ grepl( "^519[0-9]{2}$", bea_fips ) ]
va_map <- rbindlist( lapply( seq_len( nrow( va ) ), function( i ) {
  nm <- sub( ",? *VA\\*?$", "", va$bea_name[ i ] )
  nm <- sub( "\\*$", "", nm )
  tok <- trimws( unlist( strsplit( nm, "\\+|," ) ) )
  tok <- tok[ tok != "" ]
  tgt <- c( paste( tok[1], "county" ),
            if ( length( tok ) > 1 ) paste( sub( " city$", "", tok[ -1 ], ignore.case = TRUE ), "city" ) )
  tgt <- tolower( tgt )
  g <- va_spine$geoid[ match( tgt, va_spine$nm ) ]
  data.table( bea_fips = va$bea_fips[ i ], bea_name = va$bea_name[ i ], target = tgt, geoid = g )
} ) )
if ( va_map[ is.na( geoid ), .N ] ) {
  print( va_map[ is.na( geoid ) ] )
  stop( "unmatched Virginia combination components" )
}

other_map <- data.table(
  bea_fips = c( "15901", "15901", "55901", "55901",
                "02261", "02261", "02232", "02232", "02280", "02280",
                "02201", "02231", "02231", "02231", "02010", "02010" ),
  geoid    = c( "15009", "15005", "55115", "55078",
                "02063", "02066", "02105", "02230", "02195", "02275",
                "02198", "02105", "02230", "02282", "02013", "02016" ) )

direct <- codes[ bea_fips %in% spine$geoid | bea_fips %in% COUNTY_RECODE$from |
                 grepl( "^09(00[1-9]|01[0-5])$", bea_fips ) ]
direct_map <- unique( direct[ , .( bea_fips, geoid = recode_county( bea_fips ) ) ] )
## legacy CT counties pass through for now (moved to planning regions below)
direct_map[ grepl( "^09(00[1-9]|01[0-5])$", bea_fips ), geoid := bea_fips ]

geo_map <- unique( rbind( direct_map, va_map[ , .( bea_fips, geoid ) ],
                          other_map[ bea_fips %in% codes$bea_fips ] ) )
unmapped <- setdiff( codes$bea_fips, geo_map$bea_fips )
if ( length( unmapped ) ) {
  log_msg( "unmapped BEA codes with data (dropped): ", paste( unmapped, collapse = " " ) )
  print( codes[ bea_fips %in% unmapped ] )
}
fwrite( merge( geo_map, codes, by = "bea_fips" ), file.path( OUT$diag, "bea_geo_map.csv" ) )

## population shares for splitting combined areas
pop <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )
gm <- merge( geo_map, CJ( geoid = unique( geo_map$geoid ), year = 2000:2024 ),
             by = "geoid", allow.cartesian = TRUE )
gm <- merge( gm, pop[ , .( geoid, year, population ) ], by = c( "geoid", "year" ), all.x = TRUE )
## legacy CT counties are not in the population file: weight 1 (single target)
gm[ is.na( population ), population := 1 ]
gm[ , w := population / sum( population ), by = .( bea_fips, year ) ]

d <- merge( raw[ !is.na( value ) ], gm[ , .( bea_fips, year, geoid, w ) ],
            by = c( "bea_fips", "year" ), allow.cartesian = TRUE )
d <- d[ , .( value = sum( value * w ) ), by = .( geoid, year, var ) ]

## ---- Connecticut: planning regions where published, else from legacy ----

ct_pr  <- d[ geoid %in% .ct$pr ]
ct_leg <- d[ grepl( "^09(00[1-9]|01[0-5])$", geoid ) ]
d <- d[ !substr( geoid, 1, 2 ) %in% "09" ]
have_pr <- unique( ct_pr[ , .( year, var ) ] )
leg_need <- ct_leg[ !have_pr, on = .( year, var ) ]
if ( nrow( leg_need ) ) {
  w <- dcast( leg_need, geoid + year ~ var, value.var = "value" )
  vcols <- setdiff( names( w ), c( "geoid", "year" ) )
  w <- ct_legacy_to_pr( w, vcols, "total" )
  leg_conv <- melt( w[ geoid %in% .ct$pr ], id.vars = c( "geoid", "year" ),
                    variable.name = "var", value.name = "value", na.rm = TRUE )
  leg_conv[ , var := as.character( var ) ]
  ## ct_legacy_to_pr() returns 0 where every input is NA: keep only
  ## year x variable cells that had legacy data
  leg_conv <- leg_conv[ unique( leg_need[ , .( year, var ) ] ), on = .( year, var ), nomatch = 0 ]
} else leg_conv <- d[ 0 ]
ct_src <- rbind( ct_pr[ , src := "planning_region" ], leg_conv[ , src := "legacy_converted" ] )
fwrite( ct_src[ , .N, by = .( var, src, year ) ][ order( var, year ) ],
        file.path( OUT$diag, "bea_ct_source.csv" ) )
d <- rbind( d, ct_src[ , .( geoid, year, var, value ) ] )

## ---- wide output on the full spine ---------------------------------------

stopifnot( !anyDuplicated( d[ , .( geoid, year, var ) ] ) )
w <- dcast( d, geoid + year ~ var, value.var = "value" )
out <- merge( CJ( geoid = spine$geoid, year = 2000:2024 ), w, by = c( "geoid", "year" ), all.x = TRUE )
vars <- unique( VMAP$var )
for ( v in setdiff( vars, names( out ) ) ) out[ , ( v ) := NA_real_ ]
setcolorder( out, c( "geoid", "year", vars ) )
stopifnot( !anyDuplicated( out[ , .( geoid, year ) ] ) )
fwrite( out, file.path( OUT$cov, "cov_bea.csv" ) )

## ---- codebook ------------------------------------------------------------

cb <- VMAP[ , .( definition = first( sub( " \\(SIC, 2000 only\\)", "", label ) ),
                 source = paste( unique( paste0( table, " line ", line ) ), collapse = "; " ) ),
            by = .( variable = var ) ]
cov_years <- melt( out, id.vars = c( "geoid", "year" ), variable.name = "variable" )[
  !is.na( value ), .( years = paste( range( year ), collapse = "-" ) ), by = variable ]
cb <- merge( cb, cov_years, by = "variable", all.x = TRUE )
cb[ , units := fifelse( variable %in% c( "bea_pop" ), "persons",
               fifelse( variable %in% c( "emp_total", "gov_emp_total", "gov_emp_sl" ), "jobs",
               fifelse( variable == "gdp_real", "chained 2017 dollars", "current dollars" ) ) ) ]
cb[ , type := fifelse( variable == "bea_pop", "C", "B" ) ]
cb[ , note := "total; derive per capita / shares downstream" ]
cb <- rbind( cb, data.table(
  variable = c( "pcpi (derived)", "gov_earn_sl_share (derived)", "gov_emp_sl_share (derived)", "gdp_per_job (derived)" ),
  definition = c( "personal_income / bea_pop", "gov_earn_sl / earn_total",
                  "gov_emp_sl / emp_total", "gdp_real / emp_total" ),
  source = "derived", years = c( "2000-2024", "2000-2024", "2000-2022", "2001-2022" ),
  units = c( "dollars", "share", "share", "chained 2017 dollars per job" ),
  type = "A", note = "compute after aggregation" ), fill = TRUE )
fwrite( cb, file.path( OUT$cov, "cov_bea_codebook.csv" ) )

## ---- validation ------------------------------------------------------------

cov_tab <- melt( out, id.vars = c( "geoid", "year" ) )[ , .( n = sum( !is.na( value ) ) ),
                                                          by = .( variable, year ) ]
cov_wide <- dcast( cov_tab, year ~ variable, value.var = "n" )
fwrite( cov_wide, file.path( OUT$diag, "bea_coverage.csv" ) )
print( cov_wide )

## national totals from BEA US rows
us <- rbindlist( lapply( c( "CAINC1", "CAINC5N", "CAINC35" ), function( t ) {
  z <- file.path( BEA_DIR, paste0( t, ".zip" ) )
  f <- grep( "ALL_AREAS", unzip( z, list = TRUE )$Name, value = TRUE )
  tmpd <- tmpfile(); dir.create( tmpd ); unzip( z, files = f, exdir = tmpd )
  x <- fread( file.path( tmpd, f ), colClasses = "character", encoding = "Latin-1", fill = TRUE )
  unlink( tmpd, recursive = TRUE )
  x[ , GeoFIPS := trimws( gsub( '"', "", GeoFIPS ) ) ]
  x <- x[ GeoFIPS == "00000" & trimws( LineCode ) %in% VMAP[ table == t, line ] ]
  m <- melt( x[ , c( "LineCode", intersect( c( "2005", "2015", "2024" ), names( x ) ) ), with = FALSE ],
             id.vars = "LineCode", variable.name = "year" )
  m[ , .( table = t, line = trimws( LineCode ), year = as.integer( as.character( year ) ),
          us = as.numeric( value ) * 1000 ) ]
} ) )
us <- merge( us, VMAP[ , .( table, line, var ) ], by = c( "table", "line" ) )
us[ var == "bea_pop", us := us / 1000 ]
sums <- melt( out[ year %in% c( 2005, 2015, 2024 ) ], id.vars = c( "geoid", "year" ),
              variable.name = "var" )[ , .( county_sum = sum( value, na.rm = TRUE ) ), by = .( var, year ) ]
chk <- merge( us, sums, by = c( "var", "year" ) )[ , ratio := county_sum / us ]
print( chk[ , .( var, year, us, county_sum, ratio = round( ratio, 4 ) ) ] )
fwrite( chk, file.path( OUT$diag, "bea_national_check.csv" ) )

## spot checks
spot <- out[ geoid %in% c( "51003", "51540", "51019", "15005", "15009", "09110", "09170", "02063", "02066" ) &
             year %in% c( 2000, 2010, 2022, 2024 ),
             .( geoid, year, personal_income, bea_pop, pcpi = round( personal_income / bea_pop ),
                gov_earn_sl, gdp_real ) ]
print( spot )
log_msg( "wrote ", file.path( OUT$cov, "cov_bea.csv" ), "  rows: ", nrow( out ) )
