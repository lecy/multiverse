## 22_cov_government.R
## Direct vs indirect government covariates, first wave
## (notes/government-funding-measures.md). BEA transfer receipts (CAINC35)
## are added in 12_cov_bea.R, which already handles the BEA geography.
##
##   Part A  QCEW government employment and wages by place of work, ownership
##           1 (federal), 2 (state), 3 (local), county annual averages
##           2000-2024, read from the QCEW files already cached for 20.
##   Part B  ACS 5-year class of worker (B24080), residents employed by local,
##           state and federal government and by private nonprofits,
##           end years 2009-2024. 2000 is left missing: SF3 has class of
##           worker only crossed by industry (P051).
##   Part C  Census of Governments local government finance, census years
##           2002, 2007, 2012, 2017, 2022 (local governments only: counties,
##           cities, townships, special districts, school districts; state
##           governments are not geographic). Every unit is assigned to the
##           county where it is located (multi-county districts are not
##           split).
##             2002, 2007  Census county-area files (complete, with
##                         imputations), on Census county codes
##             2012        county-area file; the 2012 individual unit file
##                         is aggregated as a cross-check
##             2017, 2022  individual unit files (IDs carry FIPS codes)
##           The 2022 public-use file drops salaries (Z00), folds equipment
##           purchases (G) into construction (F) and welfare vendor
##           payments (E74/E75) into E79, so the definitions below use only
##           sums that are stable across years.
##
## Outputs: D:/MULTIVERSE/data/cov/cov_gov.csv (+ codebook)
##          D:/MULTIVERSE/data/diagnostics/cog_*.csv

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
source( "R/13_16_cov_helpers.R" )

PART <- if ( length( commandArgs( TRUE ) ) ) commandArgs( TRUE )[1] else "all"
f_out <- file.path( OUT$cov, "cov_gov.csv" )

## ---- Part A: QCEW government ---------------------------------------------------

qdir <- file.path( OUT$raw_cov, "qcew" )
OWN <- c( "1" = "fgov", "2" = "sgov", "3" = "lgov" )
rd_q <- function( y ) {
  f <- file.path( qdir, sprintf( "qcew_%d_10.csv", y ) )
  if ( !file.exists( f ) ) { warning( "missing ", f ); return( NULL ) }
  d <- fread( f, select = c( "area_fips", "own_code", "industry_code", "agglvl_code", "year",
                             "disclosure_code", "annual_avg_emplvl", "total_annual_wages" ),
              colClasses = list( character = c( "area_fips", "disclosure_code", "industry_code", "own_code" ) ) )
  ## the 2000 file omits suppressed county rows instead of flagging them:
  ## every county with a total-covered row gets a row per ownership
  ctys <- unique( d[ own_code == "0" & agglvl_code == 70L & grepl( "^[0-9]{5}$", area_fips ) &
                       !grepl( "000$|999$|^(72|78)", area_fips ), area_fips ] )
  d <- d[ own_code %in% names( OWN ) & grepl( "^[0-9]{5}$", area_fips ) & !grepl( "^(72|78)", area_fips ) &
            ( ( !grepl( "000$|999$", area_fips ) & agglvl_code == 71L ) |
              ( grepl( "000$", area_fips ) & agglvl_code == 51L ) ) ]
  d[ , `:=`( emp = as.numeric( annual_avg_emplvl ), wages = as.numeric( total_annual_wages ),
             sup = disclosure_code %in% "N", lev = OWN[ own_code ], st = substr( area_fips, 1, 2 ),
             year = as.integer( year ) ) ]
  d[ ( sup ), `:=`( emp = NA_real_, wages = NA_real_ ) ]
  ## suppressed county cells (about 300 a year for local government, often
  ## a single local employer): state total minus disclosed counties,
  ## allocated to the suppressed counties by population
  cty <- merge( CJ( area_fips = ctys, own_code = names( OWN ) ), d[ agglvl_code == 71L ],
                by = c( "area_fips", "own_code" ), all.x = TRUE )
  cty[ is.na( agglvl_code ), `:=`( sup = TRUE, emp = NA_real_, wages = NA_real_ ) ]
  cty[ , `:=`( lev = OWN[ own_code ], st = substr( area_fips, 1, 2 ), year = as.integer( y ) ) ]
  stt <- d[ agglvl_code == 51L & !sup, .( st, lev, s_emp = emp, s_wages = wages ) ]
  cty <- merge( cty, pop[ year == y, .( area_fips = geoid, population ) ], by = "area_fips", all.x = TRUE )
  cty[ is.na( population ), population := 1 ]
  cty <- merge( cty, stt, by = c( "st", "lev" ), all.x = TRUE )
  cty[ , `:=`( r_emp = pmax( s_emp - sum( emp, na.rm = TRUE ), 0 ), r_wages = pmax( s_wages - sum( wages, na.rm = TRUE ), 0 ),
               w = population * sup / sum( population * sup ) ), by = .( st, lev ) ]
  cty[ ( sup ), `:=`( emp = r_emp * w, wages = r_wages * w ) ]
  w <- dcast( cty, area_fips + year ~ lev, value.var = c( "emp", "wages", "sup" ) )
  setnames( w, names( w ), sub( "^emp_(.*)$", "qcew_\\1_emp", sub( "^wages_(.*)$", "qcew_\\1_wages",
                           sub( "^sup_(.*)$", "qcew_\\1_imputed", names( w ) ) ) ) )
  setnames( w, "area_fips", "geoid" )
  w
}
pop <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )
q <- rbindlist( lapply( YEARS, rd_q ), fill = TRUE )
qv <- as.vector( outer( c( "qcew_fgov", "qcew_sgov", "qcew_lgov" ), c( "_emp", "_wages" ), paste0 ) )
qf <- c( "qcew_fgov_imputed", "qcew_sgov_imputed", "qcew_lgov_imputed" )
for ( v in setdiff( c( qv, qf ), names( q ) ) ) q[ , ( v ) := NA_real_ ]
for ( v in c( qv, qf ) ) set( q, j = v, value = as.numeric( q[[ v ]] ) )
q <- harmonize_counties( q, counts = c( qv, qf ) )
validate_cov( q, "QCEW government" )
print( q[ , .( counties = .N,
               fgov_emp_k = round( sum( qcew_fgov_emp, na.rm = TRUE ) / 1e3 ),
               sgov_emp_k = round( sum( qcew_sgov_emp, na.rm = TRUE ) / 1e3 ),
               lgov_emp_k = round( sum( qcew_lgov_emp, na.rm = TRUE ) / 1e3 ),
               lgov_wages_bn = round( sum( qcew_lgov_wages, na.rm = TRUE ) / 1e9 ),
               lgov_imputed = sum( qcew_lgov_imputed > 0, na.rm = TRUE ),
               lgov_imputed_emp_k = round( sum( qcew_lgov_emp[ qcew_lgov_imputed > 0 ], na.rm = TRUE ) / 1e3 ),
               sgov_imputed = sum( qcew_sgov_imputed > 0, na.rm = TRUE ),
               lgov_na = sum( is.na( qcew_lgov_emp ) ) ), keyby = year ] )

## ---- Part B: ACS class of worker ------------------------------------------------

suppressPackageStartupMessages( library( tidycensus ) )
Sys.setenv( CENSUS_API_KEY = census_key() )
acs_cache <- file.path( OUT$raw_cov, "acs" )
cow_vars <- c( cow_den = "B24080_001",
               npo_m = "B24080_006", lgov_m = "B24080_007", sgov_m = "B24080_008", fgov_m = "B24080_009",
               npo_f = "B24080_016", lgov_f = "B24080_017", sgov_f = "B24080_018", fgov_f = "B24080_019" )
get_cow <- function( y ) {
  f <- file.path( acs_cache, sprintf( "acs5_cow_%d.csv", y ) )
  if ( file.exists( f ) ) return( fread( f, colClasses = list( character = "GEOID" ) ) )
  d <- as.data.table( get_acs( geography = "county", variables = cow_vars, year = y,
                               survey = "acs5", output = "wide" ) )
  d[ , year := y ]
  fwrite( d, f )
  d
}
a <- rbindlist( lapply( 2009:2024, function( y )
  tryCatch( get_cow( y ), error = function( e ) { message( "ACS ", y, ": ", conditionMessage( e ) ); NULL } ) ),
  fill = TRUE )
a <- a[ substr( GEOID, 1, 2 ) != "72",
        .( geoid = GEOID, year = as.integer( year ), acs_cow_den = cow_denE,
           acs_cow_npo = npo_mE + npo_fE, acs_cow_lgov = lgov_mE + lgov_fE,
           acs_cow_sgov = sgov_mE + sgov_fE, acs_cow_fgov = fgov_mE + fgov_fE ) ]
av <- c( "acs_cow_den", "acs_cow_npo", "acs_cow_lgov", "acs_cow_sgov", "acs_cow_fgov" )
a <- harmonize_counties( a, counts = av )
## single missing county-years (Rio Arriba NM 2018): linear between neighbours
a <- merge( CJ( geoid = unique( a$geoid ), year = 2009:2024 ), a, by = c( "geoid", "year" ), all.x = TRUE )
log_msg( "ACS county-years filled by interpolation: ", a[ is.na( acs_cow_den ), paste( geoid, year, collapse = " " ) ] )
for ( v in av ) a[ , ( v ) := { x <- get( v ); ok <- !is.na( x )
  if ( any( !ok ) && sum( ok ) >= 2 ) approx( year[ ok ], x[ ok ], year, rule = 1 )$y else x }, by = geoid ]
validate_cov( a, "ACS class of worker" )
print( a[ , .( counties = .N, workers_m = round( sum( acs_cow_den ) / 1e6, 1 ),
               pct_lgov = round( sum( acs_cow_lgov ) / sum( acs_cow_den ), 4 ),
               pct_sgov = round( sum( acs_cow_sgov ) / sum( acs_cow_den ), 4 ),
               pct_fgov = round( sum( acs_cow_fgov ) / sum( acs_cow_den ), 4 ),
               pct_npo = round( sum( acs_cow_npo ) / sum( acs_cow_den ), 4 ) ), keyby = year ] )

## ---- Part C: Census of Governments, local finance ----------------------------------

CD <- file.path( OUT$raw_cov, "cog" ); XD <- file.path( CD, "x" )
dir.create( XD, recursive = TRUE, showWarnings = FALSE )
SRC <- c( "https://www2.census.gov/programs-surveys/gov-finances/datasets/2002/public-use-datasets/02countyarea.zip",
          "https://www2.census.gov/programs-surveys/gov-finances/datasets/2002/public-use-datasets/02countyareagid.zip",
          "https://www2.census.gov/programs-surveys/gov-finances/datasets/historical/county_area_finances_2007.zip",
          "https://www2.census.gov/programs-surveys/gov-finances/datasets/historical/county_area_finances_2012.zip",
          "https://www2.census.gov/programs-surveys/gov-finances/tables/2012/summary-tables/2012_Individual_Unit_file.zip",
          "https://www2.census.gov/programs-surveys/gov-finances/tables/2017/2017_Individual_Unit_File.zip",
          "https://www2.census.gov/programs-surveys/gov-finances/tables/2022/2022_Individual_Unit_File.zip" )
for ( u in SRC ) {
  z <- fetch( u, CD )
  if ( !length( list.files( XD, pattern = "^(2002_Census|02countyareagid|County Area|2012Fin|Fin_GID_2012|2017Fin|2022_Ind)" ) ) ||
       PART == "unzip" ) unzip( z, exdir = XD, overwrite = TRUE )
}
xf <- function( pat ) { f <- list.files( XD, pattern = pat, recursive = TRUE, full.names = TRUE ); stopifnot( length( f ) == 1 ); f }

## item groups (amounts are $000s; local governments only)
UTIL <- sprintf( "%02d", 90:94 )                     # liquor stores and utilities
grp <- function( item ) {
  p <- substr( item, 1, 1 ); fn <- substr( item, 2, 3 )
  data.table( item,
    curop    = p == "E" & !fn %in% UTIL,
    capital  = p %in% c( "F", "G" ) & !fn %in% UTIL,
    assist   = p == "J",
    interest = item == "I89",
    educ     = p == "E" & fn %in% c( "12", "16", "18", "21" ),
    welfare  = ( p %in% c( "E", "J" ) & fn %in% c( "67", "68", "74", "75", "77", "79" ) ),
    health   = p == "E" & fn %in% c( "32", "36", "38" ),
    salaries = item == "Z00",
    fed_igr  = p == "B",
    st_igr   = p == "C" )
}
collapse_items <- function( d ) {          # d: geoid, item, amt ($000)
  g <- grp( unique( d$item ) )
  d <- merge( d, g, by = "item" )
  out <- d[ , .( cog_curop    = sum( amt[ curop ] ),
                 cog_capital  = sum( amt[ capital ] ),
                 cog_assist   = sum( amt[ assist ] ),
                 cog_interest = sum( amt[ interest ] ),
                 cog_educ_curop = sum( amt[ educ ] ),
                 cog_welfare  = sum( amt[ welfare ] ),
                 cog_health_curop = sum( amt[ health ] ),
                 cog_salaries = if ( any( salaries ) ) sum( amt[ salaries ] ) else NA_real_,
                 cog_fed_igr  = sum( amt[ fed_igr ] ),
                 cog_state_igr = sum( amt[ st_igr ] ) ), by = geoid ]
  out[ , cog_dge := cog_curop + cog_capital + cog_assist + cog_interest ]
  cv <- setdiff( names( out ), "geoid" )
  for ( v in cv ) set( out, j = v, value = out[[ v ]] * 1000 )
  out
}

## Census (state, county) codes -> FIPS county: 2012 GID file, 2002 county-area GID as fallback
gid12 <- readLines( xf( "^Fin_GID_2012" ) )
gid12 <- unique( data.table( ccode = substr( gid12, 1, 2 ), ccty = substr( gid12, 4, 6 ),
                             fips = substr( gid12, 114, 118 ) )[ ccty != "000" & grepl( "^[0-9]{5}$", fips ) ] )
gid12 <- gid12[ , .( fips = names( which.max( table( fips ) ) ) ), by = .( ccode, ccty ) ]
gid02 <- readLines( xf( "^02countyareagid" ) )
gid02 <- data.table( ccode = substr( gid02, 1, 2 ), ccty = substr( gid02, 3, 5 ),
                     fips = substr( gid02, nchar( gid02 ) - 4, nchar( gid02 ) ) )
cmap <- rbind( gid12[ , src := "gid2012" ], gid02[ !gid12, on = .( ccode, ccty ) ][ , src := "gid2002" ] )
map_census <- function( d, label ) {
  d <- merge( d, cmap[ , .( ccode, ccty, geoid = fips ) ], by = c( "ccode", "ccty" ), all.x = TRUE )
  bad <- d[ is.na( geoid ), .( amt_bn = sum( amt ) / 1e6 ), by = .( ccode, ccty ) ]
  if ( nrow( bad ) ) { log_msg( label, ": unmatched Census county codes" ); print( bad ) }
  d[ !is.na( geoid ) ]
}

## 2002 county-area file (fixed width, Census codes)
l <- readLines( xf( "^2002_Census_State_County" ) )
c02 <- data.table( ccode = substr( l, 1, 2 ), ccty = substr( l, 4, 6 ), item = substr( l, 15, 17 ),
                   amt = as.numeric( substr( l, 18, 29 ) ) )[ ccty != "000" ]
c02 <- collapse_items( map_census( c02, "2002" )[ , .( geoid, item, amt ) ] )[ , year := 2002L ]

## 2007 and 2012 county-area files (CSV, Census codes)
rd_ca <- function( y ) {
  d <- fread( xf( sprintf( "County_Area_Finances_%d\\.txt$", y ) ), colClasses = "character" )
  d <- d[ County != "000", .( ccode = State, ccty = County, item = Code, amt = as.numeric( Amt ),
                              imp = as.numeric( Imputed ) ) ]
  imp <- d[ , .( amt_bn = sum( amt ) / 1e6, imputed_share = sum( imp, na.rm = TRUE ) / sum( amt ) ),
            by = .( E = substr( item, 1, 1 ) == "E" ) ]
  log_msg( y, " county-area imputed share (E = current operations): " )
  print( imp )
  collapse_items( map_census( d, y )[ , .( geoid, item, amt ) ] )[ , year := as.integer( y ) ]
}
c07 <- rd_ca( 2007 ); c12 <- rd_ca( 2012 )

## individual unit files: 12-char FIPS IDs (2017, 2022); 14-char Census IDs (2012 check)
rd_unit <- function( f, idlen ) {
  l <- readLines( f )
  data.table( id = substr( l, 1, idlen ), item = substr( l, idlen + 1, idlen + 3 ),
              amt = as.numeric( substr( l, idlen + 4, idlen + 15 ) ) )
}
u17 <- rd_unit( xf( "^2017FinEstDAT.*\\.txt$" ), 12 )
u22 <- rd_unit( xf( "^2022FinEstDAT.*\\.txt$" ), 12 )
unit_fips <- function( u, y ) {
  u <- u[ substr( id, 3, 3 ) != "0" ]                      # drop state governments
  collapse_items( u[ , .( geoid = paste0( substr( id, 1, 2 ), substr( id, 4, 6 ) ), item, amt ) ] )[ , year := y ]
}
c17 <- unit_fips( u17, 2017L ); c22 <- unit_fips( u22, 2022L )
u12 <- rd_unit( xf( "^2012FinEstDAT.*\\.txt$" ), 14 )
u12 <- u12[ substr( id, 3, 3 ) != "0", .( ccode = substr( id, 1, 2 ), ccty = substr( id, 4, 6 ), item, amt ) ]
c12u <- collapse_items( map_census( u12, "2012 unit" )[ , .( geoid, item, amt ) ] )[ , year := 2012L ]

## cross-check: 2012 county-area file vs 2012 unit file
cv <- c( "cog_dge", "cog_curop", "cog_capital", "cog_assist", "cog_welfare", "cog_health_curop", "cog_salaries" )
chk <- merge( melt( c12, id.vars = c( "geoid", "year" ), measure.vars = cv ),
              melt( c12u, id.vars = c( "geoid", "year" ), measure.vars = cv ),
              by = c( "geoid", "year", "variable" ), suffixes = c( "_ca", "_unit" ), all = TRUE )
chk12 <- chk[ , .( counties = .N, total_ca_bn = sum( value_ca, na.rm = TRUE ) / 1e9,
                   total_unit_bn = sum( value_unit, na.rm = TRUE ) / 1e9,
                   r_log = cor( log1p( value_ca ), log1p( value_unit ), use = "complete.obs" ),
                   median_ratio = median( value_unit / value_ca, na.rm = TRUE ),
                   share_within_1pct = mean( abs( value_unit / value_ca - 1 ) < 0.01, na.rm = TRUE ) ), by = variable ]
log_msg( "2012 county-area vs unit file" ); print( chk12 )
fwrite( chk12, file.path( OUT$diag, "cog_2012_ca_vs_unit.csv" ) )

cog <- rbindlist( list( c02, c07, c12, c17, c22 ), use.names = TRUE )
cogv <- setdiff( names( cog ), c( "geoid", "year" ) )
## salaries are not in the 2022 public-use file: keep NA, not zero
cog <- harmonize_counties( cog, counts = cogv )
cog[ year == 2022, cog_salaries := NA_real_ ]
## New York City is one government filed under New York County: pool the five
## boroughs and allocate by population (other cities that span counties stay
## in their home county; CBSA totals are unaffected)
NYC <- c( "36005", "36047", "36061", "36081", "36085" )
nyc <- merge( CJ( geoid = NYC, year = unique( cog$year ) ), cog, by = c( "geoid", "year" ), all.x = TRUE )
nyc <- merge( nyc, pop[ , .( geoid, year, population ) ], by = c( "geoid", "year" ) )
for ( v in cogv ) nyc[ , ( v ) := sum( get( v ), na.rm = !all( is.na( get( v ) ) ) ) * population / sum( population ), by = year ]
cog <- rbind( cog[ !geoid %in% NYC ], nyc[ , c( "geoid", "year", cogv ), with = FALSE ] )
cog[ year == 2022, cog_salaries := NA_real_ ]
validate_cov( cog, "Census of Governments local finance" )
nat <- cog[ , lapply( .SD, function( x ) round( sum( x, na.rm = TRUE ) / 1e9, 1 ) ), keyby = year, .SDcols = cogv ]
log_msg( "local government totals ($bn)" ); print( nat )
fwrite( nat, file.path( OUT$diag, "cog_national_totals.csv" ) )

## ---- combine and write -----------------------------------------------------------

sp <- spine_geoids()
out <- merge( CJ( geoid = sp, year = YEARS ), q, by = c( "geoid", "year" ), all.x = TRUE )
out <- merge( out, a, by = c( "geoid", "year" ), all.x = TRUE )
out <- merge( out, cog, by = c( "geoid", "year" ), all.x = TRUE )
setorder( out, geoid, year )
stopifnot( !anyDuplicated( out[ , .( geoid, year ) ] ) )
fwrite( out, f_out )

cb <- data.table(
  variable = c( qv, qf, av, cogv ),
  definition = c(
    "QCEW federal government employment (annual average, place of work)",
    "QCEW state government employment", "QCEW local government employment (incl. public schools and hospitals)",
    "QCEW federal government total annual wages ($)", "QCEW state government wages ($)", "QCEW local government wages ($)",
    paste( "1 = suppressed county cell, imputed as state total minus disclosed counties, allocated by population:",
           c( "federal", "state", "local" ) ),
    "ACS civilian employed residents 16+ (B24080 universe)", "ACS residents working for private not-for-profits",
    "ACS residents working for local government", "ACS residents working for state government",
    "ACS residents working for federal government",
    setNames( c( "Local current operations, excl. utilities and liquor stores (E codes)",
                 "Local capital outlay, excl. utilities (F+G)", "Local assistance and subsidies (J)",
                 "Local interest on general debt (I89)",
                 "Local current operations, education (E12, E16, E18, E21)",
                 "Local public welfare spending (E/J 67, 68, 74, 75, 77, 79)",
                 "Local health and hospitals current operations (E32, E36, E38)",
                 "Local salaries and wages (Z00; not published for 2022)",
                 "Local intergovernmental revenue from federal government (B)",
                 "Local intergovernmental revenue from state government (C)",
                 "Local direct general expenditure = curop + capital + assist + interest" ),
              cogv )[ cogv ] ),
  source = c( rep( "BLS QCEW county annual, own 1/2/3, industry 10", 9 ), rep( "ACS 5-year B24080", 5 ),
              rep( "Census of Governments (county-area files 2002-2012; unit files 2017-2022)", length( cogv ) ) ),
  years = c( rep( "2000-2024", 9 ), rep( "2009-2024 (ACS end year)", 5 ), rep( "2002, 2007, 2012, 2017, 2022", length( cogv ) ) ),
  type = c( rep( "B", 6 ), rep( "flag", 3 ), rep( "B", 5 + length( cogv ) ) ),
  units = c( rep( c( "jobs", "dollars" ), each = 3 ), rep( "count of imputed counties", 3 ), rep( "persons", 5 ),
             rep( "dollars", length( cogv ) ) ) )
fwrite( cb, file.path( OUT$cov, "cov_gov_codebook.csv" ) )
log_msg( "wrote ", f_out, ": ", nrow( out ), " rows x ", ncol( out ), " cols" )
