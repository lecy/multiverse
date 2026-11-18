## 01_geo_spine.R
## County-equivalent spine (2023 geography), annual county population
## 2000-2025 from Census intercensal / postcensal estimates, and the
## county -> CBSA 2023 crosswalk.
##
## Why rebuild population: the earlier county_population_2000_2025.csv holds
## only the 2000/2010/2020 census counts with straight-line interpolation
## between them. Interpolated population manufactures smooth growth and would
## contaminate the temporal scaling exponent in within-metro designs.
##
## Geography: 2023 county-equivalents = 2020 counties, except Connecticut,
## which uses the 9 planning regions (09110-09190) that the 2023 CBSA
## delineation is built on. CT is rebuilt from town (MCD) estimates, which
## nest exactly in both legacy counties and planning regions.
##
## Outputs
##   data/geo/county_spine.csv          one row per county-equivalent
##   data/pop/county_pop_2000_2025.csv  geoid x year population + source
##   data/pop/cbsa_pop_2000_2025.csv    CBSA (2023) x year population
##   data/diagnostics/pop_vs_interpolated.csv

source( "R/lib/00_setup.R" )

CB <- "https://www2.census.gov/programs-surveys/popest/datasets"
f_co00  <- fetch( paste0( CB, "/2000-2010/intercensal/county/co-est00int-tot.csv" ), RAW$census_popest )
f_sub00 <- fetch( paste0( CB, "/2000-2010/intercensal/cities/sub-est00int.csv" ), RAW$census_popest )
f_sub10 <- fetch( paste0( CB, "/2010-2020/intercensal/cities/sub-est2020int.csv" ), RAW$census_popest )
f_co20  <- fetch( paste0( CB, "/2020-2025/counties/totals/co-est2025-alldata.csv" ), RAW$census_popest )
f_ctxw  <- fetch( "https://www2.census.gov/geo/docs/reference/ct_change/ct_cou_to_cousub_crosswalk.txt",
                  RAW$census_popest )

rd <- function( f ) fread( f, encoding = "Latin-1", colClasses = "character" )

## wide POPESTIMATEyyyy columns -> long
melt_pop <- function( d, id, years ) {
  cols <- paste0( "POPESTIMATE", years )
  m <- melt( d[ , c( id, cols ), with = FALSE ], id.vars = id,
             variable.name = "year", value.name = "population" )
  m[ , year := as.integer( sub( "POPESTIMATE", "", year ) ) ]
  m[ , population := as.numeric( population ) ]
  m[]
}

## ---- 1. counties outside CT -------------------------------------------

co00 <- rd( f_co00 )[ as.integer( SUMLEV ) == 50 ]
co00[ , geoid := paste0( pad( STATE, 2 ), pad( COUNTY, 3 ) ) ]
p00 <- melt_pop( co00, "geoid", 2000:2009 )[ , src := "PEP intercensal 2000-2010" ]

sub10 <- rd( f_sub10 )
co10 <- sub10[ as.integer( SUMLEV ) == 50 ]
co10[ , geoid := paste0( pad( STATE, 2 ), pad( COUNTY, 3 ) ) ]
p10 <- melt_pop( co10, "geoid", 2010:2019 )[ , src := "PEP intercensal 2010-2020" ]

co20 <- rd( f_co20 )[ as.integer( SUMLEV ) == 50 ]
co20[ , geoid := paste0( pad( STATE, 2 ), pad( COUNTY, 3 ) ) ]
p20 <- melt_pop( co20, "geoid", 2020:2025 )[ , src := "PEP Vintage 2025" ]

pop <- rbind( p00, p10, p20 )
pop <- pop[ substr( geoid, 1, 2 ) != "09" ]           # CT rebuilt below

## renames / merges onto 2023 county-equivalents
pop <- merge( pop, COUNTY_RECODE[ , .( geoid = from, to ) ], by = "geoid", all.x = TRUE )
pop[ !is.na( to ), geoid := to ][ , to := NULL ]

## Valdez-Cordova split by 2020 population shares of its successors
vc_share <- p20[ year == 2020 & geoid %in% c( "02063", "02066" ),
                 .( geoid, share = population / sum( population ) ) ]
vc <- pop[ geoid == "02261" ]
pop <- pop[ geoid != "02261" ]
if ( nrow( vc ) ) {
  vc_split <- vc[ , .( geoid = vc_share$geoid,
                       population = population * vc_share$share ),
                  by = .( year, src ) ]
  pop <- rbind( pop, vc_split[ , .( geoid, year, population,
                                    src = paste( src, "(Valdez-Cordova split)" ) ) ] )
}
pop <- pop[ , .( population = sum( population ), src = first( src ) ), by = .( geoid, year ) ]

## ---- 2. Connecticut planning regions from towns ------------------------

ctxw <- fread( f_ctxw, sep = "|", colClasses = "character", encoding = "UTF-8" )
setnames( ctxw, gsub( "\\s*\\(.*$", "", gsub( "\n", " ", names( ctxw ) ) ) )
setnames( ctxw, 1, "STATEFP" )   # first column name carries a BOM
ctxw[ , town := substr( OLD_COUSUB_GEOID, 3, 10 ) ]    # old county(3) + cousub(5)
ctxw[ , pr := paste0( "09", NEW_COUNTYFP ) ]

ct_towns <- function( d, years, src ) {
  t <- d[ pad( STATE, 2 ) == "09" & as.integer( SUMLEV ) == 61 ]
  t[ , town := paste0( pad( COUNTY, 3 ), pad( COUSUB, 5 ) ) ]
  t <- unique( t, by = "town" )
  m <- melt_pop( t, "town", years )
  m <- merge( m, ctxw[ , .( town, pr ) ], by = "town", all.x = TRUE )
  if ( m[ is.na( pr ), .N ] ) warning( "CT towns without a planning region: ",
                                       m[ is.na( pr ), uniqueN( town ) ] )
  m[ !is.na( pr ), .( population = sum( population ), src = src ),
     by = .( geoid = pr, year ) ]
}

ct <- rbind(
  ct_towns( rd( f_sub00 ), 2000:2009, "PEP intercensal 2000-2010 (CT towns)" ),
  ct_towns( sub10,         2010:2019, "PEP intercensal 2010-2020 (CT towns)" ),
  p20[ substr( geoid, 1, 2 ) == "09" ] )

## check: towns sum to the legacy state total in every year
ct_chk <- merge( ct[ , .( towns = sum( population ) ), by = year ],
                 rbind( p00, p10 )[ substr( geoid, 1, 2 ) == "09",
                                    .( counties = sum( population ) ), by = year ],
                 by = "year" )
ct_chk[ , gap := towns / counties - 1 ]
stopifnot( ct_chk[ , max( abs( gap ) ) ] < 0.001 )

pop <- rbind( pop, ct )
setorder( pop, geoid, year )

## CT lookup for placing organizations: town name -> planning region, and
## each legacy county's dominant planning region (by 2019 town population)
t19 <- sub10[ pad( STATE, 2 ) == "09" & as.integer( SUMLEV ) == 61 ]
t19[ , town := paste0( pad( COUNTY, 3 ), pad( COUSUB, 5 ) ) ]
t19 <- unique( t19, by = "town" )[ , .( town, pop2019 = as.numeric( POPESTIMATE2019 ) ) ]
ct_lookup <- merge( ctxw[ , .( town, legacy_county = paste0( "09", OLD_COUNTYFP ), pr,
                               town_name = toupper( trimws( sub( " (town|city|borough)$", "",
                                                                 COUSUB_NAMELSAD ) ) ) ) ],
                    t19, by = "town", all.x = TRUE )
ct_lookup[ , dominant_pr := pr[ which.max( pop2019 ) ], by = legacy_county ]
fwrite( ct_lookup, file.path( OUT$geo, "ct_town_lookup.csv" ) )

## ---- 3. spine and CBSA crosswalk ---------------------------------------

xw <- fread( RAW$cbsa_xw, colClasses = "character" )
xw[ , geoid := paste0( fipsstatecode, fipscountycode ) ]
xw <- xw[ , .( geoid,
               cbsa       = cbsacode,
               cbsa_title = cbsatitle,
               cbsa_type  = fifelse( grepl( "^Metro", metropolitanmicropolitanstatis ),
                                     "metro", "micro" ),
               central    = centraloutlyingcounty == "Central",
               csa        = csacode,
               csa_title  = csatitle,
               metdiv     = metropolitandivisioncode ) ]

names20 <- co20[ , .( geoid, county_name = CTYNAME, state_name = STNAME ) ]
spine <- merge( names20, xw, by = "geoid", all.x = TRUE )
spine[ , state_fips := substr( geoid, 1, 2 ) ]
spine[ is.na( cbsa ), cbsa_type := "nonmetro" ]

## every population county is in the spine and vice versa
miss_pop   <- setdiff( spine$geoid, pop$geoid )
miss_spine <- setdiff( pop$geoid, spine$geoid )
if ( length( miss_pop ) )   warning( "spine counties with no population: ", paste( miss_pop, collapse = " " ) )
if ( length( miss_spine ) ) warning( "population counties not in spine: ", paste( miss_spine, collapse = " " ) )

## complete panel check
cnt <- pop[ , .N, by = geoid ]
if ( cnt[ N != length( POP_YEARS ), .N ] )
  warning( "counties without a full 2000-2025 series: ",
           paste( cnt[ N != length( POP_YEARS ), geoid ], collapse = " " ) )

fwrite( spine, file.path( OUT$geo, "county_spine.csv" ) )
fwrite( pop,   file.path( OUT$pop, "county_pop_2000_2025.csv" ) )

cbsa_pop <- merge( pop, spine[ !is.na( cbsa ), .( geoid, cbsa, cbsa_title, cbsa_type ) ],
                   by = "geoid" )[
  , .( population = sum( population ), n_counties = .N ),
  by = .( cbsa, cbsa_title, cbsa_type, year ) ]
setorder( cbsa_pop, cbsa, year )
fwrite( cbsa_pop, file.path( OUT$pop, "cbsa_pop_2000_2025.csv" ) )

## ---- 4. compare against the interpolated file ---------------------------

old <- fread( file.path( DATA_HUB, "population", "cbsa_population_2000_2025.csv" ),
              colClasses = list( character = "cbsacode" ) )
cmp <- merge( cbsa_pop[ , .( cbsa, year, pop_new = population ) ],
              old[ , .( cbsa = cbsacode, year, pop_old = population ) ],
              by = c( "cbsa", "year" ), all = TRUE )
cmp[ , pct_diff := 100 * ( pop_old / pop_new - 1 ) ]
fwrite( cmp, file.path( OUT$diag, "pop_vs_interpolated.csv" ) )

log_msg( "counties: ", uniqueN( pop$geoid ), "  years: ", paste( range( pop$year ), collapse = "-" ) )
log_msg( "CBSAs: ", uniqueN( cbsa_pop$cbsa ), " (",
         cbsa_pop[ cbsa_type == "metro", uniqueN( cbsa ) ], " metro)" )
print( cmp[ , .( mean_abs_pct_diff = mean( abs( pct_diff ), na.rm = TRUE ),
                 max_abs_pct_diff  = max( abs( pct_diff ), na.rm = TRUE ),
                 cbsas_missing_old = sum( is.na( pop_old ) ) ), by = year ] )
