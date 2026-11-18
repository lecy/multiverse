## 16_cov_elections.R
## County presidential returns, 2000-2024 (election years only; observed values,
## no interpolation). Block: political culture.
##
## Sources
##   2000-2016 : MIT Election Data + Science Lab, countypres_2000-2016.csv,
##               from the lab's own GitHub repository (MEDSL/county-returns).
##               (The 2000-2024 Dataverse file, doi:10.7910/DVN/VOQCHQ, sits
##               behind a guestbook form, so it is not fetched by script.)
##   2020, 2024: tonmcg/US_County_Level_Election_Results_08-24 (GitHub),
##               county results compiled from news-organization feeds;
##               validated below against official national totals.
## Handling
##   Alaska    : all sources report state house districts, not boroughs, and
##               the district sums are unreliable. Certified statewide results
##               (total, D, R) are assigned to boroughs in proportion to
##               population (dem_margin = statewide value); ak_statewide = TRUE.
##   Totals    : 2000-2016 county total = sum of candidate votes (MEDSL's
##               totalvotes field is wrong for some rows, e.g. Bedford city 2004).
##   Kansas City MO (separate jurisdiction in MEDSL) -> Jackson/Clay/Platte by
##               2010 city population shares. DC wards (2024) -> 11001.
##   CT        : 2000-2020 legacy counties -> planning regions (vote totals
##               split by population overlap); 2024 already by planning region.
##   VA cities : reported separately and kept; Bedford city merged (2013 rule).
##   Rows without a county FIPS (statewide write-ins etc.) are dropped and
##   their votes reported.
##
## Output: D:/MULTIVERSE/data/cov/cov_elections.csv (+ codebook_elections.csv)

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
source( "R/13_16_cov_helpers.R" )

dir <- file.path( OUT$raw_cov, "elections" )
GH <- "https://raw.githubusercontent.com/"
f_medsl <- fetch_ua( paste0( GH, "MEDSL/county-returns/master/countypres_2000-2016.csv" ), dir )
f_20 <- fetch_ua( paste0( GH, "tonmcg/US_County_Level_Election_Results_08-24/master/2020_US_County_Level_Presidential_Results.csv" ),
                  dir, "tonmcg_2020.csv" )
f_24 <- fetch_ua( paste0( GH, "tonmcg/US_County_Level_Election_Results_08-24/master/2024_US_County_Level_Presidential_Results.csv" ),
                  dir, "tonmcg_2024.csv" )

## ---- 2000-2016 (MEDSL) ----------------------------------------------------
m <- fread( f_medsl, colClasses = c( FIPS = "character" ), na.strings = c( "NA", "" ) )
m <- m[ office == "President" ]
nofips <- m[ is.na( FIPS ), .( votes = sum( candidatevotes, na.rm = TRUE ) ), by = .( year, state_po ) ]
m <- m[ !is.na( FIPS ) ]
m[ , geoid := pad( FIPS, 5 ) ]
## county total = sum of candidate votes (incl. "Other"). MEDSL's totalvotes
## field is wrong in places (e.g. Bedford city VA 2004 carries the county
## total; Alaska 2004 district totals sum to 488.7k vs 312.6k official).
e1 <- m[ , .( votes_cand_sum = sum( candidatevotes, na.rm = TRUE ),
              votes_field    = max( totalvotes, na.rm = TRUE ),
              votes_dem   = sum( candidatevotes[ party %in% "democrat" ], na.rm = TRUE ),
              votes_rep   = sum( candidatevotes[ party %in% "republican" ], na.rm = TRUE ),
              state = first( state_po ) ),
         by = .( geoid, year ) ]
## Rows with no candidate votes carry no data. Falling back to the totalvotes
## field there double-counted Bedford County VA in 2016: the empty Bedford city
## row (51515) carries the county's total, 42,525, and is merged into 51019 below
## (found by 16b_verify_elections.R). Drop such rows instead.
cat( "MEDSL county-years with no candidate votes (dropped):\n" ); print( e1[ votes_cand_sum == 0, .( geoid, year, votes_field ) ] )
e1 <- e1[ votes_cand_sum > 0 ]
e1[ , votes_total := votes_cand_sum ]
cat( "MEDSL county-years where totalvotes field differs from candidate sum by >1%:",
     e1[ votes_cand_sum > 0 & abs( votes_field / votes_cand_sum - 1 ) > 0.01, .N ], "\n" )
e1[ , votes_field := NULL ]

## ---- 2020, 2024 (tonmcg) ------------------------------------------------------
rd_t <- function( f, y ) {
  t <- fread( f, colClasses = c( county_fips = "character" ) )
  t[ , .( geoid = pad( county_fips, 5 ), year = y, votes_total = as.numeric( total_votes ),
          votes_dem = as.numeric( votes_dem ), votes_rep = as.numeric( votes_gop ),
          votes_cand_sum = NA_real_, state = state_name ) ]
}
e2 <- rbind( rd_t( f_20, 2020L ), rd_t( f_24, 2024L ) )

e <- rbind( e1, e2 )

## Kansas City, MO is a separate reporting jurisdiction in MEDSL 2000-2016
## (code 36000; county rows exclude it). Split its votes across Jackson,
## Clay and Platte counties by the 2010 census population of the city's parts
## (Jackson 315,801; Clay 105,386; Platte 38,600; Cass portion negligible).
kc_share <- data.table( geoid = c( "29095", "29047", "29165" ),
                        share = c( 315801, 105386, 38600 ) / ( 315801 + 105386 + 38600 ) )
kc <- e[ geoid == "36000" ]
if ( nrow( kc ) ) {
  kc2 <- kc[ , .( geoid = kc_share$geoid, share = kc_share$share ), by = .( year, votes_total, votes_dem, votes_rep ) ]
  kc2 <- kc2[ , .( geoid, year, votes_total = votes_total * share, votes_dem = votes_dem * share,
                   votes_rep = votes_rep * share, votes_cand_sum = NA_real_, state = "MO" ) ]
  e <- rbind( e[ geoid != "36000" ], kc2 )
  e <- e[ , .( votes_total = sum( votes_total ), votes_dem = sum( votes_dem ), votes_rep = sum( votes_rep ),
               votes_cand_sum = sum( votes_cand_sum ), state = first( state ) ), by = .( geoid, year ) ]
}
## DC: 2024 file reports wards (11001-11008) -> one county-equivalent 11001
e[ substr( geoid, 1, 2 ) == "11", geoid := "11001" ]
e <- e[ , .( votes_total = sum( votes_total ), votes_dem = sum( votes_dem ), votes_rep = sum( votes_rep ),
             votes_cand_sum = sum( votes_cand_sum ), state = first( state ) ), by = .( geoid, year ) ]

## ---- Alaska: statewide -> boroughs by population ----------------------------
ak <- e[ substr( geoid, 1, 2 ) == "02", .( votes_total = sum( votes_total ), votes_dem = sum( votes_dem ),
                                           votes_rep = sum( votes_rep ) ), by = year ]
e <- e[ substr( geoid, 1, 2 ) != "02" ]
## Alaska district sums are unreliable (MEDSL 2004 +56%; compiled 2020 +8.9%,
## 2024 -4.2% vs certified). Use certified statewide results (Alaska Division
## of Elections official returns) for every year.
ak_official <- data.table( year = c( 2000, 2004, 2008, 2012, 2016, 2020, 2024 ),
                           votes_total = c( 285560, 312598, 326197, 300495, 318608, 359530, 338177 ),
                           votes_dem   = c(  79004, 111025, 123594, 122640, 116454, 153778, 140026 ),
                           votes_rep   = c( 167398, 190889, 193841, 164676, 163387, 189951, 184458 ) )
print( merge( ak, ak_official, by = "year", suffixes = c( "_districts", "_official" ) )[
  , .( year, votes_total_districts, votes_total_official,
       pct_gap = round( 100 * ( votes_total_districts / votes_total_official - 1 ), 2 ) ) ] )
ak <- ak_official
pop <-fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )
akp <- pop[ substr( geoid, 1, 2 ) == "02" & year %in% ak$year ]
akp[ , share := population / sum( population ), by = year ]
ak_b <- merge( akp[ , .( geoid, year, share ) ], ak, by = "year" )
ak_b <- ak_b[ , .( geoid, year, votes_total = votes_total * share, votes_dem = votes_dem * share,
                   votes_rep = votes_rep * share, votes_cand_sum = NA_real_ ) ]

e <- harmonize_counties( e[ , .( geoid, year, votes_total, votes_dem, votes_rep ) ],
                         counts = c( "votes_total", "votes_dem", "votes_rep" ) )
e <- rbind( e, ak_b[ , .( geoid, year, votes_total, votes_dem, votes_rep ) ] )
e[ , ak_statewide := substr( geoid, 1, 2 ) == "02" ]
e[ , `:=`( dem_share = votes_dem / votes_total, rep_share = votes_rep / votes_total,
           dem_margin = ( votes_dem - votes_rep ) / votes_total ) ]
e[ , competitiveness := 1 - abs( dem_margin ) ]
setorder( e, geoid, year )

validate_cov( e, "County presidential returns" )
official <- data.table( year = c( 2000, 2004, 2008, 2012, 2016, 2020, 2024 ),
                        official_total_m = c( 105.4, 122.3, 131.3, 129.1, 136.7, 158.4, 155.5 ) )
chk <- e[ , .( total_m = round( sum( votes_total ) / 1e6, 2 ), dem_m = round( sum( votes_dem ) / 1e6, 2 ),
               rep_m = round( sum( votes_rep ) / 1e6, 2 ),
               natl_dem_margin = round( 100 * ( sum( votes_dem ) - sum( votes_rep ) ) / sum( votes_total ), 2 ) ),
          keyby = year ]
print( merge( chk, official, by = "year" ) )
cat( "votes on rows without county FIPS (dropped):\n" ); print( nofips[ votes > 0 ] )
cat( "county rows where dem+rep > total:", e[ votes_dem + votes_rep > votes_total * 1.0001, .N ], "\n" )

fwrite( e, file.path( OUT$cov, "cov_elections.csv" ) )
fwrite( data.table(
  variable = c( "votes_total", "votes_dem", "votes_rep", "dem_share", "rep_share", "dem_margin",
                "competitiveness", "ak_statewide" ),
  definition = c( "Total presidential votes cast (all candidates)", "Democratic presidential votes",
                  "Republican presidential votes", "votes_dem / votes_total", "votes_rep / votes_total",
                  "(votes_dem - votes_rep) / votes_total", "1 - |dem_margin|",
                  "TRUE = Alaska borough values allocated from statewide totals by population" ),
  source = c( rep( "MIT Election Lab countypres_2000-2016 (GitHub MEDSL/county-returns); 2020/2024 tonmcg county results", 7 ),
              "derived" ),
  years = "2000, 2004, 2008, 2012, 2016, 2020, 2024",
  type = c( "B", "B", "B", "A", "A", "A", "A", "flag" ) ),
  file.path( OUT$cov, "codebook_elections.csv" ) )
log_msg( "wrote cov_elections.csv: ", nrow( e ), " rows" )
