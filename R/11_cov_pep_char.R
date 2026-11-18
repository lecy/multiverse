## 11_cov_pep_char.R
## Census PEP county characteristics (race/ethnicity and age), 2000-2024, on
## 2023 county-equivalents. Annual, observed (no interpolation).
##
## Sources (www2.census.gov/programs-surveys/popest/datasets/)
##   2000-2009  2000-2010/intercensal/county/co-est00int-alldata-SS.csv  (YEAR 2-11 = 7/1/2000-2009)
##   2010-2019  2010-2020/intercensal/county/asrh/cc-est2020int-alldata.csv  (YEAR 2-11 = 7/1/2010-2019)
##              + cc-est2020int-agesex-all.csv (exact 18+)
##   2020-2024  2020-2025/counties/asrh/cc-est2025-alldata.csv  (YEAR 2-6 = 7/1/2020-2024)
##              + cc-est2025-agesex-all.csv (exact 18+)
##
## Geography
##   - VA/SD/AK renames and merges via recode_county() (counts summed)
##   - Valdez-Cordova (02261, pre-2019) split into 02063 / 02066 by their 2020
##     population shares (same rule as 01_geo_spine.R)
##   - CT 2000-2019 legacy counties -> planning regions with
##     ct_legacy_to_pr( "total" ), then all counts raked so pop_total equals the
##     town-based planning-region population in data/pop (exact for totals)
##
## Under-18: exact (POPESTIMATE - AGE18PLUS_TOT) for 2010-2024. Single-year age
## is not published for 2000-2009, so under-18 there = ages 0-14 + 3/5 of
## 15-19, multiplied by each county's 2010 ratio exact/approximate (national
## ratio where a county ratio is unavailable). Flag: u18_method.
##
## Output: OUT$cov/cov_pep_char.csv  (+ cov_pep_char_codebook.csv)

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )

dir <- file.path( OUT$raw_cov, "pep_char" )
dir.create( dir, recursive = TRUE, showWarnings = FALSE )
B <- "https://www2.census.gov/programs-surveys/popest/datasets"

spine  <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
popref <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )
states <- sort( unique( spine$state_fips ) )

f00  <- vapply( states, function( s )
  fetch( sprintf( "%s/2000-2010/intercensal/county/co-est00int-alldata-%s.csv", B, s ), dir ), "" )
f10  <- fetch( paste0( B, "/2010-2020/intercensal/county/asrh/cc-est2020int-alldata.csv" ), dir )
f10a <- fetch( paste0( B, "/2010-2020/intercensal/county/asrh/cc-est2020int-agesex-all.csv" ), dir )
f20  <- fetch( paste0( B, "/2020-2025/counties/asrh/cc-est2025-alldata.csv" ), dir )
f20a <- fetch( paste0( B, "/2020-2025/counties/asrh/cc-est2025-agesex-all.csv" ), dir )

RACE <- c( n_nhwhite = "NHWA", n_nhblack = "NHBA", n_nhaian = "NHIA", n_nhasian = "NHAA",
           n_nhnhpi = "NHNA", n_nhtwo = "NHTOM", n_hisp = "H" )
AGE  <- sprintf( "ag%02d", 1:18 )
num  <- function( x ) suppressWarnings( as.numeric( x ) )

## ---- read one alldata vintage -> county x year counts --------------------

## co-est00int codes AGEGRP 99 = total, 0 = under 1, 1 = 1-4, 2-18 = 5-year
## bins; the later vintages use 0 = total, 1 = 0-4, 2-18 = 5-year bins.
read_alldata <- function( files, yrmap, agegrp00 = FALSE ) {
  rbindlist( lapply( files, function( f ) {
    need <- c( "SUMLEV", "STATE", "COUNTY", "YEAR", "AGEGRP", "TOT_POP",
               paste0( rep( RACE, each = 2 ), c( "_MALE", "_FEMALE" ) ) )
    d <- fread( f, select = need, colClasses = "character", encoding = "Latin-1", showProgress = FALSE )
    d <- d[ as.integer( SUMLEV ) == 50 & YEAR %in% names( yrmap ) ]
    d[ , year := unname( yrmap[ YEAR ] ) ]
    d[ , geoid := paste0( pad( STATE, 2 ), pad( COUNTY, 3 ) ) ]
    d[ , AGEGRP := as.integer( AGEGRP ) ]
    if ( agegrp00 ) d[ , AGEGRP := fcase( AGEGRP == 99L, 0L, AGEGRP == 0L, 1L, default = AGEGRP ) ]
    d[ , TOT_POP := num( TOT_POP ) ]
    r <- d[ AGEGRP == 0 ]
    for ( g in names( RACE ) )
      r[ , ( g ) := num( get( paste0( RACE[[ g ]], "_MALE" ) ) ) + num( get( paste0( RACE[[ g ]], "_FEMALE" ) ) ) ]
    r <- r[ , c( "geoid", "year", "TOT_POP", names( RACE ) ), with = FALSE ]
    setnames( r, "TOT_POP", "pop_total" )
    a <- dcast( d[ AGEGRP %in% 1:18, .( geoid, year, ag = sprintf( "ag%02d", AGEGRP ), TOT_POP ) ],
                geoid + year ~ ag, value.var = "TOT_POP", fun.aggregate = sum )
    merge( r, a, by = c( "geoid", "year" ) )
  } ) )
}

read_agesex <- function( f, yrmap ) {
  d <- fread( f, select = c( "SUMLEV", "STATE", "COUNTY", "YEAR", "POPESTIMATE", "AGE18PLUS_TOT" ),
              colClasses = "character", encoding = "Latin-1", showProgress = FALSE )
  d <- d[ as.integer( SUMLEV ) == 50 & YEAR %in% names( yrmap ) ]
  d[ , .( geoid = paste0( pad( STATE, 2 ), pad( COUNTY, 3 ) ), year = unname( yrmap[ YEAR ] ),
          u18_exact = num( POPESTIMATE ) - num( AGE18PLUS_TOT ) ) ]
}

ym <- function( codes, years ) setNames( years, as.character( codes ) )
log_msg( "reading 2000-2009" )
v00 <- read_alldata( f00, ym( 2:11, 2000:2009 ), agegrp00 = TRUE )
log_msg( "reading 2010-2019" )
v10 <- merge( read_alldata( f10, ym( 2:11, 2010:2019 ) ), read_agesex( f10a, ym( 2:11, 2010:2019 ) ),
              by = c( "geoid", "year" ), all.x = TRUE )
log_msg( "reading 2020-2024" )
v20 <- merge( read_alldata( f20, ym( 2:6, 2020:2024 ) ), read_agesex( f20a, ym( 2:6, 2020:2024 ) ),
              by = c( "geoid", "year" ), all.x = TRUE )
v00[ , u18_exact := NA_real_ ]

d <- rbind( v00, v10, v20 )
CNT <- c( "pop_total", names( RACE ), AGE, "u18_exact" )

## ---- geography onto 2023 county-equivalents ------------------------------

## Valdez-Cordova split (counts) by 2020 population shares of successors
vc_sh <- popref[ year == 2020 & geoid %in% c( "02063", "02066" ), .( to = geoid, sh = population / sum( population ) ) ]
vc <- d[ geoid == "02261" ]
if ( nrow( vc ) ) {
  vcs <- vc[ rep( seq_len( .N ), each = 2 ) ][ , `:=`( to = rep( vc_sh$to, nrow( vc ) ), sh = rep( vc_sh$sh, nrow( vc ) ) ) ]
  for ( v in CNT ) set( vcs, j = v, value = vcs[[ v ]] * vcs$sh )
  vcs[ , geoid := to ][ , c( "to", "sh" ) := NULL ]
  d <- rbind( d[ geoid != "02261" ], vcs )
}
d[ , geoid := recode_county( geoid ) ]
d <- d[ , lapply( .SD, function( x ) if ( all( is.na( x ) ) ) NA_real_ else sum( x, na.rm = TRUE ) ),
        by = .( geoid, year ), .SDcols = CNT ]

## CT legacy counties (2000-2019) -> planning regions, then rake to town-based totals
d <- ct_legacy_to_pr( d, CNT, "total" )
d <- d[ , lapply( .SD, sum ), by = .( geoid, year ), .SDcols = CNT ]       # safety re-aggregation
ct_rows <- which( d$geoid %in% .ct$pr & d$year <= 2019 )
if ( length( ct_rows ) ) {
  ref <- popref$population[ match( paste( d$geoid[ ct_rows ], d$year[ ct_rows ] ),
                                   paste( popref$geoid, popref$year ) ) ]
  fac <- ref / d$pop_total[ ct_rows ]
  for ( v in setdiff( CNT, "u18_exact" ) ) set( d, i = ct_rows, j = v, value = d[[ v ]][ ct_rows ] * fac )
  ## CT 2010-2019 u18_exact came through the legacy -> PR split; rake as well
  set( d, i = ct_rows, j = "u18_exact", value = d$u18_exact[ ct_rows ] * fac )
}
for ( v in CNT ) d[ u18_exact == 0 & year <= 2009, u18_exact := NA_real_ ]

d <- d[ geoid %in% spine$geoid & year %in% 2000:2024 ]

## ---- under-18: exact 2010+, calibrated approximation 2000-2009 -----------

d[ , u18_approx := ag01 + ag02 + ag03 + 0.6 * ag04 ]
cal <- d[ year == 2010 & !is.na( u18_exact ) & u18_approx > 0, .( geoid, cal = u18_exact / u18_approx ) ]
nat_cal <- d[ year == 2010, sum( u18_exact, na.rm = TRUE ) / sum( u18_approx[ !is.na( u18_exact ) ] ) ]
d <- merge( d, cal, by = "geoid", all.x = TRUE )
d[ is.na( cal ), cal := nat_cal ]
d[ , pop_under18 := fifelse( !is.na( u18_exact ), u18_exact, u18_approx * cal ) ]
d[ , u18_method := fifelse( !is.na( u18_exact ), "exact_agesex", "bins_calibrated_2010" ) ]
d[ , c( "u18_exact", "u18_approx", "cal" ) := NULL ]

## ---- derived measures ----------------------------------------------------

d[ , pop_18plus := pop_total - pop_under18 ]
d[ , pop_65plus := ag14 + ag15 + ag16 + ag17 + ag18 ]
d[ , race_den := n_nhwhite + n_nhblack + n_nhaian + n_nhasian + n_nhnhpi + n_nhtwo + n_hisp ]
d[ , race_hhi := ( n_nhwhite^2 + n_nhblack^2 + n_nhaian^2 + n_nhasian^2 + n_nhnhpi^2 +
                     n_nhtwo^2 + n_hisp^2 ) / race_den^2 ]
d[ , race_diversity := 1 - race_hhi ]
d[ , pct_nonwhite := 1 - n_nhwhite / pop_total ]
d[ , pct_hispanic := n_hisp / pop_total ]
d[ , pct_black    := n_nhblack / pop_total ]
d[ , pct_65plus   := pop_65plus / pop_total ]
d[ , pct_under18  := pop_under18 / pop_total ]

mid <- c( seq( 2.5, 82.5, by = 5 ), 87.5 )
A <- as.matrix( d[ , ..AGE ] )
age_tot <- rowSums( A )
P <- A / age_tot
mu <- as.vector( P %*% mid )
d[ , age_mean := mu ]
d[ , age_sd := sqrt( rowSums( P * outer( rep( 1, nrow( P ) ), mid )^2 ) - mu^2 ) ]
d[ , age_hhi := rowSums( P^2 ) ]

## rename age bins for readability
setnames( d, AGE, sprintf( "pop_age_%02d_%s", seq( 0, 85, 5 ), c( sprintf( "%02d", seq( 4, 84, 5 ) ), "plus" ) ) )

setcolorder( d, c( "geoid", "year", "pop_total", "pop_under18", "pop_18plus", "pop_65plus",
                   names( RACE ), "race_den" ) )
setorder( d, geoid, year )
fwrite( d, file.path( OUT$cov, "cov_pep_char.csv" ) )

## ---- codebook ------------------------------------------------------------

cb <- rbind(
  data.table( variable = c( "pop_total", "pop_under18", "pop_18plus", "pop_65plus" ),
              definition = c( "Total resident population (PEP July 1)",
                              "Population under 18 (exact 2010+; 2000-2009 from 5-year bins calibrated to county 2010 ratio; see u18_method)",
                              "Population 18+ (= pop_total - pop_under18); voting-age denominator",
                              "Population 65+ (AGEGRP 14-18)" ), type = "C" ),
  data.table( variable = names( RACE ),
              definition = paste( "Count:", c( "non-Hispanic White alone", "non-Hispanic Black alone",
                                               "non-Hispanic AIAN alone", "non-Hispanic Asian alone",
                                               "non-Hispanic NHPI alone", "non-Hispanic two or more races",
                                               "Hispanic (any race)" ) ), type = "B" ),
  data.table( variable = "race_den", definition = "Sum of the 7 race/ethnicity groups (= pop_total)", type = "B" ),
  data.table( variable = c( "race_hhi", "race_diversity", "pct_nonwhite", "pct_hispanic", "pct_black",
                            "pct_65plus", "pct_under18", "age_mean", "age_sd", "age_hhi" ),
              definition = c( "Herfindahl index of the 7 race/ethnicity shares (num: group counts; den: race_den)",
                              "1 - race_hhi",
                              "1 - n_nhwhite / pop_total",
                              "n_hisp / pop_total",
                              "n_nhblack / pop_total (non-Hispanic Black alone)",
                              "pop_65plus / pop_total",
                              "pop_under18 / pop_total",
                              "Mean age from 18 five-year bins (midpoints; 85+ = 87.5)",
                              "SD of age from 18 five-year bins (midpoints; 85+ = 87.5)",
                              "Herfindahl index over the 18 five-year age bins" ), type = "A" ),
  data.table( variable = grep( "^pop_age_", names( d ), value = TRUE ),
              definition = "Count in five-year age bin (numerators for age_sd / age_hhi)", type = "B" ),
  data.table( variable = "u18_method", definition = "exact_agesex | bins_calibrated_2010", type = "flag" ) )
cb[ , `:=`( source = "Census PEP county characteristics (co-est00int / cc-est2020int / cc-est2025)",
            years = "2000-2024" ) ]
fwrite( cb, file.path( OUT$cov, "cov_pep_char_codebook.csv" ) )

## ---- validation ----------------------------------------------------------

cat( "\ncounties per year\n" )
print( d[ , .( counties = .N, missing_race = sum( is.na( race_hhi ) ) ), by = year ] )
cat( "\nmissing spine county-years:", nrow( CJ( geoid = spine$geoid, year = 2000:2024 ) ) - nrow( d ), "\n" )

cat( "\nnational trends\n" )
print( d[ , .( pct_nonwhite = round( 1 - sum( n_nhwhite ) / sum( pop_total ), 4 ),
               pct_65plus = round( sum( pop_65plus ) / sum( pop_total ), 4 ),
               pct_under18 = round( sum( pop_under18 ) / sum( pop_total ), 4 ) ), by = year ] )

chk <- merge( d[ , .( geoid, year, pop_total ) ], popref[ , .( geoid, year, ref = population ) ],
              by = c( "geoid", "year" ) )
chk[ , pct := 100 * ( pop_total / ref - 1 ) ]
cat( "\npop_total vs data/pop: county-years beyond 0.5%:", chk[ abs( pct ) > 0.5, .N ], "of", nrow( chk ), "\n" )
print( chk[ abs( pct ) > 0.5 ][ order( -abs( pct ) ) ][ 1:min( 15, .N ) ] )
print( chk[ , .( max_abs_pct = max( abs( pct ) ) ), by = year ] )
