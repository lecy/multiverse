## 26_cov_lag_kernel.R
## Population-lag kernels for the "lag" block of 40_multiverse.R (moved from
## "82_lag_fork.R build" in Phase 2 of the reproducibility plan). The kernel of past
## log population change is G[t] = a * G[t-1] + dlogP[t], a = 0.5^(1/half-life):
##   lag_g5, lag_g10      5- and 10-year half-lives (used by the multiverse)
##   lag_p25g, lag_p25h   2.5-year G and its second-order H (dropped alternative)
## Population back to 1970: decennial census county counts (1970, 1980, 1990;
## raw/census-hist/, as in 73_pre2000_growth.R) mapped to 2023 counties, summed to
## CBSAs and interpolated log-linearly to the CBSA's 2000 population.
##   - partial county coverage: the covered counties' growth is applied to the CBSA;
##   - no coverage (Connecticut: 2023 planning regions do not nest in old counties):
##     the state's growth is applied.
## 2000-2024 CBSA population is summed from the county file here, exactly as
## 30_assemble_analysis.R sums it, so this script no longer reads the panel that
## 30 builds (82 read it through load_panel()).
##
## Inputs:  raw/census-hist/{e7079co,e8089co,99c8_00}.txt, data/geo/county_spine.csv,
##          data/pop/county_pop_2000_2025.csv, data/npo/npo_county_year.csv (06)
## Outputs: data/cov/cov_lag_kernel.csv (cbsa x year 2000-2024; merged by 30),
##          data/cov/cov_lag_kernel_coverage.csv

source( "R/lib/00_setup.R" )
COV_FILE <- file.path( OUT$cov, "cov_lag_kernel.csv" )
HIST <- file.path( BUILD, "raw", "census-hist" )
LAG_VARS <- c( "lag_g5", "lag_g10" )

## decennial county counts (parsers as in 73_pre2000_growth.R)
read_old <- function( f ) {
  L <- readLines( file.path( HIST, f ), warn = FALSE )
  i <- grep( "^[0-9]{5} ", L ); out <- list()
  for ( k in i ) {
    fips <- substr( L[ k ], 1, 5 ); if ( !is.null( out[[ fips ]] ) || grepl( "000$", fips ) ) next
    tk <- strsplit( trimws( substring( L[ k ], 7 ) ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ]
    if ( !length( nums ) ) { tk <- strsplit( trimws( L[ k + 1 ] ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ] }
    if ( length( nums ) ) out[[ fips ]] <- as.numeric( nums[ 1 ] )
  }
  data.table( fips = names( out ), pop = unlist( out ) ) }
p70 <- read_old( "e7079co.txt" ); p80 <- read_old( "e8089co.txt" )
L <- readLines( file.path( HIST, "99c8_00.txt" ), warn = FALSE ); L <- L[ grepl( "^1 +[0-9]{5} ", L ) ]
p90 <- rbindlist( lapply( L, function( x ) { tk <- strsplit( trimws( x ), " +" )[[ 1 ]]
  nums <- tk[ grepl( "^[0-9,]+$", tk ) ][ -1 ]
  data.table( fips = tk[ 2 ], pop = as.numeric( gsub( ",", "", nums[ length( nums ) ] ) ) ) } ) )
RECODE <- c( "12025" = "12086", "51780" = "51083", "51560" = "51005", "51515" = "51019",
             "51123" = "51800", "51695" = "51800", "30113" = "30067" )
hist <- rbindlist( list( p70[ , year := 1970 ], p80[ , year := 1980 ], p90[ , year := 1990 ] ) )
hist[ fips %in% names( RECODE ), fips := RECODE[ fips ] ]
hist <- hist[ , .( pop = sum( pop ) ), by = .( fips, year ) ]
log_msg( "counties: 1970 ", hist[ year == 1970, .N ], ", 1980 ", hist[ year == 1980, .N ], ", 1990 ", hist[ year == 1990, .N ] )

sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "state_fips" ) ) )
sp <- sp[ cbsa_type %in% c( "metro", "micro" ) & !is.na( cbsa ) & cbsa != "" ]
cp <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )[ year == 2000 ]
sp <- merge( sp, cp[ , .( geoid, pop2000 = population ) ], by = "geoid", all.x = TRUE )
## state growth ratios (historical state total / 2000 state total) for CBSAs with no matched counties
st <- rbindlist( lapply( c( 1970, 1980, 1990 ), function( y )
  hist[ year == y, .( year = y, hp = sum( pop ) ), by = .( state_fips = substr( fips, 1, 2 ) ) ] ) )
st <- merge( st, cp[ , .( p2000 = sum( population ) ), by = .( state_fips = substr( geoid, 1, 2 ) ) ], by = "state_fips" )
st[ , ratio_state := hp / p2000 ]
## CBSA ratio of historical to 2000 population from the covered counties
rt <- rbindlist( lapply( c( 1970, 1980, 1990 ), function( y ) { h <- hist[ year == y ]
  sp[ , { f <- geoid %in% h$fips
    .( year = y, coverage = sum( pop2000[ f ], na.rm = TRUE ) / sum( pop2000, na.rm = TRUE ),
       ratio_cty = if ( any( f ) ) sum( h$pop[ match( geoid[ f ], h$fips ) ] ) / sum( pop2000[ f ], na.rm = TRUE ) else NA_real_,
       main_state = names( which.max( tapply( pop2000, state_fips, sum, na.rm = TRUE ) ) ) ) },
     by = .( cbsa, cbsa_title, cbsa_type ) ] } ) )
rt <- merge( rt, st[ , .( main_state = state_fips, year, ratio_state ) ], by = c( "main_state", "year" ), all.x = TRUE )
rt[ , source := fifelse( coverage >= 0.98, "counties", fifelse( coverage > 0, "partial counties", "state" ) ) ]
rt[ , ratio := fifelse( is.na( ratio_cty ) | coverage == 0, ratio_state, ratio_cty ) ]
cat( "history source by CBSA-year:\n" ); print( rt[ , .N, keyby = .( cbsa_type, source ) ] )
print( rt[ source != "counties", .( cbsa_title, year, coverage = round( coverage, 3 ), source ) ][ order( cbsa_title, year ) ], nrows = 60 )

## 2000-2024 CBSA population, summed from the counties as in 30_assemble_analysis.R
npo <- fread( file.path( OUT$npo, "npo_county_year.csv" ), select = c( "geoid", "year", "population" ),
              colClasses = list( character = "geoid" ) )
spc <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
npo <- merge( npo, spc[ , .( geoid, cbsa ) ], by = "geoid", all.x = TRUE )[ !is.na( cbsa ) & cbsa != "" & year %in% YEARS ]
panel <- npo[ , .( logP = log( if ( all( is.na( population ) ) ) NA_real_ else as.numeric( sum( population, na.rm = TRUE ) ) ) ),
              by = .( cbsa, year ) ]

## annual log population 1970-2024 anchored to the CBSA's 2000 value
a00 <- panel[ year == 2000, .( cbsa, l2000 = logP ) ]
w <- merge( dcast( rt, cbsa ~ year, value.var = "ratio" ), a00, by = "cbsa" )
setnames( w, c( "1970", "1980", "1990" ), c( "r70", "r80", "r90" ) )
pre <- w[ , .( year = 1970:1999, logP = approx( c( 1970, 1980, 1990, 2000 ), c( l2000 + log( c( r70, r80, r90 ) ), l2000 ), xout = 1970:1999 )$y ), by = cbsa ]
full <- rbind( pre, panel[ cbsa %in% w$cbsa ] ); setorder( full, cbsa, year )
rec <- function( x, g, a ) ave( x, g, FUN = function( z ) as.numeric( stats::filter( z, a, method = "recursive" ) ) )
a_of <- function( h ) 0.5^( 1 / h )
full[ , dp := c( 0, diff( logP ) ), by = cbsa ]
full[ , `:=`( lag_g5 = rec( dp, cbsa, a_of( 5 ) ), lag_g10 = rec( dp, cbsa, a_of( 10 ) ), lag_p25g = rec( dp, cbsa, a_of( 2.5 ) ) ) ]
full[ , lag_p25h := rec( lag_p25g, cbsa, a_of( 2.5 ) ) ]
out <- full[ year >= 2000, .( cbsa, year, lag_g5, lag_g10, lag_p25g, lag_p25h ) ]
out <- merge( out, unique( rt[ , .( cbsa, hist_source = fifelse( all( source == "counties" ), "counties",
                                       fifelse( any( source == "state" ), "state", "partial counties" ) ) ), by = cbsa ][ , .( cbsa, hist_source ) ] ),
              by = "cbsa" )
fwrite( out, COV_FILE ); fwrite( rt, file.path( OUT$cov, "cov_lag_kernel_coverage.csv" ) )
log_msg( "wrote ", COV_FILE, ": ", uniqueN( out$cbsa ), " CBSAs x ", uniqueN( out$year ), " years" )
print( out[ year %in% c( 2010, 2022 ), lapply( .SD, function( z ) round( c( mean = mean( z ), sd = sd( z ), min = min( z ), max = max( z ) ), 4 ) ),
            .SDcols = LAG_VARS, by = year ] )
