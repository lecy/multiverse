## 17_cov_acs.R
## Education (% BA+, age 25+), foreign-born share, and income Gini by county:
##   Census 2000 SF3 (year 2000) and ACS 5-year (end years 2009-2024).
## Years between 2000 and 2009 are left missing (interpolation is a fork,
## handled centrally). ACS 5-year estimates are stamped with their END year;
## the midpoint (end - 2) is stored for the timing fork.
##
## Gini 2000: SF3 has no Gini table; it is computed from the 16 household
## income brackets (P052) by trapezoid integration of the grouped Lorenz curve
## (bracket midpoints; top bracket = $250k). Flagged gini_method = "grouped".
##
## CT: ACS 2022+ is on planning regions; earlier years and 2000 are on legacy
## counties and are moved with ct_legacy_to_pr (counts as totals; Gini as a
## population-weighted rate).
##
## Output: D:/MULTIVERSE/data/cov/cov_acs.csv (+ codebook)

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )
suppressPackageStartupMessages( library( tidycensus ) )

KEY <- census_key()                                  # R/lib/config.R
Sys.setenv( CENSUS_API_KEY = KEY )
options( tigris_use_cache = FALSE )
cache <- file.path( OUT$raw_cov, "acs" ); dir.create( cache, recursive = TRUE, showWarnings = FALSE )

## ---- ACS 5-year ----------------------------------------------------------

## B15002: sex by educational attainment, 25+ (stable 2009-2024)
ba_m <- sprintf( "B15002_%03d", 15:18 ); ba_f <- sprintf( "B15002_%03d", 32:35 )
acs_vars <- c( edu_den = "B15002_001", setNames( c( ba_m, ba_f ), paste0( "ba_", 1:8 ) ),
               fb_den = "B05002_001", fb_num = "B05002_013", gini = "B19083_001" )

get_year <- function( y ) {
  f <- file.path( cache, sprintf( "acs5_%d.csv", y ) )
  if ( file.exists( f ) ) return( fread( f, colClasses = list( character = "GEOID" ) ) )
  d <- as.data.table( get_acs( geography = "county", variables = acs_vars, year = y,
                               survey = "acs5", output = "wide" ) )
  d[ , year := y ]
  fwrite( d, f )
  d
}
acs <- rbindlist( lapply( 2009:2024, function( y ) {
  r <- tryCatch( get_year( y ), error = function( e ) { message( "ACS ", y, ": ", conditionMessage( e ) ); NULL } )
  if ( !is.null( r ) ) log_msg( "ACS 5-year ", y, ": ", nrow( r ), " counties" )
  r
} ), fill = TRUE )

acs <- acs[ , .( geoid = GEOID, year,
                 edu_den = edu_denE,
                 edu_ba_num = rowSums( .SD ),
                 fb_num = fb_numE, fb_den = fb_denE, gini = giniE ),
            .SDcols = paste0( "ba_", 1:8, "E" ) ]
acs[ , gini_method := "acs" ]

## ---- 2000 SF3 --------------------------------------------------------------

f00 <- file.path( cache, "sf3_2000.csv" )
if ( !file.exists( f00 ) ) {
  sf3_vars <- c( edu_den = "P037001",
                 setNames( sprintf( "P0370%02d", c( 15:18, 32:35 ) ), paste0( "ba_", 1:8 ) ),
                 fb_den = "P021001", fb_num = "P021013",
                 setNames( sprintf( "P0520%02d", 1:17 ), paste0( "hh_", 0:16 ) ) )
  s <- as.data.table( get_decennial( geography = "county", variables = sf3_vars, year = 2000,
                                     sumfile = "sf3", output = "wide" ) )
  fwrite( s, f00 )
}
s <- fread( f00, colClasses = list( character = "GEOID" ) )

## grouped-data Gini from household income brackets
lo  <- c( 0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75, 100, 125, 150, 200 ) * 1000
hi  <- c( lo[ -1 ], 300000 )
mid <- ( lo + hi ) / 2; mid[ 16 ] <- 250000
gini_grouped <- function( n ) {
  if ( sum( n ) <= 0 ) return( NA_real_ )
  p <- n / sum( n ); inc <- p * mid; L <- cumsum( inc ) / sum( inc ); F <- cumsum( p )
  Lp <- c( 0, L[ -length( L ) ] )
  1 - sum( p * ( L + Lp ) )
}
hh <- as.matrix( s[ , paste0( "hh_", 1:16 ), with = FALSE ] )
s00 <- s[ , .( geoid = GEOID, year = 2000L, edu_den,
               edu_ba_num = rowSums( .SD ), fb_num, fb_den ),
          .SDcols = paste0( "ba_", 1:8 ) ]
s00[ , gini := apply( hh, 1, gini_grouped ) ][ , gini_method := "grouped" ]

d <- rbind( s00, acs, fill = TRUE )
d[ , geoid := pad( geoid, 5 ) ]
d <- d[ substr( geoid, 1, 2 ) != "72" ]

## ---- geography fixes --------------------------------------------------------

cnt <- c( "edu_den", "edu_ba_num", "fb_num", "fb_den" )
d[ , geoid := recode_county( geoid ) ]
d <- d[ , c( lapply( .SD, sum, na.rm = TRUE ),
             .( gini = weighted.mean( gini, edu_den, na.rm = TRUE ), gini_method = first( gini_method ) ) ),
        by = .( geoid, year ), .SDcols = cnt ]
g_ct <- ct_legacy_to_pr( d[ , .( geoid, year, gini ) ], "gini", "rate" )
d <- ct_legacy_to_pr( d[ , c( "geoid", "year", cnt, "gini_method" ), with = FALSE ], cnt, "total" )
d[ g_ct, on = .( geoid, year ), gini := i.gini ]
d[ is.na( gini_method ), gini_method := fifelse( year == 2000, "grouped", "acs" ) ]

d[ , pct_ba := edu_ba_num / edu_den ]
d[ , pct_foreign_born := fb_num / fb_den ]
d[ , acs_mid_year := fifelse( year == 2000, 2000L, as.integer( year ) - 2L ) ]

spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
d <- d[ geoid %in% spine$geoid ]
setorder( d, geoid, year )
fwrite( d, file.path( OUT$cov, "cov_acs.csv" ) )

cb <- data.table(
  variable = c( "edu_den", "edu_ba_num", "pct_ba", "fb_num", "fb_den", "pct_foreign_born",
                "gini", "gini_method", "acs_mid_year" ),
  definition = c( "Population 25+", "Population 25+ with BA or higher", "Share 25+ with BA+",
                  "Foreign-born population", "Total population (place of birth universe)",
                  "Foreign-born share", "Household income Gini index",
                  "acs = ACS B19083; grouped = computed from SF3 P052 brackets",
                  "Midpoint year of the ACS 5-year window" ),
  source = c( rep( "ACS5 B15002 / SF3 P037", 3 ), rep( "ACS5 B05002 / SF3 P021", 3 ),
              "ACS5 B19083 / SF3 P052", "", "" ),
  years = "2000, 2009-2024", type = c( "C", "B", "A", "B", "C", "A", "A", "", "" ) )
fwrite( cb, file.path( OUT$cov, "cov_acs_codebook.csv" ) )

print( d[ , .( counties = .N, pct_ba = round( sum( edu_ba_num ) / sum( edu_den ), 3 ),
               pct_fb = round( sum( fb_num ) / sum( fb_den ), 3 ),
               gini_med = round( median( gini, na.rm = TRUE ), 3 ) ), keyby = year ] )
