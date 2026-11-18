## 27_cov_acs_metro.R
## Metro-level ACS measures from summed county brackets (moved from
## "89_need_socap_diversity.R build" in Phase 2 of the reproducibility plan).
## The panel's gini and med_hh_income are population-weighted means of county
## values, so they miss inequality between the counties of a metro. This script
## sums ACS county income brackets to the CBSA and computes metro-level inequality
## directly, plus need and attachment rates:
##   gini_m      grouped Gini of the metro household income distribution (B19001; brackets at
##               midpoints, top bracket mean from a Pareto tail fitted to 150k+/200k+)
##   inc_hhi5    HHI of the metro's household shares in the five NATIONAL income quintiles
##               (B19080 US cutoffs; 0.2 = the national mix, 1 = everyone in one quintile).
##               econ_div = (1 - inc_hhi5) / 0.8 (1 = national mix)
##   deep_pov    share of people below 50% of poverty (C17002); near_pov share below 200%
##   snap_rate   households receiving SNAP (B22003); rent_burden renters paying 30%+ (B25070)
##   own_rate    owner-occupied share (B25003); stay_1yr same house a year ago (B07003)
## ACS 5-year, stamped with the end year (as in 17_cov_acs.R), 2010-2024.
##
## Inputs:  Census API (cached in raw/cov/acs89/), data/geo/county_spine.csv
## Outputs: data/cov/cov_gini_metro.csv (cbsa, year, gini_m; merged by 30)
##          data/need_socap/acs_need_cbsa.csv (all measures; read by 89)

source( "R/lib/00_setup.R" )
OD <- file.path( BUILD, "data", "need_socap" ); dir.create( OD, showWarnings = FALSE )
FN <- file.path( OD, "acs_need_cbsa.csv" )
## 2010-2024 since 2026-11-16 (was 2015-2022): the inequality fork needs gini_m in the
## within stratum (2010-2022)
ACS_YEARS <- 2010:2024

source( "R/lib/geo_helpers.R" )
suppressPackageStartupMessages( library( tidycensus ) )
Sys.setenv( CENSUS_API_KEY = census_key() )        # R/lib/config.R
cache <- file.path( OUT$raw_cov, "acs89" ); dir.create( cache, recursive = TRUE, showWarnings = FALSE )

V <- c( setNames( sprintf( "B19001_%03d", 1:17 ), c( "hh_tot", paste0( "hh_", 1:16 ) ) ),
        setNames( sprintf( "C17002_%03d", 1:8 ), c( "pr_tot", paste0( "pr_", 1:7 ) ) ),
        snap_den = "B22003_001", snap_num = "B22003_002",
        rb_tot = "B25070_001", rb_1 = "B25070_007", rb_2 = "B25070_008", rb_3 = "B25070_009",
        rb_4 = "B25070_010", rb_nc = "B25070_011",
        ten_tot = "B25003_001", ten_own = "B25003_002",
        mob_tot = "B07003_001", mob_same = "B07003_004" )
get_year <- function( y, geo ) {
  f <- file.path( cache, sprintf( "acs5_%s_%d.csv", geo, y ) )
  if ( file.exists( f ) ) return( fread( f, colClasses = list( character = "GEOID" ) ) )
  vv <- switch( geo, county = V, us = c( q1 = "B19080_001", q2 = "B19080_002", q3 = "B19080_003", q4 = "B19080_004" ),
                cbsa = c( gini_pub = "B19083_001" ) )
  gg <- switch( geo, county = "county", us = "us", cbsa = "metropolitan statistical area/micropolitan statistical area" )
  d <- as.data.table( get_acs( geography = gg, variables = vv, year = y, survey = "acs5", output = "wide" ) )
  d[ , year := y ]; fwrite( d, f ); d
}
cty <- rbindlist( lapply( ACS_YEARS, get_year, geo = "county" ), fill = TRUE )
us  <- rbindlist( lapply( ACS_YEARS, get_year, geo = "us" ), fill = TRUE )
pub <- rbindlist( lapply( ACS_YEARS, get_year, geo = "cbsa" ), fill = TRUE )
log_msg( "county rows ", nrow( cty ), "; us ", nrow( us ), "; published cbsa ", nrow( pub ) )

cnt <- sub( "E$", "", grep( "E$", names( cty ), value = TRUE ) )
cnt <- setdiff( cnt, "NAM" )
d <- cty[ , c( "GEOID", "year", paste0( cnt, "E" ) ), with = FALSE ]
setnames( d, c( "geoid", "year", cnt ) )
d <- d[ substr( geoid, 1, 2 ) != "72" ]
d[ , geoid := recode_county( geoid ) ]
d <- d[ , lapply( .SD, sum, na.rm = TRUE ), by = .( geoid, year ), .SDcols = cnt ]
d <- ct_legacy_to_pr( d, cnt, "total" )
spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
d <- merge( d, spine[ cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
m <- d[ , lapply( .SD, sum, na.rm = TRUE ), by = .( cbsa, year ), .SDcols = cnt ]

## income distribution from 16 brackets (national quintile cutoffs merged first: merge
## reorders rows, and the bracket matrix must follow the final order)
usq <- us[ , .( year, q1 = q1E, q2 = q2E, q3 = q3E, q4 = q4E ) ]
m <- merge( m, usq, by = "year" )
lo <- c( 0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75, 100, 125, 150, 200 ) * 1000
hi <- c( lo[ -1 ], Inf )
hh <- as.matrix( m[ , paste0( "hh_", 1:16 ), with = FALSE ] )
alpha <- pmin( pmax( log( ( hh[ , 15 ] + hh[ , 16 ] ) / hh[ , 16 ] ) / log( 200 / 150 ), 1.5 ), 4 )
gini_g <- function( n, a ) {
  mid <- c( ( lo[ -16 ] + hi[ -16 ] ) / 2, 200000 * a / ( a - 1 ) )
  p <- n / sum( n ); inc <- p * mid; L <- cumsum( inc ) / sum( inc ); Lp <- c( 0, L[ -16 ] )
  1 - sum( p * ( L + Lp ) )
}
## share of households below income c (uniform within brackets, Pareto above 200k)
cdf_at <- function( n, a, c ) {
  p <- n / sum( n ); F <- 0
  for ( k in 1:16 ) {
    if ( k < 16 ) F <- F + p[ k ] * min( max( ( c - lo[ k ] ) / ( hi[ k ] - lo[ k ] ), 0 ), 1 )
    else if ( c > 200000 ) F <- F + p[ k ] * ( 1 - ( 200000 / c )^a )
  }
  F
}
m[ , gini_m := vapply( seq_len( .N ), function( i ) gini_g( hh[ i, ], alpha[ i ] ), numeric( 1 ) ) ]
m[ , inc_hhi5 := vapply( seq_len( .N ), function( i ) {
  F <- vapply( c( q1[ i ], q2[ i ], q3[ i ], q4[ i ] ), function( c ) cdf_at( hh[ i, ], alpha[ i ], c ), numeric( 1 ) )
  s <- diff( c( 0, F, 1 ) ); sum( s^2 ) }, numeric( 1 ) ) ]
m[ , `:=`( econ_div = ( 1 - inc_hhi5 ) / 0.8,
           deep_pov = pr_1 / pr_tot, near_pov = ( pr_1 + pr_2 + pr_3 + pr_4 + pr_5 + pr_6 ) / pr_tot,
           pov_acs = ( pr_1 + pr_2 ) / pr_tot,
           snap_rate = snap_num / snap_den,
           rent_burden = ( rb_1 + rb_2 + rb_3 + rb_4 ) / ( rb_tot - rb_nc ),
           own_rate = ten_own / ten_tot, stay_1yr = mob_same / mob_tot,
           hh_low = vapply( seq_len( .N ), function( i ) cdf_at( hh[ i, ], alpha[ i ], q1[ i ] ), numeric( 1 ) ) ) ]
m[ , hh_high := vapply( seq_len( .N ), function( i ) 1 - cdf_at( hh[ i, ], alpha[ i ], q4[ i ] ), numeric( 1 ) ) ]

## validation against the published CBSA Gini (same code; delineations differ for a few)
pub <- pub[ , .( cbsa = GEOID, year, gini_pub = gini_pubE ) ]
m <- merge( m, pub, by = c( "cbsa", "year" ), all.x = TRUE )
print( m[ !is.na( gini_pub ), .( n = .N, r = round( cor( gini_m, gini_pub ), 3 ),
                                 mean_m = round( mean( gini_m ), 3 ), mean_pub = round( mean( gini_pub ), 3 ) ), by = year ] )
out <- m[ , .( cbsa, year, gini_m, gini_pub, inc_hhi5, econ_div, hh_low, hh_high, deep_pov, pov_acs, near_pov,
               snap_rate, rent_burden, own_rate, stay_1yr, hh_tot ) ]
setorder( out, cbsa, year )
fwrite( out, FN )
log_msg( "wrote ", FN, ": ", nrow( out ), " rows, ", uniqueN( out$cbsa ), " CBSAs" )
## covariate file for the multiverse "inequality" add-on block (merged by 30_assemble_analysis.R)
fwrite( out[ , .( cbsa, year, gini_m ) ], file.path( OUT$cov, "cov_gini_metro.csv" ) )
