## 10c_ntee_resolved_check.R
## Diagnostic (2026-11-17): does the NCCS "NTEE-resolved" crosswalk fill NTEE codes that the
## build's legacy-snapshot backfill (06 / 02b) leaves missing? Same missing-code sets as
## 10b_ntee_1023ez_check.R: active 501(c)(3) e-file filers 2020-24 and Dec 2023/2024
## monthly-BMF public charities. Not part of the build.
## Input: D:/MULTIVERSE/raw/ntee_resolved/ntee_resolved_crosswalk.parquet, from
##   https://nccsdata.s3.dualstack.us-east-1.amazonaws.com/crosswalks/ntee-resolved/ntee_resolved_crosswalk.parquet
##   (Last-Modified 2026-09-21; one row per EIN; legacy snapshots + monthly BMF vintages to 2026-07)
## Output: data/diagnostics/ntee_missing_vs_resolved.csv

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
bad <- function( x ) is.na( x ) | x == "" | substr( toupper( x ), 1, 3 ) %in% c( "UND", "INV", "Z99" )
ok  <- function( x ) grepl( "^[A-Z][0-9]{2}", x ) & !bad( x )

cw <- as.data.table( read_parquet( file.path( BUILD, "raw", "ntee_resolved", "ntee_resolved_crosswalk.parquet" ),
        col_select = c( "ein", "ntee_most_recent", "ntee_most_recent_source", "ntee_most_recent_vintage",
                        "ntee_modal", "ntee_agreement", "ntee_current", "current_vintage" ) ) )
cw[ , ein := pad( gsub( "[^0-9]", "", ein ), 9 ) ]
cat( "crosswalk EINs:", nrow( cw ), "; valid ntee_most_recent:", sum( ok( cw$ntee_most_recent ) ),
     "; invalid values:\n" ); print( cw[ !ok( ntee_most_recent ), .N, by = ntee_most_recent ][ order( -N ) ][ 1:10 ] )
cw <- cw[ ok( ntee_most_recent ), .( ein, ntee_cw = toupper( substr( ntee_most_recent, 1, 3 ) ),
                                     ntee_cw_modal = toupper( substr( ntee_modal, 1, 3 ) ),
                                     cw_source = ntee_most_recent_source, cw_vintage = ntee_most_recent_vintage, ntee_agreement ) ]

## legacy-snapshot backfill used by the build
loc <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "ntee3" ),
              colClasses = list( character = c( "EIN", "ntee3" ) ) )
best <- unique( loc[ !bad( ntee3 ) ][ order( EIN, -year ) ], by = "EIN" )[ , .( ein = EIN, ntee_bmf = toupper( substr( ntee3, 1, 3 ) ) ) ]
rm( loc ); gc()

summ <- function( d, label ) {
  d <- merge( d, best, by = "ein", all.x = TRUE )
  d <- merge( d, cw, by = "ein", all.x = TRUE )
  d[ , `:=`( in_bmf = !is.na( ntee_bmf ), in_cw = !is.na( ntee_cw ) ) ]
  list( summary = d[ , .( set = label, missing = .N, bmf_recovers = sum( in_bmf ), cw_has_code = sum( in_cw ),
                          cw_only = sum( in_cw & !in_bmf ), cw_only_from_current = sum( in_cw & !in_bmf & cw_source == "current" ),
                          still_missing_after_both = sum( !in_cw & !in_bmf ), both = sum( in_cw & in_bmf ),
                          agree_3char = sum( in_cw & in_bmf & ntee_cw == ntee_bmf ),
                          agree_major = sum( in_cw & in_bmf & substr( ntee_cw, 1, 1 ) == substr( ntee_bmf, 1, 1 ) ) ), by = year ],
        detail = d )
}

## A. e-file active 501(c)(3) filers 2020-2024
ef <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ) ) )[ year %in% 2020:2024 & subsec_group == "c3" ]
ef <- merge( ef, fread( file.path( OUT$diag, "pce_deflator.csv" ) )[ , .( year, floor_nom ) ], by = "year" )
ef <- ef[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
A <- summ( ef[ bad( ntee3 ), .( ein, year ) ], "e-file active c3 filers" )

## B. Dec 2023 / Dec 2024 monthly-BMF public charities (50 states + DC)
mb <- rbindlist( lapply( 2023:2024, function( y ) {
  m <- fread( file.path( BUILD, "raw", "bmf_monthly", sprintf( "bmf_%d_12_processed.csv", y ) ),
              select = c( "ein", "subsection_code", "foundation_code", "ntee_code_clean", "org_addr_state", "ruling_date_ym_str" ),
              colClasses = "character" )
  m <- unique( m, by = "ein" )[ org_addr_state %in% c( state.abb, "DC" ) &
                                 suppressWarnings( as.integer( subsection_code ) ) == 3L &
                                 !suppressWarnings( as.integer( foundation_code ) ) %in% 2:4 ]
  m[ , .( ein = pad( gsub( "[^0-9]", "", ein ), 9 ), year = y,
          ntee = fifelse( ntee_code_clean %in% c( "UNDEFINED", "INVALID" ), "", ntee_code_clean ),
          ryear = suppressWarnings( as.integer( substr( ruling_date_ym_str, 1, 4 ) ) ) ) ]
} ) )
B <- summ( mb[ bad( ntee ), .( ein, year, ryear ) ], "monthly BMF public charities" )

res <- rbind( A$summary, B$summary )
res[ , `:=`( pct_cw_only = round( 100 * cw_only / missing, 1 ), pct_after_both = round( 100 * still_missing_after_both / missing, 1 ),
             residual_filled_pct = round( 100 * cw_only / ( missing - bmf_recovers ), 1 ),
             agree3_pct = round( 100 * agree_3char / both, 1 ), agree_major_pct = round( 100 * agree_major / both, 1 ) ) ]
setorder( res, set, year ); print( res )
fwrite( res, file.path( OUT$diag, "ntee_missing_vs_resolved.csv" ) )

## where the crosswalk-only codes come from, and what they are (Dec 2024 public charities)
d <- B$detail[ year == 2024 & in_cw & !in_bmf ]
print( d[ , .N, keyby = .( cw_source, vintage_year = substr( cw_vintage, 1, 4 ) ) ] )
print( d[ , .N, keyby = ntee_agreement ] )
print( d[ , .N, keyby = .( ruling = fcase( is.na( ryear ), "unknown", ryear < 2014, "before 2014", ryear <= 2019, "2014-2019", default = "2020+" ) ) ] )
cat( "crosswalk-only human services (I-P), Dec 2024:", d[ substr( ntee_cw, 1, 1 ) %in% LETTERS[ 9:16 ], .N ],
     "; hospitals/universities:", d[ ntee_cw %in% c( NTEE_HOSP, NTEE_UNIV ), .N ], "\n" )
r <- B$detail[ year == 2024 & !in_cw & !in_bmf ]
cat( "Dec 2024 still missing after both:", nrow( r ), "; in crosswalk at all (any code):",
     sum( r$ein %in% pad( gsub( "[^0-9]", "", read_parquet( file.path( BUILD, "raw", "ntee_resolved", "ntee_resolved_crosswalk.parquet" ), col_select = "ein" )$ein ), 9 ) ), "\n" )
