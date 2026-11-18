## 05_npo_efile.R
## NCCS e-file release via panel990, tax years 2021-2024 (near-complete
## universe of 990, 990-EZ and 990-PF filers). Used for 2021-2024 in the
## spliced series; 2021-2023 overlaps SOI for the splice diagnostics.
##
## - one filing per EIN-year with panel990::panel_deduplicate()
##   (prefers non-group, non-partial, then amended, then latest timestamp)
## - location and NTEE from the NCCS master BMF that panel990 appends
##   (current geocode, the same rule as the SOI frame)
##
## Usage: Rscript R/05_npo_efile.R [years]   e.g.  Rscript R/05_npo_efile.R 2024
## Outputs: D:/MULTIVERSE/data/npo/orgyear_efile.parquet (990 + EZ)
##          D:/MULTIVERSE/data/npo/orgyear_efile_pf.parquet

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
suppressPackageStartupMessages( { library( arrow ); library( panel990 ) } )

args  <- commandArgs( TRUE )
YRS   <- if ( length( args ) ) as.integer( strsplit( args[1], "," )[[1]] ) else 2021:2024
log_msg( "e-file years: ", paste( YRS, collapse = " " ), "  panel990 ",
         as.character( packageVersion( "panel990" ) ), " / efile ", efile_version() )

options( timeout = 7200 )

## ---- 990 and 990-EZ -----------------------------------------------------

COLS_990 <- c( "F9_00_RETURN_TYPE", "F9_00_TAX_PERIOD_END_DATE", "F9_00_GRO_RCPT",
               "F9_00_EXEMPT_STAT_501C3_X", "F9_00_EXEMPT_STAT_501C_TYPE",
               "F9_00_RETURN_GROUP_X", "F9_00_RETURN_AMENDED_X", "F9_00_RETURN_TIME_STAMP",
               "F9_00_ORG_ADDR_ZIP", "F9_00_ORG_ADDR_STATE", "F9_00_ORG_ADDR_CITY",
               "F9_01_REV_TOT_CY", "F9_01_EXP_TOT_CY", "F9_01_REV_CONTR_TOT_CY",
               "F9_01_REV_CONTR_TOT_CY_V2", "F9_10_ASSET_TOT_EOY" )

build_990 <- function( y ) {
  out <- file.path( RAW$panel990, sprintf( "efile_990_%d.parquet", y ) )
  if ( file.exists( out ) ) return( out )
  p <- panelize( tables = c( "P00", "P01", "P10" ), years = y, columns = COLS_990,
                 bmf = TRUE, backend = "duckdb", cache = "retain", path = RAW$panel990,
                 unique_rows = FALSE, verbose = TRUE )
  d <- as.data.table( panel_data( p ) )
  n0 <- nrow( d )
  log_msg( "  columns: ", paste( names( d ), collapse = " " ) )
  d <- as.data.table( panel_deduplicate( d ) )      # defaults: RETURN_GROUP_X, RETURN_PARTIAL_X, ...

  log_msg( sprintf( "  990/EZ %d: %s filings -> %s EIN-years", y,
                    format( n0, big.mark = "," ), format( nrow( d ), big.mark = "," ) ) )
  write_parquet( d, out )
  out
}

## ---- 990-PF -------------------------------------------------------------

COLS_PF <- c( "F9_00_TAX_PERIOD_END_DATE", "F9_00_RETURN_AMENDED_X", "F9_00_RETURN_TIME_STAMP",
              "F9_00_ORG_ADDR_ZIP", "F9_00_ORG_ADDR_STATE", "F9_00_ORG_ADDR_CITY",
              "PF_01_REV_TOT_BOOKS", "PF_01_EXP_TOT_EXP_DISBMT_BOOKS",
              "PF_01_EXP_CONTR_PAID_BOOKS", "PF_02_ASSET_TOT_EOY_BV", "PF_02_ASSET_TOT_EOY_FMV" )

build_pf <- function( y ) {
  out <- file.path( RAW$panel990, sprintf( "efile_pf_%d.parquet", y ) )
  if ( file.exists( out ) ) return( out )
  p <- panelize_pf( tables = c( "P00", "PF01", "PF02" ), years = y, columns = COLS_PF,
                    bmf = TRUE, backend = "duckdb", cache = "retain", path = RAW$panel990,
                    unique_rows = FALSE, verbose = TRUE )
  d <- as.data.table( panel_data( p ) )
  n0 <- nrow( d )
  log_msg( "  columns: ", paste( names( d ), collapse = " " ) )
  dd <- function( v ) if ( v %in% names( d ) ) v else NULL
  d <- as.data.table( panel_deduplicate( d, group = dd( "RETURN_GROUP_X" ), partial = dd( "RETURN_PARTIAL_X" ),
                                         amended = dd( "RETURN_AMENDED_X" ) ) )
  log_msg( sprintf( "  990-PF %d: %s filings -> %s EIN-years", y,
                    format( n0, big.mark = "," ), format( nrow( d ), big.mark = "," ) ) )
  write_parquet( d, out )
  out
}

f990 <- vapply( YRS, build_990, character( 1 ) )
fpf  <- vapply( YRS, build_pf,  character( 1 ) )

## ---- standardize --------------------------------------------------------

num <- function( x ) suppressWarnings( as.numeric( x ) )
gcol <- function( d, v ) {
  for ( k in c( v, sub( "^F9_00_", "", v ) ) ) if ( k %in% names( d ) ) return( d[[ k ]] )
  rep( NA, nrow( d ) )
}

locate <- function( d ) {
  ## unified-BMF geocode (lat/long) -> 2023 county by point-in-polygon;
  ## no coordinates -> ZIP -> county map learned from the BMF snapshots
  lat <- num( gcol( d, "geo_lat" ) ); lon <- num( gcol( d, "geo_lon" ) )
  g <- county_from_latlon( lat, lon )
  zip <- gcol( d, "org_addr_zip5" )
  zip[ is.na( zip ) ] <- substr( gcol( d, "F9_00_ORG_ADDR_ZIP" )[ is.na( zip ) ], 1, 5 )
  nz <- is.na( g )
  g[ nz ] <- county_from_zip( zip[ nz ] )
  list( geoid = recode_county( g ), lat = lat, lon = lon,
        method = ifelse( !nz, "latlon", ifelse( !is.na( g ), "zip_modal", "unlocated" ) ) )
}

std_990 <- rbindlist( lapply( f990, function( f ) {
  d <- as.data.table( read_parquet( f ) )
  loc <- locate( d )
  subsec <- suppressWarnings( as.integer( gcol( d, "subsection_code" ) ) )
  c3 <- toupper( gcol( d, "F9_00_EXEMPT_STAT_501C3_X" ) ) %in% c( "X", "1", "TRUE", "Y" )
  con <- num( gcol( d, "F9_01_REV_CONTR_TOT_CY" ) )
  con2 <- num( gcol( d, "F9_01_REV_CONTR_TOT_CY_V2" ) )
  data.table( source = "EFILE", form = gcol( d, "F9_00_RETURN_TYPE" ),
              subsec_group = fifelse( c3 | subsec %in% 3L, "c3", "other" ),
              subsec = subsec, ein = pad( gsub( "[^0-9]", "", d$EIN2 ), 9 ),
              year = as.integer( d$TAX_YEAR ), taxpd_end = gcol( d, "F9_00_TAX_PERIOD_END_DATE" ),
              gross_receipts = num( gcol( d, "F9_00_GRO_RCPT" ) ),
              rev = num( gcol( d, "F9_01_REV_TOT_CY" ) ), exp = num( gcol( d, "F9_01_EXP_TOT_CY" ) ),
              con = fifelse( is.na( con ), con2, con ),
              assets = num( gcol( d, "F9_10_ASSET_TOT_EOY" ) ),
              wages = NA_real_, grants_paid = NA_real_,
              ntee3 = toupper( substr( gcol( d, "ntee_code_clean" ), 1, 3 ) ),
              ind = substr( gcol( d, "nteev2" ), 1, 3 ),
              geoid = loc$geoid, lat = loc$lat, lon = loc$lon )
} ), fill = TRUE )

std_pf <- rbindlist( lapply( fpf, function( f ) {
  d <- as.data.table( read_parquet( f ) )
  loc <- locate( d )
  data.table( source = "EFILE", form = "990PF", subsec_group = "pf", subsec = 3L,
              ein = pad( gsub( "[^0-9]", "", d$EIN2 ), 9 ), year = as.integer( d$TAX_YEAR ),
              taxpd_end = gcol( d, "F9_00_TAX_PERIOD_END_DATE" ),
              gross_receipts = NA_real_,
              rev = num( gcol( d, "PF_01_REV_TOT_BOOKS" ) ),
              exp = num( gcol( d, "PF_01_EXP_TOT_EXP_DISBMT_BOOKS" ) ),
              con = NA_real_,
              assets = num( gcol( d, "PF_02_ASSET_TOT_EOY_BV" ) ),
              assets_fmv = num( gcol( d, "PF_02_ASSET_TOT_EOY_FMV" ) ),
              wages = NA_real_,
              grants_paid = num( gcol( d, "PF_01_EXP_CONTR_PAID_BOOKS" ) ),
              ntee3 = toupper( substr( gcol( d, "ntee_code_clean" ), 1, 3 ) ),
              geoid = loc$geoid, lat = loc$lat, lon = loc$lon )
} ), fill = TRUE )

write_parquet( std_990, file.path( OUT$npo, "orgyear_efile.parquet" ) )
write_parquet( std_pf,  file.path( OUT$npo, "orgyear_efile_pf.parquet" ) )

print( std_990[ , .( orgs = .N, c3 = sum( subsec_group == "c3" ),
                     exp_bn = round( sum( pmax( exp, 0 ), na.rm = TRUE ) / 1e9 ),
                     located = round( 100 * mean( !is.na( geoid ) ), 1 ) ), keyby = year ] )
print( std_pf[ , .( pf = .N, assets_bn = round( sum( pmax( assets, 0 ), na.rm = TRUE ) / 1e9 ),
                    located = round( 100 * mean( !is.na( geoid ) ), 1 ) ), keyby = year ] )
