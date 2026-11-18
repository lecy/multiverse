## 15_cov_irs_soi.R
## IRS SOI county income data (individual income tax returns). Block: philanthropy.
##   TY 2000-2009 : state Excel files -> returns (N1) and AGI only
##   TY 2010      : 10incyallagi.csv (rows by AGI class, summed)
##   TY 2011-2023 : {yy}incyallnoagi.csv (county totals)
## Itemized charitable contributions (N19700 / A19700) first appear in TY 2010.
## TCJA (TY 2018+) roughly doubled the standard deduction; itemized counts
## and contributions drop sharply from 2018 -- a structural break.
##
## Output: D:/MULTIVERSE/data/cov/cov_irs_soi.csv (+ codebook_irs_soi.csv)
## Amounts are converted from $ thousands to dollars.

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
source( "R/13_16_cov_helpers.R" )
suppressPackageStartupMessages( library( readxl ) )

dir <- file.path( OUT$raw_cov, "irs_soi" )
dir.create( dir, recursive = TRUE, showWarnings = FALSE )
IRS <- "https://www.irs.gov/pub/irs-soi/"
num <- function( x ) suppressWarnings( as.numeric( gsub( "[^0-9.-]", "", as.character( x ) ) ) )

## ---- 2000-2009: state Excel files ---------------------------------------
read_xls_year <- function( y ) {
  z <- fetch_ua( paste0( IRS, y, "countyincome.zip" ), dir )
  if ( is.na( z ) ) return( NULL )
  ex <- file.path( dir, paste0( "x", y ) )
  if ( !dir.exists( ex ) ) unzip( z, exdir = ex )
  fs <- list.files( ex, recursive = TRUE, full.names = TRUE, pattern = "\\.xls$", ignore.case = TRUE )
  rbindlist( lapply( fs, function( f ) {
    x <- tryCatch( suppressMessages( read_excel( f, col_names = FALSE, col_types = "text" ) ),
                   error = function( e ) NULL )
    if ( is.null( x ) || ncol( x ) < 6 ) return( NULL )
    x <- as.data.table( x )
    ## first column holding state codes followed by county codes (some files,
    ## e.g. Oregon 2000-02, carry an extra blank leading column)
    isnum <- function( v ) sum( grepl( "^[0-9]+$", trimws( v ) ) )
    j <- which( vapply( seq_len( ncol( x ) - 5 ), function( k ) isnum( x[[ k ]] ) >= 3 &&
                                                     isnum( x[[ k + 1 ]] ) >= 3, logical( 1 ) ) )[1]
    if ( is.na( j ) ) return( NULL )
    x <- x[ , j:( j + 5 ) ]
    setnames( x, c( "st", "co", "name", "n1", "exempt", "agi" ) )
    x <- x[ grepl( "^[0-9]+$", trimws( st ) ) & grepl( "^[0-9]+$", trimws( co ) ) ]
    x[ , .( geoid = paste0( pad( trimws( st ), 2 ), pad( trimws( co ), 3 ) ),
            n1 = num( n1 ), a00100 = num( agi ) ) ]
  } ) )[ , year := y ]
}

## ---- 2010: AGI-class file, summed ----------------------------------------
read_2010 <- function() {
  z <- fetch_ua( paste0( IRS, "2010countydata.zip" ), dir )
  ex <- file.path( dir, "y2010" )
  if ( !file.exists( file.path( ex, "10incyallagi.csv" ) ) ) unzip( z, files = "10incyallagi.csv", exdir = ex )
  d <- fread( file.path( ex, "10incyallagi.csv" ), colClasses = c( STATEFIPS = "character", COUNTYFIPS = "character" ) )
  setnames( d, toupper( names( d ) ) )
  d[ , geoid := paste0( pad( STATEFIPS, 2 ), pad( COUNTYFIPS, 3 ) ) ]
  v <- c( "N1", "A00100", "N04470", "A04470", "N19700", "A19700" )
  d <- d[ , lapply( .SD, function( x ) sum( num( x ), na.rm = TRUE ) ), by = geoid, .SDcols = v ]
  setnames( d, v, tolower( v ) )
  d[ , year := 2010L ]
}

## ---- 2011-2023: county totals ---------------------------------------------
read_noagi <- function( y ) {
  f <- fetch_ua( sprintf( "%s%02dincyallnoagi.csv", IRS, y %% 100 ), dir )
  if ( is.na( f ) ) f <- fetch_ua( sprintf( "%s%02dcyallnoagi.csv", IRS, y %% 100 ), dir )   # TY 2012 naming
  if ( is.na( f ) ) return( NULL )
  d <- fread( f, colClasses = "character" )
  setnames( d, toupper( names( d ) ) )
  v <- intersect( c( "N1", "A00100", "N04470", "A04470", "N19700", "A19700" ), names( d ) )
  d <- d[ , c( "STATEFIPS", "COUNTYFIPS", v ), with = FALSE ]
  d[ , geoid := paste0( pad( STATEFIPS, 2 ), pad( COUNTYFIPS, 3 ) ) ]
  for ( k in v ) set( d, j = k, value = num( d[[ k ]] ) )
  d <- d[ , c( "geoid", v ), with = FALSE ]
  setnames( d, v, tolower( v ) )
  d[ , year := y ]
}

d <- rbindlist( c( lapply( 2000:2009, read_xls_year ), list( read_2010() ),
                   lapply( 2011:2024, read_noagi ) ), fill = TRUE )
d <- d[ substr( geoid, 3, 5 ) != "000" & as.integer( substr( geoid, 1, 2 ) ) <= 56 ]   # counties, 50 states + DC

## dollars, not thousands
for ( v in c( "a00100", "a04470", "a19700" ) ) set( d, j = v, value = d[[ v ]] * 1000 )
cnt <- c( "n1", "a00100", "n04470", "a04470", "n19700", "a19700" )
for ( v in setdiff( cnt, names( d ) ) ) d[ , ( v ) := NA_real_ ]

## duplicates within a year (e.g., a county listed twice in an Excel file)
d <- unique( d, by = c( "geoid", "year" ) )
d <- harmonize_counties( d, counts = cnt )
## harmonize sums NA to 0 for split/merged rows: restore NA where the source had none
for ( v in c( "n04470", "a04470", "n19700", "a19700" ) ) d[ year < 2010, ( v ) := NA_real_ ]

d[ , `:=`( agi_per_return     = a00100 / n1,
           itemizer_share     = n04470 / n1,
           contrib_claim_share = n19700 / n1,
           contrib_per_return = a19700 / n1,
           contrib_per_claim  = a19700 / n19700,
           contrib_share_agi  = a19700 / a00100,
           post_tcja          = year >= 2018,
           ## pre-2010 county files cover ~15% fewer returns than the 2010+
           ## files (e.g. 122.9M in 2009 vs 142.8M in 2010): a coverage break
           irs_era_pre2010    = year < 2010 ) ]
setorder( d, geoid, year )

validate_cov( d, "IRS SOI county" )
print( d[ , .( returns_m = round( sum( n1, na.rm = TRUE ) / 1e6, 1 ),
               agi_tn = round( sum( a00100, na.rm = TRUE ) / 1e12, 2 ),
               contrib_bn = round( sum( a19700, na.rm = TRUE ) / 1e9, 1 ),
               itemizers_m = round( sum( n04470, na.rm = TRUE ) / 1e6, 1 ) ), keyby = year ] )

fwrite( d, file.path( OUT$cov, "cov_irs_soi.csv" ) )
fwrite( data.table(
  variable = c( "n1", "a00100", "n04470", "a04470", "n19700", "a19700",
                "agi_per_return", "itemizer_share", "contrib_claim_share", "contrib_per_return",
                "contrib_per_claim", "contrib_share_agi", "post_tcja", "irs_era_pre2010" ),
  definition = c( "Number of returns", "Adjusted gross income ($)",
                  "Returns with itemized deductions", "Total itemized deductions ($)",
                  "Returns claiming itemized charitable contributions",
                  "Itemized charitable contributions ($)",
                  "AGI per return ($)", "Share of returns itemizing",
                  "Share of returns claiming charitable contributions",
                  "Itemized charitable contributions per return ($)",
                  "Contributions per claiming return ($)",
                  "Itemized contributions / AGI",
                  "TRUE for TY 2018+ (TCJA): itemization and contribution series break",
                  "TRUE for TY 2000-2009 (state Excel files): ~15% fewer returns covered than 2010+ files; coverage break in n1 and a00100" ),
  source = "IRS SOI County Income Data",
  years = c( "2000-2023", "2000-2023", rep( "2010-2023", 4 ), "2000-2023", rep( "2010-2023", 5 ), "2000-2023", "2000-2023" ),
  type = c( "B", "B", "B", "B", "B", "B", "A", "A", "A", "A", "A", "A", "flag", "flag" ) ),
  file.path( OUT$cov, "codebook_irs_soi.csv" ) )
log_msg( "wrote cov_irs_soi.csv: ", nrow( d ), " rows; years ", paste( range( d$year ), collapse = "-" ) )
