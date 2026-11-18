## 03_npo_core.R
## NCCS harmonized CORE, tax years 2000-2019:
##   501c3-pz  (990 + 990-EZ public charities and other 501(c)(3) filers)
##   501ce-pz  (990 + 990-EZ filers in other 501(c) subsections)
##   501c3-pf  (990-PF private foundations)
## Used for 2000-2010 in the spliced series; 2011-2019 kept for the
## CORE/SOI overlap diagnostics (plan section 2.2).
##
## Each file is downloaded to a temp file, reduced to the needed columns,
## deduplicated to one return per EIN, written as a compact parquet extract
## in RAW$core_hrmn, and the raw CSV is deleted.
##
## Output (one standardized org-year table, same schema as 04/05):
##   data/npo/orgyear_core.parquet

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )

## File choice (checked 2026-11-12 against the NCCS CORE catalog):
## The catalog's PZ/CE "marts ... -V1" files store every money field as a
## denormalized double (~1.1e-308; an int64/double encoding bug), so they are
## unusable for financials. The unversioned harmonized PZ/CE files carry
## correct values plus ZIP and NTEE, and are used instead. PF uses the
## catalog's V0 marts (values are fine; TY2016-2018 are nearly empty and are
## filled from IRS SOI 990-PF extracts in 03b_npo_pf_soi.R).
CORE <- "https://nccsdata.s3.us-east-1.amazonaws.com/harmonized/core"
url_core <- function( form, y ) switch( form,
  pz = sprintf( "%s/501c3-pz/CORE-%d-501C3-CHARITIES-PZ-HRMN.csv", CORE, y ),
  ce = sprintf( "%s/501ce-pz/CORE-%d-501CE-NONPROFIT-PZ-HRMN.csv", CORE, y ),
  pf = sprintf( "%s/501c3-pf/marts/CORE-%d-501C3-PRIVFOUND-PF-HRMN-V0.csv", CORE, y ) )

## source column -> standard name
## Location and NTEE come primarily from the BMF snapshot for the same
## EIN-year (02_npo_bmf_counts.R); the file's own ZIP/NTEE are kept as backup
MAP_PZ <- c( EIN2 = "ein2", TAX_YEAR = "year",
             F9_00_TAX_PERIOD_END_DATE = "taxpd_end",
             F9_00_SUBSECTION_CODE = "subsec",
             F9_00_GRO_RCPT = "gross_receipts",
             F9_08_REV_TOT_TOT = "rev", F9_09_EXP_TOT_TOT = "exp",
             F9_08_REV_CONTR_TOT = "con", F9_10_ASSET_TOT_EOY = "assets",
             F9_09_EXP_OTH_SAL_WAGE_TOT = "wages",
             GEO_ZIP5 = "zip5", F9_00_ORG_ADDR_STATE = "state",
             F9_00_ORG_ADDR_CITY = "city", MISSION_NTEE = "ntee3" )
MAP_PF <- c( EIN2 = "ein2", TAX_YEAR = "year",
             F9_00_TAX_PERIOD_END_DATE = "taxpd_end",
             PF_01_REV_TOT_BOOKS = "rev",
             PF_01_EXP_TOT_EXP_DISBMT_BOOKS = "exp",
             PF_01_EXP_CONTR_PAID_BOOKS = "grants_paid",
             PF_02_ASSET_TOT_EOY_BV = "assets",
             PF_02_ASSET_TOT_EOY_FMV = "assets_fmv",
             F9_00_ORG_ADDR_ZIP5 = "zip5", F9_00_ORG_ADDR_STATE = "state",
             F9_00_ORG_ADDR_CITY = "city" )

NUM <- c( "gross_receipts", "rev", "exp", "con", "assets", "wages",
          "grants_paid", "assets_fmv" )

extract_core <- function( form, y ) {
  ver <- if ( form == "pf" ) "v0" else "hrmn"
  out <- file.path( RAW$core_hrmn, sprintf( "core_%s_%d_%s.parquet", form, y, ver ) )
  if ( file.exists( out ) ) return( out )
  map <- if ( form == "pf" ) MAP_PF else MAP_PZ
  tmp <- tmpfile( ".csv" )
  on.exit( unlink( tmp ) )
  ok <- tryCatch( { options( timeout = 7200 )
                    utils::download.file( url_core( form, y ), tmp, mode = "wb", quiet = TRUE )
                    TRUE }, error = function( e ) { message( "  FAILED ", form, " ", y, ": ",
                                                             conditionMessage( e ) ); FALSE } )
  if ( !ok ) return( NA_character_ )
  hdr <- names( fread( tmp, nrows = 0 ) )
  have <- intersect( names( map ), hdr )
  d <- fread( tmp, select = have, colClasses = "character", encoding = "Latin-1",
              showProgress = FALSE )
  setnames( d, have, map[ have ] )
  for ( v in setdiff( unname( map ), names( d ) ) ) d[ , ( v ) := NA_character_ ]
  for ( v in intersect( NUM, names( d ) ) ) set( d, j = v, value = suppressWarnings( as.numeric( d[[ v ]] ) ) )
  d[ , year := as.integer( year ) ]
  ## guard against the denormalized-double encoding bug seen in V1 marts
  if ( isTRUE( max( abs( d$rev ), na.rm = TRUE ) < 1 ) )
    stop( "money fields look corrupted (max |rev| < 1) in ", url_core( form, y ) )
  ## one return per EIN: the latest tax period in the file
  n0 <- nrow( d )
  setorder( d, ein2, -taxpd_end )
  d <- unique( d, by = "ein2" )
  d[ , `:=`( form = form, source = "CORE", n_raw = n0 ) ]
  write_parquet( d, out )
  log_msg( sprintf( "  CORE %s %d: %s rows -> %s EINs", form, y,
                    format( n0, big.mark = "," ), format( nrow( d ), big.mark = "," ) ) )
  out
}

grid <- CJ( form = c( "pz", "ce", "pf" ), year = 2000:2019 )
grid[ , path := mapply( extract_core, form, year ) ]
fwrite( grid, file.path( OUT$diag, "core_extract_manifest.csv" ) )

## ---- standardize and stack ---------------------------------------------

STD_COLS <- c( "source", "form", "subsec_group", "subsec", "ein", "year", "taxpd_end",
               "gross_receipts", "rev", "exp", "con", "assets", "wages", "grants_paid",
               "zip5", "state", "city", "ntee3" )

std <- rbindlist( lapply( grid[ !is.na( path ), path ], function( p ) {
  d <- as.data.table( read_parquet( p ) )
  d[ , ein := pad( gsub( "[^0-9]", "", ein2 ), 9 ) ]
  if ( "zip5" %in% names( d ) )
    d[ , zip5 := fifelse( !is.na( zip5 ) & zip5 != "", pad( substr( zip5, 1, 5 ), 5 ), NA_character_ ) ]
  d[ , subsec_group := fcase( form == "pz", "c3", form == "ce", "other", form == "pf", "pf" ) ]
  for ( v in setdiff( STD_COLS, names( d ) ) ) d[ , ( v ) := NA ]
  d[ , ..STD_COLS ]
} ), fill = TRUE )

write_parquet( std, file.path( OUT$npo, "orgyear_core.parquet" ) )
print( std[ , .( orgs = .N, exp_bn = round( sum( pmax( exp, 0 ), na.rm = TRUE ) / 1e9 ) ),
            keyby = .( year, subsec_group ) ] |>
         dcast( year ~ subsec_group, value.var = c( "orgs", "exp_bn" ) ) )
