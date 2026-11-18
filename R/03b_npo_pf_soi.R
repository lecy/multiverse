## 03b_npo_pf_soi.R
## Private foundations from the IRS SOI annual 990-PF extracts (local copies,
## filing years 2012-2016 and 2020-2024). The IRS published no 990-PF
## extracts for filing years 2017-2019, and harmonized CORE PF is nearly
## empty for TY2016-2018, so those tax years are thin in every source; the
## coverage table written here decides how 06_npo_assemble.R fills them.
##
## Tax year = year the tax period begins: period ending in December -> same
## year; any other month -> previous year. One return per EIN x tax year
## (the latest filing year wins).
##
## Output: D:/MULTIVERSE/data/npo/orgyear_soi_pf.parquet
##         D:/MULTIVERSE/data/diagnostics/pf_coverage_by_taxyear.csv

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )

dir_x <- file.path( DATA_HUB, "SOI", "01_data", "01_unzipped" )
files <- list.files( dir_x, pattern = "^irs-990-pf-soi-extract-\\d{4}\\.csv$", full.names = TRUE )

want <- c( "EIN", "TAX_PRD", "TOTRCPTPERBKS", "TOTEXPNSPBKS", "CONTRPDPBKS",
           "TOTASSETSEND", "FAIRMRKTVALAMT" )

pf <- rbindlist( lapply( files, function( f ) {
  fy <- as.integer( sub( ".*-(\\d{4})\\.csv$", "\\1", f ) )
  hdr <- toupper( gsub( "[^A-Za-z0-9_]", "", names( fread( f, nrows = 0 ) ) ) )
  d <- fread( f, colClasses = "character", showProgress = FALSE )
  setnames( d, hdr )
  for ( v in setdiff( want, names( d ) ) ) d[ , ( v ) := NA_character_ ]
  d <- d[ , ..want ][ , file_year := fy ]
  log_msg( basename( f ), ": ", nrow( d ) )
  d
} ) )

num <- function( x ) suppressWarnings( as.numeric( x ) )
pf[ , `:=`( rev = num( TOTRCPTPERBKS ), exp = num( TOTEXPNSPBKS ),
            grants_paid = num( CONTRPDPBKS ), assets = num( TOTASSETSEND ),
            assets_fmv = num( FAIRMRKTVALAMT ) ) ]
pf[ , end_y := as.integer( substr( TAX_PRD, 1, 4 ) ) ][ , end_m := as.integer( substr( TAX_PRD, 5, 6 ) ) ]
pf[ , year := fifelse( end_m == 12L, end_y, end_y - 1L ) ]
pf[ , ein := pad( gsub( "[^0-9]", "", EIN ), 9 ) ]
setorder( pf, ein, year, -file_year )
pf <- unique( pf[ !is.na( year ) ], by = c( "ein", "year" ) )

cov <- dcast( pf[ year %in% 2010:2024, .N, by = .( year, file_year ) ],
              year ~ file_year, value.var = "N", fill = 0 )
fwrite( cov, file.path( OUT$diag, "pf_coverage_by_taxyear.csv" ) )
print( pf[ year %in% 2010:2024, .( foundations = .N,
                                   assets_bn = round( sum( pmax( assets, 0 ), na.rm = TRUE ) / 1e9 ) ),
           keyby = year ] )

std <- pf[ , .( source = "SOI_PF", form = "990PF", subsec_group = "pf", subsec = 3L, ein, year,
                taxpd_end = TAX_PRD, gross_receipts = NA_real_, rev, exp, con = NA_real_,
                assets, assets_fmv, wages = NA_real_, grants_paid ) ]
write_parquet( std, file.path( OUT$npo, "orgyear_soi_pf.parquet" ) )
