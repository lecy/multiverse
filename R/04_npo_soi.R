## 04_npo_soi.R
## SOI extract panel (IRS annual 990 / 990-EZ extracts), tax years 2011-2023.
## Used for 2011-2020 in the spliced series; 2011-2019 overlaps CORE and
## 2021-2023 overlaps e-file for the splice diagnostics.
##
## Source: the augmented sample frame (one row per EIN x tax year).
##  - rows with MISSING_YEAR == 1 are gap-fillers with no financials: dropped
##  - V501C switches between "3" and "03" across years: normalized as integer
##  - location = the frame's geocode (CENSUS_BLOCK_FIPS, current address);
##    CT legacy-county blocks are placed by lat/long into planning regions
##  - foundations (990-PF) are not in the frame; they come from CORE / e-file
##
## Output: D:/MULTIVERSE/data/npo/orgyear_soi.parquet (standard org-year schema)

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
suppressPackageStartupMessages( library( arrow ) )

cols <- c( "EIN2", "TAXYEAR", "V501C", "SOURCE", "MISSING_YEAR", "TOTREV", "TOTEXP",
           "REVCONT", "TOTASS", "TAXPEND", "NTEE_IRS", "NTEEV2", "CENSUS_BLOCK_FIPS",
           "LATITUDE", "LONGITUDE", "BMF_SUBSECTION_CODE" )
d <- fread( RAW$soi_frame, select = cols, colClasses = "character", showProgress = FALSE )
log_msg( "SOI frame rows: ", format( nrow( d ), big.mark = "," ) )

d <- d[ MISSING_YEAR != "1" ]
d[ , year := as.integer( TAXYEAR ) ]
d <- d[ year %in% 2010:2024 ]

num <- function( x ) suppressWarnings( as.numeric( x ) )
d[ , `:=`( rev = num( TOTREV ), exp = num( TOTEXP ), con = num( REVCONT ), assets = num( TOTASS ) ) ]
d[ , subsec := suppressWarnings( as.integer( V501C ) ) ]
d[ is.na( subsec ), subsec := suppressWarnings( as.integer( BMF_SUBSECTION_CODE ) ) ]
d[ , form := sub( "^SOI_[0-9]{4}_", "", SOURCE ) ]

## one return per EIN-year: latest tax period, then the larger return
d[ , ein := pad( gsub( "[^0-9]", "", EIN2 ), 9 ) ]
setorder( d, ein, year, -TAXPEND, -exp )
n0 <- nrow( d )
d <- unique( d, by = c( "ein", "year" ) )
log_msg( "dropped duplicate EIN-years: ", n0 - nrow( d ) )

## NTEE (3 characters): IRS code first, NTEEV2 middle segment as fallback
d[ , ntee3 := toupper( substr( NTEE_IRS, 1, 3 ) ) ]
d[ ntee3 == "" | is.na( ntee3 ), ntee3 := toupper( substr( sub( "^[A-Z]{3}-", "", NTEEV2 ), 1, 3 ) ) ]
d[ , ind := substr( NTEEV2, 1, 3 ) ]                      # NTEEV2 industry (HOS, UNI, ...)

## location
d[ , lat := num( LATITUDE ) ][ , lon := num( LONGITUDE ) ]
d[ , geoid := fifelse( !is.na( CENSUS_BLOCK_FIPS ) & nchar( CENSUS_BLOCK_FIPS ) >= 14,
                       substr( pad( CENSUS_BLOCK_FIPS, 15 ), 1, 5 ), NA_character_ ) ]
ct <- d[ , which( substr( geoid, 1, 2 ) == "09" & !geoid %in% .ct$pr ) ]
if ( length( ct ) ) {
  pr <- ct_pr_from_latlon( d$lat[ ct ], d$lon[ ct ] )
  set( d, i = ct, j = "geoid", value = ifelse( is.na( pr ),
       .ct_dom$dominant_pr[ match( d$geoid[ ct ], .ct_dom$legacy_county ) ], pr ) )
}
d[ , geoid := recode_county( geoid ) ]
log_msg( "located: ", round( 100 * mean( !is.na( d$geoid ) ), 2 ), "%;  outside spine: ",
         sum( not_in_spine( d$geoid ) ) )

std <- d[ , .( source = "SOI", form, subsec_group = fifelse( subsec %in% 3L, "c3", "other" ),
               subsec, ein, year, taxpd_end = TAXPEND,
               gross_receipts = NA_real_, rev, exp, con, assets,
               wages = NA_real_, grants_paid = NA_real_,
               ntee3, ind, geoid, lat, lon ) ]

write_parquet( std, file.path( OUT$npo, "orgyear_soi.parquet" ) )
print( std[ , .( orgs = .N, c3 = sum( subsec_group == "c3" ),
                 exp_bn = round( sum( pmax( exp, 0 ), na.rm = TRUE ) / 1e9 ),
                 located = round( 100 * mean( !is.na( geoid ) ), 1 ) ), keyby = year ] )
