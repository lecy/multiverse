## 02b_npo_bmf_monthly.R
## Registered-organization counts for 2023 and 2024 (the legacy NCCS BMF
## snapshots used in 02 stop at 2022-08). Source: NCCS "transformed monthly
## BMF" (cleaned IRS EO BMF), December 2023 and December 2024 releases
##   https://nccsdata.s3.us-east-1.amazonaws.com/processed/bmf/YYYY_12/
## Same filters as 02 (501(c)(3) = subsection 03; private foundation =
## foundation code 02/03/04; hospitals/universities by NTEE). The monthly
## files carry no county, so location is (1) the EIN's county in the latest
## legacy snapshot (02), else (2) the unified-BMF geocode (lat/long -> 2023
## county), else (3) the ZIP -> modal-county map from 02.
## Scope differs slightly from the legacy files (NCCS "OUT" screen not
## applied; restricted to the 50 states + DC); 2022 -> 2023 level changes are
## reported in data/diagnostics/bmf_monthly_splice.csv and flagged
## (bmf_src_monthly = 1).
##
## Output: data/npo/bmf_county_year_monthly.csv (read by 06_npo_assemble.R)

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
suppressPackageStartupMessages( library( arrow ) )

mdir <- file.path( BUILD, "raw", "bmf_monthly" )
FILES <- c( "2023" = "bmf_2023_12_processed.csv", "2024" = "bmf_2024_12_processed.csv" )
stopifnot( all( file.exists( file.path( mdir, FILES ) ) ) )
STATES <- c( state.abb, "DC" )

## prior locations (legacy snapshots): latest county per EIN
loc <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "ntee3" ),
              colClasses = list( character = c( "EIN", "geoid", "ntee3" ) ) )
## invalid NTEE -> the EIN's most recent valid legacy-snapshot code (same rule as
## 06_npo_assemble.R and 23_cov_compete.R; added 2026-11-17). ~24% of registered
## public charities in the Dec 2023 / Dec 2024 monthly files are UNDEFINED/INVALID;
## without this, n_bmf_c3pc_hs fell 9.5% from 2022 to 2023.
bad_ntee <- function( x ) is.na( x ) | x == "" | substr( toupper( x ), 1, 3 ) %in% c( "UND", "INV", "Z99" )
ntee_best <- unique( loc[ !bad_ntee( ntee3 ) ][ order( EIN, -year ) ], by = "EIN" )[ , .( EIN, ntee_bmf = toupper( substr( ntee3, 1, 3 ) ) ) ]
loc <- loc[ !is.na( geoid ) ][ order( EIN, -year ) ][ , .( geoid_prev = geoid[1] ), by = EIN ]
ntee_diag <- list()
## unified BMF geocodes
ub <- as.data.table( open_dataset( file.path( RAW$panel990, "bmf", "bmf_unified_geocoded.parquet" ) ) |>
                       dplyr::select( ein, geo_lat, geo_lon ) |> dplyr::collect() )
ub[ , EIN := gsub( "[^0-9]", "", ein ) ][ , ein := NULL ]
ub <- ub[ is.finite( geo_lat ) & is.finite( geo_lon ) ]
zipmap <- fread( file.path( OUT$geo, "zip_county_modal_bmf.csv" ), colClasses = list( character = c( "zip5", "geoid_z" ) ) )

cols <- c( "ein", "subsection_code", "foundation_code", "ntee_code_clean", "org_addr_zip5",
           "org_addr_state", "org_addr_city", "ruling_date_ym_str" )
out <- list(); diag <- list(); ein_m <- list()
for ( y in names( FILES ) ) {
  log_msg( "monthly BMF ", y, " <- ", FILES[[ y ]] )
  d <- fread( file.path( mdir, FILES[[ y ]] ), select = cols, colClasses = "character", encoding = "UTF-8" )
  n0 <- nrow( d )
  d <- unique( d, by = "ein" )[ org_addr_state %in% STATES ]
  d[ , EIN := gsub( "[^0-9]", "", ein ) ]
  d[ , subsec := pad( suppressWarnings( as.integer( subsection_code ) ), 2 ) ]
  d[ , c3 := subsec == "03" ]
  d[ , pf := c3 & pad( suppressWarnings( as.integer( foundation_code ) ), 2 ) %in% c( "02", "03", "04" ) ]
  d[ , ntee3 := toupper( substr( fifelse( ntee_code_clean %in% c( "UNDEFINED", "INVALID" ), "", ntee_code_clean ), 1, 3 ) ) ]
  d[ ntee_best, on = "EIN", ntee_bmf := i.ntee_bmf ]
  ntee_diag[[ y ]] <- d[ c3 & !pf, .( year = as.integer( y ), c3pc = .N, invalid = sum( bad_ntee( ntee3 ) ),
                                      backfilled = sum( bad_ntee( ntee3 ) & !is.na( ntee_bmf ) ) ) ]
  d[ bad_ntee( ntee3 ) & !is.na( ntee_bmf ), ntee3 := ntee_bmf ]
  d[ , ntee_bmf := NULL ]
  d[ , zip5 := fifelse( grepl( "^[0-9]{5}$", org_addr_zip5 ), org_addr_zip5, NA_character_ ) ]

  ## 1. prior legacy-snapshot county
  d[ loc, on = "EIN", geoid := i.geoid_prev ]
  d[ !is.na( geoid ), loc_method := "legacy_snapshot" ]
  ## 2. unified geocode
  need <- d[ is.na( geoid ), .( EIN ) ]
  g <- ub[ need, on = "EIN", nomatch = 0L ]
  if ( nrow( g ) ) {
    g[ , geoid_g := county_from_latlon( geo_lat, geo_lon ) ]
    d[ g[ !is.na( geoid_g ) ], on = "EIN", `:=`( geoid = i.geoid_g, loc_method = "unified_geocode" ) ]
  }
  ## 3. ZIP modal
  d[ zipmap, on = "zip5", geoid_z := i.geoid_z ]
  d[ is.na( geoid ) & !is.na( geoid_z ), `:=`( geoid = geoid_z, loc_method = "zip_modal" ) ]
  d[ is.na( geoid ), loc_method := "unlocated" ]
  d[ , geoid := recode_county( geoid ) ]
  diag[[ y ]] <- d[ , .N, by = loc_method ][ , year := as.integer( y ) ]
  log_msg( "   rows ", n0, "; in 50+DC ", nrow( d ), "; located ", round( 100 * mean( !is.na( d$geoid ) ), 1 ), "%" )

  out[[ y ]] <- d[ !is.na( geoid ), .(
    n_bmf_all       = .N,
    n_bmf_c3        = sum( c3 ),
    n_bmf_c3pc      = sum( c3 & !pf ),
    n_bmf_c3pc_nohu = sum( c3 & !pf & !ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) ),
    n_bmf_c3pc_hs   = sum( c3 & !pf & substr( ntee3, 1, 1 ) %in% LETTERS[ 9:16 ] ),
    n_bmf_pf        = sum( pf ) ), by = geoid ][ , year := as.integer( y ) ]
  ## ruling-year cohorts in this file (used by 08_npo_entry.R)
  fwrite( d[ c3 == TRUE, .( EIN, geoid, ruling_ym = ruling_date_ym_str, pf, ntee3 ) ],
          file.path( mdir, sprintf( "c3_rulings_%s.csv.gz", y ) ) )
  ## EIN-level rows for the stock reconstruction below
  ein_m[[ y ]] <- d[ , .( EIN, year = as.integer( y ), geoid, subsec, pf, ntee3, c3,
                          ryear = suppressWarnings( as.integer( substr( ruling_date_ym_str, 1, 4 ) ) ) ) ]
  rm( d ); gc()
}
cnt <- rbindlist( out )
cnt[ , `:=`( bmf_months_off = 0, bmf_src_monthly = 1L ) ]
setcolorder( cnt, c( "geoid", "year" ) )
fwrite( cnt, file.path( OUT$npo, "bmf_county_year_monthly.csv" ) )
fwrite( rbindlist( diag ), file.path( OUT$diag, "bmf_monthly_location_methods.csv" ) )
print( rbindlist( diag ) )
fwrite( rbindlist( ntee_diag ), file.path( OUT$diag, "bmf_monthly_ntee_backfill.csv" ) )
print( rbindlist( ntee_diag ) )

## splice check against the legacy series
leg <- fread( file.path( OUT$npo, "bmf_county_year.csv" ), colClasses = list( character = "geoid" ) )
nat <- rbind( leg[ , lapply( .SD, sum ), by = year, .SDcols = patterns( "^n_bmf" ) ][ , src := "legacy" ],
              cnt[ , lapply( .SD, sum ), by = year, .SDcols = patterns( "^n_bmf" ) ][ , src := "monthly" ] )
setorder( nat, year )
print( nat )
fwrite( nat, file.path( OUT$diag, "bmf_monthly_splice.csv" ) )
log_msg( "wrote bmf_county_year_monthly.csv: ", nrow( cnt ), " county-years" )

## ---- reconstructed registered-organization stock, 2000-2024 -------------------
## The snapshot counts have artifacts: NCCS's 2016-2018 legacy files omit most
## newly ruled organizations (the 2019 file adds ~214k EINs ruled 2016-2019),
## so the snapshot stock dips 2016-2018 and jumps in 2019. Reconstruction:
## an EIN is in the stock in every year from its START to the last year it
## is observed in any file (legacy snapshots 2000-2022, monthly 2023-2024),
## gaps included. START = ruling year when the EIN first appears within 3
## years of its ruling (a late-added new org), otherwise the first year
## observed (e.g., group subordinates or old orgs added to the file later,
## which are not back-filled). Attributes (county, subsection, foundation
## status, NTEE) come from the nearest observed year.
## Output: data/npo/bmf_county_year_recon.csv  (06 uses these as n_bmf_*)

lg <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ),
             colClasses = list( character = c( "EIN", "geoid", "subsec", "ntee3" ) ) )
stopifnot( "ryear" %in% names( lg ) )
lg[ , c3 := subsec %in% "03" ]
obs <- rbind( lg[ , .( EIN, year, geoid, subsec, pf = as.logical( pf ), ntee3, c3, ryear ) ],
              rbindlist( ein_m ), use.names = TRUE )
rm( lg ); gc()
obs[ , EIN := pad( EIN, 9 ) ]
## same invalid-NTEE rule for the legacy observations (monthly rows are already backfilled)
obs[ ntee_best, on = "EIN", ntee_bmf := i.ntee_bmf ]
log_msg( "recon: EIN-years with invalid NTEE ", obs[ bad_ntee( ntee3 ), .N ], "; backfilled ",
         obs[ bad_ntee( ntee3 ) & !is.na( ntee_bmf ), .N ] )
obs[ bad_ntee( ntee3 ) & !is.na( ntee_bmf ), ntee3 := ntee_bmf ]
obs[ , ntee_bmf := NULL ]
span <- obs[ , .( first = min( year ), last = max( year ) ), by = EIN ]
rr <- obs[ !is.na( ryear ) & ryear >= 1800L, .( r = min( ryear ) ), by = EIN ]
span <- merge( span, rr, by = "EIN", all.x = TRUE ); rm( rr )
## also back-fill any EIN ruled 2016+ that first appears in 2019 or later:
## the 2019-2022 legacy files added the missing new public charities but no
## new foundations, which first appear in the 2023 monthly file
span[ , start := fifelse( !is.na( r ) & r <= first & ( first - r <= 3L | ( r >= 2016L & first >= 2019L ) ),
                          pmax( r, min( YEARS ) ), first ) ]
span[ , backfilled := start < first ]
log_msg( "EINs: ", nrow( span ), "; back-filled from ruling year: ", sum( span$backfilled ) )
ex <- span[ , .( year = seq( start, last ) ), by = EIN ][ year %in% YEARS ]
## nearest observed attributes
setkey( obs, EIN, year )
att <- obs[ ex, on = .( EIN, year ), roll = "nearest" ]
log_msg( "reconstructed EIN-years: ", nrow( att ) )
rec <- att[ !is.na( geoid ), .(
  n_bmf_all       = .N,
  n_bmf_c3        = sum( c3 ),
  n_bmf_c3pc      = sum( c3 & !pf ),
  n_bmf_c3pc_nohu = sum( c3 & !pf & !ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) ),
  n_bmf_c3pc_hs   = sum( c3 & !pf & substr( ntee3, 1, 1 ) %in% LETTERS[ 9:16 ] ),
  n_bmf_pf        = sum( pf ) ), by = .( geoid, year ) ]
setcolorder( rec, c( "geoid", "year" ) )
fwrite( rec, file.path( OUT$npo, "bmf_county_year_recon.csv" ) )
snap <- rbind( leg, cnt, fill = TRUE )
cmp <- merge( rec[ , .( recon_c3pc = sum( n_bmf_c3pc ), recon_pf = sum( n_bmf_pf ), recon_all = sum( n_bmf_all ) ), by = year ],
              snap[ , .( snap_c3pc = sum( n_bmf_c3pc ), snap_pf = sum( n_bmf_pf ), snap_all = sum( n_bmf_all ) ), by = year ],
              by = "year", all = TRUE )
print( cmp )
fwrite( cmp, file.path( OUT$diag, "bmf_recon_vs_snapshot.csv" ) )
log_msg( "wrote bmf_county_year_recon.csv: ", nrow( rec ), " county-years" )
