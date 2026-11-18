## 02_npo_bmf_counts.R
## Registered-organization counts (n_bmf) by county-equivalent x year from
## the NCCS BMF snapshots, plus an EIN x year location table used to place
## CORE filers (time-varying location rule, plan section 1).
##
## Snapshot rule: for each year, the snapshot closest to Dec 31 (tolerance
## 9 months). 2021 has no snapshot in-year; 2022-01 is used for it. Years
## after the last snapshot (2022-08) are left missing here and reconstructed
## later from the unified BMF (ruling and revocation dates).
##
## Outputs
##   data/npo/bmf_county_year.csv      counts by county x year x filter
##   data/npo/ein_location_bmf.csv.gz  EIN x year -> county (from snapshots)
##   data/npo/ein_ntee_best.csv.gz     EIN -> latest valid NTEE (lookup for all later steps)
##   data/diagnostics/bmf_snapshots.csv, bmf_legacy_ntee_backfill.csv

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )

files <- list.files( RAW$bmf_snap, pattern = "^BMF-\\d{4}-\\d{2}-501CX", full.names = TRUE )
snap <- data.table( file = files )
snap[ , date := as.IDate( paste0( substr( basename( file ), 5, 11 ), "-15" ) ) ]

## latest snapshot dated April of year Y through January of Y+1
pick <- rbindlist( lapply( 2000:2024, function( y ) {
  lo <- as.IDate( paste0( y, "-04-01" ) ); hi <- as.IDate( paste0( y + 1, "-01-31" ) )
  s <- snap[ date >= lo & date <= hi ][ order( -date ) ]
  if ( !nrow( s ) ) return( data.table( year = y, file = NA_character_, months_off = NA_real_ ) )
  data.table( year = y, file = s$file[1],
              months_off = round( as.numeric( s$date[1] - as.IDate( paste0( y, "-12-31" ) ) ) / 30.4, 1 ) )
} ) )
stopifnot( !anyDuplicated( na.omit( pick$file ) ) )
fwrite( pick[ , .( year, snapshot = basename( file ), months_off ) ],
        file.path( OUT$diag, "bmf_snapshots.csv" ) )
print( pick[ , .( year, snapshot = basename( file ), months_off ) ] )

want <- c( "EIN", "FIPS", "STATE", "CITY", "ZIP5", "SUBSECCD", "FNDNCD",
           "NTEEFINAL", "NTEECC", "OUTNCCS", "RULEDATE", "LATITUDE", "LONGITUDE" )

read_snap <- function( f ) {
  hdr <- names( fread( f, nrows = 0 ) )
  cols <- hdr[ toupper( hdr ) %in% want ]
  d <- fread( f, select = cols, colClasses = "character", encoding = "Latin-1" )
  setnames( d, toupper( names( d ) ) )
  for ( v in setdiff( want, names( d ) ) ) d[ , ( v ) := NA_character_ ]
  d[ , ..want ]
}

loc_list <- list()
cnt_list <- list()

for ( i in which( !is.na( pick$file ) ) ) {
  y <- pick$year[i]
  log_msg( "BMF ", y, " <- ", basename( pick$file[i] ) )
  d <- read_snap( pick$file[i] )
  d <- d[ is.na( OUTNCCS ) | OUTNCCS != "OUT" ]           # NCCS in-scope
  d <- unique( d, by = "EIN" )
  d[ , EIN := pad( EIN, 9 ) ]
  d[ , ntee := toupper( substr( fifelse( !is.na( NTEEFINAL ) & NTEEFINAL != "",
                                         NTEEFINAL, NTEECC ), 1, 3 ) ) ]
  ## codes arrive as "03"/"3" or, in the 2019+ files, "3.0" / "4.0"
  code2 <- function( x ) pad( suppressWarnings( as.integer( sub( "\\.0+$", "", trimws( x ) ) ) ), 2 )
  d[ , subsec := code2( SUBSECCD ) ]
  d[ , c3 := subsec %in% "03" ]
  d[ , pf := c3 & code2( FNDNCD ) %in% c( "02", "03", "04" ) ]
  d[ , ryear := suppressWarnings( as.integer( substr( gsub( "[^0-9]", "", RULEDATE ), 1, 4 ) ) ) ]

  ## county: FIPS field; CT legacy counties -> planning regions
  ## FIPS arrives as "25013", "25013.0", or "6073" (leading zero dropped)
  d[ , FIPS := sub( "\\.0+$", "", trimws( FIPS ) ) ]
  d[ , geoid := fifelse( !is.na( FIPS ) & nchar( FIPS ) %in% 4:5, pad( FIPS, 5 ), NA_character_ ) ]
  d[ , ZIP5 := fifelse( !is.na( ZIP5 ) & nchar( ZIP5 ) %in% 3:5, pad( ZIP5, 5 ), NA_character_ ) ]
  d[ , geoid := ct_to_planning_region( geoid, CITY ) ]
  d[ , geoid := recode_county( geoid ) ]

  loc_list[[ as.character( y ) ]] <- d[ , .( EIN, year = y, geoid, zip5 = ZIP5, state = STATE,
                                             ntee3 = ntee, subsec, pf, c3, ryear ) ]
  log_msg( "   orgs: ", nrow( d ), "  with FIPS: ", round( 100 * mean( !is.na( d$geoid ) ), 1 ), "%" )
  rm( d ); gc()
}

loc <- rbindlist( loc_list )

## NTEE: the build's single invalid-code rule (R/lib/00_setup.R). Each EIN's most
## recent valid code across all snapshots is the lookup every later step uses;
## invalid snapshot codes take it here, so the counts below, ein_location_bmf and
## everything built from them carry resolved codes (unresolved -> NA).
best <- ntee_best_from( loc )
fwrite( best, NTEE_BEST )
st <- resolve_ntee( loc, "EIN", "ntee3", best )
loc[ , ntee_status := st ]; rm( st )
nd <- loc[ , .( n = .N, invalid = sum( ntee_status != "valid" ), backfilled = sum( ntee_status == "filled" ) ),
           keyby = .( year, c3pc = c3 & !pf ) ]
fwrite( nd, file.path( OUT$diag, "bmf_legacy_ntee_backfill.csv" ) )
print( nd[ c3pc == TRUE ] )
log_msg( "NTEE lookup: ", nrow( best ), " EINs; snapshot rows invalid ", loc[ ntee_status != "valid", .N ],
         ", backfilled ", loc[ ntee_status == "filled", .N ] )
loc[ , ntee_status := NULL ]; rm( nd )

loc[ , loc_method := fifelse( !is.na( geoid ), "fips", NA_character_ ) ]

## Late snapshots (2019+) leave ~15-25% of orgs without FIPS, mostly newer
## 501(c)(3)s. Fill (1) from the same EIN's FIPS in the nearest other
## snapshot, then (2) from a ZIP -> county map learned from geocoded BMF
## records 2000-2018 (modal county per ZIP; handles PO-box ZIPs).
ein_geo <- loc[ !is.na( geoid ), .( EIN, year_g = year, geoid_e = geoid ) ]
setkey( ein_geo, EIN, year_g )
miss <- loc[ is.na( geoid ), .( EIN, year ) ]
fill_e <- ein_geo[ miss, on = .( EIN, year_g = year ), roll = "nearest" ]
loc[ fill_e[ !is.na( geoid_e ) ], on = .( EIN, year = year_g ),
     `:=`( geoid = i.geoid_e, loc_method = "fips_other_year" ) ]

zipmap <- loc[ year <= 2018 & !is.na( geoid ) & !is.na( zip5 ), .N, by = .( zip5, geoid ) ]
zipmap <- zipmap[ order( zip5, -N ) ][ , .( geoid_z = geoid[1], share = N[1] / sum( N ), n = sum( N ) ),
                                        by = zip5 ]
fwrite( zipmap, file.path( OUT$geo, "zip_county_modal_bmf.csv" ) )
loc[ zipmap, on = "zip5", geoid_z := i.geoid_z ]
loc[ is.na( geoid ) & !is.na( geoid_z ), `:=`( geoid = geoid_z, loc_method = "zip_modal" ) ]
loc[ , geoid_z := NULL ]
loc[ is.na( geoid ), loc_method := "unlocated" ]
fwrite( dcast( loc[ , .N, by = .( year, loc_method ) ], year ~ loc_method, value.var = "N", fill = 0 ),
        file.path( OUT$diag, "bmf_location_methods.csv" ) )
print( dcast( loc[ , .N, by = .( year, loc_method ) ], year ~ loc_method, value.var = "N", fill = 0 ) )

cnt <- loc[ !is.na( geoid ), .(
    n_bmf_all       = .N,
    n_bmf_c3        = sum( c3 ),
    n_bmf_c3pc      = sum( c3 & !pf ),
    n_bmf_c3pc_nohu = sum( c3 & !pf & !ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) ),
    n_bmf_c3pc_hs   = sum( c3 & !pf & substr( ntee3, 1, 1 ) %in% LETTERS[ 9:16 ] ),  # I-P
    n_bmf_pf        = sum( pf ) ),
  by = .( geoid, year ) ]
setcolorder( cnt, c( "geoid", "year" ) )
cnt <- merge( cnt, pick[ , .( year, bmf_months_off = months_off ) ], by = "year" )
fwrite( cnt, file.path( OUT$npo, "bmf_county_year.csv" ) )
fwrite( loc[ , .( EIN, year, geoid, zip5, state, ntee3, subsec, pf, ryear, loc_method ) ],
        file.path( OUT$npo, "ein_location_bmf.csv.gz" ) )

print( cnt[ , .( counties = .N, n_bmf_all = sum( n_bmf_all ), n_bmf_c3pc = sum( n_bmf_c3pc ) ), by = year ] )
