## 28_cov_corporate_hq.R
## Corporate headquarters by county and year, from two public sources:
##   A. SEC EDGAR: public companies whose annual report (10-K family, 20-F, 40-F)
##      gives a US business address, 2000-2024
##   B. Census County Business Patterns, NAICS 551114 (corporate, subsidiary and
##      regional managing offices), 2000-2023 (CBP 2024 not yet released)
## Motivation: Marquis, Glynn and Davis (2013, "Golfing Alone") find that
## public-company headquarters grow large nonprofits; the project had no measure of
## corporate presence (site/10-next.qmd).
##
## ---- A. EDGAR construction ----------------------------------------------------
## Universe: every annual-report filing in the EDGAR quarterly full index
## (form.idx), forms ANNUAL below, amendments (/A) excluded. A filing with
## co-registrants is listed once per CIK in the index; it is counted once, for its
## primary registrant (the first FILER block of the header, or the FSDS cik).
## Location: the business address (not the mailing address) at the time of filing.
##   - 2009Q2 on, filings in the SEC Financial Statement Data Sets (sub.txt: cityba,
##     stprba, zipba, countryba, sic, afs) take the address from FSDS;
##   - all other filings (every filing before 2009Q2; non-XBRL filings after, mostly
##     asset-backed issuers and pre-2011 smaller companies) from the filing's SEC
##     header (<accession>.hdr.sgml; the .txt submission when that is missing),
##     cached per quarter.
## One location per CIK per year: the latest annual filing filed that year, so a
## firm that moves is counted where it reported from that year. Year = calendar
## year the report was filed (a 10-K filed in March 2005 counts in 2005).
## Exclusions: non-US business addresses (state not one of the 50 states + DC);
## SIC 6189 (asset-backed securities), 6221 (commodity pools), 6770 (blank checks /
## SPACs), 8888 (foreign governments), 9995 (non-operating shells); filings without
## a business ZIP.
## Large filers (2011+): filer status "1-LAF" (large accelerated filer: public float
## of $700M or more) on the year's annual report in FSDS. Before 2011 the measure is
## NA: XBRL (and so FSDS) was phased in, with only large accelerated filers with
## float above $5B in 2009-10, so FSDS cannot identify the full set.
## ZIP -> county: Census 2010 ZCTA-county relationship file (county with the
## largest population part); ZCTAs new in 2020 from the 2020 relationship file
## (largest land part); ZIPs that are not ZCTAs (PO boxes, unique ZIPs) from the
## BMF-learned modal county (data/geo/zip_county_modal_bmf.csv, step 02). Legacy
## codes move to 2023 county-equivalents with recode_county(); Connecticut goes to
## planning regions by city name (ct_to_planning_region()).
##
## ---- B. CBP construction ------------------------------------------------------
## County file cbpYYco (bulk download), NAICS 551114, which is unchanged across the
## 1997/2002/2007/2012/2017/2022 NAICS vintages; statewide (county 999) rows
## dropped. Counties not listed have no establishments, except that from 2017 the
## CBP no longer publishes cells with fewer than 3 establishments (cbp_hq_nosmall).
## Employment: suppressed cells (pre-2017 flags A-M; 2017 noise flag S) are imputed
## as the sum over size classes of establishments x national mean employment per
## establishment in the class (cbpYYus; class midpoints before 2008), clamped to
## the flag's range; cbp_hq_emp_imp holds the imputed part.
## Establishments with 100+ employees: from the size-class counts. From 2017 some
## size classes are withheld ("N"); a county's unallocated establishments
## (est - published classes) are spread over its withheld classes in proportion to
## the national count of withheld establishments in each class (US file minus the
## sum of published county cells); cbp_hq_est100_imp holds the allocated part.
## Counts are moved onto 2023 county-equivalents with harmonize_counties().
##
## SEC requests need a User-Agent naming a contact (SEC fair-access policy, at most
## 10 requests per second; this script makes at most 8). Set SEC_USER_AGENT in
## ~/.Renviron, e.g. "Org Name contact@domain"; it is never written anywhere. It is
## needed only while the caches in raw/sec-edgar/ are incomplete.
##
## Outputs: data/cov/cov_corphq.csv (county x year), data/cov/cbsa/cbsa_corphq.csv,
##          data/cov/cov_corphq_codebook.csv, data/cov/sec_hq_firm_year.csv.gz

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )
source( "R/lib/13_16_cov_helpers.R" )

SEC_DIR <- file.path( BUILD, "raw", "sec-edgar" )
CBP_DIR <- file.path( BUILD, "raw", "census-cbp" )
for ( d in file.path( SEC_DIR, c( "full-index", "hdr", "fsds", "zcta" ) ) ) dir.create( d, recursive = TRUE, showWarnings = FALSE )
dir.create( CBP_DIR, recursive = TRUE, showWarnings = FALSE )
dir.create( file.path( OUT$cov, "cbsa" ), showWarnings = FALSE )

EDGAR_YEARS <- 2000:2024
LARGE_START <- 2011
## the index writes the small-business forms without a hyphen (10KSB, 10KSB40, 10KT405)
ANNUAL <- c( "10-K", "10-K405", "10-KT", "10-KT405", "10KT405", "10-KSB", "10KSB", "10-KSB40", "10KSB40",
             "10KSBT", "10-KSBT", "20-F", "40-F" )
EXCL_SIC <- c( 6189L, 6221L, 6770L, 8888L, 9995L )
US_ST <- c( state.abb, "DC" )
quarters <- CJ( year = EDGAR_YEARS, q = 1:4 )[ , qtr := paste0( year, "Q", q ) ]
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )

## ============================================================================
## A. SEC EDGAR
## ============================================================================
## Section A runs inside build_edgar(). Without SEC_USER_AGENT and with the SEC
## caches incomplete it stops; the step then writes the CBP measures with the
## EDGAR columns NA (loud message) and reruns once the caches exist.
build_edgar <- function() {

sec_ua <- function() {
  ua <- Sys.getenv( "SEC_USER_AGENT", "" )
  if ( !nzchar( ua ) ) stop( "SEC caches incomplete and SEC_USER_AGENT is not set (see the header of this script)" )
  ua
}
## one throttled request (index files, FSDS zips)
sec_get <- function( url, dest ) {
  Sys.sleep( 0.2 )
  h <- curl::new_handle( useragent = sec_ua(), followlocation = TRUE )
  tmp <- paste0( dest, ".tmp" )
  r <- curl::curl_fetch_disk( url, tmp, handle = h )
  if ( r$status_code != 200 ) { unlink( tmp ); stop( "HTTP ", r$status_code, " for ", url ) }
  file.rename( tmp, dest )
  dest
}

## ---- A1. quarterly full index --------------------------------------------------
idx_path <- function( qtr ) file.path( SEC_DIR, "full-index", paste0( "form_", qtr, ".idx.gz" ) )
read_index <- function( yr, q ) {
  qtr <- paste0( yr, "Q", q )
  f <- idx_path( qtr )
  if ( !file.exists( f ) ) {
    tmp <- tmpfile( ".idx" )
    sec_get( sprintf( "https://www.sec.gov/Archives/edgar/full-index/%d/QTR%d/form.idx", yr, q ), tmp )
    l <- readLines( tmp, warn = FALSE ); unlink( tmp )
    con <- gzfile( f, "w" ); writeLines( l, con ); close( con )
  }
  l <- readLines( f, warn = FALSE )
  l <- l[ ( grep( "^-{20,}", l )[1] + 1 ):length( l ) ]
  l <- l[ sub( " .*$", "", l ) %in% ANNUAL ]          # cheap pre-filter (the annual forms have no spaces)
  m <- regmatches( l, regexec( "^(\\S+(?: \\S+)*)\\s{2,}(.*?)\\s+(\\d+)\\s+(\\d{4}-?\\d{2}-?\\d{2})\\s+(edgar/\\S+)\\s*$", l, perl = TRUE ) )
  m <- m[ lengths( m ) == 6 ]
  d <- data.table( form = vapply( m, `[`, "", 2 ), cik_idx = vapply( m, `[`, "", 4 ),
                   filed = gsub( "-", "", vapply( m, `[`, "", 5 ) ), path = vapply( m, `[`, "", 6 ) )
  d <- d[ form %in% ANNUAL ]
  d[ , accession := sub( "\\.txt$", "", basename( path ) ) ]
  d[ , `:=`( qtr = qtr, year = yr ) ]
  d
}
log_msg( "EDGAR full index" )
idx <- rbindlist( Map( read_index, quarters$year, quarters$q ) )
log_msg( "annual-report index rows: ", nrow( idx ), "; filings: ", uniqueN( idx$accession ) )
print( dcast( idx[ , .N, by = .( year, form ) ], year ~ form, value.var = "N", fill = 0 ) )

## ---- A2. Financial Statement Data Sets (2009Q2 on) -----------------------------
fsds_q <- quarters[ year > 2009 | ( year == 2009 & q >= 2 ) ]
fsds_path <- function( qtr ) file.path( SEC_DIR, "fsds", paste0( "sub_", tolower( qtr ), ".txt.gz" ) )
get_fsds <- function( yr, q ) {
  qtr <- paste0( yr, "Q", q )
  f <- fsds_path( qtr )
  if ( !file.exists( f ) ) {
    z <- tmpfile( ".zip" )
    sec_get( sprintf( "https://www.sec.gov/files/dera/data/financial-statement-data-sets/%dq%d.zip", yr, q ), z )
    l <- readLines( unz( z, "sub.txt" ), warn = FALSE, encoding = "UTF-8" ); unlink( z )
    con <- gzfile( f, "w" ); writeLines( l, con, useBytes = TRUE ); close( con )
  }
  d <- fread( f, sep = "\t", quote = "", colClasses = "character", encoding = "UTF-8", fill = TRUE )
  keep <- intersect( c( "adsh", "cik", "name", "sic", "countryba", "stprba", "cityba", "zipba", "afs", "form", "filed" ), names( d ) )
  d[ , ..keep ]
}
log_msg( "FSDS sub.txt" )
fsds <- rbindlist( Map( get_fsds, fsds_q$year, fsds_q$q ), fill = TRUE )
fsds <- unique( fsds[ !grepl( "/A$", form ) ], by = "adsh" )
log_msg( "FSDS submissions: ", nrow( fsds ) )

## ---- A3. filing headers for filings not in FSDS ---------------------------------
hdr_path <- function( qtr ) file.path( SEC_DIR, "hdr", paste0( "hdr_", qtr, ".txt.gz" ) )
acc_in <- function( f ) {
  if ( !file.exists( f ) ) return( character() )
  l <- readLines( f, warn = FALSE )
  l <- l[ startsWith( l, "<<<ACC " ) ]
  sub( "^<<<ACC (\\S+)>>>$", "\\1", l, useBytes = TRUE )
}
## <accession>.hdr.sgml (under 1 kB) in the filing folder; when it is missing (404),
## the full .txt submission, cut at </SEC-HEADER>. A filing with neither is stored
## as an empty record so the quarter can complete. Returns accessions that got 404.
fetch_headers <- function( todo, part, rate = 8 ) {
  ua <- sec_ua()
  done <- acc_in( part )
  todo <- todo[ !accession %in% done ]
  if ( !nrow( todo ) ) return( character() )
  log_msg( "  fetching ", nrow( todo ), " headers -> ", basename( part ) )
  con <- file( part, open = "ab" ); on.exit( close( con ) )
  ## token bucket: requests start at `rate` per second on average, with up to
  ## `max_fly` in flight (SEC responses take 1-2 s, so batches of 8 ran at ~3/s)
  max_fly <- 16L
  pool <- curl::new_pool( host_con = max_fly, total_con = max_fly )
  gone <- character(); retry <- integer(); why <- character(); blocked <- FALSE
  fly <- 0L; nxt <- 1L; sent <- 0L; t0 <- Sys.time(); n <- nrow( todo )
  add <- function( k ) {
    h <- curl::new_handle( url = todo$url[ k ], useragent = ua )
    curl::multi_add( h, pool = pool,
      done = function( r ) {
        fly <<- fly - 1L
        if ( r$status_code == 200L ) {
          ok <- tryCatch( {
            txt <- rawToChar( r$content[ r$content != as.raw( 0 ) ] )
            Encoding( txt ) <- "bytes"                   # headers can hold non-UTF-8 bytes
            txt <- sub( "</SEC-HEADER>.*$", "", txt, useBytes = TRUE )
            writeLines( c( paste0( "<<<ACC ", todo$accession[ k ], ">>>" ), txt ), con, useBytes = TRUE )
            TRUE }, error = function( e ) { why <<- c( why, substr( conditionMessage( e ), 1, 80 ) ); FALSE } )
          if ( !ok ) retry <<- c( retry, k )
        } else if ( r$status_code == 404L ) gone <<- c( gone, todo$accession[ k ] )
        else { retry <<- c( retry, k ); why <<- c( why, paste( "HTTP", r$status_code ) )
               if ( r$status_code %in% c( 403L, 429L ) ) blocked <<- TRUE }
      },
      fail = function( e ) { fly <<- fly - 1L; retry <<- c( retry, k ); why <<- c( why, substr( conditionMessage( e ), 1, 80 ) ) } )
  }
  while ( nxt <= n || fly > 0L ) {
    if ( blocked ) {
      log_msg( "  SEC refused requests (403/429); pausing 10 minutes" )
      while ( fly > 0L ) curl::multi_run( timeout = 1, pool = pool )
      Sys.sleep( 600 ); blocked <- FALSE; t0 <- Sys.time(); sent <- 0L
    }
    allow <- floor( as.numeric( difftime( Sys.time(), t0, units = "secs" ) ) * rate ) - sent
    while ( allow > 0L && nxt <= n && fly < max_fly ) {
      add( nxt ); nxt <- nxt + 1L; sent <- sent + 1L; fly <- fly + 1L; allow <- allow - 1L
      if ( nxt %% 5000L == 0L ) log_msg( "  ", nxt, " / ", n )
    }
    curl::multi_run( timeout = 0.05, pool = pool )
  }
  if ( length( retry ) ) {
    tb <- sort( table( why ), decreasing = TRUE )
    log_msg( "  ", length( retry ), " requests failed: ", paste( names( tb ), tb, sep = " x", collapse = "; " ) )
  }
  gone
}
put_empty <- function( acc, part ) {
  if ( !length( acc ) ) return( invisible() )
  con <- file( part, open = "ab" ); writeLines( paste0( "<<<ACC ", acc, ">>>" ), con ); close( con )
}
filings <- unique( idx, by = "accession" )
need_hdr <- filings[ !accession %in% fsds$adsh ]
log_msg( "filings needing a header: ", nrow( need_hdr ), " of ", nrow( filings ) )
for ( qq in unique( need_hdr$qtr ) ) {
  f <- hdr_path( qq )
  if ( file.exists( f ) ) next
  part <- sub( "\\.txt\\.gz$", ".part", f )
  todo <- need_hdr[ qtr == qq ]
  todo[ , url := sprintf( "https://www.sec.gov/Archives/%s/%s/%s.hdr.sgml", dirname( path ),
                          gsub( "-", "", accession ), accession ) ]
  for ( attempt in 1:4 ) {                 # failed requests (not 404s) are retried
    if ( attempt > 1 ) Sys.sleep( 30 * attempt )
    gone <- fetch_headers( todo, part )
    if ( length( gone ) ) {
      log_msg( "  ", length( gone ), " without .hdr.sgml; trying the full .txt" )
      alt <- todo[ accession %in% gone ][ , url := paste0( "https://www.sec.gov/Archives/", path ) ]
      put_empty( fetch_headers( alt, part ), part )
    }
    if ( all( todo$accession %in% acc_in( part ) ) ) break
  }
  left <- setdiff( todo$accession, acc_in( part ) )
  if ( length( left ) && length( left ) <= 0.01 * nrow( todo ) ) {
    log_msg( "  WARNING ", qq, ": ", length( left ), " headers still failing after 4 attempts; stored empty (dropped)" )
    put_empty( left, part )
  }
  if ( all( todo$accession %in% acc_in( part ) ) ) {
    l <- readLines( part, warn = FALSE )
    con <- gzfile( f, "w" ); writeLines( l, con, useBytes = TRUE ); close( con ); unlink( part )
  } else stop( "headers incomplete for ", qq, "; rerun to retry the failed requests" )
}

## parse: first FILER block, COMPANY DATA and BUSINESS ADDRESS fields
parse_headers <- function( f ) {
  l <- readLines( f, warn = FALSE, encoding = "latin1" )
  d <- data.table( line = l )
  d[ , rec := cumsum( startsWith( line, "<<<ACC " ) ) ]
  acc <- d[ startsWith( line, "<<<ACC " ), .( rec, accession = sub( "^<<<ACC (\\S+)>>>$", "\\1", line ) ) ]
  d <- d[ !startsWith( line, "<<<ACC " ) & !startsWith( line, "</" ) ]
  ## two layouts: .hdr.sgml "<TAG>value"; .txt header "KEY:<tabs>value" (mapped to the tags)
  sg <- grepl( "^<[A-Z0-9-]+>", d$line )
  d[ sg, `:=`( tag = sub( "^<([A-Z0-9-]+)>.*$", "\\1", line ), val = trimws( sub( "^<[A-Z0-9-]+>", "", line ) ) ) ]
  tx <- !sg & grepl( "^\\s*[A-Z0-9 ]+:", d$line )
  d[ tx, `:=`( tag = gsub( " ", "-", trimws( sub( ":.*$", "", line ) ) ), val = trimws( sub( "^[^:]*:", "", line ) ) ) ]
  TXT_TAG <- c( "COMPANY-CONFORMED-NAME" = "CONFORMED-NAME", "CENTRAL-INDEX-KEY" = "CIK",
                "STANDARD-INDUSTRIAL-CLASSIFICATION" = "ASSIGNED-SIC" )
  d[ tag %in% names( TXT_TAG ), tag := TXT_TAG[ tag ] ]
  d <- d[ !is.na( tag ) ]
  d[ , blk := cumsum( tag %in% c( "FILER", "FILED-BY", "SUBJECT-COMPANY", "ISSUER", "REPORTING-OWNER" ) ), by = rec ]
  d <- d[ blk == 1L ]
  d[ , sec := fifelse( tag %in% c( "COMPANY-DATA", "FILING-VALUES", "BUSINESS-ADDRESS", "MAIL-ADDRESS", "FORMER-COMPANY" ),
                       tag, NA_character_ ) ]
  d[ , sec := zoo::na.locf( sec, na.rm = FALSE ), by = rec ]
  d <- d[ ( sec %in% "COMPANY-DATA" & tag %in% c( "CONFORMED-NAME", "CIK", "ASSIGNED-SIC" ) ) |
          ( sec %in% "BUSINESS-ADDRESS" & tag %in% c( "CITY", "STATE", "ZIP" ) ) ]
  w <- dcast( unique( d, by = c( "rec", "tag" ) ), rec ~ tag, value.var = "val" )
  for ( v in c( "CONFORMED-NAME", "CIK", "ASSIGNED-SIC", "CITY", "STATE", "ZIP" ) )
    if ( !v %in% names( w ) ) w[ , ( v ) := NA_character_ ]
  w <- merge( acc, w, by = "rec", all.x = TRUE )
  w[ , .( accession, cik = CIK, name = `CONFORMED-NAME`,
          sic = sub( "^.*\\[(\\d+)\\].*$", "\\1", `ASSIGNED-SIC` ),
          city = CITY, state = STATE, zip = ZIP, country = NA_character_, afs = NA_character_,
          addr_src = "header" ) ]
}
log_msg( "parsing headers" )
hdr <- rbindlist( lapply( hdr_path( unique( need_hdr$qtr ) ), parse_headers ) )
hdr[ !grepl( "^\\d{1,4}$", sic ), sic := NA_character_ ]

## ---- A4. one location per CIK-year ------------------------------------------------
fs <- fsds[ , .( accession = adsh, cik, name, sic, city = cityba, state = stprba, zip = zipba,
                 country = countryba, afs, addr_src = "fsds" ) ]
loc <- rbind( fs[ accession %in% filings$accession ], hdr )
f1 <- merge( filings[ , .( accession, form, filed, year, cik_idx ) ], loc, by = "accession", all.x = TRUE )
f1[ is.na( cik ), cik := cik_idx ]
f1[ , cik := as.integer( cik ) ]
f1[ , sic := as.integer( sic ) ]
f1[ , zip5 := substr( gsub( "[^0-9]", "", zip ), 1, 5 ) ]
f1[ nchar( zip5 ) < 5, zip5 := NA_character_ ]
f1[ , state := toupper( trimws( state ) ) ]
log_msg( "filings: ", nrow( f1 ), "; no header or FSDS record: ", f1[ is.na( addr_src ), .N ] )
setorder( f1, cik, year, -filed, -accession )
fy <- unique( f1, by = c( "cik", "year" ) )
fy[ , us := state %in% US_ST & ( is.na( country ) | country %in% c( "", "US" ) ) ]
fy[ , excl_sic := sic %in% EXCL_SIC ]
cat( "\nCIK-years by status:\n" )
print( fy[ , .( cik_years = .N, us = sum( us ), us_excl_sic = sum( us & excl_sic ),
                us_kept = sum( us & !excl_sic ), us_kept_nozip = sum( us & !excl_sic & is.na( zip5 ) ),
                fsds_addr = sum( addr_src %in% "fsds" ) ), keyby = year ] )
hq <- fy[ us & !excl_sic & !is.na( zip5 ) ]

## ---- A5. ZIP -> 2023 county ----------------------------------------------------------
z10f <- file.path( SEC_DIR, "zcta", "zcta_county_rel_10.txt" )
z20f <- file.path( SEC_DIR, "zcta", "tab20_zcta520_county20_natl.txt" )
fetch( "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt", dirname( z10f ) )
fetch( "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt", dirname( z20f ) )
z10 <- fread( z10f, colClasses = "character" )
z10 <- z10[ order( ZCTA5, -as.numeric( POPPT ), -as.numeric( AREALANDPT ) ) ][ , .SD[1], by = ZCTA5 ][ , .( zip5 = ZCTA5, g10 = GEOID ) ]
z20 <- fread( z20f, colClasses = "character", sep = "|", encoding = "UTF-8" )
setnames( z20, sub( "^\ufeff", "", names( z20 ) ) )
z20 <- z20[ nzchar( GEOID_ZCTA5_20 ) ][ order( GEOID_ZCTA5_20, -as.numeric( AREALAND_PART ) ) ][
  , .SD[1], by = GEOID_ZCTA5_20 ][ , .( zip5 = GEOID_ZCTA5_20, g20 = GEOID_COUNTY_20 ) ]
hq <- merge( hq, z10, by = "zip5", all.x = TRUE )
hq <- merge( hq, z20, by = "zip5", all.x = TRUE )
hq[ , g_bmf := county_from_zip( zip5 ) ]
hq[ , geo_src := fcase( !is.na( g10 ), "zcta2010", !is.na( g20 ), "zcta2020", !is.na( g_bmf ), "bmf_zip", default = "none" ) ]
hq[ , geoid := fcoalesce( g10, g20, g_bmf ) ]
hq[ , geoid := recode_county( ct_to_planning_region( geoid, city ) ) ]
hq[ geoid == "02261", geoid := "02063" ]
st_fips <- setNames( c( sprintf( "%02d", c( 1, 2, 4:6, 8:13, 15:42, 44:51, 53:56 ) ) ),
                     c( "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY",
                        "LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH",
                        "OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY" ) )
hq[ , st_match := substr( geoid, 1, 2 ) == st_fips[ state ] ]
cat( "\nZIP geocoding:\n" ); print( hq[ , .N, keyby = geo_src ] )
cat( "county state differs from address state:", hq[ st_match %in% FALSE, .N ], "\n" )
cat( "not on the 2023 spine:", hq[ !geoid %in% sp$geoid, .N ], "\n" )
hq <- hq[ geoid %in% sp$geoid ]
hq[ , large := fifelse( year >= LARGE_START, as.integer( afs %in% "1-LAF" ), NA_integer_ ) ]
fwrite( hq[ , .( cik, year, name, form, filed, accession, sic, city, state, zip5, geoid, geo_src, addr_src, afs, large ) ],
        FIRM_FILE )

hq[ , .( hq_pub = .N, hq_pub_large = sum( large ) ), by = .( geoid, year ) ]
}
FIRM_FILE <- file.path( OUT$cov, "sec_hq_firm_year.csv.gz" )
edgar <- tryCatch( build_edgar(), error = function( e ) {
  message( "\n*** EDGAR measures NOT built: ", conditionMessage( e ), "\n*** hq_pub and hq_pub_large are NA in this build\n" )
  if ( !file.exists( FIRM_FILE ) ) fwrite( data.table( cik = integer(), year = integer() ), FIRM_FILE )
  NULL } )
if ( is.null( edgar ) ) edgar <- data.table( geoid = character(), year = integer(), hq_pub = integer(), hq_pub_large = integer() )
EDGAR_OK <- nrow( edgar ) > 0

## ============================================================================
## B. County Business Patterns, NAICS 551114
## ============================================================================

cbp_url <- function( y, lvl ) sprintf( "https://www2.census.gov/programs-surveys/cbp/datasets/%d/cbp%02d%s.zip", y, y %% 100, lvl )
cbp_file <- function( y, lvl ) {
  dest <- file.path( CBP_DIR, sprintf( "cbp%02d%s.zip", y %% 100, lvl ) )
  if ( file.exists( dest ) && file.size( dest ) > 1e5 ) return( dest )
  h <- curl::new_handle( followlocation = TRUE )
  r <- tryCatch( curl::curl_fetch_disk( cbp_url( y, lvl ), dest, handle = h ), error = function( e ) NULL )
  if ( is.null( r ) || r$status_code != 200 || file.size( dest ) < 1e5 ) { unlink( dest ); return( NA_character_ ) }
  dest
}
read_cbp <- function( z ) {
  ex <- utils::unzip( z, exdir = TMP )
  on.exit( unlink( ex ) )
  d <- fread( ex[1], colClasses = "character" )
  setnames( d, tolower( names( d ) ) )
  d[ naics == "551114" ]
}
CLS <- c( "n1_4", "n5_9", "n10_19", "n20_49", "n50_99", "n100_249", "n250_499", "n500_999", "n1000" )
MID <- c( 2.5, 7, 14.5, 34.5, 74.5, 174.5, 374.5, 749.5, NA )           # 1000+: from n1000_1..4
MID_1000 <- c( n1000_1 = 1249.5, n1000_2 = 1999.5, n1000_3 = 3749.5, n1000_4 = 7500 )
FLAG_RNG <- data.table( flag = c( "A", "B", "C", "E", "F", "G", "H", "I", "J", "K", "L", "M" ),
                        lo = c( 0, 20, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 1e5 ),
                        hi = c( 19, 99, 249, 499, 999, 2499, 4999, 9999, 24999, 49999, 99999, Inf ) )
num <- function( x ) suppressWarnings( as.numeric( x ) )

cbp_year <- function( y ) {
  zc <- cbp_file( y, "co" )
  if ( is.na( zc ) ) { log_msg( "CBP ", y, ": not available" ); return( NULL ) }
  d <- read_cbp( zc )
  setnames( d, "n<5", "n1_4", skip_absent = TRUE )
  if ( !"empflag" %in% names( d ) ) d[ , empflag := "" ]
  if ( !"emp_nf" %in% names( d ) ) d[ , emp_nf := "" ]
  ## national file: mean employment per establishment by class, and withheld counts
  zu <- if ( y >= 2008 ) cbp_file( y, "us" ) else NA_character_
  m <- setNames( MID, CLS ); wN <- NULL
  if ( !is.na( zu ) ) {
    u <- read_cbp( zu )[ lfo %in% c( "-", "" ) ][ 1 ]
    setnames( u, gsub( "<5", "1_4", names( u ) ) )
    for ( k in CLS ) {
      e <- num( u[[ sub( "^n", "e", k ) ]] ); n <- num( u[[ k ]] )
      if ( length( e ) && length( n ) && isTRUE( n > 0 ) && isTRUE( e > 0 ) ) m[ k ] <- e / n
    }
    pubsum <- vapply( CLS, function( k ) sum( num( d[[ k ]] ), na.rm = TRUE ), 0 )
    wN <- pmax( vapply( CLS, function( k ) num( u[[ k ]] ), 0 ) - pubsum, 0 )
  }
  d <- d[ fipscty != "999" ]
  d[ , geoid := paste0( fipstate, fipscty ) ]
  d[ , est := num( est ) ][ , emp := num( emp ) ]
  n <- as.matrix( d[ , lapply( .SD, num ), .SDcols = CLS ] )
  withheld <- is.na( n )
  ## allocate unallocated establishments to withheld classes
  alloc <- matrix( 0, nrow( n ), ncol( n ), dimnames = dimnames( n ) )
  if ( any( withheld ) ) {
    if ( is.null( wN ) ) stop( "CBP ", y, ": withheld size classes but no national file" )
    resid <- pmax( d$est - rowSums( n, na.rm = TRUE ), 0 )
    w <- withheld * matrix( wN + 1e-9, nrow( n ), ncol( n ), byrow = TRUE )
    alloc <- w / rowSums( w ) * resid
    alloc[ !withheld ] <- 0; alloc[ is.na( alloc ) ] <- 0
    ## rake: rows sum to the county residual; no class gets more than the national
    ## withheld count (withheld cells of dropped <3 counties are mostly small classes)
    for ( it in 1:100 ) {
      cs <- colSums( alloc ); over <- cs > wN & cs > 0
      if ( !any( over ) ) break
      alloc[ , over ] <- sweep( alloc[ , over, drop = FALSE ], 2, wN[ over ] / cs[ over ], `*` )
      rs <- rowSums( alloc )
      alloc <- ifelse( rs > 0, resid / rs, 0 ) * alloc
    }
  }
  n0 <- n; n0[ withheld ] <- 0
  nn <- n0 + alloc
  big <- c( "n100_249", "n250_499", "n500_999", "n1000" )
  d[ , est100 := rowSums( nn[ , big, drop = FALSE ] ) ]
  d[ , est100_imp := rowSums( alloc[ , big, drop = FALSE ] ) ]
  ## employment imputation for suppressed cells
  m1000 <- if ( !is.na( m[ "n1000" ] ) ) rep( m[ "n1000" ], nrow( d ) ) else {
    s <- as.matrix( d[ , lapply( .SD, num ), .SDcols = names( MID_1000 ) ] )
    ifelse( nn[ , "n1000" ] > 0 & rowSums( s, na.rm = TRUE ) > 0,
            rowSums( sweep( s, 2, MID_1000, `*` ), na.rm = TRUE ) / rowSums( s, na.rm = TRUE ), 1749.5 ) }
  mm <- matrix( m, nrow( nn ), ncol( nn ), byrow = TRUE ); mm[ , ncol( mm ) ] <- m1000
  ehat <- rowSums( nn * mm )
  d[ , ehat := ehat ]
  d[ , supp := est > 0 & emp == 0 & ( empflag %in% FLAG_RNG$flag | emp_nf %in% c( "D", "S" ) ) ]
  d[ FLAG_RNG, on = c( empflag = "flag" ), `:=`( lo = i.lo, hi = i.hi ) ]
  d[ , emp_imp := fifelse( supp, pmin( pmax( ehat, fcoalesce( lo, 0 ) ), fcoalesce( hi, Inf ) ), 0 ) ]
  d[ , emp := emp + emp_imp ]
  log_msg( sprintf( "CBP %d: %d counties, est %s, emp %s (imputed %s), est100 %s (allocated %s)", y, nrow( d ),
                    format( sum( d$est ), big.mark = "," ), format( round( sum( d$emp ) ), big.mark = "," ),
                    format( round( sum( d$emp_imp ) ), big.mark = "," ), format( round( sum( d$est100 ) ), big.mark = "," ),
                    format( round( sum( d$est100_imp ), 1 ), big.mark = "," ) ) )
  d[ , .( geoid, year = y, cbp_hq_est = est, cbp_hq_est100 = est100, cbp_hq_emp = emp,
          cbp_hq_emp_imp = emp_imp, cbp_hq_est100_imp = est100_imp ) ]
}
log_msg( "CBP NAICS 551114" )
cbp <- rbindlist( lapply( YEARS, cbp_year ) )
CBP_YEARS <- sort( unique( cbp$year ) )
CBPV <- c( "cbp_hq_est", "cbp_hq_est100", "cbp_hq_emp", "cbp_hq_emp_imp", "cbp_hq_est100_imp" )
cbp <- harmonize_counties( cbp, counts = CBPV )

## ============================================================================
## C. county x year panel, CBSA sums, codebook
## ============================================================================

grid <- CJ( geoid = sp$geoid, year = YEARS )
out <- merge( grid, edgar, by = c( "geoid", "year" ), all.x = TRUE )
out <- merge( out, cbp, by = c( "geoid", "year" ), all.x = TRUE )
if ( EDGAR_OK ) out[ year %in% EDGAR_YEARS & is.na( hq_pub ), hq_pub := 0L ]
if ( EDGAR_OK ) out[ year %in% EDGAR_YEARS & year >= LARGE_START & is.na( hq_pub_large ), hq_pub_large := 0L ]
out[ year < LARGE_START, hq_pub_large := NA_integer_ ]
for ( v in CBPV ) set( out, i = which( out$year %in% CBP_YEARS & is.na( out[[ v ]] ) ), j = v, value = 0 )
out[ , cbp_hq_nosmall := fifelse( year %in% CBP_YEARS, as.integer( year >= 2017 ), NA_integer_ ) ]
setorder( out, geoid, year )
validate_cov( out[ , .( geoid, year ) ], "corporate HQ" )
fwrite( out, file.path( OUT$cov, "cov_corphq.csv" ) )

pop <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )
cb <- merge( out, sp[ nzchar( cbsa ), .( geoid, cbsa ) ], by = "geoid" )
cb <- merge( cb, pop[ , .( geoid, year, population ) ], by = c( "geoid", "year" ), all.x = TRUE )
SUMV <- c( "hq_pub", "hq_pub_large", CBPV, "population" )
cbsa <- cb[ , lapply( .SD, function( x ) if ( all( is.na( x ) ) ) NA_real_ else sum( as.numeric( x ), na.rm = TRUE ) ),
            by = .( cbsa, year ), .SDcols = SUMV ]
cbsa[ , `:=`( hq_pub_per100k = 1e5 * hq_pub / population, hq_pub_large_per100k = 1e5 * hq_pub_large / population,
              cbp_hq_est_per100k = 1e5 * cbp_hq_est / population ) ]
cbsa <- merge( cbsa, unique( sp[ nzchar( cbsa ), .( cbsa, cbsa_title, cbsa_type ) ] ), by = "cbsa" )
setcolorder( cbsa, c( "cbsa", "cbsa_title", "cbsa_type", "year" ) )
fwrite( cbsa[ order( cbsa, year ) ], file.path( OUT$cov, "cbsa", "cbsa_corphq.csv" ) )

fwrite( data.table(
  variable = c( "hq_pub", "hq_pub_large", "cbp_hq_est", "cbp_hq_est100", "cbp_hq_emp",
                "cbp_hq_emp_imp", "cbp_hq_est100_imp", "cbp_hq_nosmall" ),
  definition = c(
    "Public-company headquarters: SEC registrants whose latest annual report (10-K family, 20-F, 40-F) filed that year gives a business address in the county; excl. asset-backed issuers, commodity pools, blank checks, non-operating shells, foreign governments",
    "Large public-company headquarters: hq_pub filers with large-accelerated-filer status (public float >= $700M) on that report (FSDS afs = 1-LAF); 2011+",
    "Establishments in NAICS 551114, corporate, subsidiary and regional managing offices (CBP)",
    "NAICS 551114 establishments with 100+ employees (2017+: withheld size classes allocated, see cbp_hq_est100_imp)",
    "NAICS 551114 mid-March employment, suppressed cells imputed from size classes (see cbp_hq_emp_imp)",
    "Imputed part of cbp_hq_emp (suppressed cells)",
    "Allocated part of cbp_hq_est100 (withheld size classes, 2017+)",
    "1 = CBP year 2017+, when cells with fewer than 3 establishments are not published (counted as 0)" ),
  source = c( "SEC EDGAR full index, filing headers, Financial Statement Data Sets", "SEC Financial Statement Data Sets",
              rep( "Census County Business Patterns", 6 ) ),
  years = c( paste( range( EDGAR_YEARS ), collapse = "-" ), paste( LARGE_START, max( EDGAR_YEARS ), sep = "-" ),
             rep( paste( range( CBP_YEARS ), collapse = "-" ), 6 ) ),
  type = c( "B", "B", "B", "B", "B", "B", "B", "flag" ) ),
  file.path( OUT$cov, "cov_corphq_codebook.csv" ) )

## ---- validation printout --------------------------------------------------------
cat( "\nNational totals by year:\n" )
print( out[ , .( hq_pub = sum( hq_pub ), hq_pub_large = sum( hq_pub_large ), cbp_est = round( sum( cbp_hq_est ) ),
                 cbp_est100 = round( sum( cbp_hq_est100 ) ), cbp_emp = round( sum( cbp_hq_emp ) ) ), keyby = year ] )
cat( "\nTop 15 CBSAs, 2023 (by public HQs; by CBP 551114 establishments if EDGAR is missing):\n" )
print( cbsa[ year == 2023 ][ order( -fcoalesce( as.numeric( hq_pub ), cbp_hq_est ) ) ][
  1:15, .( cbsa_title, hq_pub, hq_pub_large, cbp_hq_est, cbp_hq_est100, hq_per100k = round( hq_pub_per100k, 1 ) ) ] )
if ( EDGAR_OK ) {
  m <- cbsa[ cbsa_type == "metro" & year %in% CBP_YEARS & population > 0 ]
  cat( "\nMetro correlations, EDGAR vs CBP 551114 (per 100k; log counts):\n" )
  print( m[ , .( r_density = round( cor( hq_pub_per100k, cbp_hq_est_per100k ), 3 ),
                 r_log_count = round( cor( log1p( hq_pub ), log1p( cbp_hq_est ) ), 3 ),
                 r_large_est100 = if ( all( is.na( hq_pub_large ) ) ) NA_real_ else
                   round( cor( log1p( hq_pub_large ), log1p( cbp_hq_est100 ) ), 3 ) ), keyby = year ] )
}
log_msg( "wrote cov_corphq.csv: ", nrow( out ), " rows; cbsa_corphq.csv: ", nrow( cbsa ), " rows" )
