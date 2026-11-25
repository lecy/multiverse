## env/build_sources.R
## Builds inst/extdata/sources.csv of the npdensity checkout, the source registry (PACKAGE-PLAN.md Step 4): for every
## frozen input file, where it can be downloaded again and how.
##   Rscript env/build_sources.R
## Columns: source, file (as in the package manifest: <root>/<path>), method, url, note.
##   method http      a plain download from a stable URL
##          http_ua   the publisher rejects bare scripted requests; sent with a browser User-Agent
##          frozen    no stable public file: an API response the step writes, a file extracted or
##                    derived from another download, a file placed by hand, or our own
##                    harmonized intermediate. Only the frozen copy exists (note says why).
## The URLs are the ones the build steps use (R/build/...) and inputs/<source>/SOURCE.md names.
## Reads the package manifest, so run env/build_pkg_manifest.R first.

suppressPackageStartupMessages( library( data.table ) )
source( "R/lib/load_pkg.R" ); EXT <- npdensity_extdata()   # the npdensity checkout
m <- fread( file.path( EXT, "manifest.csv" ), colClasses = list( character = "sha256" ) )[ class == "input" ]
m <- m[ , .( source = item, file ) ]
m[ , b := basename( file ) ]

CB  <- "https://www2.census.gov/programs-surveys/popest/datasets"
IRS <- "https://www.irs.gov/pub/irs-soi/"
GOV <- "https://www2.census.gov/programs-surveys/gov-finances"
GH  <- "https://raw.githubusercontent.com/"
## rules: source, file-name regex, method, function of the file name -> URL
R <- function( source, re, method, url ) list( source = source, re = re, method = method, url = url )
rules <- list(
  R( "census-popest", "^co-est00int-tot\\.csv$", "http", function( b ) paste0( CB, "/2000-2010/intercensal/county/", b ) ),
  R( "census-popest", "^sub-est00int\\.csv$", "http", function( b ) paste0( CB, "/2000-2010/intercensal/cities/", b ) ),
  R( "census-popest", "^sub-est2020int\\.csv$", "http", function( b ) paste0( CB, "/2010-2020/intercensal/cities/", b ) ),
  R( "census-popest", "^co-est2025-alldata\\.csv$", "http", function( b ) paste0( CB, "/2020-2025/counties/totals/", b ) ),
  R( "census-popest", "^ct_cou_to_cousub_crosswalk\\.txt$", "http", function( b ) paste0( "https://www2.census.gov/geo/docs/reference/ct_change/", b ) ),
  R( "census-pep-char", "^co-est00int-alldata-[0-9]{2}\\.csv$", "http", function( b ) paste0( CB, "/2000-2010/intercensal/county/", b ) ),
  R( "census-pep-char", "^cc-est2020int-.*\\.csv$", "http", function( b ) paste0( CB, "/2010-2020/intercensal/county/asrh/", b ) ),
  R( "census-pep-char", "^cc-est2025-.*\\.csv$", "http", function( b ) paste0( CB, "/2020-2025/counties/asrh/", b ) ),
  R( "census-hist", "^e7079co\\.txt$", "http", function( b ) paste0( "https://www2.census.gov/programs-surveys/popest/tables/1900-1980/counties/totals/", b ) ),
  R( "census-hist", "^e8089co\\.txt$", "http", function( b ) paste0( "https://www2.census.gov/programs-surveys/popest/tables/1980-1990/counties/totals/", b ) ),
  R( "census-hist", "^99c8_00\\.txt$", "http", function( b ) paste0( "https://www2.census.gov/programs-surveys/popest/tables/1990-2000/counties/totals/", b ) ),
  R( "bea-regional", "^[A-Z0-9]+\\.zip$", "http", function( b ) paste0( "https://apps.bea.gov/regional/zip/", b ) ),
  R( "ers-laus", "^ers_unemployment_2000_23\\.csv$", "http_ua", function( b )
     "https://www.ers.usda.gov/media/5497/unemployment-and-median-household-income-for-the-united-states-states-and-counties-2000-23.csv?v=78722" ),
  R( "irs-soi-county", "^([0-9]{4}countyincome|2010countydata)\\.zip$|^[0-9]{2}(in)?cyallnoagi\\.csv$", "http_ua", function( b ) paste0( IRS, b ) ),
  R( "elections", "^countypres_2000-2016\\.csv$", "http_ua", function( b ) paste0( GH, "MEDSL/county-returns/master/countypres_2000-2016.csv" ) ),
  R( "elections", "^tonmcg_(2020|2024)\\.csv$", "http_ua", function( b )
     paste0( GH, "tonmcg/US_County_Level_Election_Results_08-24/master/", sub( "^tonmcg_([0-9]{4})\\.csv$", "\\1", b ), "_US_County_Level_Presidential_Results.csv" ) ),
  R( "census-cog", "^02countyarea(gid)?\\.zip$", "http", function( b ) paste0( GOV, "/datasets/2002/public-use-datasets/", b ) ),
  R( "census-cog", "^county_area_finances_(2007|2012)\\.zip$", "http", function( b ) paste0( GOV, "/datasets/historical/", b ) ),
  R( "census-cog", "^2012_Individual_Unit_file\\.zip$", "http", function( b ) paste0( GOV, "/tables/2012/summary-tables/", b ) ),
  R( "census-cog", "^(2017|2022)_Individual_Unit_File\\.zip$", "http", function( b ) paste0( GOV, "/tables/", substr( b, 1, 4 ), "/", b ) ),
  R( "census-cbp", "^cbp[0-9]{2}(co|us)\\.zip$", "http", function( b ) {
     yy <- as.integer( substr( b, 4, 5 ) ); sprintf( "https://www2.census.gov/programs-surveys/cbp/datasets/%d/%s", 2000L + yy, b ) } ),
  R( "nccs-bmf-monthly", "^bmf_[0-9]{4}_12_(processed|data_dictionary)\\.csv$", "http", function( b )
     sprintf( "https://nccsdata.s3.us-east-1.amazonaws.com/processed/bmf/%s/%s", substr( b, 5, 11 ), b ) ),
  R( "nccs-bmf-unified", "^bmf_unified_geocoded\\.parquet$", "http", function( b )
     "https://nccsdata.s3.us-east-1.amazonaws.com/geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet" ),
  R( "sec-edgar", "^zcta_county_rel_10\\.txt$", "http", function( b ) paste0( "https://www2.census.gov/geo/docs/maps-data/data/rel/", b ) ),
  R( "sec-edgar", "^tab20_zcta520_county20_natl\\.txt$", "http", function( b ) paste0( "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/", b ) ) )

m[ , `:=`( method = "frozen", url = NA_character_, note = NA_character_ ) ]
for ( r in rules ) {
  i <- which( m$source == r$source & grepl( r$re, m$b ) & m$method == "frozen" )
  if ( !length( i ) ) stop( "rule matches no file: ", r$source, " ", r$re )
  m[ i, `:=`( method = r$method, url = vapply( b, r$url, "" ) ) ]
}
## why the rest have only a frozen copy
why <- function( s, re, note ) m[ source == s & method == "frozen" & grepl( re, file ), note := note ]
why( "census-saipe", ".", "Census API response written by step 13 (needs a Census API key); rerun the step to query again" )
why( "census-acs", ".", "Census API responses written by steps 17 and 27 (tidycensus)" )
why( "census-cog", "/x/", "extracted from the Census of Governments zip files" )
why( "census-cog", ".", "no download recorded in the build scripts (placed by hand)" )
why( "bea-regional", "__ALL_AREAS_", "extracted from the BEA table zip" )
why( "bea-pcepi", ".", "derived by step 06 from BEA's NipaDataM.txt, a file that is revised monthly" )
why( "irs-soi-county", ".", "extracted from the IRS county-income zip files" )
why( "bls-qcew", ".", "QCEW API responses written by step 20" )
why( "sec-edgar", "\\.gz$", "SEC files re-compressed by step 28 (sec_get + gzip); bytes differ from the server's" )
why( "sec-edgar", ".", "SEC filing headers fetched by step 28" )
why( "nccs-core", ".", "our harmonized CORE extracts (an intermediate of the build)" )
why( "nccs-efile", ".", "panel990 downloads and tables (panel990::download_tables), assembled by step 05" )
why( "census-popest", "^hub/", "diagnostic population file placed in the data hub by hand" )
why( "census-geo", ".", "delineation file placed in the hub, and boundaries cached by tigris" )
why( "check-only", ".", "check files placed by hand (not read by the build)" )
m[ method == "frozen" & is.na( note ), note := "placed by hand or from a source with no stable public file (see inputs/<source>/SOURCE.md)" ]
m[ , b := NULL ]
fwrite( m, file.path( EXT, "sources.csv" ) )
print( m[ , .N, keyby = .( source, method ) ], nrows = 100 )
cat( sum( m$method != "frozen" ), "of", nrow( m ), "files can be downloaded again\n" )
