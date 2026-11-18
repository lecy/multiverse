## docs/make_site_data.R
## Writes the small CSVs the website reads (site/data/), so the site renders without
## AWS access or the build drive. Rerun after a freeze or a change to inputs/.
##   Rscript docs/make_site_data.R
## Reads: s3://lecy/nonprofit-density/inputs/<folder>/MANIFEST.csv (AWS profile
## lecy-data), inputs/*/SOURCE.md, docs/data_inventory.csv, pipeline.csv.
## Writes: site/data/frozen_manifest.csv  one row per frozen file
##         site/data/sources.csv          one row per source folder
##         site/data/pipeline_steps.csv   one row per pipeline step

suppressPackageStartupMessages( library( data.table ) )
PROFILE <- Sys.getenv( "AWS_PROFILE", "lecy-data" )
SRC <- "s3://lecy/nonprofit-density/inputs"
OUT <- "site/data"; dir.create( OUT, showWarnings = FALSE, recursive = TRUE )

## ---- frozen files, from the S3 manifests -------------------------------------------
s3_manifest <- function( folder ) {
  f <- tempfile( fileext = ".csv" )
  st <- system2( "aws", c( "s3", "cp", shQuote( paste0( SRC, "/", folder, "/MANIFEST.csv" ) ), shQuote( f ),
                           "--profile", PROFILE, "--only-show-errors" ) )
  if ( st != 0 ) stop( "could not read the manifest for ", folder )
  m <- fread( f, colClasses = list( character = "sha256" ) ); unlink( f ); m
}
folders <- basename( dirname( Sys.glob( "inputs/*/SOURCE.md" ) ) )
man <- rbindlist( lapply( setdiff( folders, "nccs-bmf-unified" ), function( fo ) {
  m <- s3_manifest( fo )
  if ( fo == "soi" ) m <- m[ grepl( "^(01_data|02_sample-framework)/", key ) ]   # what restore uses
  data.table( folder = fo, path = m$key, bytes = m$bytes, sha256 = m$sha256,
              steps = if ( "steps" %in% names( m ) ) m$steps else "" )
} ) )
man <- rbind( man, data.table( folder = "nccs-bmf-unified",
  path = "unified-bmf-geocoded/2026-09-21/bmf_unified_geocoded.parquet", bytes = 641496865,
  sha256 = "c296860c572822404dc55861af89ea13ce11b48f51030313d97d067f1766a3ad", steps = "05" ) )
setorder( man, folder, path )
fwrite( man, file.path( OUT, "frozen_manifest.csv" ) )

## ---- one row per source -----------------------------------------------------------
field <- function( lines, names ) {
  pat <- paste0( "^\\|\\s*(", paste( names, collapse = "|" ), ")\\s*\\|\\s*(.*?)\\s*\\|\\s*$" )
  hit <- regmatches( lines, regexec( pat, lines, perl = TRUE ) )
  hit <- Filter( length, hit ); if ( length( hit ) ) hit[[ 1 ]][ 3 ] else ""
}
CITE <- c(                                    # folder -> references.bib keys
  "bea-pcepi" = "data_bea_pcepi", "bea-regional" = "data_bea_regional",
  "bls-nonprofits" = "data_bls_nonprofits", "bls-qcew" = "data_bls_qcew",
  "census-acs" = "data_census_acs; data_census_sf3", "census-cog" = "data_census_cog",
  "census-geo" = "data_census_tiger; data_omb_cbsa", "census-hist" = "data_census_hist",
  "census-pep-char" = "data_census_pep_char", "census-popest" = "data_census_popest",
  "census-saipe" = "data_census_saipe", "check-only" = "data_irs_1023ez; data_nccs_ntee_crosswalk",
  "elections" = "data_medsl_county; data_mcgovern_elections", "ers-laus" = "data_ers_laus",
  "irs-soi-county" = "data_irs_soi_county", "nccs-bmf-legacy" = "data_nccs_bmf_legacy",
  "nccs-bmf-long-panel" = "data_bmf_long_panel", "nccs-bmf-monthly" = "data_nccs_bmf_monthly",
  "nccs-bmf-unified" = "data_nccs_bmf_unified", "nccs-core" = "data_nccs_core",
  "nccs-efile" = "data_nccs_efile", "patents" = "data_patentsview",
  "religion-census" = "data_rcms_2000_2010; data_rcms_2020",
  "social-capital" = "data_nercrd_socap; rupasingha2006production",
  "soi" = "data_irs_soi_eo; data_nccs_bmf_unified" )
stopifnot( all( folders %in% names( CITE ) ) )
inv <- fread( "docs/data_inventory.csv" )
src <- rbindlist( lapply( folders, function( fo ) {
  l <- readLines( file.path( "inputs", fo, "SOURCE.md" ), encoding = "UTF-8", warn = FALSE )
  data.table( folder = fo, title = sub( "^#\\s*", "", l[ 1 ] ),
              publisher = field( l, c( "Publisher", "Producer", "Sources" ) ),
              years = field( l, c( "Years used" ) ),
              licence = field( l, c( "Licence" ) ),
              cite = CITE[[ fo ]] )
} ) )
src <- merge( src, inv[ , .( folder, archive = status ) ], by = "folder", all.x = TRUE )
src <- merge( src, man[ , .( files = .N, gb = round( sum( bytes ) / 1e9, 2 ) ), by = folder ], by = "folder", all.x = TRUE )
fwrite( src, file.path( OUT, "sources.csv" ) )

## ---- pipeline steps -----------------------------------------------------------------
pl <- fread( "pipeline.csv", colClasses = "character" )
fwrite( pl[ , .( id, stage, script, args, inputs, outputs, notes ) ], file.path( OUT, "pipeline_steps.csv" ) )

cat( sprintf( "%d frozen files (%.2f GB) in %d folders; %d sources; %d steps\n",
              nrow( man ), sum( man$bytes ) / 1e9, uniqueN( man$folder ), nrow( src ), nrow( pl ) ) )
