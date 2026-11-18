## 00_setup.R
## Shared paths, constants, and helpers for the multiverse data build.
## Every build script starts with: source( "R/lib/00_setup.R" )
## Working directory = the multiverse/ project folder.

suppressPackageStartupMessages( {
  library( data.table )
} )

## ---- paths -------------------------------------------------------------

## Scripts live in Dropbox; every build product, download cache, temp file,
## and log lives under BUILD. Read-only source data stays where it is in the
## data hub. The locations come from environment variables (see R/lib/config.R):
##   DROPBOX, DATA_HUB (MV_INPUTS), BLS_DIR (MV_BLS), BUILD (MV_BUILD),
##   PROJECT_ROOT, census_key()

source( "R/lib/config.R" )

TMP <- file.path( BUILD, "tmp" )                       # large temp downloads
LOGS <- file.path( BUILD, "logs" )

RAW <- list(
  census_popest = file.path( BUILD, "raw", "census-popest" ),   # downloaded PEP files
  core_hrmn     = file.path( BUILD, "raw", "nccs-core-hrmn" ),  # harmonized CORE extracts
  panel990      = file.path( BUILD, "raw", "panel990" ),        # panel990 efile cache
  bmf_snap      = file.path( DATA_HUB, "00-NCCS", "bmf" ),   # NCCS BMF snapshots
  soi_frame     = file.path( DATA_HUB, "SOI", "02_sample-framework",
                             "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED.CSV" ),
  cbsa_xw       = file.path( DATA_HUB, "population", "cbsa2fipsxw.csv" ),
  bls           = BLS_DIR
)

OUT <- list(
  geo  = file.path( BUILD, "data", "geo" ),
  pop  = file.path( BUILD, "data", "pop" ),
  npo  = file.path( BUILD, "data", "npo" ),
  diag = file.path( BUILD, "data", "diagnostics" ),
  fig  = file.path( BUILD, "data", "diagnostics", "figures" ),
  spec = file.path( BUILD, "data", "spec" ),
  cov  = file.path( BUILD, "data", "cov" ),           # covariates, one file per source
  raw_cov = file.path( BUILD, "raw", "cov" )          # covariate download cache
)

for ( p in c( unlist( OUT ), RAW$census_popest, RAW$core_hrmn, RAW$panel990, TMP, LOGS ) )
  dir.create( p, recursive = TRUE, showWarnings = FALSE )

## keep large temp files (downloads, arrow/duckdb spill) off C:
tmpfile <- function( ext = "" ) tempfile( tmpdir = TMP, fileext = ext )
Sys.setenv( TMPDIR = TMP, TEMP = TMP, TMP = TMP )      # inherited by child processes

## ---- constants ---------------------------------------------------------

YEARS <- 2000:2024                 # analysis panel window
POP_YEARS <- 2000:2025             # population carried one year beyond

## real-dollar floor for the active-filer universe (plan section 2.3)
FLOOR_REAL_2010 <- 50000

## hospital and university NTEE codes (plan section 2.5)
NTEE_HOSP <- c( "E20", "E21", "E22", "E23", "E24" )
NTEE_UNIV <- c( "B40", "B41", "B42", "B43", "B50" )

## ---- helpers -----------------------------------------------------------

## left-pad numeric/character codes with zeros
pad <- function( x, n ) stringr::str_pad( as.character( x ), n, "left", "0" )

## download once into a cache folder; returns the local path
fetch <- function( url, dir, file = basename( url ), overwrite = FALSE ) {
  dest <- file.path( dir, file )
  if ( file.exists( dest ) && !overwrite && file.size( dest ) > 0 ) return( dest )
  old <- options( timeout = max( 3600, getOption( "timeout" ) ) )
  on.exit( options( old ) )
  message( "downloading ", url )
  utils::download.file( url, dest, mode = "wb", quiet = TRUE )
  dest
}

## county-equivalent boundary fixes onto 2023 county-equivalents
## (CT handled separately through towns -> planning regions)
COUNTY_RECODE <- data.table(
  from = c( "51515", "46113", "02270" ),
  to   = c( "51019", "46102", "02158" ),
  note = c( "Bedford city merged into Bedford County (2013)",
            "Shannon County renamed Oglala Lakota (2015)",
            "Wade Hampton renamed Kusilvak (2015)" ) )

## Valdez-Cordova (02261) split into Chugach (02063) and Copper River (02066)
## in 2019; earlier values are split by 2020 population shares (set in 01_geo)

log_msg <- function( ... ) message( format( Sys.time(), "%H:%M:%S " ), ... )

## ---- NTEE codes ----------------------------------------------------------
## One rule for invalid NTEE codes, used by every step that reads them
## (02, 02b, 06, 08, 23, 24; checks 10b, 10c). Invalid = blank, "UND" (UNDEFINED),
## "INV" (INVALID) or "Z99" (unknown). 02_npo_bmf_counts.R writes each EIN's most
## recent valid code across the legacy BMF snapshots to data/npo/ein_ntee_best.csv.gz;
## resolve_ntee() replaces invalid codes with it and sets the rest to NA. Change the
## rule here, rebuild from 02, and every later step picks it up.
NTEE_INVALID <- c( "UND", "INV", "Z99" )
NTEE_BEST <- file.path( OUT$npo, "ein_ntee_best.csv.gz" )

bad_ntee <- function( x ) is.na( x ) | trimws( x ) == "" | toupper( substr( trimws( x ), 1, 3 ) ) %in% NTEE_INVALID

## EIN -> latest valid code; d has EIN, year, ntee3 (one row per EIN-year)
ntee_best_from <- function( d ) {
  d <- d[ !bad_ntee( ntee3 ), .( EIN = pad( gsub( "[^0-9]", "", EIN ), 9 ), year,
                                 ntee_best = toupper( substr( trimws( ntee3 ), 1, 3 ) ) ) ]
  unique( d[ order( EIN, -year ) ], by = "EIN" )[ , .( EIN, ntee_best ) ]
}

read_ntee_best <- function( f = NTEE_BEST ) {
  if ( !file.exists( f ) ) stop( "missing ", f, ": run R/build/B_npo/02_npo_bmf_counts.R first" )
  fread( f, colClasses = "character" )
}

## Normalizes dt[[ ntee ]] to 3 upper-case characters, replaces invalid codes with the
## EIN's best code, sets unresolved ones to NA (by reference). Returns, per row,
## "valid", "filled" or "unresolved", so callers can tabulate the backfill.
resolve_ntee <- function( dt, ein = "ein", ntee = "ntee3", best = read_ntee_best() ) {
  code <- toupper( substr( trimws( as.character( dt[[ ntee ]] ) ), 1, 3 ) )
  bad <- bad_ntee( code )
  fill <- best$ntee_best[ match( pad( gsub( "[^0-9]", "", dt[[ ein ]][ bad ] ), 9 ), best$EIN ) ]
  code[ bad ] <- fill
  set( dt, j = ntee, value = code )
  status <- rep( "valid", length( code ) )
  status[ bad ] <- fifelse( is.na( fill ), "unresolved", "filled" )
  status
}
