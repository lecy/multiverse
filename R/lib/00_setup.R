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

## ---- the project package --------------------------------------------------
## Constants (YEARS, POP_YEARS, FLOOR_REAL_2010, NTEE_HOSP, NTEE_UNIV, NTEE_INVALID,
## COUNTY_RECODE) and helpers (pad, log_msg, recode_county, the NTEE rule, the geo
## helpers, winsor/flog/slog/resid_logP, mv_fetch) come from npdensity (github.com/Nonprofit-Open-Data-Collective/npdensity; R/lib/load_pkg.R).
source( "R/lib/load_pkg.R" )
options( npdensity.geo_dir = OUT$geo )                 # where the geo helpers find step 01's lookups

## Valdez-Cordova (02261) split into Chugach (02063) and Copper River (02066)
## in 2019; earlier values are split by 2020 population shares (set in 01_geo)

## download once into a cache folder; returns the local path
fetch <- function( url, dir, file = basename( url ), overwrite = FALSE )
  mv_fetch( url, dir, file, overwrite = overwrite )

## ---- NTEE codes ----------------------------------------------------------
## One rule for invalid NTEE codes, used by every step that reads them
## (02, 02b, 06, 08, 23, 24; checks 10b, 10c); see ?npdensity::bad_ntee.
## 02_npo_bmf_counts.R writes each EIN's most recent valid code across the legacy
## BMF snapshots to data/npo/ein_ntee_best.csv.gz; resolve_ntee() replaces invalid
## codes with it. These two bind the package functions to this build's lookup file.
NTEE_BEST <- file.path( OUT$npo, "ein_ntee_best.csv.gz" )
read_ntee_best <- function( f = NTEE_BEST ) npdensity::read_ntee_best( f )
resolve_ntee <- function( dt, ein = "ein", ntee = "ntee3", best = read_ntee_best() )
  npdensity::resolve_ntee( dt, ein, ntee, best )
