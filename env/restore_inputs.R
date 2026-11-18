## env/restore_inputs.R   (Phase 4/5 of REPRODUCIBILITY-PLAN.md)
## Restores the frozen inputs from s3://lecy/nonprofit-density/inputs/ into four roots,
## so a clean-room build reads nothing else. Download only: never writes to S3.
##
##   Rscript env/restore_inputs.R --build D:/MV_CLEAN/build --inputs D:/MV_CLEAN/inputs \
##           --bls D:/MV_CLEAN/bls --dropbox D:/MV_CLEAN/dropbox [--folders a,b] [--check]
##
## Roots default to MV_BUILD, MV_INPUTS, MV_BLS, MV_DROPBOX (R/lib/config.R).
## Layouts (see env/freeze_inputs.R):
##   <folder>/<root>/<path>, root in build | hub | bls | dropbox -> <that root>/<path>;
##     MANIFEST.csv: key, bytes, sha256, source, build_path, steps, frozen_at
##   soi (frozen earlier): soi/<key> -> MV_INPUTS/SOI/<key>, for keys under 01_data/
##     and 02_sample-framework/ (00_crosswalks and scripts are provenance only)
##   nccs-bmf-unified (frozen earlier): one file -> MV_BUILD/raw/panel990/bmf/
## Every file is checked against its sha256 after download; a mismatch stops the run.
## A file already in place with the manifest's size and hash is skipped, so a rerun resumes.
## --check: after restoring, runs `run.R --raw` with the four roots and stops if any
## declared raw input resolves to no file.
## Needs the AWS CLI with profile lecy-data (AWS_PROFILE overrides).

source( "R/lib/config.R" )
suppressPackageStartupMessages( library( data.table ) )
args <- commandArgs( TRUE )
opt <- function( f, d ) { i <- match( f, args ); if ( is.na( i ) ) d else args[ i + 1 ] }
ROOT <- c( build = opt( "--build", BUILD ), hub = opt( "--inputs", DATA_HUB ),
           bls = opt( "--bls", BLS_DIR ), dropbox = opt( "--dropbox", DROPBOX ) )
FOLDERS <- { f <- opt( "--folders", "" ); if ( nzchar( f ) ) strsplit( f, "," )[[ 1 ]] else character( 0 ) }
CHECK <- "--check" %in% args
PROFILE <- Sys.getenv( "AWS_PROFILE", "lecy-data" )
SRC <- "s3://lecy/nonprofit-density/inputs"
UNIFIED <- list( key = "nccs-bmf/unified-bmf-geocoded/2026-09-21/bmf_unified_geocoded.parquet",
                 bytes = 641496865, sha256 = "c296860c572822404dc55861af89ea13ce11b48f51030313d97d067f1766a3ad",
                 dest = "raw/panel990/bmf/bmf_unified_geocoded.parquet" )
cat( "restoring into:\n" ); print( ROOT )

aws <- function( ..., ok = 0L ) {
  a <- c( ..., "--profile", PROFILE, if ( identical( ..2, "cp" ) ) "--only-show-errors" )
  out <- suppressWarnings( system2( "aws", shQuote( a ), stdout = TRUE, stderr = TRUE ) )
  st <- attr( out, "status" ); if ( !is.null( st ) && !st %in% ok ) stop( "aws ", paste( a[ 1:2 ], collapse = " " ), " failed: ", paste( out, collapse = " " ) )
  out
}
sha256 <- function( f ) digest::digest( f, algo = "sha256", file = TRUE )
have_ok <- function( f, bytes, h ) file.exists( f ) && file.size( f ) == bytes && identical( sha256( f ), h )
fetch <- function( key, dest, bytes, h ) {
  if ( have_ok( dest, bytes, h ) ) return( "kept" )
  dir.create( dirname( dest ), recursive = TRUE, showWarnings = FALSE )
  tmp <- paste0( dest, ".part" )
  aws( "s3", "cp", paste0( SRC, "/", key ), tmp )
  got <- sha256( tmp )
  if ( !identical( got, h ) ) { unlink( tmp ); stop( "sha256 mismatch for ", key, ": manifest ", h, ", downloaded ", got ) }
  if ( file.exists( dest ) ) unlink( dest )
  stopifnot( file.rename( tmp, dest ) )
  "restored"
}
read_manifest <- function( folder ) {
  f <- tempfile( fileext = ".csv" ); aws( "s3", "cp", paste0( SRC, "/", folder, "/MANIFEST.csv" ), f )
  m <- fread( f, colClasses = list( character = "sha256" ) ); unlink( f ); m
}

## ---- folders on S3 ----------------------------------------------------------------------
top <- aws( "s3", "ls", paste0( SRC, "/" ) )
folders <- sub( "/$", "", sub( "^\\s*PRE\\s+", "", grep( "PRE ", top, value = TRUE ) ) )
folders <- c( setdiff( folders, "nccs-bmf" ), "nccs-bmf-unified" )        # nccs-bmf/ holds the unified BMF only
if ( length( FOLDERS ) ) { stopifnot( all( FOLDERS %in% folders ) ); folders <- intersect( folders, FOLDERS ) }

log <- list()
for ( fo in folders ) {
  if ( fo == "nccs-bmf-unified" ) {
    st <- fetch( UNIFIED$key, file.path( ROOT[[ "build" ]], UNIFIED$dest ), UNIFIED$bytes, UNIFIED$sha256 )
    log[[ fo ]] <- data.table( folder = fo, files = 1L, restored = sum( st == "restored" ), kept = sum( st == "kept" ), bytes = UNIFIED$bytes )
  } else {
    m <- read_manifest( fo )
    if ( fo == "soi" ) {
      m <- m[ grepl( "^(01_data|02_sample-framework)/", key ) ]
      m[ , dest := file.path( ROOT[[ "hub" ]], "SOI", key ) ]
    } else {
      m[ , root := sub( "/.*", "", key ) ]; stopifnot( all( m$root %in% names( ROOT ) ) )
      m[ , dest := file.path( ROOT[ root ], sub( "^[^/]+/", "", key ) ) ]
    }
    m[ , key := paste0( fo, "/", key ) ]
    st <- vapply( seq_len( nrow( m ) ), function( i ) fetch( m$key[ i ], m$dest[ i ], m$bytes[ i ], m$sha256[ i ] ), "" )
    log[[ fo ]] <- data.table( folder = fo, files = nrow( m ), restored = sum( st == "restored" ), kept = sum( st == "kept" ), bytes = sum( m$bytes ) )
  }
  cat( format( Sys.time(), "%H:%M:%S" ), fo, ":", log[[ fo ]]$restored, "restored,", log[[ fo ]]$kept, "already in place\n" )
}
res <- rbindlist( log )
print( res )
cat( sprintf( "%d files, %.2f GB, all sha256 verified\n", sum( res$files ), sum( res$bytes ) / 1e9 ) )

## ---- check: every declared raw input resolves under the restored roots ------------------
if ( CHECK ) {
  rscript <- file.path( R.home( "bin" ), if ( .Platform$OS.type == "windows" ) "Rscript.exe" else "Rscript" )
  ## ~/.Renviron would override inherited MV_* variables in the child, so the child
  ## reads a temporary .Renviron with the four roots instead
  renv_f <- tempfile( fileext = ".Renviron" )
  writeLines( c( paste0( "MV_BUILD=", ROOT[[ "build" ]] ), paste0( "MV_INPUTS=", ROOT[[ "hub" ]] ),
                 paste0( "MV_BLS=", ROOT[[ "bls" ]] ), paste0( "MV_DROPBOX=", ROOT[[ "dropbox" ]] ) ), renv_f )
  old <- Sys.getenv( "R_ENVIRON_USER", unset = NA ); Sys.setenv( R_ENVIRON_USER = renv_f )
  out <- system2( rscript, c( "run.R", "--raw" ), stdout = TRUE, stderr = TRUE )
  if ( is.na( old ) ) Sys.unsetenv( "R_ENVIRON_USER" ) else Sys.setenv( R_ENVIRON_USER = old ); unlink( renv_f )
  if ( !any( grepl( ROOT[[ "build" ]], out, fixed = TRUE ) ) ) stop( "run.R --raw did not use the restored roots:\n", paste( tail( out, 3 ), collapse = "\n" ) )
  cat( tail( out, 3 ), sep = "\n" )
  r <- fread( file.path( ROOT[[ "build" ]], "raw_inputs.csv" ) )
  miss <- r[ files == 0 ]
  if ( nrow( miss ) ) { print( miss[ , .( step, source, entry ) ], nrows = 200 ); stop( nrow( miss ), " declared raw inputs resolve to no file under the restored roots" ) }
  cat( "CHECK PASS:", nrow( r ), "raw entries resolve\n" )
}
