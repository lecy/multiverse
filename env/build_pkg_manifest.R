## env/build_pkg_manifest.R
## Builds pkg/inst/extdata/manifest.csv, the list of files the package getters can fetch,
## each with its sha256 (PACKAGE-PLAN.md Step 3). Read only: never writes to S3.
##   Rscript env/build_pkg_manifest.R [runs_dir=D:/MV_CLEAN/build/data/spec]
## Rows (class, release, item, file, bytes, sha256, location):
##   frame  the research frames of release v1.0-data: the slices in data/ and MANIFEST.csv,
##          FRAMES.csv and variable_dictionary.csv (item "frames"); location = path in data/
##   run    the runs built from those frames in the Phase 5 clean room (v11, its grassroots
##          refit, v10): specs.parquet and coefs.parquet; location = release asset name
##   input  the frozen inputs, from each folder's MANIFEST.csv on
##          s3://lecy/nonprofit-density/inputs/ (release freeze-2026-11); file = <root>/<path>,
##          location = key under the freeze. Layouts as in env/restore_inputs.R.
## Needs the AWS CLI with profile lecy-data (AWS_PROFILE overrides).

suppressPackageStartupMessages( library( data.table ) )
a <- commandArgs( TRUE )
RUNS <- if ( length( a ) ) a[1] else "D:/MV_CLEAN/build/data/spec"
PROFILE <- Sys.getenv( "AWS_PROFILE", "lecy-data" )
SRC <- "s3://lecy/nonprofit-density/inputs"
REL <- "v1.0-data"; FREEZE <- "freeze-2026-11"
sha <- function( f ) digest::digest( f, algo = "sha256", file = TRUE )
aws <- function( ... ) { o <- suppressWarnings( system2( "aws", shQuote( c( ..., "--profile", PROFILE ) ), stdout = TRUE, stderr = TRUE ) )
  if ( !is.null( attr( o, "status" ) ) ) stop( "aws failed: ", paste( o, collapse = " " ) ); o }

## ---- frames: the files of data/ at the release tag -----------------------------------------
## files of the tag that have changed since (files added after the tag, e.g. spec_dictionary.csv, are not part of the release)
stopifnot( length( system2( "git", c( "diff", "--name-only", "--diff-filter=MD", REL, "--", "data" ), stdout = TRUE ) ) == 0 )
man <- fread( "data/MANIFEST.csv", colClasses = list( character = "sha256" ) )
fr <- man[ , .( class = "frame", release = REL, item = frame, file, bytes, sha256, location = file ) ]
## the text files are hashed as the tag stores them (git blobs, LF line endings), which is what
## GitHub serves; a Windows checkout with core.autocrlf has CRLF and other bytes
blob <- function( p ) { f <- tempfile(); st <- system2( "git", c( "cat-file", "blob", paste0( REL, ":", p ) ), stdout = f )
  stopifnot( st == 0 ); f }
for ( f in c( "MANIFEST.csv", "FRAMES.csv", "variable_dictionary.csv" ) ) {
  b <- blob( paste0( "data/", f ) )
  fr <- rbind( fr, data.table( class = "frame", release = REL, item = "frames", file = f, bytes = file.size( b ), sha256 = sha( b ), location = f ) )
}
sl <- fr[ item != "frames" ]
chk <- vapply( sl$file, function( p ) sha( blob( paste0( "data/", p ) ) ), "" )
stopifnot( all( chk == sl$sha256 ) )                                     # the slices in the tag are the ones in MANIFEST.csv

## ---- runs ------------------------------------------------------------------------------------
ru <- rbindlist( lapply( c( "principled_v11", "principled_v11_grass", "principled_v10" ), function( r ) {
  rbindlist( lapply( c( "specs.parquet", "coefs.parquet" ), function( f ) {
    p <- file.path( RUNS, paste0( "run_", r ), f )
    data.table( class = "run", release = REL, item = r, file = f, bytes = file.size( p ), sha256 = sha( p ),
                location = paste0( "run_", r, "_", f ) ) } ) ) } ) )

## ---- inputs ----------------------------------------------------------------------------------
top <- aws( "s3", "ls", paste0( SRC, "/" ) )
folders <- sub( "/$", "", sub( "^[[:space:]]*PRE[[:space:]]+", "", grep( "PRE ", top, value = TRUE ) ) )
inp <- list()
for ( fo in setdiff( folders, "nccs-bmf" ) ) {
  f <- tempfile( fileext = ".csv" )
  ok <- tryCatch( { aws( "s3", "cp", paste0( SRC, "/", fo, "/MANIFEST.csv" ), f, "--only-show-errors" ); TRUE }, error = function( e ) FALSE )
  if ( !ok ) { message( fo, ": no MANIFEST.csv, skipped" ); next }
  m <- fread( f, colClasses = list( character = "sha256" ) ); unlink( f )
  if ( fo == "soi" ) {
    m <- m[ grepl( "^(01_data|02_sample-framework)/", key ) ]
    inp[[ fo ]] <- m[ , .( class = "input", release = FREEZE, item = fo, file = paste0( "hub/SOI/", key ), bytes, sha256,
                           location = paste0( fo, "/", key ) ) ]
  } else inp[[ fo ]] <- m[ , .( class = "input", release = FREEZE, item = fo, file = key, bytes, sha256, location = paste0( fo, "/", key ) ) ]
  message( fo, ": ", nrow( inp[[ fo ]] ), " files" )
}
## the unified BMF, frozen earlier as one file (see env/restore_inputs.R)
inp$unified <- data.table( class = "input", release = FREEZE, item = "nccs-bmf-unified",
                           file = "build/raw/panel990/bmf/bmf_unified_geocoded.parquet", bytes = 641496865,
                           sha256 = "c296860c572822404dc55861af89ea13ce11b48f51030313d97d067f1766a3ad",
                           location = "nccs-bmf/unified-bmf-geocoded/2026-09-21/bmf_unified_geocoded.parquet" )
out <- rbind( fr, ru, rbindlist( inp ) )
stopifnot( !anyDuplicated( out[ , .( class, release, item, file ) ] ) )
dir.create( "pkg/inst/extdata", recursive = TRUE, showWarnings = FALSE )
fwrite( out, "pkg/inst/extdata/manifest.csv" )
print( out[ , .( files = .N, GB = round( sum( bytes ) / 1e9, 2 ) ), by = .( class, release ) ] )
