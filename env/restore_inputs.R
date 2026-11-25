## env/restore_inputs.R   (Phase 4/5 of REPRODUCIBILITY-PLAN.md)
## Restores the frozen inputs from s3://lecy/nonprofit-density/inputs/ into four roots,
## so a clean-room build reads nothing else. Download only: never writes to S3.
##
##   Rscript env/restore_inputs.R --build D:/MV_CLEAN/build --inputs D:/MV_CLEAN/inputs \
##           --bls D:/MV_CLEAN/bls --dropbox D:/MV_CLEAN/dropbox [--folders a,b] [--check]
##
## Roots default to MV_BUILD, MV_INPUTS, MV_BLS, MV_DROPBOX (R/lib/config.R).
## The work is npdensity::mv_get_inputs(): the files, their destinations and their sha256 come
## from the package manifest (npdensity::mv_manifest(), built by env/build_pkg_manifest.R
## from each folder's MANIFEST.csv; layouts in env/freeze_inputs.R). Every file is checked
## after download; a mismatch stops the run. A file already in place with the manifest's size
## and hash is kept, so a rerun resumes. After a new freeze, rebuild the manifest first.
## --check: after restoring, runs `run.R --raw` with the four roots and stops if any
## declared raw input resolves to no file.
## Needs the AWS CLI with profile lecy-data (AWS_PROFILE overrides).

source( "R/lib/config.R" )
source( "R/lib/load_pkg.R" )
suppressPackageStartupMessages( library( data.table ) )
args <- commandArgs( TRUE )
opt <- function( f, d ) { i <- match( f, args ); if ( is.na( i ) ) d else args[ i + 1 ] }
ROOT <- c( build = opt( "--build", BUILD ), hub = opt( "--inputs", DATA_HUB ),
           bls = opt( "--bls", BLS_DIR ), dropbox = opt( "--dropbox", DROPBOX ) )
FOLDERS <- { f <- opt( "--folders", "" ); if ( nzchar( f ) ) strsplit( f, "," )[[ 1 ]] else NULL }
CHECK <- "--check" %in% args
cat( "restoring into:\n" ); print( ROOT )

res <- mv_get_inputs( FOLDERS, roots = ROOT )
print( res )
cat( sprintf( "%d files, %.2f GB, all sha256 verified\n", sum( res$files ), sum( as.numeric( res$bytes ) ) / 1e9 ) )

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
