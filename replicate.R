## replicate.R
## One script that replicates the multiverse results of the nonprofit-density project.
## Download this file alone and run it; it fetches everything else from GitHub.
##
##   Rscript replicate.R                      # or source( "replicate.R" ) in R / RStudio
##
## What it does:
##   1. Creates a local multiverse folder (MV_DIR, below) with code/, build/ and library/.
##   2. Downloads the tagged release of github.com/lecy/multiverse (code and the research
##      frames in data/, about 160 MB) into code/.
##   3. Installs the R packages the multiverse needs, at the versions in the release's
##      renv.lock, into library/ (your own R library is not touched).
##   4. Rebuilds the research frames from their per-year slices, checking every file
##      against data/MANIFEST.csv, and runs:
##        - run v11: 432,432 specifications of the 13-covariate default (about 35 minutes
##          on 22 cores; scale by your core count)
##        - its summary (R/multiverse/45_summarize_run.R)
##        - the grassroots refit (R/notes/104_fit_grassroots_v11.R)
##        - the decision matrices of the paper (R/multiverse/98_decision_matrix.R)
##        - with RUN_V10 = TRUE: run v10 (605,352 specifications, about 40 minutes) and the
##          v10/v11 comparison tables (R/multiverse/97_v11_decision_summary.R)
##   Finished steps are kept: run it again and it resumes.
##
## Results: MV_DIR/build/data/spec/run_principled_v11/ (specs.parquet, coefs.parquet and
## summaries), MV_DIR/build/data/outline/v11/ (decision tables) and
## MV_DIR/code/paper/figures/decision_matrix_v11_*.png.

## ---- settings (edit here, or set the environment variables) ------------------------------
MV_DIR   <- Sys.getenv( "MV_DIR", file.path( path.expand( "~" ), "multiverse-replication" ) )
REF      <- Sys.getenv( "MV_REF", "v1.0-data" )        # release tag (or a local .zip, for testing)
WORKERS  <- as.integer( Sys.getenv( "MV_WORKERS", max( 1L, parallel::detectCores() - 2L ) ) )
RUN_V10  <- toupper( Sys.getenv( "MV_RUN_V10", "FALSE" ) ) %in% c( "TRUE", "1" )
PACKAGES <- Sys.getenv( "MV_PACKAGES", "renv" )         # "renv": lockfile versions; "cran": current CRAN
REPO     <- "lecy/multiverse"

say <- function( ... ) message( format( Sys.time(), "%H:%M:%S " ), ... )
dir.create( MV_DIR, recursive = TRUE, showWarnings = FALSE )
MV_DIR <- normalizePath( MV_DIR, winslash = "/" )
CODE <- file.path( MV_DIR, "code" ); BUILD <- file.path( MV_DIR, "build" ); LIB <- file.path( MV_DIR, "library" )
say( "multiverse folder: ", MV_DIR, "  (release ", REF, ", ", WORKERS, " workers)" )

## ---- 1-2. code and data from GitHub ------------------------------------------------------------
stamp <- file.path( CODE, ".release" )
if ( !file.exists( stamp ) || !identical( readLines( stamp, warn = FALSE ), REF ) ) {
  zip <- if ( file.exists( REF ) ) REF else {
    z <- file.path( MV_DIR, paste0( "multiverse-", REF, ".zip" ) )
    url <- sprintf( "https://github.com/%s/archive/refs/tags/%s.zip", REPO, REF )
    say( "downloading ", url )
    old <- options( timeout = max( 3600, getOption( "timeout" ) ) ); on.exit( options( old ), add = TRUE )
    utils::download.file( url, z, mode = "wb", quiet = TRUE )
    z
  }
  tmp <- file.path( MV_DIR, "unzip_tmp" ); unlink( tmp, recursive = TRUE )
  utils::unzip( zip, exdir = tmp )
  top <- list.dirs( tmp, recursive = FALSE )
  if ( length( top ) != 1 ) stop( "unexpected archive layout in ", zip )
  unlink( CODE, recursive = TRUE )
  if ( !file.rename( top, CODE ) ) stop( "could not move the release into ", CODE )
  unlink( tmp, recursive = TRUE )
  writeLines( REF, stamp )
}
stopifnot( file.exists( file.path( CODE, "R", "multiverse", "run_from_release.R" ) ),
           file.exists( file.path( CODE, "data", "MANIFEST.csv" ) ) )

## ---- 3. packages, in a private library ---------------------------------------------------------
NEED <- c( "data.table", "arrow", "digest", "fixest", "lme4", "ggplot2", "ragg", "stringr", "patchwork" )
dir.create( LIB, recursive = TRUE, showWarnings = FALSE )
.libPaths( c( LIB, .libPaths() ) )
missing <- function() NEED[ !vapply( NEED, requireNamespace, TRUE, quietly = TRUE ) ]
if ( length( missing() ) ) {
  if ( PACKAGES == "renv" ) {
    if ( !requireNamespace( "renv", quietly = TRUE ) ) utils::install.packages( "renv", lib = LIB, repos = "https://cloud.r-project.org" )
    say( "installing ", paste( missing(), collapse = ", " ), " at the renv.lock versions into ", LIB )
    renv::restore( lockfile = file.path( CODE, "renv.lock" ), library = LIB, packages = missing(), prompt = FALSE )
  } else {
    say( "installing ", paste( missing(), collapse = ", " ), " from CRAN into ", LIB )
    utils::install.packages( missing(), lib = LIB, repos = "https://cloud.r-project.org" )
  }
}
if ( length( missing() ) ) stop( "packages still missing: ", paste( missing(), collapse = ", " ) )

## ---- 4. rebuild the frames and run the multiverse ---------------------------------------------
## Child R processes inherit these: the private library, the build folder and the options.
Sys.setenv( R_LIBS = paste( .libPaths(), collapse = .Platform$path.sep ), R_LIBS_USER = LIB,
            MV_RUN_V10 = if ( RUN_V10 ) "1" else "0" )
owd <- setwd( CODE ); on.exit( setwd( owd ), add = TRUE )
st <- system2( file.path( R.home( "bin" ), "Rscript" ),
               c( file.path( "R", "multiverse", "run_from_release.R" ), WORKERS, shQuote( BUILD ) ) )
if ( st != 0 ) stop( "the multiverse run failed (exit ", st, "); rerun to resume" )
say( "done. Results:\n  ", file.path( BUILD, "data", "spec", "run_principled_v11" ),
     "\n  ", file.path( BUILD, "data", "outline", "v11" ),
     "\n  ", file.path( CODE, "paper", "figures" ), " (decision_matrix_v11_*.png)" )
