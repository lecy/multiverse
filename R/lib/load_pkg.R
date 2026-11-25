## load_pkg.R
## Attaches npdensity, the package that holds this project's shared code
## (github.com/Nonprofit-Open-Data-Collective/npdensity), installing it first when the
## installed copy is not the one this compendium asks for. Sourced by 00_setup.R and the
## env/ scripts; working directory = the multiverse/ project folder.
##
## Two sources, in this order:
##  1. A local checkout of the package (development). Its path is the environment variable
##     NPDENSITY_SRC, or the first line of env/npdensity_path.txt (not in git: one per
##     machine). The installed copy records the sha256 of the sources it was built from;
##     when the checkout changes, the next script that sources this reinstalls it.
##  2. Otherwise the commit pinned in env/npdensity.ref, downloaded from GitHub. This is what
##     a replicator gets: the package code the results in this tag were made with.
##
## The package is installed into the first library on .libPaths() (the renv library, the
## clean room's library, or replicate.R's private library). Steps run one at a time, and a
## step sources this before it starts workers, so workers find it current.

NPDENSITY_REPO <- "Nonprofit-Open-Data-Collective/npdensity"

## the local checkout of the package, or "" when there is none
npdensity_src <- function() {
  p <- Sys.getenv( "NPDENSITY_SRC" )
  f <- "env/npdensity_path.txt"
  if ( !nzchar( p ) && file.exists( f ) ) p <- trimws( readLines( f, n = 1, warn = FALSE ) )
  if ( nzchar( p ) && !file.exists( file.path( p, "DESCRIPTION" ) ) )
    stop( "no package at ", p, " (NPDENSITY_SRC or env/npdensity_path.txt)" )
  p
}

## inst/extdata of the local checkout, where env/build_pkg_manifest.R, env/build_sources.R
## and env/build_spec_dictionary.R write the package's data files
npdensity_extdata <- function() {
  p <- npdensity_src()
  if ( !nzchar( p ) ) stop( "no local npdensity checkout: set NPDENSITY_SRC or write its path to env/npdensity_path.txt" )
  file.path( p, "inst", "extdata" )
}

local( {
  src <- npdensity_src()
  if ( nzchar( src ) ) {
    files <- sort( c( list.files( file.path( src, "R" ), full.names = TRUE ),
                      list.files( file.path( src, "inst" ), full.names = TRUE, recursive = TRUE ),
                      file.path( src, c( "DESCRIPTION", "NAMESPACE" ) ) ) )
    want <- digest::digest( paste( vapply( files, digest::digest, "", algo = "sha256", file = TRUE ), collapse = "" ),
                            "sha256", serialize = FALSE )
  } else {
    if ( !file.exists( "env/npdensity.ref" ) ) stop( "env/npdensity.ref not found: run from the multiverse/ folder" )
    ref  <- trimws( readLines( "env/npdensity.ref", n = 1, warn = FALSE ) )
    if ( !nzchar( ref ) ) stop( "env/npdensity.ref is empty: pin a commit of ", NPDENSITY_REPO )
    want <- paste0( "github:", ref )
  }
  f <- system.file( "SOURCE_SHA256", package = "npdensity" )
  if ( identical( if ( nzchar( f ) ) readLines( f, warn = FALSE ) else "", want ) ) return( invisible() )

  lib <- .libPaths()[1]
  if ( !nzchar( src ) ) {
    ## the pinned commit, as a source tarball from GitHub
    url <- sprintf( "https://codeload.github.com/%s/tar.gz/%s", NPDENSITY_REPO, ref )
    tmp <- tempfile( "npdensity_" ); dir.create( tmp ); on.exit( unlink( tmp, recursive = TRUE ), add = TRUE )
    message( "downloading npdensity ", ref, " from github.com/", NPDENSITY_REPO )
    old <- options( timeout = max( 600, getOption( "timeout" ) ) ); on.exit( options( old ), add = TRUE )
    utils::download.file( url, file.path( tmp, "src.tar.gz" ), mode = "wb", quiet = TRUE )
    utils::untar( file.path( tmp, "src.tar.gz" ), exdir = tmp )
    src <- list.dirs( tmp, recursive = FALSE )[1]
  }
  message( "installing npdensity from ", src, " into ", lib )
  if ( "npdensity" %in% loadedNamespaces() ) unloadNamespace( "npdensity" )
  st <- system2( file.path( R.home( "bin" ), "R" ),
                 c( "CMD", "INSTALL", "--no-docs", "--no-multiarch", "--no-test-load",
                    paste0( "--library=", shQuote( lib ) ), shQuote( src ) ), stdout = FALSE, stderr = FALSE )
  if ( st != 0 ) stop( "R CMD INSTALL of npdensity failed (exit ", st, ")" )
  writeLines( want, file.path( lib, "npdensity", "SOURCE_SHA256" ) )
} )
suppressPackageStartupMessages( library( npdensity ) )
