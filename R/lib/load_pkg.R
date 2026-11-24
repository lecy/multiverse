## load_pkg.R
## Attaches the project package npdensity (pkg/), installing it first when it is missing
## or older than the sources in pkg/. Sourced by 00_setup.R; working directory = the
## multiverse/ project folder.
##
## The installed copy records the sha256 of the sources it was built from
## (SOURCE_SHA256 in its folder). When pkg/ changes, the next script that sources
## 00_setup.R reinstalls it into the first library on .libPaths() (the renv library,
## the clean room's library, or replicate.R's private library). Steps run one at a time,
## and a step sources this before it starts workers, so workers find it current.

local( {
  src <- sort( c( list.files( "pkg/R", full.names = TRUE ), list.files( "pkg/inst", full.names = TRUE, recursive = TRUE ),
                  "pkg/DESCRIPTION", "pkg/NAMESPACE" ) )
  if ( !length( src ) || !file.exists( "pkg/DESCRIPTION" ) ) stop( "pkg/ not found: run from the multiverse/ folder" )
  h <- digest::digest( paste( vapply( src, digest::digest, "", algo = "sha256", file = TRUE ), collapse = "" ),
                       "sha256", serialize = FALSE )
  f <- system.file( "SOURCE_SHA256", package = "npdensity" )
  if ( !identical( if ( nzchar( f ) ) readLines( f, warn = FALSE ) else "", h ) ) {
    lib <- .libPaths()[1]
    message( "installing npdensity from pkg/ into ", lib )
    if ( "npdensity" %in% loadedNamespaces() ) unloadNamespace( "npdensity" )
    st <- system2( file.path( R.home( "bin" ), "R" ),
                   c( "CMD", "INSTALL", "--no-docs", "--no-multiarch", "--no-test-load",
                      paste0( "--library=", shQuote( lib ) ), "pkg" ), stdout = FALSE, stderr = FALSE )
    if ( st != 0 ) stop( "R CMD INSTALL pkg failed (exit ", st, ")" )
    writeLines( h, file.path( lib, "npdensity", "SOURCE_SHA256" ) )
  }
} )
suppressPackageStartupMessages( library( npdensity ) )
