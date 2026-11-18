## config.R
## Machine-specific locations, read from environment variables. Sourced first by
## 00_setup.R, so every script gets them through source( "R/lib/00_setup.R" ).
## Working directory = the multiverse/ project folder.
##
## Set these in your user-level ~/.Renviron (Documents/.Renviron on Windows), never
## in a .Renviron inside the project: the project folder syncs between machines.
##
##   MV_BUILD        build outputs, download caches, temp files, logs
##                   default D:/MULTIVERSE
##   MV_INPUTS       read-only source-data hub ("00 - DATA")
##                   default <Dropbox>/00 - DATA
##   MV_BLS          BLS nonprofit QCEW research files
##                   default <Dropbox>/00 - URBAN/00-DATA/bls
##   MV_DROPBOX      Dropbox root, used only for the two defaults above and the
##                   replication folder read by 18_cov_rcms.R
##                   default: four folders above the project
##   CENSUS_API_KEY  Census API key. If unset, read from ../CENSUS-DATA.R
##                   (outside the project; never copy that file into it)
##
## MV_PROJECT_ROOT is set here (not read) so that parallel workers, which inherit
## the environment, can setwd( Sys.getenv( "MV_PROJECT_ROOT" ) ).

local( {
  if ( !file.exists( file.path( "R", "lib", "00_setup.R" ) ) )
    stop( "run from the multiverse/ project folder (getwd() is ", getwd(), ")" )
} )

PROJECT_ROOT <- getwd()
Sys.setenv( MV_PROJECT_ROOT = PROJECT_ROOT )

env_or <- function( var, default ) {
  v <- Sys.getenv( var, "" )
  if ( nzchar( v ) ) gsub( "\\\\", "/", v ) else default
}

DROPBOX  <- env_or( "MV_DROPBOX", dirname( dirname( dirname( dirname( PROJECT_ROOT ) ) ) ) )
DATA_HUB <- env_or( "MV_INPUTS", file.path( DROPBOX, "00 - DATA" ) )     # read only
BLS_DIR  <- env_or( "MV_BLS", file.path( DROPBOX, "00 - URBAN", "00-DATA", "bls" ) )
BUILD    <- env_or( "MV_BUILD", "D:/MULTIVERSE" )

## Census API key: environment first, then the key file next to the project.
## Never print or write the value.
census_key <- function() {
  k <- Sys.getenv( "CENSUS_API_KEY", "" )
  if ( nzchar( k ) ) return( k )
  f <- file.path( dirname( PROJECT_ROOT ), "CENSUS-DATA.R" )
  if ( !file.exists( f ) )
    stop( "set CENSUS_API_KEY in ~/.Renviron (or provide ", f, ")" )
  l <- suppressWarnings( readLines( f ) )
  sub( '^KEY <- "([^"]+)".*$', "\\1", grep( '^KEY <- "', l, value = TRUE )[1] )
}
