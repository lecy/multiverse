## build_renv_lock.R
## Builds a package library with the exact versions recorded in
## env/packages-2026-11-17.csv (panel990 pinned to its GitHub commit) and writes
## renv.lock in the project root.
##
## The library lives OUTSIDE the project ($MV_BUILD/renv-build), because the project
## folder syncs through Dropbox: an renv library or autoloader inside it would
## switch R sessions on every other machine to that library.
##
## Dependencies of the recorded packages come from a dated Posit Package Manager
## snapshot close to the 2026-11 build, so they match what CRAN served then.
##
## Usage (from the multiverse/ folder):
##   Rscript env/build_renv_lock.R > $MV_BUILD/logs/renv_lock.log 2>&1
## Restore elsewhere with: renv::restore( lockfile = "renv.lock" )

PPM_DATE <- "2026-11-16"
repo <- c( CRAN = sprintf( "https://packagemanager.posit.co/cran/%s", PPM_DATE ) )
options( repos = repo )

project <- getwd()
stopifnot( file.exists( file.path( project, "R", "00_setup.R" ) ) )
build <- Sys.getenv( "MV_BUILD", "D:/MULTIVERSE" )
rproj <- file.path( build, "renv-build" )
dir.create( rproj, recursive = TRUE, showWarnings = FALSE )

if ( !requireNamespace( "renv", quietly = TRUE ) ) install.packages( "renv" )

pk <- read.csv( file.path( project, "env", "packages-2026-11-17.csv" ),
                colClasses = "character" )
base <- rownames( installed.packages( priority = "base" ) )
pk <- pk[ !pk$package %in% base, ]
spec <- ifelse( grepl( "^github:", pk$source ),
                paste0( sub( "^github:", "", pk$source ), "@", pk$remote_sha ),
                paste0( pk$package, "@", pk$version ) )
writeLines( c( "deps <- c(", paste0( '  "', pk$package, '"', collapse = ",\n" ), ")" ),
            file.path( rproj, "deps.R" ) )     # so snapshot sees them as used

setwd( rproj )
renv::init( bare = TRUE, restart = FALSE, load = TRUE )
options( repos = repo )
renv::install( spec, prompt = FALSE )
renv::snapshot( type = "all", prompt = FALSE )

## check: every recorded package at its recorded version
lib <- renv::paths$library( project = rproj )
got <- installed.packages( lib.loc = lib )[ , "Version" ]
chk <- data.frame( package = pk$package, want = pk$version, got = unname( got[ pk$package ] ) )
chk$ok <- chk$want == chk$got
print( chk, row.names = FALSE )
if ( !all( chk$ok ) ) stop( "version mismatch: ", paste( chk$package[ !chk$ok ], collapse = ", " ) )

file.copy( file.path( rproj, "renv.lock" ), file.path( project, "renv.lock" ), overwrite = TRUE )
cat( "renv.lock written; library at", lib, "\n" )
