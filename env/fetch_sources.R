## env/fetch_sources.R   (PACKAGE-PLAN.md Step 4)
## Downloads the public sources again from their publishers and compares every file with the
## frozen copy (npdensity::mv_fetch_source, mode = "live"). env/restore_inputs.R restores the
## frozen copies themselves, which give the published numbers.
##
##   Rscript env/fetch_sources.R [--sources a,b] [--on-change stop|report|use] [--out check.csv] \
##           [--build B] [--inputs I] [--bls L] [--dropbox D]
##
## --on-change report (default here): keep the frozen copies, list what changed.
##             stop: stop at the first change. use: put the new files in place (the build
##             then no longer reproduces the published numbers).
## The URLs are in the source registry, npdensity::mv_sources() (env/build_sources.R).
## Files with no stable public file are listed as "frozen only".
## Roots default to MV_BUILD, MV_INPUTS, MV_BLS, MV_DROPBOX (R/lib/config.R).

source( "R/lib/config.R" )
source( "R/lib/load_pkg.R" )
suppressPackageStartupMessages( library( data.table ) )
args <- commandArgs( TRUE )
opt <- function( f, d ) { i <- match( f, args ); if ( is.na( i ) ) d else args[ i + 1 ] }
ROOT <- c( build = opt( "--build", BUILD ), hub = opt( "--inputs", DATA_HUB ),
           bls = opt( "--bls", BLS_DIR ), dropbox = opt( "--dropbox", DROPBOX ) )
SOURCES <- { f <- opt( "--sources", "" ); if ( nzchar( f ) ) strsplit( f, "," )[[ 1 ]] else NULL }
ON <- opt( "--on-change", "report" )
OUTF <- opt( "--out", file.path( ROOT[[ "build" ]], "check", "live_sources.csv" ) )

r <- mv_fetch_source( SOURCES, roots = ROOT, mode = "live", on_change = ON )
dir.create( dirname( OUTF ), recursive = TRUE, showWarnings = FALSE )
fwrite( r, OUTF )
print( r[ , .N, keyby = .( source, status ) ], nrows = 200 )
cat( "wrote", OUTF, "\n" )
