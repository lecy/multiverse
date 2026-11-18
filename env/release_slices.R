## env/release_slices.R
## Writes the final research frames into the repository as one parquet file per year, so
## each piece stays far below GitHub's 50 MB warning and the frames ship with a tagged release.
##   Rscript env/release_slices.R [build_dir] [out_dir=data]
## build_dir defaults to MV_BUILD; it must hold data/analysis/{cbsa_year,county_year}.parquet.
## Writes:
##   data/cbsa_year/cbsa_year_<year>.parquet     23,125 rows in 25 files (every column, year included)
##   data/county_year/county_year_<year>.parquet 78,600 rows in 25 files
##   data/variable_dictionary.csv                 copy of docs/variable_dictionary.csv
##   data/MANIFEST.csv   file, frame, year, rows, cols, bytes, sha256
##   data/FRAMES.csv     frame, rows, cols, years, sort keys, sha256 of the column names and types
## R/multiverse/run_from_release.R recombines the slices and checks them against both files.

suppressPackageStartupMessages( { library( data.table ); library( arrow ) } )
a <- commandArgs( TRUE )
BUILD <- if ( length( a ) >= 1 ) a[1] else Sys.getenv( "MV_BUILD" )
OUT   <- if ( length( a ) >= 2 ) a[2] else "data"
KEYS  <- list( cbsa_year = c( "cbsa", "year" ), county_year = c( "geoid", "year" ) )
sha   <- function( f ) digest::digest( f, "sha256", file = TRUE )
schema_sig <- function( d ) digest::digest( paste( names( d ), vapply( d, function( x ) class( x )[1], "" ), collapse = ";" ), "sha256", serialize = FALSE )

man <- list(); frames <- list()
for ( t in names( KEYS ) ) {
  src <- file.path( BUILD, "data", "analysis", paste0( t, ".parquet" ) )
  stopifnot( file.exists( src ) )
  d <- as.data.table( read_parquet( src ) )
  k <- KEYS[[ t ]]
  stopifnot( uniqueN( d, by = k ) == nrow( d ) )
  od <- file.path( OUT, t ); unlink( od, recursive = TRUE ); dir.create( od, recursive = TRUE )
  for ( y in sort( unique( d$year ) ) ) {
    f <- file.path( od, sprintf( "%s_%d.parquet", t, y ) )
    write_parquet( d[ year == y ], f, compression = "zstd" )
    man[[ length( man ) + 1L ]] <- data.table( file = sub( paste0( "^", OUT, "/" ), "", f ), frame = t, year = y,
                                               rows = nrow( d[ year == y ] ), cols = ncol( d ), bytes = file.size( f ), sha256 = sha( f ) )
  }
  frames[[ t ]] <- data.table( frame = t, rows = nrow( d ), cols = ncol( d ), years = paste( range( d$year ), collapse = "-" ),
                               sort_keys = paste( k, collapse = "," ), schema_sha256 = schema_sig( d ),
                               source = paste0( "data/analysis/", t, ".parquet" ), source_sha256 = sha( src ) )
  cat( sprintf( "%s: %d rows x %d cols -> %d files, %.1f MB\n", t, nrow( d ), ncol( d ), length( unique( d$year ) ),
                sum( vapply( man, function( m ) if ( m$frame == t ) m$bytes else 0, 0 ) ) / 1e6 ) )
}
file.copy( file.path( "docs", "variable_dictionary.csv" ), file.path( OUT, "variable_dictionary.csv" ), overwrite = TRUE )
fwrite( rbindlist( man ), file.path( OUT, "MANIFEST.csv" ) )
fwrite( rbindlist( frames ), file.path( OUT, "FRAMES.csv" ) )
cat( "wrote", file.path( OUT, "MANIFEST.csv" ), "and FRAMES.csv\n" )
