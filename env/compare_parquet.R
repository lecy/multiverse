## compare_parquet.R
## Compare a rebuilt parquet file with a reference copy: file hash (sha256 if the
## digest package is installed, else md5), then shape,
## column names and types, and cell values (after sorting by the key columns).
##   Rscript env/compare_parquet.R <new.parquet> <ref.parquet> <key1,key2,...>
## Prints one line per differing column; exit status 1 if anything differs.

suppressPackageStartupMessages( { library( data.table ); library( arrow ) } )
a <- commandArgs( TRUE )
f_new <- a[1]; f_ref <- a[2]; key <- strsplit( a[3], "," )[[1]]

sha <- function( f ) digest_file( f )
digest_file <- function( f ) {
  if ( requireNamespace( "digest", quietly = TRUE ) ) return( digest::digest( f, "sha256", file = TRUE ) )
  tools::md5sum( f )[[1]]
}
cat( "file:", basename( f_new ), "\n  hash new", sha( f_new ), "\n  hash ref", sha( f_ref ), "\n" )

n <- as.data.table( read_parquet( f_new ) ); r <- as.data.table( read_parquet( f_ref ) )
cat( "  rows", nrow( n ), "vs", nrow( r ), "; cols", ncol( n ), "vs", ncol( r ), "\n" )
bad <- 0L
only_n <- setdiff( names( n ), names( r ) ); only_r <- setdiff( names( r ), names( n ) )
if ( length( only_n ) ) { cat( "  only in new:", only_n, "\n" ); bad <- bad + 1L }
if ( length( only_r ) ) { cat( "  only in ref:", only_r, "\n" ); bad <- bad + 1L }
if ( !identical( names( n ), names( r ) ) && !length( only_n ) && !length( only_r ) ) {
  cat( "  same columns, different order\n" ); bad <- bad + 1L
}
if ( nrow( n ) != nrow( r ) ) { cat( "  row counts differ\n" ); quit( status = 1 ) }
setkeyv( n, key ); setkeyv( r, key )
cols <- intersect( names( r ), names( n ) )
for ( v in cols ) {
  x <- n[[ v ]]; y <- r[[ v ]]
  if ( !identical( class( x ), class( y ) ) ) {
    cat( "  ", v, ": type", class( x )[1], "vs", class( y )[1], "\n" ); bad <- bad + 1L; next
  }
  if ( identical( x, y ) ) next
  na <- sum( is.na( x ) != is.na( y ) )
  if ( is.numeric( x ) ) {
    d <- abs( x - y ); d <- d[ is.finite( d ) ]
    cat( sprintf( "   %s: %d NA mismatches, %d value diffs, max abs diff %.3g\n", v, na,
                  sum( d > 0 ), if ( length( d ) ) max( d ) else 0 ) )
  } else cat( sprintf( "   %s: %d NA mismatches, %d value diffs\n", v, na, sum( x != y, na.rm = TRUE ) ) )
  bad <- bad + 1L
}
cat( if ( bad ) sprintf( "  %d differences\n", bad ) else "  IDENTICAL cell by cell\n" )
quit( status = as.integer( bad > 0 ) )
