#' Read a released research frame from its per-year slices
#'
#' The repository ships the final frames in `data/` as one parquet file per year, with
#' `MANIFEST.csv` (sha256 of every file) and `FRAMES.csv` (rows, columns, sort keys and a
#' signature of the column names and types). `mv_read_frame()` checks each slice against
#' the manifest, binds them, restores the sort order and checks the result against
#' `FRAMES.csv`, so the table it returns is the one that was released.
#'
#' @param frame `"cbsa_year"` or `"county_year"`.
#' @param dir The data folder (default `"data"`, the repository's `data/`).
#' @param check Verify sha256 hashes and the frame signature (default `TRUE`).
#' @return A data.table.
#' @examples
#' \dontrun{
#' cbsa <- mv_read_frame( "cbsa_year" )   # from the repository root
#' }
#' @export
mv_read_frame <- function( frame = c( "cbsa_year", "county_year" ), dir = "data", check = TRUE ) {
  frame <- match.arg( frame )
  if ( !requireNamespace( "arrow", quietly = TRUE ) ) stop( "mv_read_frame() needs the arrow package" )
  man <- data.table::fread( file.path( dir, "MANIFEST.csv" ), colClasses = list( character = "sha256" ) )
  frm <- data.table::fread( file.path( dir, "FRAMES.csv" ), colClasses = list( character = c( "schema_sha256", "source_sha256" ) ) )
  k <- man$frame == frame                # outside [ ]: frame is also a column of the manifest
  m <- man[ k ]; m <- m[ order( m$year ) ]
  if ( !nrow( m ) ) stop( "no slices of ", frame, " in ", file.path( dir, "MANIFEST.csv" ) )
  files <- file.path( dir, m$file )
  if ( check ) {
    got <- vapply( files, file_sha256, "" )
    bad <- m$file[ got != m$sha256 ]
    if ( length( bad ) ) stop( "slices do not match MANIFEST.csv: ", paste( bad, collapse = ", " ) )
  }
  d <- data.table::rbindlist( lapply( files, function( f ) data.table::as.data.table( arrow::read_parquet( f ) ) ), use.names = TRUE )
  k <- frm$frame == frame; sig <- frm[ k ]
  data.table::setorderv( d, strsplit( sig$sort_keys, "," )[[ 1 ]] )
  if ( check ) {
    if ( nrow( d ) != sig$rows || ncol( d ) != sig$cols ) stop( frame, ": rows or columns differ from FRAMES.csv" )
    if ( frame_signature( d ) != sig$schema_sha256 ) stop( frame, ": column names or types differ from FRAMES.csv" )
  }
  d[]
}

#' Signature of a table's column names and types
#'
#' sha256 of `"name class; name class; ..."`, as stored in `FRAMES.csv`.
#' @param d A data.frame.
#' @return Lower-case hex string.
#' @export
frame_signature <- function( d ) {
  digest::digest( paste( names( d ), vapply( d, function( x ) class( x )[1], "" ), collapse = ";" ),
                  "sha256", serialize = FALSE )
}
