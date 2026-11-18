#' Left-pad codes with zeros
#'
#' @param x Numeric or character codes.
#' @param n Width.
#' @return Character vector; `NA` stays `NA`.
#' @examples
#' pad( c( 1, 42, NA ), 5 )
#' @export
pad <- function( x, n ) {
  x <- as.character( x )
  i <- !is.na( x ) & nchar( x ) < n
  x[ i ] <- paste0( strrep( "0", n - nchar( x[ i ] ) ), x[ i ] )
  x
}

#' Message with a time stamp
#'
#' @param ... Pasted together, as in [message()].
#' @export
log_msg <- function( ... ) message( format( Sys.time(), "%H:%M:%S " ), ... )

#' Download a file once into a cache folder, optionally checking its sha256
#'
#' The file is kept if it already exists (and, when `sha256` is given, matches it).
#' A new download goes to a temporary name first and is moved into place only after
#' the check passes, so a failed or changed download never replaces a good file.
#'
#' @param url Source URL.
#' @param dir Cache folder (created if needed).
#' @param file File name in `dir`; defaults to the last part of `url`.
#' @param sha256 Expected sha256, or `NULL` to skip the check.
#' @param user_agent Optional User-Agent header (some publishers, e.g. the SEC, require one).
#' @param overwrite Download again even if the file exists.
#' @return The local path.
#' @export
mv_fetch <- function( url, dir, file = basename( url ), sha256 = NULL, user_agent = NULL, overwrite = FALSE ) {
  dir.create( dir, recursive = TRUE, showWarnings = FALSE )
  dest <- file.path( dir, file )
  ok <- function( f ) file.exists( f ) && file.size( f ) > 0 &&
    ( is.null( sha256 ) || identical( file_sha256( f ), tolower( sha256 ) ) )
  if ( !overwrite && ok( dest ) ) return( dest )
  tmp <- paste0( dest, ".part" ); on.exit( unlink( tmp ), add = TRUE )
  old <- options( timeout = max( 3600, getOption( "timeout" ) ) ); on.exit( options( old ), add = TRUE )
  hdr <- if ( !is.null( user_agent ) ) c( "User-Agent" = user_agent ) else NULL
  log_msg( "downloading ", url )
  utils::download.file( url, tmp, mode = "wb", quiet = TRUE, headers = hdr )
  if ( !is.null( sha256 ) && !identical( file_sha256( tmp ), tolower( sha256 ) ) )
    stop( "sha256 of ", url, " does not match the recorded value: the source has changed. ",
          "Use the frozen copy, or update the manifest on purpose." )
  if ( file.exists( dest ) ) unlink( dest )
  if ( !file.rename( tmp, dest ) ) stop( "could not move the download into ", dest )
  dest
}

#' sha256 of a file
#' @param f Path.
#' @return Lower-case hex string.
#' @export
file_sha256 <- function( f ) digest::digest( f, algo = "sha256", file = TRUE )
