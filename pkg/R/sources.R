#' Get a source's files: the frozen copy, or again from the publisher
#'
#' The sources of the build (inputs/<source>/SOURCE.md) are frozen with sha256 hashes. The
#' source registry ([mv_sources()]) records, for every frozen file, whether it can be
#' downloaded again and from where.
#'
#' * `mode = "frozen"` (default) restores the frozen copies with [mv_get_inputs()]. This gives
#'   the published numbers.
#' * `mode = "live"` downloads every file that has a URL again from its publisher and
#'   compares it with the frozen hash. Files with no stable public file (API responses,
#'   files extracted from a download, hand-placed files, our own intermediates) are reported
#'   as `"frozen only"` and not touched. What happens when a download differs from the
#'   frozen copy is set by `on_change`: `"stop"` (default) stops; `"report"` keeps the frozen
#'   copy in place, saves nothing and reports `"changed"`; `"use"` puts the new file in place
#'   (the build then no longer reproduces the published numbers).
#'
#' @param sources Source folders (default all in the registry).
#' @param roots Named character vector: `build`, `hub`, `bls`, `dropbox` folders.
#' @param mode `"frozen"` or `"live"`.
#' @param on_change `"stop"`, `"report"` or `"use"` (live mode).
#' @param user_agent The browser User-Agent sent for `http_ua` sources.
#' @param registry,manifest The source registry and the manifest (defaults: the package's).
#' @param ... Passed to [mv_get_inputs()] in frozen mode (e.g. `source`, `profile`).
#' A download that comes back as an HTML page (a server refusing the request with status 200)
#' is reported as `"rejected"`, not as a change.
#'
#' @return Live mode: a data.table, one row per file, with `status` (`"same"`, `"changed"`, `"rejected"`,
#'   `"used"`, `"failed"`, `"frozen only"`) and `sha256_live`. Frozen mode: the summary of
#'   [mv_get_inputs()].
#' @examples
#' \dontrun{
#' roots <- c( build = "build", hub = "inputs", bls = "bls", dropbox = "dropbox" )
#' mv_fetch_source( "census-popest", roots )                                   # frozen copy
#' chk <- mv_fetch_source( "census-popest", roots, mode = "live", on_change = "report" )
#' chk[ , .N, by = status ]                                                    # drift since the freeze
#' }
#' @export
mv_fetch_source <- function( sources = NULL, roots, mode = c( "frozen", "live" ), on_change = c( "stop", "report", "use" ),
                             user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36",
                             registry = mv_sources(), manifest = mv_manifest(), ... ) {
  mode <- match.arg( mode ); on_change <- match.arg( on_change )
  if ( mode == "frozen" ) return( mv_get_inputs( sources, roots = roots, manifest = manifest, ... ) )
  stopifnot( all( c( "build", "hub", "bls", "dropbox" ) %in% names( roots ) ) )
  reg <- data.table::as.data.table( registry )
  if ( !is.null( sources ) ) {
    bad <- setdiff( sources, reg$source )
    if ( length( bad ) ) stop( "not in the source registry: ", paste( bad, collapse = ", " ) )
    keep <- reg$source %in% sources; reg <- reg[ keep ]
  }
  man <- data.table::as.data.table( manifest )
  man <- man[ which( man$class == "input" ), .( file, bytes, sha256 ) ]
  reg <- merge( reg, man, by = "file", all.x = TRUE, sort = FALSE )
  root <- sub( "/.*", "", reg$file )
  reg[ , dest := file.path( roots[ root ], sub( "^[^/]+/", "", file ) ) ]
  reg[ , `:=`( status = "frozen only", sha256_live = NA_character_ ) ]
  for ( i in which( reg$method != "frozen" ) ) {
    tmp <- paste0( reg$dest[ i ], ".live" )
    dir.create( dirname( tmp ), recursive = TRUE, showWarnings = FALSE )
    ok <- tryCatch( {
      if ( reg$method[ i ] == "http_ua" ) {
        h <- curl::new_handle( useragent = user_agent, followlocation = TRUE )
        curl::handle_setheaders( h, "Accept" = "*/*", "Accept-Language" = "en-US,en;q=0.9" )
        curl::curl_download( reg$url[ i ], tmp, handle = h, quiet = TRUE )
      } else {
        old <- options( timeout = max( 3600, getOption( "timeout" ) ) ); on.exit( options( old ), add = TRUE )
        utils::download.file( reg$url[ i ], tmp, mode = "wb", quiet = TRUE )
      }
      TRUE }, error = function( e ) { log_msg( "failed: ", reg$url[ i ], " (", conditionMessage( e ), ")" ); FALSE } )
    if ( !ok ) { unlink( tmp ); reg$status[ i ] <- "failed"; next }
    h <- file_sha256( tmp ); reg$sha256_live[ i ] <- h
    ## a server that refuses a request may still answer 200 with an HTML page (the Census
    ## firewall does): that is a failed download, not a changed source
    head8 <- tolower( rawToChar( readBin( tmp, "raw", 64 ) ) )
    if ( !identical( h, reg$sha256[ i ] ) && grepl( "^\\s*(<!doctype html|<html)", head8 ) && !grepl( "\\.html?$", reg$file[ i ] ) ) {
      unlink( tmp ); reg$status[ i ] <- "rejected"
      log_msg( reg$source[ i ], ": ", basename( reg$file[ i ] ), " rejected (the server sent an HTML page)" ); next
    }
    if ( identical( h, reg$sha256[ i ] ) ) {
      if ( file.exists( reg$dest[ i ] ) ) unlink( tmp ) else file.rename( tmp, reg$dest[ i ] )
      reg$status[ i ] <- "same"
    } else if ( on_change == "use" ) {
      if ( file.exists( reg$dest[ i ] ) ) unlink( reg$dest[ i ] )
      file.rename( tmp, reg$dest[ i ] ); reg$status[ i ] <- "used"
    } else {
      unlink( tmp ); reg$status[ i ] <- "changed"
      if ( on_change == "stop" )
        stop( reg$file[ i ], " has changed at its source since the freeze (", reg$url[ i ], "). ",
              "Use mode = \"frozen\" for the published numbers, or on_change = \"report\" / \"use\".", call. = FALSE )
    }
    log_msg( reg$source[ i ], ": ", basename( reg$file[ i ] ), " ", reg$status[ i ] )
  }
  reg[ , .( source, file, method, url, status, sha256_frozen = sha256, sha256_live, note ) ]
}

#' The source registry
#'
#' One row per frozen input file: `source`, `file` (`<root>/<path>`, as in [mv_manifest()]),
#' `method` (`http`, `http_ua` with a browser User-Agent, or `frozen` when there is no stable
#' public file), `url` and `note`. Built by `env/build_sources.R` from the URLs the build steps
#' use.
#' @return A data.table.
#' @export
mv_sources <- function()
  data.table::fread( system.file( "extdata", "sources.csv", package = "npdensity", mustWork = TRUE ) )
