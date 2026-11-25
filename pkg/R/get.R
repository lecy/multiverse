#' Get released data: research frames, multiverse runs and frozen inputs
#'
#' Each getter downloads into a cache, checks every file against the sha256 recorded in
#' the package manifest ([mv_manifest()]), keeps files that are already in place and match,
#' and stops with a clear message when a file does not match.
#'
#' * `mv_get_frame()`: a research frame (class B) from a tagged release of the repository.
#'   The slices are read from the tag's `data/` folder and bound by [mv_read_frame()].
#' * `mv_get_run()`: a multiverse run (`specs.parquet`, `coefs.parquet`) from the assets of a
#'   GitHub release. Returns a list of two data.tables.
#' * `mv_get_inputs()`: the frozen inputs (class A) into the four build roots, replacing
#'   `env/restore_inputs.R`. `source` is the freeze: `s3://...` (read with the AWS CLI and
#'   `profile`) or an `https://` mirror with the same layout.
#' * `mv_cache_dir()`: the cache, `options(npdensity.cache)` or the user cache folder.
#'
#' @param frame `"cbsa_year"` or `"county_year"`.
#' @param release The release tag.
#' @param repo GitHub repository, `"owner/name"`.
#' @param base Where the release's files are: a URL or a local folder. Defaults to the
#'   tag's `data/` folder (frames) or the release's assets (runs) on GitHub.
#' @param cache Cache folder.
#' @param run Run name, e.g. `"principled_v11"`.
#' @param folders Input folders to get (default all in the manifest).
#' @param roots Named character vector: `build`, `hub`, `bls`, `dropbox` folders.
#' @param source Where the freeze is.
#' @param profile AWS CLI profile for an `s3://` source.
#' @param manifest The manifest to read the files and hashes from (default the package's, [mv_manifest()]).
#' @return `mv_get_frame()`: a data.table. `mv_get_run()`: a list with `specs` and
#'   `coefs`. `mv_get_inputs()`: a data.table, one row per folder, invisibly.
#' @examples
#' \dontrun{
#' cbsa <- mv_get_frame( "cbsa_year", release = "v1.1-data" )
#' v11  <- mv_get_run( "principled_v11", release = "v1.0-data" )
#' mv_get_inputs( c( "census-saipe", "ers-laus" ),
#'                roots = c( build = "build", hub = "inputs", bls = "bls", dropbox = "dropbox" ) )
#' }
#' @name get
NULL

#' @rdname get
#' @export
mv_cache_dir <- function() getOption( "npdensity.cache", tools::R_user_dir( "npdensity", "cache" ) )

## copy or download one recorded file into dest, checking its sha256
.mv_obtain <- function( src, dest, sha256 ) {
  if ( grepl( "^[a-z]+://", src ) ) return( mv_fetch( src, dirname( dest ), basename( dest ), sha256 = sha256 ) )
  if ( file.exists( dest ) && identical( file_sha256( dest ), sha256 ) ) return( dest )
  if ( !file.exists( src ) ) stop( "not found: ", src )
  if ( !identical( file_sha256( src ), sha256 ) ) stop( "sha256 of ", src, " does not match the manifest" )
  dir.create( dirname( dest ), recursive = TRUE, showWarnings = FALSE )
  if ( !file.copy( src, dest, overwrite = TRUE ) ) stop( "could not copy ", src )
  dest
}

.mv_manifest_rows <- function( cls, release, item = NULL, manifest = mv_manifest() ) {
  m <- data.table::as.data.table( manifest )
  ## logical vectors built outside [ ], so that release and item are the arguments, not the columns
  keep <- m$class == cls & m$release == release
  if ( !is.null( item ) ) keep <- keep & m$item == item
  m <- m[ which( keep ) ]
  if ( !nrow( m ) ) stop( "the package manifest has no ", cls, " files for release ", release,
                          if ( !is.null( item ) ) paste0( " and ", item ), "; releases: ",
                          paste( unique( manifest$release[ manifest$class == cls ] ), collapse = ", " ) )
  m
}

#' @rdname get
#' @export
mv_get_frame <- function( frame = c( "cbsa_year", "county_year" ), release = "v1.1-data", repo = "lecy/multiverse",
                          base = sprintf( "https://raw.githubusercontent.com/%s/%s/data", repo, release ),
                          cache = mv_cache_dir(), manifest = mv_manifest() ) {
  frame <- match.arg( frame )
  m <- .mv_manifest_rows( "frame", release, manifest = manifest )
  m <- m[ m$item %in% c( frame, "frames" ) ]                       # the slices plus MANIFEST.csv / FRAMES.csv
  dir <- file.path( cache, "frames", release )
  for ( i in seq_len( nrow( m ) ) )
    .mv_obtain( paste( base, m$file[ i ], sep = "/" ), file.path( dir, m$file[ i ] ), m$sha256[ i ] )
  mv_read_frame( frame, dir = dir )
}

#' @rdname get
#' @export
mv_get_run <- function( run = "principled_v11", release = "v1.0-data", repo = "lecy/multiverse",
                        base = sprintf( "https://github.com/%s/releases/download/%s", repo, release ),
                        cache = mv_cache_dir(), manifest = mv_manifest() ) {
  m <- .mv_manifest_rows( "run", release, run, manifest = manifest )
  dir <- file.path( cache, "runs", release, run )
  out <- list()
  for ( i in seq_len( nrow( m ) ) ) {
    f <- tryCatch( .mv_obtain( paste( base, m$location[ i ], sep = "/" ), file.path( dir, m$file[ i ] ), m$sha256[ i ] ),
                   error = function( e ) stop( "could not get ", m$location[ i ], " of release ", release, " (",
                                               conditionMessage( e ), "). Is the run attached to the release?", call. = FALSE ) )
    out[[ sub( "\\.parquet$", "", m$file[ i ] ) ]] <- data.table::as.data.table( arrow::read_parquet( f ) )
  }
  out
}

#' @rdname get
#' @export
mv_get_inputs <- function( folders = NULL, roots, release = "freeze-2026-11", source = "s3://lecy/nonprofit-density/inputs",
                           profile = Sys.getenv( "AWS_PROFILE", "lecy-data" ), manifest = mv_manifest() ) {
  stopifnot( all( c( "build", "hub", "bls", "dropbox" ) %in% names( roots ) ) )
  m <- .mv_manifest_rows( "input", release, manifest = manifest )
  if ( !is.null( folders ) ) {
    bad <- setdiff( folders, m$item )
    if ( length( bad ) ) stop( "not in the manifest: ", paste( bad, collapse = ", " ) )
    m <- m[ m$item %in% folders ]
  }
  ## destination: <root>/<path> from file = "<root>/<path>"
  root <- sub( "/.*", "", m$file )
  if ( !all( root %in% names( roots ) ) ) stop( "unknown roots in the manifest: ", paste( setdiff( root, names( roots ) ), collapse = ", " ) )
  dest <- file.path( roots[ root ], sub( "^[^/]+/", "", m$file ) )
  s3 <- grepl( "^s3://", source )
  get1 <- function( i ) {
    if ( file.exists( dest[ i ] ) && file.size( dest[ i ] ) == m$bytes[ i ] && identical( file_sha256( dest[ i ] ), m$sha256[ i ] ) ) return( "kept" )
    src <- paste( source, m$location[ i ], sep = "/" )
    if ( !s3 ) { mv_fetch( src, dirname( dest[ i ] ), basename( dest[ i ] ), sha256 = m$sha256[ i ], overwrite = TRUE ); return( "restored" ) }
    dir.create( dirname( dest[ i ] ), recursive = TRUE, showWarnings = FALSE )
    tmp <- paste0( dest[ i ], ".part" ); on.exit( unlink( tmp ) )
    out <- suppressWarnings( system2( "aws", shQuote( c( "s3", "cp", src, tmp, "--profile", profile, "--only-show-errors" ) ),
                                      stdout = TRUE, stderr = TRUE ) )
    if ( !is.null( attr( out, "status" ) ) ) stop( "aws s3 cp ", src, " failed: ", paste( out, collapse = " " ) )
    got <- file_sha256( tmp )
    if ( !identical( got, m$sha256[ i ] ) ) stop( "sha256 mismatch for ", m$location[ i ], ": manifest ", m$sha256[ i ], ", downloaded ", got )
    if ( file.exists( dest[ i ] ) ) unlink( dest[ i ] )
    if ( !file.rename( tmp, dest[ i ] ) ) stop( "could not move ", tmp )
    "restored"
  }
  res <- list()
  for ( fo in unique( m$item ) ) {
    ii <- which( m$item == fo )
    st <- vapply( ii, get1, "" )
    res[[ fo ]] <- data.table( folder = fo, files = length( ii ), restored = sum( st == "restored" ), kept = sum( st == "kept" ),
                               bytes = sum( m$bytes[ ii ] ) )
    log_msg( fo, ": ", res[[ fo ]]$restored, " restored, ", res[[ fo ]]$kept, " already in place" )
  }
  invisible( rbindlist( res ) )
}

#' The package manifest
#'
#' Every file the getters can fetch, with its sha256: `class` (`frame`, `run`, `input`),
#' `release`, `item` (frame, run or input folder), `file` (local path: the frame's file
#' under `data/`, the run's file, or `<root>/<path>` for an input), `bytes`, `sha256` and
#' `location` (path under the source: the release asset, or the key in the freeze).
#' Built by `env/build_pkg_manifest.R`.
#' @return A data.table.
#' @export
mv_manifest <- function() {
  if ( is.null( .geo[[ "manifest" ]] ) )
    .geo[[ "manifest" ]] <- data.table::fread( system.file( "extdata", "manifest.csv", package = "npdensity", mustWork = TRUE ),
                                               colClasses = list( character = "sha256" ) )
  .geo[[ "manifest" ]]
}

#' The specification dictionary
#'
#' One row per fork column of a multiverse grid and value: `column`, `code`, `label` and
#' `description`. Also in the release as `data/spec_dictionary.csv`.
#' @return A data.table.
#' @export
mv_spec_dictionary <- function()
  data.table::fread( system.file( "extdata", "spec_dictionary.csv", package = "npdensity", mustWork = TRUE ), colClasses = "character" )
