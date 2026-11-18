#' The NTEE validity rule and backfill
#'
#' One rule for invalid NTEE codes, used by every build step that reads them. A code is
#' invalid when it is missing, blank, or its first three characters are one of
#' [NTEE_INVALID] ("UND" undefined, "INV" invalid, "Z99" unknown).
#'
#' * `bad_ntee()` flags invalid codes.
#' * `ntee_best_from()` takes EIN-year codes (e.g. from the legacy BMF snapshots) and
#'   returns each EIN's most recent valid code: the lookup the build writes to
#'   `data/npo/ein_ntee_best.csv.gz` (step 02).
#' * `read_ntee_best()` reads that lookup.
#' * `resolve_ntee()` normalizes a code column to three upper-case characters, replaces
#'   invalid codes with the EIN's best code and sets unresolved ones to `NA`, by
#'   reference. It returns, per row, `"valid"`, `"filled"` or `"unresolved"`.
#'
#' @param x Character vector of NTEE codes.
#' @param d A data.table with columns `EIN`, `year` and `ntee3`.
#' @param f Path of the lookup file.
#' @param dt A data.table, modified by reference.
#' @param ein,ntee Names of the EIN and NTEE columns of `dt`.
#' @param best The lookup: a data.table with columns `EIN` (9 digits) and `ntee_best`.
#' @examples
#' bad_ntee( c( "B20", "UND", "", NA, "z99", "Z991" ) )
#' best <- ntee_best_from( data.table::data.table(
#'   EIN = c( "1", "1", "2" ), year = c( 2010, 2015, 2012 ), ntee3 = c( "B20", "UND", "E22" ) ) )
#' best
#' dt <- data.table::data.table( ein = c( "000000001", "000000003" ), ntee3 = c( "und", "inv" ) )
#' resolve_ntee( dt, best = best )
#' dt
#' @name ntee
NULL

#' @rdname ntee
#' @export
bad_ntee <- function( x ) is.na( x ) | trimws( x ) == "" | toupper( substr( trimws( x ), 1, 3 ) ) %in% NTEE_INVALID

#' @rdname ntee
#' @export
ntee_best_from <- function( d ) {
  d <- data.table::as.data.table( d )
  d <- d[ !bad_ntee( ntee3 ), .( EIN = pad( gsub( "[^0-9]", "", EIN ), 9 ), year,
                                 ntee_best = toupper( substr( trimws( ntee3 ), 1, 3 ) ) ) ]
  unique( d[ order( EIN, -year ) ], by = "EIN" )[ , .( EIN, ntee_best ) ]
}

#' @rdname ntee
#' @export
read_ntee_best <- function( f ) {
  if ( !file.exists( f ) ) stop( "missing ", f, ": run R/build/B_npo/02_npo_bmf_counts.R first" )
  data.table::fread( f, colClasses = "character" )
}

#' @rdname ntee
#' @export
resolve_ntee <- function( dt, ein = "ein", ntee = "ntee3", best ) {
  code <- toupper( substr( trimws( as.character( dt[[ ntee ]] ) ), 1, 3 ) )
  bad <- bad_ntee( code )
  fill <- best$ntee_best[ match( pad( gsub( "[^0-9]", "", dt[[ ein ]][ bad ] ), 9 ), best$EIN ) ]
  code[ bad ] <- fill
  data.table::set( dt, j = ntee, value = code )
  status <- rep( "valid", length( code ) )
  status[ bad ] <- data.table::fifelse( is.na( fill ), "unresolved", "filled" )
  status
}
