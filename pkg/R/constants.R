#' Constants of the build
#'
#' * `YEARS`: the analysis panel window, 2000–2024.
#' * `POP_YEARS`: population years, carried one year beyond the panel.
#' * `FLOOR_REAL_2010`: real-dollar floor (2010 dollars) of the active-filer universe.
#' * `NTEE_HOSP`, `NTEE_UNIV`: NTEE codes of hospitals and universities, excluded
#'   from the `_nohu` outcomes.
#' * `NTEE_INVALID`: NTEE codes treated as invalid (see [bad_ntee()]).
#' * `COUNTY_RECODE`: renamed or merged county-equivalents, mapped onto the 2023 codes
#'   (see [recode_county()]).
#'
#' @name constants
#' @aliases YEARS POP_YEARS FLOOR_REAL_2010 NTEE_HOSP NTEE_UNIV NTEE_INVALID COUNTY_RECODE
#' @export YEARS POP_YEARS FLOOR_REAL_2010 NTEE_HOSP NTEE_UNIV NTEE_INVALID COUNTY_RECODE
NULL

YEARS <- 2000:2024
POP_YEARS <- 2000:2025
FLOOR_REAL_2010 <- 50000
NTEE_HOSP <- c( "E20", "E21", "E22", "E23", "E24" )
NTEE_UNIV <- c( "B40", "B41", "B42", "B43", "B50" )
NTEE_INVALID <- c( "UND", "INV", "Z99" )
COUNTY_RECODE <- data.table::data.table(
  from = c( "51515", "46113", "02270" ),
  to   = c( "51019", "46102", "02158" ),
  note = c( "Bedford city merged into Bedford County (2013)",
            "Shannon County renamed Oglala Lakota (2015)",
            "Wade Hampton renamed Kusilvak (2015)" ) )

#' Map county codes onto 2023 county-equivalents
#'
#' Applies [COUNTY_RECODE] and sends Valdez-Cordova (02261) to Chugach (02063), the
#' larger of its two 2019 successors. Connecticut is handled separately (legacy counties
#' to planning regions) by the build.
#'
#' @param geoid Character vector of 5-digit county FIPS codes.
#' @return Character vector of the same length.
#' @examples
#' recode_county( c( "51515", "02261", "04013" ) )
#' @export
recode_county <- function( geoid ) {
  m <- match( geoid, COUNTY_RECODE$from )
  out <- ifelse( is.na( m ), geoid, COUNTY_RECODE$to[ m ] )
  out[ out %in% "02261" ] <- "02063"
  out
}
