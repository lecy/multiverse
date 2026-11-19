#' npdensity: nonprofit density research frames and multiverse tools
#'
#' Functions shared by the build (`R/build/`), the multiverse (`R/multiverse/`) and the
#' paper scripts of the nonprofit-density project. See `PACKAGE-PLAN.md` in the
#' repository for the plan this package implements.
#'
#' @keywords internal
#' @import data.table
"_PACKAGE"

## data.table non-standard evaluation
utils::globalVariables( c( "EIN", "year", "ntee3", "ntee_best", ".", "frame",
  "legacy_county", "dominant_pr", "town_name", "pr", "pop", "pop2019", "geoid",
  "share_of_legacy", "share_of_pr", ":=", ".SD" ) )
