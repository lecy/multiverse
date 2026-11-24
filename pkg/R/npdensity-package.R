#' npdensity: nonprofit density research frames and multiverse tools
#'
#' Functions shared by the build (`R/build/`), the multiverse (`R/multiverse/`) and the
#' paper scripts of the nonprofit-density project. See `PACKAGE-PLAN.md` in the
#' repository for the plan this package implements.
#'
#' @keywords internal
#' @import data.table
"_PACKAGE"

## data.table non-standard evaluation, and the run objects mv_run_universe() puts on workers
utils::globalVariables( c( "EIN", "year", "ntee3", "ntee_best", ".", "frame",
  "legacy_county", "dominant_pr", "town_name", "pr", "pop", "pop2019", "geoid",
  "share_of_legacy", "share_of_pr", ":=", ".SD", ".N", ".I",
  "outcome", "construct", "hu", "ident", "k", "dv_form", "pred_form", "rhs_logP", "est",
  "cs_year", "outlier", "op", "op_block", "drop", "logP_rhs", "dup_of", "spec_id",
  "logP", "population", "division", "state_fips", "cbsa_type", "var", "type", "term",
  "estimate", "se", "se_state", "std_beta", "batch", "status", "N",
  ".mv_grid", ".mv_panel", ".mv_cfg", ".mv_bat_dir", ".mv_fit" ) )
