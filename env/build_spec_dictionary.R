## env/build_spec_dictionary.R
## Writes the specification dictionary (PACKAGE-PLAN.md Step 3): one row per fork column of a
## multiverse grid and value, with a label and a description, so that specs.parquet can be read
## without the code.
##   Rscript env/build_spec_dictionary.R
## Writes pkg/inst/extdata/spec_dictionary.csv and data/spec_dictionary.csv. Stops if any value
## used by the grids (both models, both universes) or by the grassroots refit has no row.
## The covariate-variant rows (op) are generated from the configurations (npdensity::mv_spec_config),
## so they always say which measures a variant uses.

source( "R/lib/load_pkg.R" )
suppressPackageStartupMessages( library( data.table ) )

R <- function( column, code, label, description ) data.table( column, code, label, description )
dict <- rbind(
  R( "spec_id", "<integer>", "Specification id", "Row number of the full grid of the universe (algebraic duplicates included), so ids are stable when the duplicates are left out. Ids are specific to a run's model and universe." ),
  R( "construct", "count", "Counts", "Outcome construct: number of nonprofits." ),
  R( "construct", "exp", "Resources", "Outcome construct: nonprofit dollars (expenses or revenue)." ),
  R( "hu", "incl", "Hospitals & universities in", "Hospitals (NTEE E20-E24) and universities (B40-B43, B50) are included in the outcome." ),
  R( "hu", "excl", "Hospitals & universities out", "Hospitals and universities are excluded from the outcome (the _nohu outcomes)." ),
  R( "outcome", "n_filer_c3", "Filers", "501(c)(3) public charities filing a Form 990 or 990-EZ (above the real-dollar floor) per CBSA-year." ),
  R( "outcome", "n_filer_c3_nohu", "Filers excl. H&U", "Filers, excluding hospitals and universities." ),
  R( "outcome", "n_filer_c3_hs", "Filers, human services", "Filers in human services (subsector extension; kitchen universe only)." ),
  R( "outcome", "n_bmf_c3pc", "Registered", "501(c)(3) public charities registered in the IRS Business Master File (the registered stock)." ),
  R( "outcome", "n_grass", "Grassroots", "Registered minus filers: registered public charities that do not file (R/notes/104_fit_grassroots_v11.R refit)." ),
  R( "outcome", "exp_c3", "Expenses", "Total expenses of filing public charities." ),
  R( "outcome", "exp_c3_net", "Expenses, net", "Expenses net of grants paid to other organizations (series start in 2010)." ),
  R( "outcome", "exp_c3_nohu", "Expenses excl. H&U", "Expenses, excluding hospitals and universities." ),
  R( "outcome", "exp_c3_nohu_net", "Expenses excl. H&U, net", "Expenses excluding hospitals and universities, net of grants paid." ),
  R( "outcome", "exp_c3_hs", "Expenses, human services", "Expenses in human services (kitchen universe only)." ),
  R( "outcome", "rev_c3", "Revenue", "Total revenue of filing public charities." ),
  R( "outcome", "rev_c3_net", "Revenue, net", "Revenue net of grants received from other organizations." ),
  R( "outcome", "rev_c3_nohu", "Revenue excl. H&U", "Revenue, excluding hospitals and universities." ),
  R( "outcome", "rev_c3_nohu_net", "Revenue excl. H&U, net", "Revenue excluding hospitals and universities, net of grants received." ),
  R( "ident", "cs", "Cross-section", "Single-year cross-section (cs_year)." ),
  R( "ident", "pooled", "Pooled", "Pooled 2015-2022 with year fixed effects; standard errors clustered by metro." ),
  R( "ident", "within", "Within", "Two-step within: year-specific SAMIs, metro and year fixed effects, 2010-2022. Time-invariant covariates leave the model." ),
  R( "dv_form", "pc", "Per capita", "Outcome per resident (Y / P)." ),
  R( "dv_form", "logpc", "Log per capita", "log(Y / P)." ),
  R( "dv_form", "loglog", "Log-log", "log(Y) with log P on the right-hand side." ),
  R( "dv_form", "sami", "SAMI", "Scale-adjusted metropolitan indicator: residual of log(Y) on log P, fitted year by year." ),
  R( "dv_form", "pois_off", "Poisson, offset", "Poisson model of Y with offset log P." ),
  R( "dv_form", "pois_free", "Poisson, free log P", "Poisson model of Y with log P as a free regressor." ),
  R( "pred_form", "raw", "Raw", "Covariates as measured." ),
  R( "pred_form", "pc", "Per capita", "Extensive (type B) covariates per resident; intensive (type A) covariates as measured." ),
  R( "pred_form", "log", "Logged", "Extensive covariates logged with a per-capita floor (flog); intensive covariates as measured." ),
  R( "pred_form", "sami", "SAMI form", "Every covariate residualized on log P year by year (extensive ones after flog)." ),
  R( "rhs_logP", "FALSE", "No log P", "log P not on the right-hand side (it is anyway for loglog and pois_free)." ),
  R( "rhs_logP", "TRUE", "log P on the right", "log P added to the right-hand side." ),
  R( "unit", "metro", "Metros", "Metropolitan CBSAs only." ),
  R( "unit", "cbsa", "Metros + micros", "All CBSAs, metropolitan and micropolitan." ),
  R( "est", "ols", "OLS", "OLS (cross-section: heteroskedasticity-robust SEs; panels: metro-clustered; within: metro and year FE)." ),
  R( "est", "state_fe", "State FE", "State fixed effects; state-clustered SEs." ),
  R( "est", "div_fe", "Division FE", "Census division fixed effects; state-clustered SEs." ),
  R( "est", "state_re", "State RE", "Random state intercept (lme4); model-based SEs. Pooled state RE fits are left out of the summaries." ),
  R( "cs_year", "2022", "2022", "Cross-section year 2022 (also the value carried by panel specifications)." ),
  R( "cs_year", "2019", "2019", "Cross-section year 2019 (pre-pandemic check)." ),
  R( "outlier", "none", "None", "No outlier treatment." ),
  R( "outlier", "winsor1", "Winsorized 1%", "Outcome (except Poisson) and covariates winsorized at the 1st and 99th percentiles." ),
  R( "drop", "none", "Full model", "Every covariate block in the model." ),
  R( "dup_of", "<NA>", "Not a duplicate", "The specification is run." ),
  R( "dup_of", "loglog", "= log-log", "Algebraic duplicate of the log-log specification (same covariate coefficients); not run." ),
  R( "dup_of", "pois_free", "= Poisson free", "Duplicate of the free-log-P Poisson specification; not run." ),
  R( "dup_of", "pred_log", "= logged predictors", "SAMI-form predictors alongside log P equal logged / level predictors (FWL); not run." ),
  R( "op_block", "<NA>", "Default measures", "No block uses an alternative measure (op = default)." ),
  R( "op", "default", "Default measures", "Every block uses its default measures." ),
  R( "status", "ok", "Fitted", "The specification was fitted." ),
  R( "status", "no fit", "No fit", "Fewer than 30 usable rows, or the estimator failed." ),
  R( "status", "not run", "Not run", "The specification's batch has not run." ),
  R( "status", "error: ...", "Error", "The fit stopped with this error." ),
  R( "batch", "<integer>", "Batch", "Batch of the run that fitted the specification." ) )

## blocks and covariate variants, from the configurations
blk <- list(); ops <- list()
for ( m in c( "default", "lean" ) ) {
  cfg <- mv_spec_config( m )
  for ( b in names( cfg$blocks ) ) {
    B <- cfg$blocks[[ b ]]
    def <- paste( B$default, collapse = ", " )
    blk[[ length( blk ) + 1 ]] <- data.table( column = c( "drop", "op_block" ), code = b, model = m,
      label = b, description = c( sprintf( "The %s block (%s) is left out of the model.", b, def ),
                                  sprintf( "The %s block uses an alternative measure (see op).", b ) ) )
    for ( a in names( B$alt ) )
      ops[[ length( ops ) + 1 ]] <- data.table( column = "op", code = paste0( b, ":", a ), model = m, label = paste0( b, ": ", a ),
        description = if ( length( B$default ) ) sprintf( "The %s block uses %s instead of %s.", b, paste( B$alt[[ a ]], collapse = ", " ), def )
                      else sprintf( "Add-on: %s is added to the full default model.", paste( B$alt[[ a ]], collapse = ", " ) ) )
  }
}
gen <- rbindlist( c( blk, ops ) )
gen <- gen[ !( column == "drop" & code %in% mv_addon_blocks( mv_spec_config( "default" ) ) & model == "default" ) ]   # add-ons cannot be dropped
gen <- gen[ , .( label = label[1], description = if ( uniqueN( description ) == 1 ) paste0( description[1], " (model ", paste( model, collapse = " and " ), ")" )
                 else paste( sprintf( "%s: %s", model, description ), collapse = " " ) ), by = .( column, code ) ]
dict <- rbind( dict, gen )
dict <- dict[ order( match( column, c( "spec_id", "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
                                       "est", "cs_year", "outlier", "drop", "op", "op_block", "dup_of", "status", "batch" ) ) ) ]

## completeness: every value the grids use has a row
g <- rbindlist( lapply( c( "default", "lean" ), function( m ) rbind( mv_grid( "principled", model = m ), mv_grid( "kitchen", model = m ) ) ) )
miss <- rbindlist( lapply( setdiff( names( g ), "spec_id" ), function( v ) {
  u <- unique( as.character( g[[ v ]] ) ); u[ is.na( u ) ] <- "<NA>"
  data.table( column = v, code = setdiff( u, dict[ column == v, code ] ) ) } ) )
if ( nrow( miss ) ) { print( miss ); stop( nrow( miss ), " grid values have no dictionary row" ) }
fwrite( dict, "pkg/inst/extdata/spec_dictionary.csv" ); fwrite( dict, "data/spec_dictionary.csv" )
cat( nrow( dict ), "rows ->", "pkg/inst/extdata/spec_dictionary.csv, data/spec_dictionary.csv\n" )
print( dict[ , .N, by = column ] )
