## 41_composites.R
## Additive composite covariates as one more operationalization fork per block
## (evidence: notes/collinearity-composites-results.md, 70_collinearity_composites.R).
## Source AFTER 40_multiverse.R. Only the composite runner (53_run_composites.R)
## sources it: it extends BLOCKS and COVS in that R process, so the default grid
## and its spec_ids are unchanged for every other script.
##
## Construction
## - A composite is the mean of signed z-scores of its items (unit weights;
##   ML factor-score weights collapse onto one item when a loading is ~1).
## - Items follow the spec's predictor form: type-B (extensive) items are raw |
##   per capita | log(1+x); the SAMI arm uses the log base. Dollar intensives
##   (per capita income, median HH income, contributions per return) are logged
##   in every form, as in the factor analysis.
## - z constants (mean, SD) are fixed on metros pooled 2015-2022 in each base,
##   so a composite is one fixed linear index across units, years and specs.
## - The engine treats a composite as a type-A variable: used as-is, or
##   residualized on log P in the all-SAMI arm. Because the index is linear
##   with fixed weights, the FWL duplicate rules stay exact.
## - A composite is NA when any item is NA.

COMPOSITE_DEFS <- list(
  cmp_affluence   = c( pcpi = 1, med_hh_income = 1, pct_ba = 1, poverty_rate = -1 ),
  cmp_age         = c( age_sd = 1, pct_65plus = 1 ),
  cmp_diversity   = c( pct_nonwhite = 1, pct_foreign_born = 1, race_hhi = -1 ),
  cmp_foundations = c( pf_assets = 1, pf_grants = 1, n_pf = 1 ),
  cmp_giving      = c( contrib_total = 1, contrib_per_return = 1, contrib_share_agi = 1 ),
  ## census mail response, not association counts: CBP membership
  ## associations are largely nonprofits (mechanically tied to the DV)
  cmp_civic       = c( turnout = 1, socap_respn = 1 ),
  cmp_relcons     = c( pct_adherent = 1, rel_cng = 1, dem_margin = -1 ),
  cmp_government  = c( gov_earn_sl = 1, gov_emp_sl = 1, gov_emp_share = 1 ) )
COMP_TYPE_B <- c( "pf_assets", "pf_grants", "n_pf", "contrib_total", "gov_earn_sl", "gov_emp_sl", "rel_cng" )
COMP_LOG_A  <- c( "pcpi", "med_hh_income", "contrib_per_return" )
COMP_REF_YEARS <- 2015:2022

## one composite option per block (two where the EFA found two constructs)
COMP_ALTS <- list(
  diversity    = list( index = "cmp_diversity" ),
  philanthropy = list( pf_index = "cmp_foundations", giving_index = "cmp_giving",
                       both_index = c( "cmp_foundations", "cmp_giving" ) ),
  political    = list( civic_index = "cmp_civic", culture_index = c( "cmp_civic", "cmp_relcons" ) ),
  government   = list( index = "cmp_government" ),
  need         = list( index = "cmp_affluence", index_age = c( "cmp_affluence", "cmp_age" ) ) )

for ( b in names( COMP_ALTS ) ) BLOCKS[[ b ]]$alt <- c( BLOCKS[[ b ]]$alt, COMP_ALTS[[ b ]] )
COMP_OPS <- unlist( lapply( names( COMP_ALTS ), function( b ) paste0( b, ":", names( COMP_ALTS[[ b ]] ) ) ),
                    use.names = FALSE )
COVS <- rbind( COVS, data.table( var = names( COMPOSITE_DEFS ), block = "composite", type = "A" ), fill = TRUE )

comp_item <- function( panel, v, base ) {
  x <- as.numeric( panel[[ v ]] )
  if ( v %in% COMP_TYPE_B ) x <- switch( base, raw = x, pc = x / panel$population, log = flog( x, panel$population ) )   # per-capita floor (40_multiverse.R)
  else if ( v %in% COMP_LOG_A ) x <- suppressWarnings( log( x ) )
  x[ !is.finite( x ) ] <- NA_real_
  x
}

## three copies of the panel, one per composite base (raw | pc | log)
composite_panels <- function( panel ) {
  ref <- panel$cbsa_type == "metro" & panel$year %in% COMP_REF_YEARS
  out <- list(); consts <- list()
  for ( base in c( "raw", "pc", "log" ) ) {
    p <- copy( panel )
    for ( k in names( COMPOSITE_DEFS ) ) {
      w <- COMPOSITE_DEFS[[ k ]]
      z <- sapply( names( w ), function( v ) {
        x <- comp_item( panel, v, base ); m <- mean( x[ ref ], na.rm = TRUE ); s <- sd( x[ ref ], na.rm = TRUE )
        consts[[ length( consts ) + 1 ]] <<- data.table( base = base, composite = k, item = v,
                                                         sign = w[[ v ]], mean = m, sd = s )
        ( x - m ) / s } )
      set( p, j = k, value = as.numeric( z %*% w ) / length( w ) )
    }
    out[[ base ]] <- p
  }
  attr( out, "constants" ) <- rbindlist( consts )
  out
}

## run one spec, picking the panel whose composites match its predictor form
run_spec_comp <- function( s, cpanels ) {
  pf <- as.list( s )$pred_form
  run_spec( s, cpanels[[ if ( pf == "sami" ) "log" else pf ]] )
}
