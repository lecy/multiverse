#' The lean model's covariates, as the paper reports them
#'
#' Constants of the decision summaries of run v11 ([mv_decision_summary()],
#' [mv_decision_matrix()]):
#'
#' * `MV_V11_TERMS`: the 13 lean terms and their labels (the decision tables). Religion is
#'   entered as `rel_hhi` (concentration) and reported as fractionalization (`rel_frac`),
#'   so its partial r is flipped.
#' * `MV_V11_LABELS`: the shorter labels of the decision-matrix figures.
#' * `MV_V11_BLOCK_OF`: each term's block.
#' * `MV_V11_SWAP_TERM`: for each term with measurement swaps, the terms that measure the
#'   same construct under the swaps (the measurement decision).
#' * `MV_V11_CONSTRUCT_OF`: alternative term -> the construct's default term.
#' @name v11_constants
#' @aliases MV_V11_TERMS MV_V11_LABELS MV_V11_BLOCK_OF MV_V11_SWAP_TERM MV_V11_CONSTRUCT_OF
#' @format NULL
#' @export MV_V11_TERMS MV_V11_LABELS MV_V11_BLOCK_OF MV_V11_SWAP_TERM MV_V11_CONSTRUCT_OF
NULL

MV_V11_TERMS <- c( pct_ba = "% BA", pf_assets = "Foundation assets", pct_nonwhite = "% nonwhite", turnout = "Turnout",
            poverty_rate = "Poverty rate", gov_earn_sl = "S&L govt earnings", tr_medicaid = "Medicaid",
            lag_g10 = "Growth kernel", hist_pre1970 = "Pre-1970 endowment", gini_m = "Gini (metro)",
            rel_frac = "Religious fractionalization", age_sd = "Age SD", pct_65plus = "% 65+" )
MV_V11_BLOCK_OF <- c( pct_ba = "education", pf_assets = "philanthropy", pct_nonwhite = "heterogeneity", turnout = "civic",
               poverty_rate = "need", gov_earn_sl = "gov_direct", tr_medicaid = "gov_indirect", lag_g10 = "growth",
               hist_pre1970 = "history", gini_m = "heterogeneity", rel_frac = "heterogeneity", age_sd = "heterogeneity",
               pct_65plus = "need" )
MV_V11_SWAP_TERM <- list( poverty_rate = c( "poverty_rate", "unemp_rate" ), pct_nonwhite = c( "pct_nonwhite", "race_hhi", "pct_foreign_born" ),
                   pf_assets = c( "pf_assets", "n_pf", "contrib_total", "contrib_per_return", "contrib_share_agi" ),
                   turnout = c( "turnout", "socap_respn" ), lag_g10 = c( "lag_g10", "lag_g5" ),
                   gov_earn_sl = c( "gov_earn_sl", "gov_emp_sl", "cog_dge" ), tr_medicaid = c( "tr_medicaid", "tr_medmaint", "cog_welfare" ) )

MV_V11_LABELS <- c( pct_ba = "% BA", pf_assets = "Foundation assets", pct_nonwhite = "% nonwhite", turnout = "Turnout",
          poverty_rate = "Poverty rate", gov_earn_sl = "S&L govt earnings", tr_medicaid = "Medicaid",
          lag_g10 = "Growth kernel", hist_pre1970 = "Pre-1970 endowment", gini_m = "Gini (metro)",
          rel_frac = "Religious fract.", age_sd = "Age SD", pct_65plus = "% 65+" )
## measurement swaps: the alternative term is read as the construct's default term
MV_V11_CONSTRUCT_OF <- c( unemp_rate = "poverty_rate", race_hhi = "pct_nonwhite", pct_foreign_born = "pct_nonwhite",
                   n_pf = "pf_assets", contrib_total = "pf_assets", contrib_per_return = "pf_assets",
                   contrib_share_agi = "pf_assets", socap_respn = "turnout", dem_margin = "turnout",
                   lag_g5 = "lag_g10", gov_emp_sl = "gov_earn_sl", gov_emp_share = "gov_earn_sl", cog_dge = "gov_earn_sl",
                   tr_medmaint = "tr_medicaid", cog_welfare = "tr_medicaid" )

#' Partial correlation and eta squared, as the multiverse summaries compute them
#'
#' * `mv_partial_r()`: the common currency across outcome forms, r = t / sqrt(t^2 + df),
#'   with df = independent places - (terms + 1). Panel standard errors are clustered by
#'   metro, so places, not place-years, count: n / 8 for pooled and n / 13 for within.
#' * `mv_eta2()`: share of the variance of `y` between the groups `g` (marginal eta
#'   squared). `NA` with fewer than two groups or fewer than `min_n` values.
#'
#' @param stat t statistic.
#' @param n Observations of the fit.
#' @param k Terms in the fit (including log P).
#' @param ident Identification (`"cs"`, `"pooled"`, `"within"`).
#' @param y Values.
#' @param g Groups.
#' @param min_n Fewest values (10 in the decision tables, 0 in the run summary).
#' @param na.rm Drop empty groups from the between sum.
#' @export
mv_partial_r <- function( stat, n, k, ident ) {
  n_units <- n / fcase( ident == "pooled", 8, ident %in% c( "within", "within_long" ), 13, default = 1 )
  stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) )
}

#' @rdname mv_partial_r
#' @export
mv_eta2 <- function( y, g, min_n = 0L, na.rm = FALSE ) {
  g <- as.factor( g ); if ( nlevels( g ) < 2 || length( y ) < min_n ) return( NA_real_ )
  m <- tapply( y, g, mean ); n <- tapply( y, g, length )
  sum( n * ( m - mean( y ) )^2, na.rm = na.rm ) / sum( ( y - mean( y ) )^2 )
}

#' Decision tables of the lean multiverse (run v11)
#'
#' The multiverse evidence for the design-decision cards of the paper
#' (`paper/04-design-decisions.qmd`). Returns a list of tables:
#' `decision_importance_long`, `decision_importance` (eta squared of each decision for each
#' covariate), `option_medians` (median partial r by option, default model), `stability`,
#' `drop_shift` (matched default vs drop-one-block pairs), `est_spearman` (covariate t
#' across estimators), `v10_v11_benchmarks` and `naive_vs_recommended`.
#'
#' With `split = TRUE`, filers, grassroots and spending are three groups (the grassroots
#' refit, outcome `n_grass`, must be bound into `specs` and `co`); otherwise counts pool
#' filers with the registered stock.
#'
#' @param specs,co Specs and coefficients of run v11 (`specs.parquet`, `coefs.parquet`).
#' @param specs_v10,co_v10 The same for run v10 (the benchmarks table).
#' @param split Three groups instead of two.
#' @param terms,block_of,swap_term See [v11_constants].
#' @param verbose Print the headline tables.
#' @return A named list of data.tables.
#' @export
mv_decision_summary <- function( specs, co, specs_v10, co_v10, split = FALSE, terms = MV_V11_TERMS,
                                 block_of = MV_V11_BLOCK_OF, swap_term = MV_V11_SWAP_TERM, verbose = TRUE ) {
  SPLIT <- split; TERMS <- terms; BLOCK_OF <- block_of; SWAP_TERM <- swap_term
  BENCH5 <- names( TERMS )[ 1:5 ]
  r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
  specs <- as.data.table( specs )[ status == "ok" ]
  co <- as.data.table( co )
  out <- list()
  co <- co[ spec_id %in% specs$spec_id ]
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co <- merge( co, specs[ , .( spec_id, ident, est ) ], by = "spec_id" )
  co[ , n_units := n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) ]
  co[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
  co[ , pr_state := ( estimate / se_state ) / sqrt( ( estimate / se_state )^2 + pmax( n_units - k - 1, 1 ) ) ]
  co <- co[ term != "logP" & !( ident == "pooled" & est == "state_re" ) ]
  co[ term == "rel_hhi", `:=`( term = "rel_frac", pr = -pr, stat = -stat, pr_state = -pr_state ) ]
  co[ term == "race_hhi", `:=`( pr = -pr, stat = -stat ) ]            # concentration -> diversity direction
  d <- merge( co[ , .( spec_id, term, pr, stat, p, p_state ) ], specs, by = "spec_id" )
  rm( co )
  log_msg( "  ", format( nrow( d ), big.mark = "," ), " coefficient rows; ", format( nrow( specs ), big.mark = "," ), " specs" )

  eta2 <- function( y, g ) { g <- as.factor( g ); if ( nlevels( g ) < 2 || length( y ) < 10 ) return( NA_real_ )
    m <- tapply( y, g, mean ); n <- tapply( y, g, length )
    sum( n * ( m - mean( y ) )^2, na.rm = TRUE ) / sum( ( y - mean( y ) )^2 ) }
  eta_by <- function( x, fac, by ) {
    if ( !nrow( x ) ) return( NA_real_ )
    e <- x[ , .( e = eta2( pr, get( fac ) ) ), by = by ]
    mean( e$e, na.rm = TRUE ) }

  GROUPS <- if ( SPLIT ) list( filers = quote( outcome == "n_filer_c3" ), grassroots = quote( outcome == "n_grass" ),
                               spending = quote( construct == "exp" & hu == "excl" ) ) else
            list( counts = quote( construct == "count" & hu == "incl" ),
                  spending = quote( construct == "exp" & hu == "excl" ) )
  ## strata for the summary tables: one outcome population per row in split mode
  SKEY <- if ( SPLIT ) c( "construct", "hu", "outcome" ) else c( "construct", "hu" )
  if ( SPLIT ) d <- d[ outcome != "n_bmf_c3pc" ]                  # the registered stock is replaced by grassroots

  ## ---- decision importance --------------------------------------------------------------------
  log_msg( "decision importance" )
  res <- list()
  for ( gname in names( GROUPS ) ) for ( b in names( TERMS ) ) {
    base <- d[ drop == "none" ]
    x  <- base[ eval( GROUPS[[ gname ]] ) & ident != "within" ]
    xd <- x[ op == "default" & term == b ]
    row <- function( dec, val ) res[[ length( res ) + 1 ]] <<- data.table( group = gname, term = b, decision = dec, eta2 = val )
    if ( gname == "counts" ) row( "D1", eta_by( xd, "outcome", "ident" ) )
    else if ( gname != "spending" ) row( "D1", eta_by( base[ outcome %in% c( "n_filer_c3", "n_grass" ) & ident != "within" & op == "default" & term == b ],
                                                       "outcome", "ident" ) )
    else row( "D1", eta_by( base[ hu == "excl" & ident != "within" & op == "default" & term == b &
                                    outcome %in% c( "n_filer_c3_nohu", "exp_c3_nohu" ) ], "construct", "ident" ) )
    row( "D2", eta_by( xd, "unit", c( "ident", "outcome" ) ) )
    row( "D3", eta_by( base[ eval( GROUPS[[ gname ]] ) & op == "default" & term == b ], "ident", "outcome" ) )
    row( "D4", eta_by( xd, "dv_form", c( "ident", "outcome" ) ) )
    row( "D5", eta_by( copy( xd )[ , pois := dv_form %like% "^pois" ], "pois", c( "ident", "outcome" ) ) )
    row( "D6", eta_by( xd, "pred_form", c( "ident", "outcome" ) ) )
    ## measurement (old D7): default vs swaps of this term's own block, read on the term that
    ## measures the construct; for terms with no swap, swaps of other blocks (neighbor effect)
    if ( b %in% names( SWAP_TERM ) ) {
      z <- x[ ( op == "default" | sub( ":.*", "", op ) == BLOCK_OF[[ b ]] ) & term %in% SWAP_TERM[[ b ]] ]
      row( "D7 own measure", eta_by( z, "op", c( "ident", "outcome" ) ) )
    } else row( "D7 own measure", NA_real_ )
    row( "D7 neighbors' measures", eta_by( x[ term == b & sub( ":.*", "", op ) != BLOCK_OF[[ b ]] ], "op", c( "ident", "outcome" ) ) )
    ## D8: endowment in or out; growth kernel in or out (drop-one-block contrasts, default measures)
    xx <- d[ eval( GROUPS[[ gname ]] ) & ident != "within" & op == "default" & term == b ]
    row( "D8 endowment", eta_by( xx[ drop %in% c( "none", "history" ) ], "drop", c( "ident", "outcome" ) ) )
    row( "D8 growth kernel", eta_by( xx[ drop %in% c( "none", "growth" ) ], "drop", c( "ident", "outcome" ) ) )
    row( "Drop any one block", eta_by( xx[ drop != BLOCK_OF[[ b ]] ], "drop", c( "ident", "outcome" ) ) )
    row( "D9", eta_by( xd, "est", c( "ident", "outcome" ) ) )
    if ( gname == "spending" ) row( "D10a", eta_by( xd, "outcome", "ident" ) )
    z <- base[ construct == ifelse( gname == "spending", "exp", "count" ) & ident != "within" & op == "default" & term == b &
               outcome %in% c( "n_filer_c3", "n_filer_c3_nohu", "exp_c3", "exp_c3_nohu" ) ]
    row( "D10b", if ( gname == "grassroots" ) NA_real_ else eta_by( z, "hu", "ident" ) )
    row( "Year", eta_by( xd[ ident == "cs" ], "cs_year", "outcome" ) )
    row( "Winsorizing", eta_by( xd, "outlier", c( "ident", "outcome" ) ) )
    row( "log P on RHS", eta_by( xd, "rhs_logP", c( "ident", "outcome" ) ) )
  }
  imp <- rbindlist( res )
  out$decision_importance_long <- imp
  w <- dcast( imp, group + decision ~ term, value.var = "eta2" )
  setcolorder( w, c( "group", "decision", intersect( names( TERMS ), names( w ) ) ) )
  out$decision_importance <- w
  if ( verbose ) print( r3( w ) )

  ## ---- option medians, default model --------------------------------------------------------------
  log_msg( "option medians" )
  DEC <- c( D1 = "outcome", D2 = "unit", D3 = "ident", D4 = "dv_form", D6 = "pred_form", D9 = "est", Year = "cs_year",
            Winsor = "outlier", RHS = "rhs_logP" )
  om <- list()
  for ( gname in names( GROUPS ) ) {
    x <- d[ eval( GROUPS[[ gname ]] ) & drop == "none" & op == "default" ]
    for ( dn in names( DEC ) ) {
      f <- DEC[[ dn ]]
      xs <- if ( dn == "D3" ) x else x[ ident != "within" ]
      if ( dn == "D1" && gname == "spending" ) xs <- d[ construct == "exp" & drop == "none" & op == "default" & ident != "within" ]
      if ( dn == "D1" && gname %in% c( "filers", "grassroots" ) ) xs <- d[ outcome %in% c( "n_filer_c3", "n_grass" ) & drop == "none" & op == "default" & ident != "within" ]
      om[[ length( om ) + 1 ]] <- xs[ , .( group = gname, decision = dn, specs = .N, median_pr = median( pr ),
                                           same_sign = mean( sign( pr ) == sign( median( pr ) ) ),
                                           sig = mean( p < .05 ), sig_state = mean( p_state < .05, na.rm = TRUE ) ),
                                      by = .( term, option = as.character( get( f ) ) ) ][ , ident_scope := if ( dn == "D3" ) "all" else "cs+pooled" ]
    }
    ## predictor form x log P on the right (D6 Evidence 1), cross-section
    om[[ length( om ) + 1 ]] <- x[ ident == "cs", .( group = gname, decision = "D6xRHS", specs = .N, median_pr = median( pr ),
                                                     same_sign = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( p < .05 ),
                                                     sig_state = mean( p_state < .05, na.rm = TRUE ) ),
                                   by = .( term, option = paste( pred_form, ifelse( rhs_logP | dv_form %in% c( "loglog", "pois_free" ), "logP", "noP" ) ) ) ][ , ident_scope := "cs" ]
    ## OLS vs Poisson (D5)
    om[[ length( om ) + 1 ]] <- x[ ident != "within", .( group = gname, decision = "D5", specs = .N, median_pr = median( pr ),
                                                         same_sign = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( p < .05 ),
                                                         sig_state = mean( p_state < .05, na.rm = TRUE ) ),
                                   by = .( term, option = ifelse( dv_form %like% "^pois", "Poisson", "OLS" ) ) ][ , ident_scope := "cs+pooled" ]
  }
  ## D10: hospitals and universities, and the money measure (spending, default model)
  om[[ length( om ) + 1 ]] <- d[ construct == "exp" & drop == "none" & op == "default" & ident != "within",
    .( group = "spending", decision = "D10", specs = .N, median_pr = median( pr ), same_sign = mean( sign( pr ) == sign( median( pr ) ) ),
       sig = mean( p < .05 ), sig_state = mean( p_state < .05, na.rm = TRUE ) ), by = .( term, option = outcome ) ][ , ident_scope := "cs+pooled" ]
  om <- rbindlist( om, fill = TRUE )
  om <- om[ term %in% names( TERMS ) ]
  out$option_medians <- om

  ## ---- stability: every lean term, by stratum ------------------------------------------------------
  log_msg( "stability" )
  st <- d[ term %in% names( TERMS ), .( specs = .N, median_pr = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
                                        same_sign = mean( sign( pr ) == sign( median( pr ) ) ),
                                        pos_sig = mean( pr > 0 & p < .05 ), neg_sig = mean( pr < 0 & p < .05 ),
                                        sig = mean( p < .05 ), sig_state = mean( p_state < .05, na.rm = TRUE ) ),
           keyby = c( "term", SKEY, "ident" ) ]
  out$stability <- st
  if ( verbose ) print( r3( st[ hu == ifelse( construct == "count", "incl", "excl" ), .( term, construct, ident, median_pr, same_sign, sig, sig_state ) ] ) )

  ## ---- matched drop-one-block shifts ----------------------------------------------------------------
  log_msg( "drop shifts" )
  key <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year", "outlier" )
  dd <- d[ op == "default" & ident != "within" & term %in% names( TERMS ) ]
  b0 <- dd[ drop == "none", c( key, "term", "pr" ), with = FALSE ]; setnames( b0, "pr", "pr0" )
  sh <- merge( dd[ drop != "none", c( key, "drop", "term", "pr" ), with = FALSE ], b0, by = c( key, "term" ) )
  ds <- sh[ , .( pairs = .N, median_shift = median( pr - pr0 ), mean_abs_shift = mean( abs( pr - pr0 ) ),
                 median_pr_default = median( pr0 ), median_pr_dropped = median( pr ),
                 flips = mean( sign( pr ) != sign( pr0 ) & pmax( abs( pr ), abs( pr0 ) ) > .05 ) ),
            keyby = c( "drop", "term", if ( SPLIT ) "outcome" else "construct", "ident" ) ]
  out$drop_shift <- ds
  if ( verbose ) print( r3( ds[ drop %in% c( "history", "growth" ) ] ), nrows = 200 )

  ## ---- Spearman of covariate t across estimators ----------------------------------------------------
  log_msg( "estimator spearman" )
  k2 <- setdiff( c( key, "op", "drop" ), "est" )
  e <- d[ ident == "cs" & drop == "none" & op == "default" & term %in% names( TERMS ), c( k2, "est", "term", "stat" ), with = FALSE ]
  ew <- dcast( e, ... ~ est, value.var = "stat" )
  sp <- function( a, b ) ew[ is.finite( get( a ) ) & is.finite( get( b ) ),
    .( rho = cor( get( a ), get( b ), method = "spearman" ) ), by = setdiff( k2, character( 0 ) ) ][ , .( median_rho = median( rho, na.rm = TRUE ),
                                                                                                       q25 = quantile( rho, .25, na.rm = TRUE ),
                                                                                                       q75 = quantile( rho, .75, na.rm = TRUE ) ),
                                                                                                    by = SKEY ][ , pair := paste( a, "vs", b ) ]
  es <- rbind( sp( "ols", "state_fe" ), sp( "ols", "div_fe" ), sp( "div_fe", "state_fe" ), sp( "ols", "state_re" ) )
  out$est_spearman <- es
  if ( verbose ) print( r3( es ) )

  ## ---- v10 vs v11 benchmarks ------------------------------------------------------------------------
  log_msg( "v10 vs v11" )

  s10 <- as.data.table( specs_v10 )[ status == "ok" & op == "default" & drop == "none" ]
  c10 <- as.data.table( co_v10 )[ spec_id %in% s10$spec_id ]
  c10 <- merge( c10, c10[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  c10 <- merge( c10, s10[ , .( spec_id, construct, hu, outcome, ident, est ) ], by = "spec_id" )
  c10[ , n_units := n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) ]
  c10[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
  c10 <- c10[ term %in% c( BENCH5, "gov_earn_sl" ) & !( ident == "pooled" & est == "state_re" ) ]
  a <- c10[ , .( v10_median = median( pr ), v10_same_sign = mean( sign( pr ) == sign( median( pr ) ) ), v10_sig = mean( p < .05 ) ),
            keyby = c( "term", SKEY, "ident" ) ]
  b <- d[ op == "default" & drop == "none" & term %in% c( BENCH5, "gov_earn_sl" ),
          .( v11_median = median( pr ), v11_same_sign = mean( sign( pr ) == sign( median( pr ) ) ), v11_sig = mean( p < .05 ) ),
          keyby = c( "term", SKEY, "ident" ) ]
  vv <- merge( a, b, by = c( "term", SKEY, "ident" ), all.y = TRUE )
  out$v10_v11_benchmarks <- vv
  if ( verbose ) print( r3( vv[ hu == ifelse( construct == "count", "incl", "excl" ) ] ) )

  ## ---- naive vs. recommended: every card followed at once ----------------------------------------------
  ## naive = median over every default-model specification of the group (all identifications);
  ## recommended = cross-section, metro, filers (counts) or expenses excl. H&U (spending), log-log or SAMI,
  ## SAMI-form or logged predictors with log P on the right, OLS / division FE / state FE (no state RE)
  log_msg( "naive vs recommended" )
  dm <- d[ op == "default" & drop == "none" & term %in% names( TERMS ) ]
  rec <- quote( ident == "cs" & unit == "metro" & dv_form %in% c( "loglog", "sami" ) &
                ( pred_form == "sami" | ( pred_form == "log" & ( rhs_logP | dv_form == "loglog" ) ) ) &
                est %in% c( "ols", "div_fe", "state_fe" ) )
  nrs <- function( cond, g, s ) dm[ eval( cond ), .( group = g, set = s, specs = .N, median_pr = median( pr ),
                                                    agree = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( p < .05 ) ), by = term ]
  nr <- if ( SPLIT ) rbind(
    nrs( quote( outcome == "n_filer_c3" ), "filers", "naive" ), nrs( bquote( outcome == "n_filer_c3" & .( rec ) ), "filers", "recommended" ),
    nrs( quote( outcome == "n_grass" ), "grassroots", "naive" ), nrs( bquote( outcome == "n_grass" & .( rec ) ), "grassroots", "recommended" ),
    nrs( GROUPS$spending, "spending", "naive" ), nrs( bquote( outcome == "exp_c3_nohu" & .( rec ) ), "spending", "recommended" ) ) else rbind(
    dm[ eval( GROUPS$counts ), .( group = "counts", set = "naive", specs = .N, median_pr = median( pr ),
                                  agree = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( p < .05 ) ), by = term ],
    dm[ eval( GROUPS$spending ), .( group = "spending", set = "naive", specs = .N, median_pr = median( pr ),
                                    agree = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( p < .05 ) ), by = term ],
    dm[ outcome == "n_filer_c3" & eval( rec ), .( group = "counts", set = "recommended", specs = .N, median_pr = median( pr ),
                                                  agree = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( p < .05 ) ), by = term ],
    dm[ outcome == "exp_c3_nohu" & eval( rec ), .( group = "spending", set = "recommended", specs = .N, median_pr = median( pr ),
                                                   agree = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( p < .05 ) ), by = term ] )
  out$naive_vs_recommended <- nr
  if ( verbose ) print( r3( dcast( nr, term ~ group + set, value.var = c( "median_pr", "sig" ) ) ) )
  if ( verbose ) print( nr[ , .( specs = max( specs ) ), by = .( group, set ) ] )
  out[ c( "decision_importance_long", "decision_importance", "option_medians", "stability", "drop_shift",
          "est_spearman", "v10_v11_benchmarks", "naive_vs_recommended" ) ]
}

#' Decision matrix of a multiverse run
#'
#' The numbers behind the decision-matrix figures: for each covariate and design decision,
#' the eta squared of the decision, the mean partial r of each option, and, from matched
#' pairs (specifications that differ only in this decision), the mean |change in r| and the
#' share of pairs whose verdict changes (significantly positive / not significant /
#' significantly negative). Also the all-specifications summary per covariate and the
#' global score per decision.
#'
#' @param specs,co Specs (status `"ok"`) and coefficients of the run; with `split = TRUE`,
#'   the grassroots refit bound in and the registered stock (`n_bmf_c3pc`) left out.
#' @param split Filers, grassroots and spending as three groups instead of counts and spending.
#' @param eta_min The eta squared from which a decision counts as meaningful.
#' @param labels Covariates to report, named, in order (see [v11_constants]).
#' @param construct_of Alternative term -> construct's default term.
#' @return A list: `cells`, `option_means`, `all_specs`, `global_scores` (the four tables
#'   written by `98_decision_matrix.R`), and `d` (the coefficient rows), `groups` and
#'   `terms`, which the figures use.
#' @export
mv_decision_matrix <- function( specs, co, split = FALSE, eta_min = .06, labels = MV_V11_LABELS,
                                construct_of = MV_V11_CONSTRUCT_OF ) {
  SPLIT <- split; ETA_MIN <- eta_min; LAB <- labels; CONSTRUCT_OF <- construct_of
  specs <- as.data.table( specs ); co <- as.data.table( co )
  keep <- specs[ drop %in% c( "none", "history", "growth" ) & ( op == "default" | drop == "none" ) ]
  co <- co[ spec_id %in% keep$spec_id ]
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co <- merge( co, keep[ , .( spec_id, ident, est ) ], by = "spec_id" )
  co[ , n_units := n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) ]
  co[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
  co <- co[ term != "logP" & !( ident == "pooled" & est == "state_re" ) ]
  co[ term %in% c( "rel_hhi", "race_hhi" ), pr := -pr ]                         # concentration -> diversity
  co[ term == "rel_hhi", term := "rel_frac" ]
  co[ , cls := fifelse( p < .05, sign( pr ), 0 ) ]
  d <- merge( co[ , .( spec_id, term, pr, cls ) ], keep, by = "spec_id" )
  rm( co )
  d[ , construct_term := fifelse( term %in% names( CONSTRUCT_OF ), CONSTRUCT_OF[ term ], term ) ]
  TERMS <- intersect( names( LAB ), unique( d[ op == "default" & drop == "none", term ] ) )
  log_msg( "  terms: ", paste( TERMS, collapse = ", " ) )

  FORKS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year",
              "outlier", "op", "drop" )
  eta2 <- function( y, g ) { g <- as.factor( g ); if ( nlevels( g ) < 2 || length( y ) < 10 ) return( NA_real_ )
    m <- tapply( y, g, mean ); n <- tapply( y, g, length )
    sum( n * ( m - mean( y ) )^2, na.rm = TRUE ) / sum( ( y - mean( y ) )^2 ) }
  ## pairwise summaries within a matched group
  pw_abs  <- function( r ) { k <- length( r ); if ( k < 2 ) return( NA_real_ ); sum( abs( outer( r, r, "-" ) ) ) / ( k * ( k - 1 ) ) }
  pw_flip <- function( c ) { k <- length( c ); if ( k < 2 ) return( NA_real_ ); sum( outer( c, c, "!=" ) ) / ( k * ( k - 1 ) ) }

  ## ---- decisions --------------------------------------------------------------------------------
  ## each: data (default model unless noted), the fork column, the strata eta^2 is averaged over,
  ## the columns that vary together with the fork (dropped from the matching key), option labels
  DEFAULT <- quote( op == "default" & drop == "none" )
  GROUPS <- if ( SPLIT ) list( filers = quote( outcome == "n_filer_c3" ), grassroots = quote( outcome == "n_grass" ),
                               spending = quote( construct == "exp" & hu == "excl" ) ) else
            list( counts = quote( construct == "count" & hu == "incl" ), spending = quote( construct == "exp" & hu == "excl" ) )
  decisions <- function( g ) {
    x0 <- d[ eval( DEFAULT ) & ident != "within" ]
    xg <- x0[ eval( GROUPS[[ g ]] ) ]
    L <- list(
      D1 = if ( g == "counts" ) list( x = xg, f = "outcome", by = "ident", lab = "Outcome",
                                      opt = c( n_filer_c3 = "filers", n_bmf_c3pc = "registered" ) )
           else if ( g != "spending" ) list( x = x0[ outcome %in% c( "n_filer_c3", "n_grass" ) ], f = "outcome", by = "ident", lab = "Population",
                                             opt = c( n_filer_c3 = "filers", n_grass = "grassroots" ) )
           else list( x = x0[ hu == "excl" & outcome %in% c( "n_filer_c3_nohu", "exp_c3_nohu" ) ], f = "construct",
                      by = "ident", lab = "Counts vs\nspending", vary = "outcome", opt = c( count = "counts", exp = "spending" ) ),
      D2 = list( x = xg, f = "unit", by = c( "ident", "outcome" ), lab = "Unit", opt = c( metro = "metro", cbsa = "+ micro" ) ),
      D3 = list( x = d[ eval( DEFAULT ) & eval( GROUPS[[ g ]] ) & est == "ols" & cs_year == 2022L ], f = "ident", by = "outcome", lab = "Identification",
                 opt = c( cs = "cross-section", pooled = "pooled", within = "within" ) ),
      D4 = list( x = xg, f = "dv_form", by = c( "ident", "outcome" ), lab = "DV form",
                 opt = c( pc = "per capita", logpc = "log pc", loglog = "log-log", sami = "SAMI", pois_off = "Poisson offset", pois_free = "Poisson free" ) ),
      D6 = list( x = xg, f = "pred_form", by = c( "ident", "outcome" ), lab = "Predictor form",
                 opt = c( raw = "raw", pc = "per capita", log = "logged", sami = "SAMI form" ) ),
      RHS = list( x = xg, f = "rhs_logP", by = c( "ident", "outcome" ), lab = "log P on\nthe right", opt = c( "FALSE" = "no", "TRUE" = "yes" ) ),
      D7 = list( x = d[ drop == "none" & ident != "within" & eval( GROUPS[[ g ]] ) ], f = "op", by = c( "ident", "outcome" ),
                 lab = "Measure\n(own swap)", opt = NULL, own = TRUE ),
      D8 = list( x = d[ op == "default" & drop %in% c( "none", "history" ) & ident != "within" & eval( GROUPS[[ g ]] ) ], f = "drop",
                 by = c( "ident", "outcome" ), lab = "Endowment", opt = c( none = "in", history = "out" ) ),
      Growth = list( x = d[ op == "default" & drop %in% c( "none", "growth" ) & ident != "within" & eval( GROUPS[[ g ]] ) ], f = "drop",
                     by = c( "ident", "outcome" ), lab = "Growth\nkernel", opt = c( none = "in", growth = "out" ) ),
      D9 = list( x = xg, f = "est", by = c( "ident", "outcome" ), lab = "Estimator",
                 opt = c( ols = "OLS", div_fe = "division FE", state_fe = "state FE", state_re = "state RE" ) ),
      D10 = { z <- x0[ construct == ifelse( g == "spending", "exp", "count" ) &
                       outcome %in% c( "n_filer_c3", "n_filer_c3_nohu", "exp_c3", "exp_c3_nohu" ) ]
              list( x = z, f = "hu", by = "ident", lab = "Hospitals &\nuniversities", vary = "outcome", opt = c( incl = "in", excl = "out" ) ) },
      Year = list( x = xg[ ident == "cs" ], f = "cs_year", by = "outcome", lab = "Year", opt = c( "2022" = "2022", "2019" = "2019" ) ),
      Winsor = list( x = xg, f = "outlier", by = c( "ident", "outcome" ), lab = "Winsorizing", opt = c( none = "none", winsor1 = "1%" ) ) )
    if ( g == "spending" ) L$Money <- list( x = xg, f = "outcome", by = "ident", lab = "Money\nmeasure",
                                            opt = c( exp_c3_nohu = "expenses", exp_c3_nohu_net = "net", rev_c3_nohu = "revenue", rev_c3_nohu_net = "rev. net" ) )
    if ( g == "grassroots" ) L$D10 <- NULL                         # no grassroots series without hospitals & universities
    if ( !any( d$drop == "growth" ) ) L$Growth <- NULL
    if ( !any( d$drop == "history" ) ) L$D8 <- NULL
    L
  }

  cell_stats <- function( D, term_v ) {
    x <- copy( D$x )
    if ( isTRUE( D$own ) ) {
      ## measurement: default vs swaps of the term's own block, read on the construct's term
      x <- x[ construct_term == term_v ]
      sw <- unique( x[ op != "default" & term != term_v, op ] )
      x <- x[ op == "default" | op %in% sw ]
      x <- x[ op == "default" & term == term_v | op %in% sw ]
    } else x <- x[ term == term_v ]
    if ( !nrow( x ) || uniqueN( x[[ D$f ]] ) < 2 ) return( NULL )
    x[ , opt := as.character( get( D$f ) ) ]
    e <- x[ , .( e = eta2( pr, opt ) ), by = c( D$by ) ][ , mean( e, na.rm = TRUE ) ]
    key <- setdiff( FORKS, c( D$f, D$vary, if ( isTRUE( D$own ) ) "op" ) )
    if ( !is.null( D$vary ) && D$f == "hu" ) { x[ , base_y := sub( "_nohu$", "", outcome ) ]; key <- c( key, "base_y" ) }
    m <- x[ , .( n_opt = .N, dabs = pw_abs( pr ), flip = pw_flip( cls ) ), by = key ][ n_opt >= 2 ]
    means <- x[ , .( mean_pr = mean( pr ), n = .N ), by = opt ]
    list( eta2 = e, dabs = mean( m$dabs ), flip = mean( m$flip ), pairs = nrow( m ), means = means )
  }

  out_cells <- list(); out_means <- list(); out_all <- list(); out_glob <- list()
  for ( g in names( GROUPS ) ) {
    DL <- decisions( g )
    ## all-specifications strip (every covariate set and option, all identifications)
    a <- d[ eval( GROUPS[[ g ]] ) & construct_term %in% TERMS & term %in% TERMS ]
    out_all[[ g ]] <- a[ , .( group = g, n = .N, median = median( pr ), agree = mean( sign( pr ) == sign( median( pr ) ) ),
                              sig = mean( cls != 0 ), pos_sig = mean( cls > 0 ), neg_sig = mean( cls < 0 ) ), by = term ]
    for ( dn in names( DL ) ) for ( tv in TERMS ) {
      s <- tryCatch( cell_stats( DL[[ dn ]], tv ), error = function( e ) { message( dn, " ", tv, ": ", conditionMessage( e ) ); NULL } )
      if ( is.null( s ) ) next
      out_cells[[ length( out_cells ) + 1 ]] <- data.table( group = g, decision = dn, label = DL[[ dn ]]$lab, term = tv,
                                                            eta2 = s$eta2, dabs = s$dabs, flip = s$flip, pairs = s$pairs )
      mm <- s$means; op <- DL[[ dn ]]$opt
      mm[ , opt_lab := if ( is.null( op ) ) ifelse( opt == "default", "default", "swap" ) else unname( op[ opt ] ) ]
      mm[ , oi := if ( is.null( op ) ) ifelse( opt == "default", 1L, 2L ) else match( opt, names( op ) ) ]
      if ( is.null( op ) ) mm <- mm[ , .( mean_pr = weighted.mean( mean_pr, n ), n = sum( n ) ), by = .( opt_lab, oi ) ]
      out_means[[ length( out_means ) + 1 ]] <- mm[ , .( group = g, decision = dn, term = tv, opt_lab, oi, mean_pr, n ) ]
    }
    log_msg( "  ", g, " done" )
  }
  cells <- rbindlist( out_cells ); means <- rbindlist( out_means, fill = TRUE ); allr <- rbindlist( out_all )
  glob <- cells[ , .( mean_dabs = mean( dabs, na.rm = TRUE ), flip_rate = mean( flip, na.rm = TRUE ),
                      n_meaningful = sum( eta2 >= ETA_MIN, na.rm = TRUE ), n_terms = .N,
                      pooled_eta2 = NA_real_ ), by = .( group, decision, label ) ]
  list( cells = cells, option_means = means, all_specs = allr, global_scores = glob, d = d, groups = GROUPS, terms = TERMS )
}
