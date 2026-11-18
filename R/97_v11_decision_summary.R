## 97_v11_decision_summary.R
## Multiverse evidence for the design-decision cards (paper/04-design-decisions.qmd) on run v11,
## the lean-default multiverse (MV_MODEL=lean in 40_multiverse.R: 13 covariates, 432,432 specs).
## Mirrors 58_outline_analyses.R part A (decision importance) with the lean model's decisions:
## measurement swaps replace the old D7 and add-on blocks; dropping the endowment (history) and
## the growth kernel are the D8 contrasts.
##
## Writes D:/MULTIVERSE/data/outline/v11/mv/:
##   decision_importance{,_long}.csv   eta^2 of each decision x covariate, counts and spending
##   option_medians.csv                median partial r by option of each decision, default model
##   stability.csv                     median, sign agreement, share significant (robust / state-clustered)
##   drop_shift.csv                    matched default vs drop-one-block pairs: median change in partial r
##   est_spearman.csv                  Spearman of covariate t across estimators, matched on other forks
##   v10_v11_benchmarks.csv            the five benchmarks' default-model medians, v10 vs v11
## Run from multiverse/ (single process; reads ~6M coefficient rows):
##   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/97_v11_decision_summary.R > D:/MULTIVERSE/logs/97.log 2>&1

## MV_SPLIT=1: report filers, grassroots and spending as three groups instead of pooling filers with the
## registered stock as "counts". Grassroots specs come from R/104_fit_grassroots_v11.R (every registered-
## stock spec refit with n_bmf_c3pc - n_filer_c3); outputs go to v11/mv_split/. D1 for the two count
## groups is then filers vs grassroots.

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ) } )
SPLIT <- Sys.getenv( "MV_SPLIT" ) == "1"
RUN <- file.path( OUT$spec, "run_principled_v11" )
OD  <- file.path( BUILD, "data", "outline", "v11", if ( SPLIT ) "mv_split" else "mv" )
dir.create( OD, recursive = TRUE, showWarnings = FALSE )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]

## the 13 lean terms; religion is entered as rel_hhi (concentration) and reported as
## fractionalization, so its partial r is flipped
TERMS <- c( pct_ba = "% BA", pf_assets = "Foundation assets", pct_nonwhite = "% nonwhite", turnout = "Turnout",
            poverty_rate = "Poverty rate", gov_earn_sl = "S&L govt earnings", tr_medicaid = "Medicaid",
            lag_g10 = "Growth kernel", hist_pre1970 = "Pre-1970 endowment", gini_m = "Gini (metro)",
            rel_frac = "Religious fractionalization", age_sd = "Age SD", pct_65plus = "% 65+" )
BENCH5 <- names( TERMS )[ 1:5 ]
BLOCK_OF <- c( pct_ba = "education", pf_assets = "philanthropy", pct_nonwhite = "heterogeneity", turnout = "civic",
               poverty_rate = "need", gov_earn_sl = "gov_direct", tr_medicaid = "gov_indirect", lag_g10 = "growth",
               hist_pre1970 = "history", gini_m = "heterogeneity", rel_frac = "heterogeneity", age_sd = "heterogeneity",
               pct_65plus = "need" )
## the term that measures the same construct under each swap (for the measurement decision)
SWAP_TERM <- list( poverty_rate = c( "poverty_rate", "unemp_rate" ), pct_nonwhite = c( "pct_nonwhite", "race_hhi", "pct_foreign_born" ),
                   pf_assets = c( "pf_assets", "n_pf", "contrib_total", "contrib_per_return", "contrib_share_agi" ),
                   turnout = c( "turnout", "socap_respn" ), lag_g10 = c( "lag_g10", "lag_g5" ),
                   gov_earn_sl = c( "gov_earn_sl", "gov_emp_sl", "cog_dge" ), tr_medicaid = c( "tr_medicaid", "tr_medmaint", "cog_welfare" ) )

log_msg( "read v11" )
specs <- as.data.table( read_parquet( file.path( RUN, "specs.parquet" ) ) )[ status == "ok" ]
co <- as.data.table( read_parquet( file.path( RUN, "coefs.parquet" ) ) )
if ( SPLIT ) {
  GRD <- file.path( OUT$spec, "run_principled_v11_grass" )
  specs <- rbind( specs, as.data.table( read_parquet( file.path( GRD, "specs.parquet" ) ) )[ status == "ok" ], fill = TRUE )
  co <- rbind( co, as.data.table( read_parquet( file.path( GRD, "coefs.parquet" ) ) ), fill = TRUE )
}
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
rm( co ); gc()
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
fwrite( imp, file.path( OD, "decision_importance_long.csv" ) )
w <- dcast( imp, group + decision ~ term, value.var = "eta2" )
setcolorder( w, c( "group", "decision", intersect( names( TERMS ), names( w ) ) ) )
fwrite( w, file.path( OD, "decision_importance.csv" ) )
options( width = 250 ); print( r3( w ) )

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
fwrite( om, file.path( OD, "option_medians.csv" ) )

## ---- stability: every lean term, by stratum ------------------------------------------------------
log_msg( "stability" )
st <- d[ term %in% names( TERMS ), .( specs = .N, median_pr = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
                                      same_sign = mean( sign( pr ) == sign( median( pr ) ) ),
                                      pos_sig = mean( pr > 0 & p < .05 ), neg_sig = mean( pr < 0 & p < .05 ),
                                      sig = mean( p < .05 ), sig_state = mean( p_state < .05, na.rm = TRUE ) ),
         keyby = c( "term", SKEY, "ident" ) ]
fwrite( st, file.path( OD, "stability.csv" ) )
print( r3( st[ hu == ifelse( construct == "count", "incl", "excl" ), .( term, construct, ident, median_pr, same_sign, sig, sig_state ) ] ) )

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
fwrite( ds, file.path( OD, "drop_shift.csv" ) )
print( r3( ds[ drop %in% c( "history", "growth" ) ] ), nrows = 200 )

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
fwrite( es, file.path( OD, "est_spearman.csv" ) )
print( r3( es ) )

## ---- v10 vs v11 benchmarks ------------------------------------------------------------------------
log_msg( "v10 vs v11" )
v10 <- file.path( OUT$spec, "run_principled_v10" )
s10 <- as.data.table( read_parquet( file.path( v10, "specs.parquet" ) ) )[ status == "ok" & op == "default" & drop == "none" ]
c10 <- as.data.table( read_parquet( file.path( v10, "coefs.parquet" ) ) )[ spec_id %in% s10$spec_id ]
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
fwrite( vv, file.path( OD, "v10_v11_benchmarks.csv" ) )
print( r3( vv[ hu == ifelse( construct == "count", "incl", "excl" ) ] ) )

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
fwrite( nr, file.path( OD, "naive_vs_recommended.csv" ) )
print( r3( dcast( nr, term ~ group + set, value.var = c( "median_pr", "sig" ) ) ) )
print( nr[ , .( specs = max( specs ) ), by = .( group, set ) ] )
log_msg( "done" )
