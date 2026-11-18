## 94_variables_panel_model.R
## Key variables for a unified panel model of nonprofit density, organized by theory
## (demand, supply, market niche). Draft section: paper/02-variables.qmd (replaces outline D7).
## Results note: notes/variables-panel-model-results.md.
##
## Parts (run all by default; or name parts as arguments):
##   build     metro panel 2010-2022 in model form -> D:/MULTIVERSE/data/variables/panel_model_form.parquet
##   describe  three correlation matrices (2022, 387 metros), VIFs within family, PC1 / Cronbach alpha
##             per construct, within-metro variance shares
##   models    demand / supply / niche / combined: cs 2022, pooled 2010-2022 (year FE + year-specific
##             log P slopes, metro-clustered SEs), within (metro + year FE)
##   index     dominant item vs PC1 index vs standardized-mean index, per construct, in the combined model
##   lean      recommended lean set: cs, pooled, within
##   figs      heatmaps and coefficient figures -> paper/figures/
##
## Conventions (mirror run_spec(), all-SAMI arm):
##   outcomes are year-specific SAMIs: resid of log Y on log P by year (resid_logP);
##   dollar totals and counts: flog(x, pop) then resid_logP (SAMI form);
##   rates as is (incomes and per-worker / per-return dollars logged);
##   log P stays on the right-hand side (cs: log P; pooled: log P slope by year), so rates are
##   effectively size-adjusted too (FWL); coefficients are per SD of the model-form covariate;
##   partial r = t / sqrt(t^2 + df), df counted in metros (G - K - 1).
##
## Run from multiverse/:
##   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/paper/94_variables_panel_model.R > D:/MULTIVERSE/logs/94.log 2>&1

suppressWarnings( suppressPackageStartupMessages( {
  source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" )
  library( ggplot2 ) } ) )

VOUT <- file.path( BUILD, "data", "variables" )
dir.create( VOUT, recursive = TRUE, showWarnings = FALSE )
FIG <- "paper/figures"
dir.create( FIG, recursive = TRUE, showWarnings = FALSE )
args <- commandArgs( trailingOnly = TRUE )
PARTS <- if ( length( args ) ) args else c( "build", "describe", "models", "index", "lean", "figs" )
YRS <- 2010:2022
CS_YEAR <- 2022L

## ---- variable catalog ----------------------------------------------------------------
## family / construct / form. form: "rate" (as is), "lrate" (log of a per-person or per-worker
## dollar rate), "sami" (dollar total or count: flog, then residualized on log P by year).
## flag: "" clean; "interp" mostly interpolated; "outcome" partly measures the outcome; "fixed"
## time-invariant.
VARS <- fread( text = "
var|src|form|family|construct|label|flag
poverty_rate|poverty_rate|rate|demand|need|Poverty rate|
unemp_rate|unemp_rate|rate|demand|need|Unemployment rate|
l_mhi|med_hh_income|lrate|demand|need|Median HH income (log)|
pct_65plus|pct_65plus|rate|demand|need|% 65+|
gini_m|gini_m|rate|demand|diversity|Gini (metro)|
pct_nonwhite|pct_nonwhite|rate|demand|diversity|% nonwhite|
race_frac|race_hhi|rate|demand|diversity|Racial fractionalization|
pct_foreign_born|pct_foreign_born|rate|demand|diversity|% foreign-born|
rel_frac|rel_hhi|rate|demand|diversity|Religious fractionalization|interp
age_sd|age_sd|rate|demand|diversity|Age SD|
pol_div|dem_margin|rate|demand|diversity|Political division|interp
l_pcpi|pcpi|lrate|supply|wealth|Per capita income (log)|
s_pf_assets|pf_assets|sami|supply|wealth|Foundation assets|
s_n_pf|n_pf|sami|supply|wealth|No. of foundations|
s_pf_grants|pf_grants|sami|supply|wealth|Foundation grants|
s_contrib|contrib_total|sami|supply|wealth|Itemized contributions|
l_contrib_pr|contrib_per_return|lrate|supply|wealth|Contributions per return (log)|
l_gdppw|gdp_per_worker|lrate|supply|wealth|GDP per worker (log)|
s_privwages|qcew_priv_wages|sami|supply|wealth|Private wage bill|
pct_ba|pct_ba|rate|supply|education|% BA|
turnout|turnout|rate|supply|socap|Turnout|interp
socap_respn|socap_respn|rate|supply|socap|Census mail response|interp
pct_adherent|pct_adherent|rate|supply|socap|Religious adherence|interp
s_cong|rel_cng|sami|supply|socap|Congregations|interp
lag_g5|lag_g5|rate|niche|growth|Growth kernel, 5-yr|
lag_g10|lag_g10|rate|niche|growth|Growth kernel, 10-yr|
gr10|logP|rate|niche|growth|Growth, past 10 yrs|
g7000|g7000|rate|niche|growth|Growth 1970-2000|fixed
s_gov_earn|gov_earn_sl|sami|niche|government|S&L govt earnings|
s_gov_emp|gov_emp_sl|sami|niche|government|S&L govt jobs|
s_medicaid|tr_medicaid|sami|niche|government|Medicaid|
s_medmaint|tr_medmaint|sami|niche|government|Medicare+Medicaid+inc. maint.|
s_welfare|cog_welfare|sami|niche|government|Local welfare spending|interp
s_dge|cog_dge|sami|niche|government|Local direct gen. expenditure|interp
s_hist|hist_pre1970|sami|niche|history|Pre-1970 endowment|fixed
bls_pct_npo|bls_pct_npo|rate|niche|history|Nonprofit share of private jobs|outcome
", sep = "|", na.strings = "" )
VARS[ is.na( flag ), flag := "" ]
LAB <- setNames( VARS$label, VARS$var )
LAB <- c( LAB, logP = "log population", y_filer = "Filer SAMI", y_grass = "Grassroots SAMI",
          y_spend = "Spending SAMI (excl. H/U)" )
OUTC <- c( y_filer = "filers", y_grass = "grassroots", y_spend = "spending" )

## candidate sets for the block models: one or two items per construct, redundant twins out
## (chosen from the correlation matrices; see the results note)
CAND <- list(
  demand = c( "poverty_rate", "unemp_rate", "pct_65plus", "gini_m", "pct_nonwhite", "pct_foreign_born",
              "rel_frac", "age_sd", "pol_div" ),
  supply = c( "l_pcpi", "s_pf_assets", "s_contrib", "pct_ba", "turnout", "socap_respn", "pct_adherent", "s_cong" ),
  niche  = c( "lag_g10", "g7000", "s_gov_earn", "s_medicaid", "s_welfare", "s_dge", "s_hist" ) )

## constructs: items signed so that higher = more of the construct; dominant item first
CONSTRUCTS <- list(
  need       = list( items = c( poverty_rate = 1, unemp_rate = 1, l_mhi = -1, pct_65plus = 1 ), dom = "poverty_rate" ),
  diversity  = list( items = c( pct_nonwhite = 1, race_frac = 1, pct_foreign_born = 1, rel_frac = 1, age_sd = 1,
                                gini_m = 1, pol_div = 1 ), dom = "pct_nonwhite" ),
  wealth     = list( items = c( s_pf_assets = 1, s_n_pf = 1, s_pf_grants = 1, s_contrib = 1, l_pcpi = 1,
                                l_gdppw = 1, s_privwages = 1 ), dom = "s_pf_assets" ),
  education  = list( items = c( pct_ba = 1, l_pcpi = 1, l_mhi = 1, poverty_rate = -1 ), dom = "pct_ba" ),
  socap      = list( items = c( turnout = 1, socap_respn = 1, pct_adherent = 1, s_cong = 1 ), dom = "turnout" ),
  government = list( items = c( s_gov_earn = 1, s_gov_emp = 1, s_medicaid = 1, s_medmaint = 1, s_welfare = 1,
                                s_dge = 1 ), dom = "s_gov_earn" ),
  growth     = list( items = c( lag_g10 = 1, lag_g5 = 1, gr10 = 1, g7000 = 1 ), dom = "lag_g10" ),
  history    = list( items = c( s_hist = 1, bls_pct_npo = 1 ), dom = "s_hist" ) )

## ---- helpers ------------------------------------------------------------------------------
fmt <- function( x, d = 2 ) sub( "^(-?)0\\.", "\\1.", formatC( x, format = "f", digits = d ) )
prt <- function( t, df ) t / sqrt( t^2 + df )
alpha_c <- function( Z ) { Z <- Z[ complete.cases( Z ), , drop = FALSE ]; k <- ncol( Z )
  if ( k < 2 ) return( NA_real_ ); k / ( k - 1 ) * ( 1 - sum( apply( Z, 2, var ) ) / var( rowSums( Z ) ) ) }
vif_of <- function( X ) { R <- cor( X, use = "pairwise.complete.obs" ); setNames( diag( solve( R ) ), colnames( X ) ) }

## ---- build ------------------------------------------------------------------------------
if ( "build" %in% PARTS ) {
  log_msg( "build" )
  p <- load_panel()
  ## 10-year growth needs log P from 2000
  p <- p[ cbsa_type == "metro" ]
  setkey( p, cbsa, year )
  p[ , gr10 := logP - shift( logP, 10 ), by = cbsa ]
  g <- fread( file.path( BUILD, "data", "catchup", "c9_metro_1970_2024.csv" ), select = c( "cbsa", "g7000" ),
              colClasses = list( character = "cbsa" ) )
  p[ g, on = "cbsa", g7000 := i.g7000 ]
  d <- p[ year %in% YRS ]
  ## outcomes
  d[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
  sam <- function( y ) { y <- ifelse( y > 0, y, NA_real_ ); resid_logP( log( y ), d$logP, d$year ) }
  d[ , y_filer := sam( n_filer_c3 ) ]
  d[ , y_grass := sam( n_grass ) ]
  d[ , y_spend := sam( exp_c3_nohu ) ]
  ## covariates in model form
  for ( i in seq_len( nrow( VARS ) ) ) {
    v <- VARS$var[ i ]; s <- VARS$src[ i ]; f <- VARS$form[ i ]
    if ( v %in% c( "gr10", "g7000" ) ) next
    x <- d[[ s ]]
    x <- switch( v, race_frac = 1 - x, rel_frac = 1 - x, pol_div = 1 - abs( x ), x )
    xv <- switch( f,
      rate  = x,
      lrate = log( x ),
      sami  = { ok <- is.finite( x ) & d$population > 0; z <- rep( NA_real_, length( x ) )
                ## flog's floor is set on the whole 2010-2022 metro panel
                z[ ok ] <- flog( x[ ok ], d$population[ ok ] ); resid_logP( z, d$logP, d$year ) } )
    set( d, j = v, value = xv )
  }
  keep <- c( "cbsa", "cbsa_title", "year", "state_fips", "division", "population", "logP",
             "n_filer_c3", "n_grass", "exp_c3_nohu", names( OUTC ), VARS$var,
             "turnout_obs", "socap_sk2", "socap_assn_n", "socap_sk" )
  d <- d[ , intersect( keep, names( d ) ), with = FALSE ]
  ## the contaminated civic measures, for the note only (never a predictor)
  d[ , s_assn := resid_logP( flog( socap_assn_n, population ), logP, year ) ]
  arrow::write_parquet( d, file.path( VOUT, "panel_model_form.parquet" ) )
  log_msg( "  ", nrow( d ), " metro-years; grassroots <= 0: ", d[ n_grass <= 0, .N ] )
}

D <- as.data.table( arrow::read_parquet( file.path( VOUT, "panel_model_form.parquet" ) ) )
D22 <- D[ year == CS_YEAR ]

## ---- describe -------------------------------------------------------------------------
if ( "describe" %in% PARTS ) {
  log_msg( "describe" )
  fams <- c( "demand", "supply", "niche" )
  ## 1. correlation matrices (2022, model form, plus log P and the three outcomes)
  for ( f in fams ) {
    v <- VARS[ family == f, var ]
    M <- D22[ , c( "logP", names( OUTC ), v ), with = FALSE ]
    R <- cor( M, use = "pairwise.complete.obs" )
    fwrite( data.table( var = rownames( R ), R ), file.path( VOUT, sprintf( "cor_%s_2022.csv", f ) ) )
    Rp <- cor( D[ , c( "logP", names( OUTC ), v ), with = FALSE ], use = "pairwise.complete.obs" )
    fwrite( data.table( var = rownames( Rp ), Rp ), file.path( VOUT, sprintf( "cor_%s_pooled.csv", f ) ) )
  }
  ## cross-family correlations among the candidate sets (2022)
  cv <- unlist( CAND )
  Rx <- cor( D22[ , c( "logP", names( OUTC ), cv ), with = FALSE ], use = "pairwise.complete.obs" )
  fwrite( data.table( var = rownames( Rx ), Rx ), file.path( VOUT, "cor_candidates_2022.csv" ) )

  ## 2. redundant pairs |r| > .7 within and across families (2022)
  allv <- VARS$var
  Ra <- cor( D22[ , allv, with = FALSE ], use = "pairwise.complete.obs" )
  pr <- as.data.table( as.table( Ra ) )[ as.integer( factor( V1, allv ) ) < as.integer( factor( V2, allv ) ) ]
  setnames( pr, c( "v1", "v2", "r" ) )
  pr[ , `:=`( f1 = VARS$family[ match( v1, VARS$var ) ], f2 = VARS$family[ match( v2, VARS$var ) ] ) ]
  fwrite( pr[ abs( r ) > .5 ][ order( -abs( r ) ) ], file.path( VOUT, "pairs_over_05_2022.csv" ) )

  ## 3. VIFs within family: all items and the candidate set, on covariates residualized on log P
  ##    (that is what the coefficients use once log P is on the right)
  res22 <- copy( D22 )
  for ( v in allv ) set( res22, j = v, value = resid_logP( res22[[ v ]], res22$logP, res22$year ) )
  vf <- rbindlist( lapply( fams, function( f ) {
    a <- VARS[ family == f, var ]
    if ( f == "niche" ) a <- setdiff( a, "bls_pct_npo" )
    va <- vif_of( as.matrix( res22[ , a, with = FALSE ] ) )
    vc <- vif_of( as.matrix( res22[ , CAND[[ f ]], with = FALSE ] ) )
    merge( data.table( family = f, var = names( va ), vif_all = va ),
           data.table( var = names( vc ), vif_cand = vc ), by = "var", all = TRUE ) } ) )
  vcomb <- vif_of( as.matrix( res22[ , cv, with = FALSE ] ) )
  vf[ data.table( var = names( vcomb ), vif_comb = vcomb ), on = "var", vif_combined := i.vif_comb ]
  fwrite( vf, file.path( VOUT, "vif_2022.csv" ) )

  ## 4. constructs: PC1 share, loadings, alpha (signed z-scores), alpha if item dropped; 2022 and pooled
  cs_tab <- rbindlist( lapply( names( CONSTRUCTS ), function( k ) {
    it <- CONSTRUCTS[[ k ]]$items
    rbindlist( lapply( list( `2022` = D22, pooled = D ), function( dd ) {
      Z <- scale( as.matrix( dd[ , names( it ), with = FALSE ] ) ) %*% diag( it, length( it ) )
      colnames( Z ) <- names( it ); Z <- Z[ complete.cases( Z ), , drop = FALSE ]
      pc <- prcomp( Z ); ld <- pc$rotation[ , 1 ]; if ( sum( ld ) < 0 ) ld <- -ld
      ev <- pc$sdev^2
      data.table( construct = k, item = names( it ), sign = it, loading = ld,
                  pc1_share = ev[ 1 ] / sum( ev ), pc2_share = if ( length( ev ) > 1 ) ev[ 2 ] / sum( ev ) else NA_real_,
                  alpha = alpha_c( Z ),
                  alpha_if_dropped = vapply( seq_along( it ), function( j ) alpha_c( Z[ , -j, drop = FALSE ] ), 0 ),
                  r_item_rest = vapply( seq_along( it ), function( j ) cor( Z[ , j ], rowMeans( Z[ , -j, drop = FALSE ] ) ), 0 ) ) } ),
      idcol = "sample" ) } ) )
  fwrite( cs_tab, file.path( VOUT, "constructs_pc1_alpha.csv" ) )

  ## 5. within-metro share of variance, 2010-2022 (two-way demeaned / total)
  ws <- rbindlist( lapply( c( names( OUTC ), allv ), function( v ) {
    x <- D[ , .( cbsa, year, x = get( v ) ) ][ is.finite( x ) ]
    x[ , w := x - mean( x ), by = cbsa ][ , w := w - mean( w ), by = year ]
    data.table( var = v, within_share = var( x$w ) / var( x$x ), years = uniqueN( x$year ) ) } ) )
  fwrite( ws, file.path( VOUT, "within_share.csv" ) )

  ## 6. contaminated civic measures vs the outcomes (2022)
  cc <- D22[ , .( r_sk2_filer = cor( socap_sk2, y_filer, use = "p" ), r_sk2_grass = cor( socap_sk2, y_grass, use = "p" ),
                  r_assn_filer = cor( s_assn, y_filer, use = "p" ), r_assn_grass = cor( s_assn, y_grass, use = "p" ),
                  r_sk_filer = cor( socap_sk, y_filer, use = "p" ), r_respn_filer = cor( socap_respn, y_filer, use = "p" ),
                  r_turnout_filer = cor( turnout, y_filer, use = "p" ),
                  r_bls_filer = cor( bls_pct_npo, y_filer, use = "p" ), r_bls_spend = cor( bls_pct_npo, y_spend, use = "p" ),
                  r_hist_filer = cor( s_hist, y_filer, use = "p" ) ) ]
  fwrite( cc, file.path( VOUT, "contaminated_r_2022.csv" ) )
  print( cc )
  print( vf )
  print( pr[ abs( r ) > .7 ][ order( -abs( r ) ) ] )
  print( cs_tab[ sample == "2022", .( construct, item, loading = round( loading, 2 ), pc1 = round( pc1_share, 2 ),
                                     alpha = round( alpha, 2 ), a_drop = round( alpha_if_dropped, 2 ) ) ] )
  print( ws[ order( within_share ) ] )
}

## ---- model engine -----------------------------------------------------------------------
## xs: covariate names already in D (or extra columns added to dd). Returns one row per term.
fit_one <- function( dd, y, xs, ident ) {
  dd <- dd[ , c( "cbsa", "year", "state_fips", "logP", y, xs ), with = FALSE ]
  dd <- dd[ complete.cases( dd ) ]
  if ( ident == "within" ) {
    ## time-invariant items (endowment, 1970-2000 growth) leave the within model: their SAMI form
    ## moves within a metro only through log P, which is not information about the item
    xs <- setdiff( xs, VARS[ flag == "fixed", var ] )
    ## drop terms with no within-metro variation
    xs <- xs[ vapply( xs, function( v ) { w <- dd[ , get( v ) - mean( get( v ) ), by = cbsa ]$V1
                                          sd( w ) > 1e-8 * ( sd( dd[[ v ]] ) + 1e-12 ) }, TRUE ) ]
  }
  for ( v in xs ) set( dd, j = v, value = dd[[ v ]] / sd( dd[[ v ]] ) )
  setnames( dd, y, "yy" )
  rhs <- paste( xs, collapse = " + " )
  f <- switch( ident,
    cs     = as.formula( paste( "yy ~", rhs, "+ logP" ) ),
    pooled = as.formula( paste( "yy ~", rhs, "| year[logP]" ) ),
    within = as.formula( paste( "yy ~", rhs, "+ logP | cbsa + year" ) ) )
  vc <- switch( ident, cs = "hetero", pooled = ~cbsa, within = ~cbsa )
  r <- feols( f, data = dd, vcov = vc )
  ct <- coeftable( r ); ct <- ct[ rownames( ct ) %in% xs, , drop = FALSE ]
  G <- uniqueN( dd$cbsa ); K <- length( xs ) + 1
  df <- G - K - 1
  w2 <- if ( ident == "within" ) as.numeric( r2( r, "wr2" ) ) else NA_real_
  data.table( term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], t = ct[ , 3 ], p = ct[ , 4 ],
              pr = prt( ct[ , 3 ], df ), n = nobs( r ), metros = G, df = df,
              r2 = as.numeric( r2( r, "r2" ) ), wr2 = w2 )
}
run_set <- function( xs, label, idents = c( "cs", "pooled", "within" ), data = D ) {
  rbindlist( lapply( idents, function( id ) {
    dd <- if ( id == "cs" ) data[ year == CS_YEAR ] else data
    rbindlist( lapply( names( OUTC ), function( y )
      fit_one( dd, y, xs, id )[ , `:=`( outcome = OUTC[[ y ]], ident = id, model = label ) ] ) ) } ) )
}

## ---- models: blocks and combined ---------------------------------------------------------
if ( "models" %in% PARTS ) {
  log_msg( "models" )
  res <- rbind( run_set( CAND$demand, "demand" ), run_set( CAND$supply, "supply" ), run_set( CAND$niche, "niche" ),
                run_set( unlist( CAND, use.names = FALSE ), "combined" ) )
  res[ , family := VARS$family[ match( term, VARS$var ) ] ]
  fwrite( res, file.path( VOUT, "block_models.csv" ) )
  ## block vs combined: survive / flip / absorbed / emerges
  bw <- dcast( res, family + term + outcome + ident ~ ifelse( model == "combined", "comb", "block" ),
               value.var = c( "est", "t", "pr" ) )
  bw[ , status := fcase(
    abs( t_block ) >= 1.96 & abs( t_comb ) >= 1.96 & sign( t_block ) == sign( t_comb ), "survives",
    sign( t_block ) != sign( t_comb ) & ( abs( t_block ) >= 1.96 | abs( t_comb ) >= 1.96 ), "flips",
    abs( t_block ) >= 1.96 & abs( t_comb ) < 1.96, "absorbed",
    abs( t_block ) < 1.96 & abs( t_comb ) >= 1.96, "emerges",
    default = "null in both" ) ]
  fwrite( bw, file.path( VOUT, "block_vs_combined.csv" ) )
  ## what absorbs what: drop one covariate from the combined model at a time (cs and pooled) and
  ## record how every other term moves; the absorber of a term is the covariate whose removal
  ## moves it furthest back toward its own-family value
  allc <- unlist( CAND, use.names = FALSE )
  d1 <- rbindlist( lapply( allc, function( v ) run_set( setdiff( allc, v ), v, idents = c( "cs", "pooled" ) ) ) )
  setnames( d1, "model", "dropped" )
  cmb <- res[ model == "combined", .( term, outcome, ident, pr_comb = pr ) ]
  blk <- res[ model != "combined", .( term, outcome, ident, pr_block = pr ) ]
  d1 <- merge( merge( d1, cmb, by = c( "term", "outcome", "ident" ) ), blk, by = c( "term", "outcome", "ident" ) )
  d1[ , restore := ( pr - pr_comb ) * sign( pr_block - pr_comb ) ]
  d1[ , fam_dropped := VARS$family[ match( dropped, VARS$var ) ] ]
  ab <- d1[ order( -restore ), head( .SD, 3 ), by = .( term, outcome, ident ),
            .SDcols = c( "dropped", "fam_dropped", "pr", "pr_comb", "pr_block", "restore" ) ]
  fwrite( d1, file.path( VOUT, "drop_one.csv" ) )
  fwrite( ab, file.path( VOUT, "absorbers_top3.csv" ) )
  print( ab[ ident == "cs" & outcome == "filers" ][ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 2 ) else z ) ], nrows = 200 )
  fit <- unique( res[ , .( model, outcome, ident, n, metros, r2, wr2 ) ] )
  fwrite( fit, file.path( VOUT, "block_models_fit.csv" ) )
  options( width = 250 )
  print( dcast( bw[ ident != "within" ], family + term ~ ident + outcome, value.var = "status" ) )
  print( dcast( res[ ident != "within" ], term + model ~ ident + outcome, value.var = "pr", fun.aggregate = function( z ) if ( length( z ) ) round( z[ 1 ], 2 ) else NA_real_ ) )
  print( dcast( res[ ident == "within" ], term + model ~ outcome, value.var = "pr", fun.aggregate = function( z ) if ( length( z ) ) round( z[ 1 ], 2 ) else NA_real_ ) )
  print( dcast( fit, model ~ ident + outcome, value.var = "r2", fun.aggregate = function( z ) if ( length( z ) ) round( z[ 1 ], 3 ) else NA_real_ ) )
}

## ---- index: dominant item vs composites -------------------------------------------------
## base = combined model with every construct carried by its dominant item; one construct at a
## time is swapped for (a) PC1 score or (b) the standardized mean of its signed items, or (c) its
## best single alternative item. z constants and PC1 weights are fixed on the pooled 2010-2022 panel.
if ( "index" %in% PARTS ) {
  log_msg( "index" )
  doms <- vapply( CONSTRUCTS, `[[`, "", "dom" )
  ## extra terms that the base keeps beside the dominant items (separate constructs inside a family)
  base_extra <- c( "pct_65plus", "gini_m" )
  DI <- copy( D )
  for ( k in names( CONSTRUCTS ) ) {
    it <- CONSTRUCTS[[ k ]]$items
    Z <- as.matrix( DI[ , names( it ), with = FALSE ] )
    mu <- colMeans( Z, na.rm = TRUE ); sg <- apply( Z, 2, sd, na.rm = TRUE )
    Z <- sweep( sweep( Z, 2, mu ), 2, sg, "/" ) %*% diag( it, length( it ) )
    ok <- complete.cases( Z )
    pc <- prcomp( Z[ ok, ] ); ld <- pc$rotation[ , 1 ]; if ( sum( ld ) < 0 ) ld <- -ld
    s1 <- rep( NA_real_, nrow( Z ) ); s1[ ok ] <- Z[ ok, ] %*% ld
    set( DI, j = paste0( "pc_", k ), value = s1 )
    set( DI, j = paste0( "zm_", k ), value = rowMeans( Z ) )
  }
  out <- rbindlist( lapply( names( CONSTRUCTS ), function( k ) {
    ## other constructs keep their dominant items (poverty stays in the education comparison);
    ## base_extra items that belong to construct k are inside its index, so they leave the base
    others <- unname( c( doms[ names( doms ) != k ], setdiff( base_extra, names( CONSTRUCTS[[ k ]]$items ) ) ) )
    alts <- c( list( item = doms[[ k ]], pc1 = paste0( "pc_", k ), zmean = paste0( "zm_", k ) ),
               setNames( as.list( setdiff( names( CONSTRUCTS[[ k ]]$items ), doms[[ k ]] ) ),
                         paste0( "alt:", setdiff( names( CONSTRUCTS[[ k ]]$items ), doms[[ k ]] ) ) ) )
    rbindlist( lapply( names( alts ), function( a ) {
      xs <- unique( c( alts[[ a ]], others ) )
      run_set( xs, a, idents = c( "cs", "pooled" ), data = DI )[ term == alts[[ a ]] ][ , `:=`( construct = k, version = a ) ] } ) )
  } ) )
  fwrite( out, file.path( VOUT, "item_vs_index.csv" ) )
  sm <- out[ , .( pr_filer_cs = pr[ outcome == "filers" & ident == "cs" ], pr_grass_cs = pr[ outcome == "grassroots" & ident == "cs" ],
                  pr_spend_cs = pr[ outcome == "spending" & ident == "cs" ],
                  pr_filer_pool = pr[ outcome == "filers" & ident == "pooled" ], pr_grass_pool = pr[ outcome == "grassroots" & ident == "pooled" ],
                  pr_spend_pool = pr[ outcome == "spending" & ident == "pooled" ],
                  mean_abs_pr = mean( abs( pr ) ), same_sign = max( mean( pr > 0 ), mean( pr < 0 ) ),
                  n_sig = sum( p < .05 ), r2_cs_filer = r2[ outcome == "filers" & ident == "cs" ] ),
             by = .( construct, version ) ]
  fwrite( sm, file.path( VOUT, "item_vs_index_summary.csv" ) )
  options( width = 250 ); print( sm[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
}

## ---- lean set -----------------------------------------------------------------------------
LEAN <- c( "poverty_rate", "pct_65plus", "gini_m", "pct_nonwhite", "rel_frac", "age_sd",
           "s_pf_assets", "pct_ba", "turnout",
           "lag_g10", "s_gov_earn", "s_medicaid", "s_hist" )
if ( "lean" %in% PARTS ) {
  log_msg( "lean" )
  le <- rbind( run_set( LEAN, "lean" ), run_set( setdiff( LEAN, "s_hist" ), "lean, no endowment" ),
               run_set( setdiff( LEAN, "gini_m" ), "lean, no Gini" ),
               run_set( c( LEAN, "l_pcpi" ), "lean + per capita income" ),
               run_set( c( LEAN, "unemp_rate" ), "lean + unemployment" ),
               run_set( c( setdiff( LEAN, "s_pf_assets" ), "pc_wealth" ), "lean, wealth index",
                        data = { DW <- copy( D ); it <- CONSTRUCTS$wealth$items
                                 Z <- scale( as.matrix( DW[ , names( it ), with = FALSE ] ) ); ok <- complete.cases( Z )
                                 pc <- prcomp( Z[ ok, ] ); ld <- pc$rotation[ , 1 ]; if ( sum( ld ) < 0 ) ld <- -ld
                                 s1 <- rep( NA_real_, nrow( Z ) ); s1[ ok ] <- Z[ ok, ] %*% ld; DW[ , pc_wealth := s1 ]; DW } ) )
  fwrite( le, file.path( VOUT, "lean_models.csv" ) )
  vl <- vif_of( as.matrix( D22[ , lapply( .SD, function( z ) resid_logP( z, D22$logP, D22$year ) ), .SDcols = LEAN ] ) )
  fwrite( data.table( var = names( vl ), vif = vl ), file.path( VOUT, "lean_vif_2022.csv" ) )
  options( width = 250 )
  print( dcast( le, term + model ~ ident + outcome, value.var = "pr", fun.aggregate = function( z ) if ( length( z ) ) round( z[ 1 ], 2 ) else NA_real_ ) )
  print( unique( le[ , .( model, ident, outcome, n, metros, r2 = round( r2, 3 ), wr2 = round( wr2, 3 ) ) ] ) )
  print( round( vl, 2 ) )
}

## ---- figures --------------------------------------------------------------------------------
if ( "figs" %in% PARTS ) {
  log_msg( "figs" )
  ## diverging blue (positive) / red (negative), neutral gray midpoint (dataviz reference palette)
  DIV <- c( "#e34948", "#f0efec", "#2a78d6" )
  heat <- function( f ) {
    R <- as.matrix( fread( file.path( VOUT, sprintf( "cor_%s_2022.csv", f ) ) )[ , -1 ] )
    v <- c( "logP", "y_filer", VARS[ family == f, var ] )
    R <- R[ match( v, colnames( R ) ), match( v, colnames( R ) ) ]
    rownames( R ) <- colnames( R ) <- v
    dt <- as.data.table( as.table( R ) ); setnames( dt, c( "a", "b", "r" ) )
    dt[ , `:=`( ia = match( a, v ), ib = match( b, v ) ) ]
    dt <- dt[ ia > ib ]
    dt[ , `:=`( a = factor( LAB[ as.character( a ) ], LAB[ v ] ), b = factor( LAB[ as.character( b ) ], LAB[ v ] ) ) ]
    dt[ , lab := fmt( r ) ]
    dt[ , ink := ifelse( abs( r ) > .55, "#ffffff", "#0b0b0b" ) ]
    nfam <- c( demand = "Demand side: need and heterogeneity", supply = "Supply side: wealth, education, social capital",
               niche = "Market niche: growth, government, history" )[[ f ]]
    ggplot( dt, aes( b, a, fill = r ) ) +
      geom_tile( color = "#fcfcfb", linewidth = 0.6 ) +
      geom_text( aes( label = lab, color = ink ), size = 2.7 ) +
      scale_color_identity() +
      scale_fill_gradient2( low = DIV[ 1 ], mid = DIV[ 2 ], high = DIV[ 3 ], midpoint = 0, limits = c( -1, 1 ),
                            name = "r", breaks = c( -1, -.5, 0, .5, 1 ), labels = c( "-1", "-.5", "0", ".5", "1" ) ) +
      scale_y_discrete( limits = rev ) +
      labs( x = NULL, y = NULL, title = nfam,
            subtitle = "Pearson r, 2022, 387 metros. Totals in SAMI form, rates as is. First two rows: log population and filer SAMI." ) +
      theme_minimal( base_size = 10 ) +
      theme( axis.text.x = element_text( angle = 40, hjust = 1, color = "#52514e" ), axis.text.y = element_text( color = "#52514e" ),
             panel.grid = element_blank(), plot.title = element_text( face = "bold", color = "#0b0b0b" ),
             plot.subtitle = element_text( color = "#52514e", size = 8.5 ), legend.position = "right",
             plot.background = element_rect( fill = "#fcfcfb", color = NA ) )
  }
  for ( f in c( "demand", "supply", "niche" ) ) {
    nv <- VARS[ family == f, .N ] + 2
    ggsave( file.path( FIG, sprintf( "var_cor_%s.png", f ) ), heat( f ), width = 2.2 + 0.52 * nv, height = 1.6 + 0.42 * nv,
            dpi = 150, bg = "#fcfcfb" )
  }

  ## block vs combined, partial r, cs 2022 and pooled, three outcomes
  res <- fread( file.path( VOUT, "block_models.csv" ) )[ ident != "within" ]
  res[ , mod := ifelse( model == "combined", "Combined", "Own family only" ) ]
  res[ , lab := factor( LAB[ term ], rev( LAB[ unlist( CAND ) ] ) ) ]
  res[ , fam := factor( c( demand = "Demand", supply = "Supply", niche = "Niche" )[ family ], c( "Demand", "Supply", "Niche" ) ) ]
  res[ , col := factor( paste( ident, outcome ) ) ]
  res[ , panel := factor( paste0( ifelse( ident == "cs", "2022", "Pooled 2010-22" ), ": ", outcome ),
                          paste0( rep( c( "2022", "Pooled 2010-22" ), each = 3 ), ": ", c( "filers", "grassroots", "spending" ) ) ) ]
  g <- ggplot( res, aes( pr, lab ) ) +
    geom_vline( xintercept = 0, color = "#c3c2b7" ) +
    geom_line( aes( group = term ), color = "#c3c2b7", linewidth = 0.5 ) +
    geom_point( aes( shape = mod, color = mod ), size = 2.2, stroke = 0.9 ) +
    scale_color_manual( values = c( Combined = "#2a78d6", `Own family only` = "#eb6834" ), name = NULL ) +
    scale_shape_manual( values = c( Combined = 16, `Own family only` = 1 ), name = NULL ) +
    facet_grid( fam ~ panel, scales = "free_y", space = "free_y" ) +
    labs( x = "Partial r (df in metros)", y = NULL,
          title = "Partial r by family model and in the combined model" ) +
    theme_minimal( base_size = 9.5 ) +
    theme( legend.position = "top", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
           strip.text = element_text( face = "bold" ), plot.title = element_text( face = "bold" ),
           plot.background = element_rect( fill = "#fcfcfb", color = NA ) )
  ggsave( file.path( FIG, "var_block_vs_combined.png" ), g, width = 11, height = 7.5, dpi = 150, bg = "#fcfcfb" )

  ## item vs index
  iv <- fread( file.path( VOUT, "item_vs_index.csv" ) )[ version %in% c( "item", "pc1", "zmean" ) ]
  iv[ , version := factor( c( item = "Dominant item", pc1 = "PC1 index", zmean = "Standardized mean" )[ version ],
                           c( "Dominant item", "PC1 index", "Standardized mean" ) ) ]
  doms <- vapply( CONSTRUCTS, `[[`, "", "dom" )
  iv[ , cons := factor( paste0( construct, " (", LAB[ doms[ construct ] ], ")" ),
                        rev( paste0( names( doms ), " (", LAB[ doms ], ")" ) ) ) ]
  iv[ , panel := factor( paste0( ifelse( ident == "cs", "2022", "Pooled 2010-22" ), ": ", outcome ),
                         paste0( rep( c( "2022", "Pooled 2010-22" ), each = 3 ), ": ", c( "filers", "grassroots", "spending" ) ) ) ]
  g2 <- ggplot( iv, aes( pr, cons, color = version, shape = version ) ) +
    geom_vline( xintercept = 0, color = "#c3c2b7" ) +
    geom_point( size = 2.2, position = position_dodge( width = 0.55 ) ) +
    scale_color_manual( values = c( "#2a78d6", "#eb6834", "#1baf7a" ), name = NULL ) +
    scale_shape_manual( values = c( 16, 17, 15 ), name = NULL ) +
    facet_wrap( ~panel, nrow = 2 ) +
    labs( x = "Partial r in the combined model (df in metros)", y = NULL,
          title = "Dominant item vs composite index, one construct swapped at a time" ) +
    theme_minimal( base_size = 9.5 ) +
    theme( legend.position = "top", panel.grid.minor = element_blank(), panel.grid.major.y = element_line( color = "#ebeae6" ),
           strip.text = element_text( face = "bold" ), plot.title = element_text( face = "bold" ),
           plot.background = element_rect( fill = "#fcfcfb", color = NA ) )
  ggsave( file.path( FIG, "var_item_vs_index.png" ), g2, width = 10.5, height = 6.5, dpi = 150, bg = "#fcfcfb" )
  for ( f in list.files( FIG, pattern = "^var_.*png$", full.names = TRUE ) ) file.copy( f, VOUT, overwrite = TRUE )
}
log_msg( "done" )
