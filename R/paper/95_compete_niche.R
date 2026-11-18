## 95_compete_niche.R
## Do market-competition (concentration) measures belong in the unified model as market-niche
## terms? Adds the measures built by 23_cov_compete.R (notes/market-competition-measures.md) to the
## 13-covariate lean model of 94_variables_panel_model.R, one at a time.
##
## Parts:
##   data      lean covariates from 94 (panel_model_form.parquet) + outcomes rebuilt from the current
##             panel (after the 2026-11-17 NTEE backfill, which moved spending) + mkt_* measures
##   describe  correlations with log P, log n, outcomes and the lean covariates; within shares
##   addone    lean + one measure: cs 2022, pooled 2010-2022, within; three outcomes
##   pairs     HHI and CR4 together; nHHI + Gini (dominance vs. inequality)
##   change    prospective test: 2012-2022 change in SAMI on 2012 concentration, 2012 SAMI and
##             the 2012 lean covariates (concentration measured before the change)
##   figs      paper/figures/var_compete_addone.png
##
## Mechanical links (why grassroots is the clean outcome): HHI >= 1/n and log HHI = log(n*HHI) - log n,
## with n the number of filers in the market. The filer SAMI is log n_filer net of log P, so HHI and
## CR4 carry the filer count by construction. Spending is the revenue being divided. Grassroots
## (registered non-filers) are not in any concentration measure.
##
## Run from multiverse/:
##   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" R/paper/95_compete_niche.R > D:/MULTIVERSE/logs/95.log 2>&1

suppressWarnings( suppressPackageStartupMessages( {
  source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" )
  library( ggplot2 ) } ) )

VIN  <- file.path( BUILD, "data", "variables" )
VOUT <- file.path( VIN, "compete" )
dir.create( VOUT, recursive = TRUE, showWarnings = FALSE )
FIG <- "paper/figures"
args <- commandArgs( trailingOnly = TRUE )
PARTS <- if ( length( args ) ) args else c( "data", "describe", "addone", "pairs", "change", "figs" )
CS_YEAR <- 2022L

LEAN <- c( "poverty_rate", "pct_65plus", "gini_m", "pct_nonwhite", "rel_frac", "age_sd",
           "s_pf_assets", "pct_ba", "turnout",
           "lag_g10", "s_gov_earn", "s_medicaid", "s_hist" )
FIXED <- "s_hist"
OUTC <- c( y_filer = "filers", y_grass = "grassroots", y_spend = "spending" )

## candidate measures: var | source column | transform | label
MK <- fread( text = "
var|src|tr|label|group
c_nhhi|mkt_nhhi_rev_local|level|Normalized HHI (revenue, local)|dominance
c_lhhi|mkt_hhi_rev_local|log|HHI, log (revenue, local)|dominance
c_cr1|mkt_cr1_rev_local|level|CR1: largest org's share (local)|dominance
c_cr4|mkt_cr4_rev_local|level|CR4: four largest orgs' share (local)|dominance
c_lshhio|mkt_shhio_rev_local|log|Subsector HHI, org-weighted, log (local)|dominance
c_nhhi_con|mkt_nhhi_con_local|level|Normalized HHI (contributions, local)|dominance
c_nhhi_nohu|mkt_nhhi_rev_nohu|level|Normalized HHI (revenue, excl. H/U)|dominance
c_lxs|mkt_xs_rev_local|log|n x HHI, log (local)|inequality
c_gini|mkt_gini_rev_local|level|Gini of org revenue (local)|inequality
c_small|mkt_small_rev_local|level|Revenue share of orgs under $1M (local)|inequality
c_small_n|mkt_small_n_local|level|Share of filers under $1M (local)|inequality
c_mix|mkt_mix_rev_local|level|Subsector mix HHI (local)|mix
", sep = "|" )
LAB <- setNames( MK$label, MK$var )

prt <- function( t, df ) t / sqrt( t^2 + df )
fmt <- function( x, d = 2 ) sub( "^(-?)0\\.", "\\1.", formatC( x, format = "f", digits = d ) )
vif_of <- function( X ) { R <- cor( X, use = "pairwise.complete.obs" ); setNames( diag( solve( R ) ), colnames( X ) ) }

## ---- data -----------------------------------------------------------------------------------
if ( "data" %in% PARTS ) {
  log_msg( "data" )
  D94 <- as.data.table( arrow::read_parquet( file.path( VIN, "panel_model_form.parquet" ) ) )
  p <- load_panel()[ cbsa_type == "metro" & year %in% 2010:2022 ]
  miss <- setdiff( MK$src, names( p ) )
  if ( length( miss ) ) stop( "missing in panel: ", paste( miss, collapse = ", " ) )
  p[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
  sam <- function( y, d ) { y <- ifelse( y > 0, y, NA_real_ ); resid_logP( log( y ), d$logP, d$year ) }
  p[ , y_filer := sam( n_filer_c3, p ) ]
  p[ , y_grass := sam( n_grass, p ) ]
  p[ , y_spend := sam( exp_c3_nohu, p ) ]
  p[ , logN := log( n_filer_c3 ) ]
  for ( i in seq_len( nrow( MK ) ) ) {
    x <- p[[ MK$src[ i ] ]]
    set( p, j = MK$var[ i ], value = if ( MK$tr[ i ] == "log" ) log( ifelse( x > 0, x, NA_real_ ) ) else x )
  }
  ## the old (pre-NTEE-fix) spending SAMI from 94, to show what the fix changed
  old <- D94[ , c( "cbsa", "year", "state_fips", "logP", LEAN, "y_spend" ), with = FALSE ]
  setnames( old, "y_spend", "y_spend_old" )
  D <- merge( old, p[ , c( "cbsa", "year", names( OUTC ), "logN", MK$var ), with = FALSE ], by = c( "cbsa", "year" ) )
  arrow::write_parquet( D, file.path( VOUT, "panel_compete.parquet" ) )
  log_msg( "  ", nrow( D ), " metro-years, ", uniqueN( D$cbsa ), " metros; r(old, new spending SAMI, 2022) = ",
           round( D[ year == 2022, cor( y_spend, y_spend_old, use = "complete.obs" ) ], 3 ) )
}

D <- as.data.table( arrow::read_parquet( file.path( VOUT, "panel_compete.parquet" ) ) )
D22 <- D[ year == CS_YEAR ]

## ---- describe ---------------------------------------------------------------------------------
if ( "describe" %in% PARTS ) {
  log_msg( "describe" )
  ## distribution and missingness
  ds <- rbindlist( lapply( MK$var, function( v ) { x <- D22[[ v ]]
    data.table( var = v, n_obs = sum( is.finite( x ) ), mean = mean( x, na.rm = TRUE ), sd = sd( x, na.rm = TRUE ),
                p05 = quantile( x, .05, na.rm = TRUE ), p50 = median( x, na.rm = TRUE ), p95 = quantile( x, .95, na.rm = TRUE ) ) } ) )
  fwrite( ds, file.path( VOUT, "describe_2022.csv" ) )
  ## correlations with size, filer count, outcomes, and the lean covariates (residualized on log P,
  ## as the model sees them)
  rlp <- function( z ) resid( lm( z ~ D22$logP, na.action = na.exclude ) )
  cr <- rbindlist( lapply( MK$var, function( v ) { x <- D22[[ v ]]
    out <- data.table( var = v, r_logP = cor( x, D22$logP, use = "complete.obs" ),
                       r_logN = cor( x, D22$logN, use = "complete.obs" ) )
    for ( y in names( OUTC ) ) set( out, j = paste0( "r_", OUTC[[ y ]] ), value = cor( x, D22[[ y ]], use = "complete.obs" ) )
    xr <- rlp( x )
    lc <- sapply( LEAN, function( l ) cor( xr, rlp( D22[[ l ]] ), use = "complete.obs" ) )
    out[ , `:=`( max_abs_r_lean = max( abs( lc ) ), lean_partner = names( lc )[ which.max( abs( lc ) ) ],
                 r_with_partner = lc[ which.max( abs( lc ) ) ] ) ]
    ## R2 of the measure on log P + the 13 lean covariates
    X <- D22[ , c( "logP", LEAN ), with = FALSE ]
    out[ , r2_on_lean := summary( lm( x ~ ., data = cbind( x = x, X ) ) )$r.squared ]
    out } ) )
  ## within-metro share of variance, 2010-2022
  ws <- rbindlist( lapply( MK$var, function( v ) { d <- D[ is.finite( get( v ) ), .( cbsa, x = get( v ) ) ]
    d[ , xm := mean( x ), by = cbsa ]; data.table( var = v, within_share = var( d$x - d$xm ) / var( d$x ) ) } ) )
  cr <- merge( cr, ws, by = "var" )
  fwrite( cr, file.path( VOUT, "correlations_2022.csv" ) )
  cm <- cor( D22[ , MK$var, with = FALSE ], use = "pairwise.complete.obs" )
  fwrite( data.table( var = rownames( cm ), round( cm, 3 ) ), file.path( VOUT, "matrix_2022.csv" ) )
  options( width = 220 )
  print( cr[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 2 ) else z ) ] )
  print( round( cm, 2 ) )
}

## ---- model engine -------------------------------------------------------------------------------
fit_one <- function( dd, y, xs, ident ) {
  dd <- dd[ , c( "cbsa", "year", "logP", y, xs ), with = FALSE ]
  dd <- dd[ complete.cases( dd ) ]
  if ( ident == "within" ) {
    xs <- setdiff( xs, FIXED )
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
  G <- uniqueN( dd$cbsa ); K <- length( xs ) + 1; df <- G - K - 1
  data.table( term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], t = ct[ , 3 ], p = ct[ , 4 ],
              pr = prt( ct[ , 3 ], df ), n = nobs( r ), metros = G,
              r2 = as.numeric( r2( r, if ( ident == "within" ) "wr2" else "r2" ) ) )
}
run_set <- function( xs, label, data = D, idents = c( "cs", "pooled", "within" ) ) {
  rbindlist( lapply( idents, function( id ) {
    dd <- if ( id == "cs" ) data[ year == CS_YEAR ] else data
    rbindlist( lapply( names( OUTC ), function( y )
      fit_one( dd, y, xs, id )[ , `:=`( outcome = OUTC[[ y ]], ident = id, model = label ) ] ) ) } ) )
}

## ---- add one ----------------------------------------------------------------------------------
if ( "addone" %in% PARTS ) {
  log_msg( "addone" )
  ## same sample for base and additions: metros with every candidate measure
  Ds <- D[ complete.cases( D[ , c( LEAN, MK$var ), with = FALSE ] ) ]
  log_msg( "  common sample: ", nrow( Ds ), " metro-years, ", uniqueN( Ds$cbsa ), " metros" )
  base <- run_set( LEAN, "lean", data = Ds )
  add  <- rbindlist( lapply( MK$var, function( v ) run_set( c( LEAN, v ), v, data = Ds ) ) )
  fwrite( rbind( base, add ), file.path( VOUT, "addone_models.csv" ) )
  ## summary per measure x outcome x ident
  b <- base[ , .( term, outcome, ident, pr_base = pr, r2_base = r2 ) ]
  sm <- add[ term == model, .( var = model, outcome, ident, pr, t, p ) ]
  r2s <- unique( add[ , .( var = model, outcome, ident, r2 ) ] )
  sm <- merge( sm, r2s, by = c( "var", "outcome", "ident" ) )
  sm <- merge( sm, unique( b[ , .( outcome, ident, r2_base ) ] ), by = c( "outcome", "ident" ) )
  sm[ , d_r2 := r2 - r2_base ]
  ## how much the lean terms move
  mv <- merge( add[ term != model, .( var = model, term, outcome, ident, pr ) ],
               b[ , .( term, outcome, ident, pr_base ) ], by = c( "term", "outcome", "ident" ) )
  mv <- mv[ , .( max_move = max( abs( pr - pr_base ) ), most_moved = term[ which.max( abs( pr - pr_base ) ) ],
                 move = ( pr - pr_base )[ which.max( abs( pr - pr_base ) ) ] ), by = .( var, outcome, ident ) ]
  sm <- merge( sm, mv, by = c( "var", "outcome", "ident" ) )
  ## VIF of the measure in lean + measure, 2022 (residualized on log P)
  D22s <- Ds[ year == CS_YEAR ]
  vf <- rbindlist( lapply( MK$var, function( v ) {
    X <- as.matrix( D22s[ , lapply( .SD, function( z ) resid( lm( z ~ D22s$logP ) ) ), .SDcols = c( LEAN, v ) ] )
    data.table( var = v, vif = vif_of( X )[[ v ]], max_vif = max( vif_of( X ) ) ) } ) )
  sm <- merge( sm, vf, by = "var" )
  fwrite( sm, file.path( VOUT, "addone_summary.csv" ) )
  options( width = 220 )
  w <- dcast( sm, var ~ ident + outcome, value.var = "pr" )
  print( w[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 2 ) else z ) ] )
  print( dcast( sm, var ~ ident + outcome, value.var = "d_r2" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  print( sm[ ident == "cs", .( var, outcome, max_move = round( max_move, 2 ), most_moved, move = round( move, 2 ), vif = round( vif, 2 ) ) ] )
}

## ---- pairs ---------------------------------------------------------------------------------------
if ( "pairs" %in% PARTS ) {
  log_msg( "pairs" )
  Ds <- D[ complete.cases( D[ , c( LEAN, MK$var ), with = FALSE ] ) ]
  pr <- rbind( run_set( c( LEAN, "c_lhhi", "c_cr4" ), "HHI + CR4", data = Ds ),
               run_set( c( LEAN, "c_nhhi", "c_cr4" ), "nHHI + CR4", data = Ds ),
               run_set( c( LEAN, "c_nhhi", "c_gini" ), "nHHI + Gini", data = Ds ),
               run_set( c( LEAN, "c_nhhi", "c_small" ), "nHHI + small share", data = Ds ) )
  fwrite( pr, file.path( VOUT, "pairs_models.csv" ) )
  print( dcast( pr[ term %in% MK$var ], model + term ~ ident + outcome, value.var = "pr" )[
    , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 2 ) else z ) ] )
}

## ---- change ------------------------------------------------------------------------------------
## Prospective test: does concentration in 2012 predict the 2012-2022 change in density, given the
## 2012 level and the 2012 lean covariates? (2012: after the 2011 CORE->SOI splice, inside one source
## until 2019; the end year 2022 is e-file, so the change crosses the 2020 splice for every metro alike.)
if ( "change" %in% PARTS ) {
  log_msg( "change" )
  a <- D[ year == 2012 ]; z <- D[ year == 2022, c( "cbsa", names( OUTC ) ), with = FALSE ]
  setnames( z, names( OUTC ), paste0( names( OUTC ), "_22" ) )
  a <- merge( a, z, by = "cbsa" )
  res <- list()
  for ( y in names( OUTC ) ) for ( v in c( "none", MK$var ) ) {
    xs <- c( LEAN, y, if ( v != "none" ) v )
    dd <- a[ , c( "cbsa", "logP", paste0( y, "_22" ), xs ), with = FALSE ]
    dd <- dd[ complete.cases( dd ) ]
    dd[ , dy := get( paste0( y, "_22" ) ) - get( y ) ]
    for ( x in xs ) set( dd, j = x, value = dd[[ x ]] / sd( dd[[ x ]] ) )
    r <- feols( as.formula( paste( "dy ~", paste( xs, collapse = " + " ), "+ logP" ) ), dd, vcov = "hetero" )
    ct <- coeftable( r ); df <- nobs( r ) - length( xs ) - 2
    tm <- if ( v == "none" ) y else v
    res[[ length( res ) + 1 ]] <- data.table( outcome = OUTC[[ y ]], var = v, term = tm, est = ct[ tm, 1 ], t = ct[ tm, 3 ],
                                              p = ct[ tm, 4 ], pr = prt( ct[ tm, 3 ], df ), n = nobs( r ),
                                              r2 = as.numeric( r2( r, "r2" ) ) )
  }
  ch <- rbindlist( res )
  fwrite( ch, file.path( VOUT, "change_2012_2022.csv" ) )
  print( dcast( ch[ var != "none" ], var ~ outcome, value.var = "pr" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 2 ) else z ) ] )
  print( ch[ var == "none", .( outcome, lagged_level_pr = round( pr, 2 ), n, r2 = round( r2, 2 ) ) ] )
}

## ---- figure ----------------------------------------------------------------------------------------
if ( "figs" %in% PARTS ) {
  log_msg( "figs" )
  sm <- fread( file.path( VOUT, "addone_summary.csv" ) )
  ch <- fread( file.path( VOUT, "change_2012_2022.csv" ) )[ var != "none" ]
  ch[ , ident := "change" ]
  f <- rbind( sm[ , .( var, outcome, ident, pr, p ) ], ch[ , .( var, outcome, ident, pr, p ) ] )
  f[ , ident := factor( ident, c( "cs", "pooled", "within", "change" ),
                        c( "Cross-section 2022", "Pooled 2010-22", "Within metro", "Change 2012-22\n(2012 measure)" ) ) ]
  f[ , outcome := factor( outcome, c( "filers", "grassroots", "spending" ),
                          c( "Filers (mechanical)", "Grassroots (clean)", "Spending (mechanical)" ) ) ]
  f[ , lab := factor( LAB[ var ], rev( LAB[ MK$var ] ) ) ]
  f[ , sig := ifelse( p < .05, "p < .05", "n.s." ) ]
  g <- ggplot( f, aes( pr, lab ) ) +
    geom_vline( xintercept = 0, colour = "grey55" ) +
    geom_vline( xintercept = c( -.1, .1 ), colour = "grey85", linetype = 2 ) +
    geom_point( aes( shape = sig ), colour = "#2a78d6", size = 2.2 ) +
    scale_shape_manual( values = c( "p < .05" = 16, "n.s." = 1 ), name = NULL ) +
    facet_grid( outcome ~ ident ) +
    labs( x = "Partial r of the added measure, given the 13 lean covariates (df in metros)", y = NULL,
          title = "Market concentration added to the lean model, one measure at a time" ) +
    theme_minimal( base_size = 10 ) +
    theme( legend.position = "top", panel.grid.minor = element_blank(), strip.text = element_text( face = "bold" ),
           plot.title.position = "plot" )
  ggsave( file.path( FIG, "var_compete_addone.png" ), g, width = 11, height = 8.5, dpi = 150, bg = "white" )
}
log_msg( "done" )
