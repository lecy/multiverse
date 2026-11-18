## 70_collinearity_composites.R
## Collinearity diagnostics for the covariate set, latent-factor composites, and
## a comparison of panel models fit with items vs composites.
##   Part 1  correlation structure: log/level forms (before SAMI), SAMI forms,
##           within-metro forms; VIFs and condition numbers for the default
##           (principled "full") model and an all-items model
##   Part 2  exploratory factor analysis (psych): joint EFA of every item and
##           block EFAs (census/demographic, philanthropy, civic/political);
##           KMO, Bartlett, parallel analysis, oblimin loadings, alpha
##   Part 3  composites (factor scores with fixed weights + additive scales)
##           and panel models: default items vs all items vs composites,
##           pooled (year FE) and within (metro + year FE); block Wald tests
## Metros only. Writes to D:/MULTIVERSE/data/collinearity/.
## Usage: Rscript R/70_collinearity_composites.R [part numbers, default all]

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( psych )
                                  library( ggplot2 ); library( corrplot ) } )
setFixest_notes( FALSE )

CO <- file.path( BUILD, "data", "collinearity" ); dir.create( CO, showWarnings = FALSE )
PARTS <- if ( length( commandArgs( TRUE ) ) ) as.integer( commandArgs( TRUE ) ) else 1:3
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
## log of an extensive covariate with a per-capita floor (as flog() in 40_multiverse.R, run v6 on;
## switched here 2026-11-16). log(x + 1) put zero-asset metros (Eagle Pass, TX) ~11 SD below the mean.
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}

d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
d <- d[ cbsa_type == "metro" ]
setorder( d, cbsa, year )
d[ , logP := log( population ) ]
d[ , pop_growth5 := logP - shift( logP, 5 ), by = cbsa ]
## extensive (type B) variables in log form; skewed dollar intensives logged
for ( v in c( "pf_assets", "n_pf", "pf_grants", "contrib_total", "gov_earn_sl", "gov_emp_sl",
              "rel_cng", "socap_assn_n", "qcew_priv_wages" ) )
  set( d, j = paste0( "l_", v ), value = flog( d[[ v ]], d$population ) )
d[ , `:=`( l_pcpi = log( pcpi ), l_mhi = log( med_hh_income ),
           l_contrib_per_return = log( contrib_per_return ), l_gdp_per_worker = log( gdp_per_worker ) ) ]

## ---- item catalog ---------------------------------------------------------------
ITEMS <- data.table( var = c(
  "race_hhi", "pct_nonwhite", "pct_foreign_born", "gini", "age_sd",
  "poverty_rate", "unemp_rate", "pct_ba", "l_pcpi", "l_mhi", "pct_65plus", "pop_growth5",
  "l_pf_assets", "l_n_pf", "l_pf_grants", "l_contrib_total", "l_contrib_per_return", "contrib_share_agi",
  "turnout", "dem_margin",
  "rel_hhi", "pct_adherent", "l_rel_cng", "l_socap_assn_n", "socap_sk2", "socap_respn",
  "l_gov_earn_sl", "l_gov_emp_sl", "gov_emp_share",
  "l_gdp_per_worker", "l_qcew_priv_wages" ),
  block = c( rep( "diversity", 5 ), rep( "need", 7 ), rep( "philanthropy", 6 ), rep( "political", 2 ),
             rep( "civic", 6 ), rep( "government", 3 ), rep( "economy", 2 ) ),
  extensive = c( rep( FALSE, 12 ), TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE ) )
IV <- ITEMS$var
DEFAULT <- c( "race_hhi", "l_pf_assets", "turnout", "l_gov_earn_sl", "poverty_rate", "pct_ba" )

## residual on log P within each year (SAMI first stage)
resid_logP <- function( y, logP, year ) {
  out <- rep( NA_real_, length( y ) )
  for ( t in unique( year ) ) {
    i <- which( year == t & is.finite( y ) & is.finite( logP ) )
    if ( length( i ) > 3 ) out[ i ] <- resid( lm( y[ i ] ~ logP[ i ] ) )
  }
  out
}

## analysis frame: metros 2010-2022, complete on every item
P <- d[ year %in% 2010:2022, c( "cbsa", "year", "state_fips", "population", "logP", IV,
                                "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu", "exp_c3" ), with = FALSE ]
P <- P[ complete.cases( P[ , c( "logP", IV ), with = FALSE ] ) ]
log_msg( "analysis frame: ", nrow( P ), " metro-years, ", uniqueN( P$cbsa ), " metros" )

## SAMI forms (every item residualized on log P by year: the all-SAMI arm)
S <- copy( P )
for ( v in IV ) set( S, j = v, value = resid_logP( P[[ v ]], P$logP, P$year ) )
## within forms: SAMI forms two-way demeaned (metro + year), 2010-2022
W <- copy( S )
dm <- demean( as.matrix( S[ , IV, with = FALSE ] ), f = S[ , .( cbsa, year ) ] )
for ( j in seq_along( IV ) ) set( W, j = IV[ j ], value = dm[ , j ] )
POOL <- 2015:2022

vif_tab <- function( X ) {
  R <- cor( X ); ev <- eigen( R, only.values = TRUE )$values
  list( vif = diag( solve( R ) ), cond = sqrt( max( ev ) / min( ev ) ) )
}

## ================================================================================
## Part 1: correlation structure
## ================================================================================
if ( 1 %in% PARTS ) {
  log_msg( "Part 1: correlations" )
  forms <- list(
    log_pooled  = P[ year %in% POOL, c( "logP", IV ), with = FALSE ],
    log_cs2019  = P[ year == 2019, c( "logP", IV ), with = FALSE ],
    sami_pooled = S[ year %in% POOL, IV, with = FALSE ],
    within      = W[ , IV, with = FALSE ] )
  pairs <- list()
  for ( f in names( forms ) ) {
    R <- cor( forms[[ f ]] )
    fwrite( data.table( var = rownames( R ), round( R, 3 ) ), file.path( CO, paste0( "p1_cor_", f, ".csv" ) ) )
    png( file.path( CO, paste0( "fig_p1_cor_", f, ".png" ) ), width = 2400, height = 2400, res = 220 )
    corrplot( R, order = "hclust", tl.col = "black", tl.cex = 0.7, type = "full", method = "color",
              addCoef.col = "grey20", number.cex = 0.45, title = f, mar = c( 0, 0, 1.5, 0 ) )
    dev.off()
    ut <- which( upper.tri( R ), arr.ind = TRUE )
    pairs[[ f ]] <- data.table( form = f, a = rownames( R )[ ut[ , 1 ] ], b = colnames( R )[ ut[ , 2 ] ],
                                r = R[ ut ] )
  }
  pr <- rbindlist( pairs )
  pr <- dcast( pr, a + b ~ form, value.var = "r" )
  pr[ , max_abs := pmax( abs( log_pooled ), abs( sami_pooled ), abs( within ), na.rm = TRUE ) ]
  fwrite( r3( pr[ order( -abs( log_pooled ) ) ] ), file.path( CO, "p1_pairs.csv" ) )

  ## correlation with log P (metro scaling), log forms
  cl <- rbindlist( lapply( IV, function( v ) data.table(
    var = v, block = ITEMS[ var == v, block ], extensive = ITEMS[ var == v, extensive ],
    r_logP_pooled = cor( P[ year %in% POOL, get( v ) ], P[ year %in% POOL, logP ] ),
    r_logP_2019 = cor( P[ year == 2019, get( v ) ], P[ year == 2019, logP ] ),
    ## share of (SAMI-form) variance that is within-metro, 2010-2022
    within_share = var( W[[ v ]] ) / var( S[[ v ]] ) ) ) )
  fwrite( r3( cl ), file.path( CO, "p1_cor_logP.csv" ) )

  ## VIFs: default ("full") principled model and every item, by form
  vsets <- list(
    default_log_plus_logP = list( X = P[ year %in% POOL, c( DEFAULT, "logP" ), with = FALSE ] ),
    default_sami          = list( X = S[ year %in% POOL, DEFAULT, with = FALSE ] ),
    default_within        = list( X = W[ , DEFAULT, with = FALSE ] ),
    all_log_plus_logP     = list( X = P[ year %in% POOL, c( IV, "logP" ), with = FALSE ] ),
    all_sami              = list( X = S[ year %in% POOL, IV, with = FALSE ] ),
    all_within            = list( X = W[ , IV, with = FALSE ] ) )
  vt <- rbindlist( lapply( names( vsets ), function( k ) {
    o <- vif_tab( vsets[[ k ]]$X )
    data.table( model = k, var = names( o$vif ), vif = o$vif, cond = o$cond ) } ) )
  fwrite( r3( dcast( vt, var ~ model, value.var = "vif" ) ), file.path( CO, "p1_vif.csv" ) )
  fwrite( r3( unique( vt[ , .( model, cond ) ] ) ), file.path( CO, "p1_condition.csv" ) )
  print( r3( cl ) ); print( r3( dcast( vt, var ~ model, value.var = "vif" ) ) )
  print( r3( unique( vt[ , .( model, cond ) ] ) ) )
  print( r3( pr[ max_abs >= 0.6 ][ order( -max_abs ) ] ), nrows = 200 )
}

## ================================================================================
## Part 2: exploratory factor analysis (SAMI forms: size removed by construction)
## ================================================================================
efa <- function( X, nf = NULL, label ) {
  X <- as.data.frame( scale( X ) )
  kmo <- KMO( X ); bt <- cortest.bartlett( cor( X ), n = nrow( X ) )
  png( file.path( CO, paste0( "fig_p2_parallel_", label, ".png" ) ), width = 1600, height = 1200, res = 200 )
  pa <- fa.parallel( X, fm = "ml", fa = "fa", n.iter = 20, plot = TRUE, main = label )
  dev.off()
  if ( is.null( nf ) ) nf <- max( 1, pa$nfact )
  f <- fa( X, nfactors = nf, rotate = if ( nf > 1 ) "oblimin" else "none", fm = "ml", scores = "regression" )
  L <- unclass( f$loadings )
  out <- data.table( set = label, var = rownames( L ), round( L, 3 ), h2 = round( f$communality, 3 ),
                     msa = round( kmo$MSAi[ rownames( L ) ], 3 ) )
  list( fit = f, load = out, kmo = kmo$MSA, bartlett_p = bt$p.value, pa_nfact = pa$nfact, nf = nf,
        phi = f$Phi, tli = if ( is.null( f$TLI ) ) NA_real_ else f$TLI,
        rmsea = if ( is.null( f$RMSEA ) ) NA_real_ else unname( f$RMSEA[ 1 ] ) )
}

BLOCKSETS <- list(
  all          = setdiff( IV, c( "l_gdp_per_worker", "l_qcew_priv_wages" ) ),
  census       = ITEMS[ block %in% c( "diversity", "need" ), var ],
  philanthropy = ITEMS[ block == "philanthropy", var ],
  civic_political = ITEMS[ block %in% c( "political", "civic" ), var ] )

if ( 2 %in% PARTS ) {
  log_msg( "Part 2: EFA" )
  Xp <- S[ year %in% POOL ]
  res <- lapply( names( BLOCKSETS ), function( b ) efa( Xp[ , BLOCKSETS[[ b ]], with = FALSE ], label = b ) )
  names( res ) <- names( BLOCKSETS )
  ## census also with one more / fewer factor than parallel analysis (sensitivity)
  for ( b in names( res ) ) {
    r <- res[[ b ]]
    cat( "\n====", b, "| KMO", round( r$kmo, 3 ), "| Bartlett p", signif( r$bartlett_p, 3 ),
         "| parallel nfact", r$pa_nfact, "| TLI", round( r$tli, 3 ), "| RMSEA", round( r$rmsea, 3 ), "\n" )
    print( r$load, nrows = 100 ); if ( !is.null( r$phi ) ) print( round( r$phi, 2 ) )
  }
  fwrite( rbindlist( lapply( res, `[[`, "load" ), fill = TRUE ), file.path( CO, "p2_loadings.csv" ) )
  fwrite( rbindlist( lapply( names( res ), function( b ) data.table(
    set = b, kmo = res[[ b ]]$kmo, bartlett_p = res[[ b ]]$bartlett_p, parallel_nfact = res[[ b ]]$pa_nfact,
    tli = res[[ b ]]$tli, rmsea = res[[ b ]]$rmsea ) ) ), file.path( CO, "p2_fit.csv" ) )
  ## census with 4 and 3 factors (sensitivity to the parallel-analysis count)
  for ( k in 3:4 ) {
    ck <- efa( Xp[ , BLOCKSETS$census, with = FALSE ], nf = k, label = paste0( "census_nf", k ) )
    cat( "\n==== census nf", k, "| TLI", round( ck$tli, 3 ), "| RMSEA", round( ck$rmsea, 3 ), "\n" )
    print( ck$load ); print( round( ck$phi, 2 ) )
    fwrite( ck$load, file.path( CO, paste0( "p2_loadings_census_nf", k, ".csv" ) ) )
    res[[ paste0( "census_nf", k ) ]] <- ck
  }
  saveRDS( res, file.path( CO, "p2_efa.rds" ) )
  ## log forms (size not removed) for philanthropy: shows the size factor
  lp <- efa( P[ year %in% POOL, c( BLOCKSETS$philanthropy, "logP" ), with = FALSE ], nf = 2, label = "philanthropy_logform" )
  print( lp$load ); fwrite( lp$load, file.path( CO, "p2_loadings_philanthropy_logform.csv" ) )
}

## ================================================================================
## Part 3: composites and panel models
## ================================================================================
## Composites follow the EFA (SAMI forms, pooled 2015-2022): census 4 factors,
## philanthropy 2, civic/political 2 (+ religious homogeneity as its own item),
## government 1. Each composite is a one-factor ML score on its own items with
## weights fixed on the pooled sample (so scores are comparable across years),
## plus an additive alternative (mean of signed z-scores). Single items are z.
COMPOSITES <- list(
  affluence        = c( l_pcpi = 1, l_mhi = 1, pct_ba = 1, poverty_rate = -1 ),
  diversity        = c( pct_nonwhite = 1, pct_foreign_born = 1, race_hhi = -1 ),
  age_structure    = c( age_sd = 1, pct_65plus = 1 ),
  inequality       = c( gini = 1 ),
  rel_homogeneity  = c( rel_hhi = 1 ),
  foundations      = c( l_pf_assets = 1, l_pf_grants = 1, l_n_pf = 1 ),
  indiv_giving     = c( l_contrib_total = 1, l_contrib_per_return = 1, contrib_share_agi = 1 ),
  civic            = c( turnout = 1, socap_respn = 1 ),
  civic_assn       = c( turnout = 1, socap_sk2 = 1, l_socap_assn_n = 1 ),
  rel_conservative = c( pct_adherent = 1, l_rel_cng = 1, dem_margin = -1 ),
  government       = c( l_gov_earn_sl = 1, l_gov_emp_sl = 1, gov_emp_share = 1 ) )
CITEMS <- unlist( lapply( COMPOSITES, names ), use.names = FALSE )
OUTV <- c( "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu" )
## main set uses census response (civic); the association-count variant (civic_assn)
## is a sensitivity: association establishments are largely nonprofits (CBP 813)
SET_MAIN <- setdiff( names( COMPOSITES ), "civic_assn" )
SET_ASSN <- setdiff( names( COMPOSITES ), "civic" )
ALL_ITEMS  <- unique( unlist( lapply( COMPOSITES[ SET_MAIN ], names ), use.names = FALSE ) )
ITEMS_ASSN <- unique( unlist( lapply( COMPOSITES[ SET_ASSN ], names ), use.names = FALSE ) )
CITEMS <- unique( CITEMS )

if ( 3 %in% PARTS ) {
  log_msg( "Part 3: composites and panel models" )
  ref <- S[ year %in% POOL ]
  mu <- vapply( CITEMS, function( v ) mean( ref[[ v ]] ), numeric( 1 ) )
  sg <- vapply( CITEMS, function( v ) sd( ref[[ v ]] ), numeric( 1 ) )
  Zm_of <- function( DT, it ) {
    z <- sapply( it, function( v ) ( DT[[ v ]] - mu[[ v ]] ) / sg[[ v ]] )
    if ( is.null( dim( z ) ) ) z <- matrix( z, ncol = 1, dimnames = list( NULL, it ) )
    z
  }

  rel <- list(); wts <- list()
  for ( k in names( COMPOSITES ) ) {
    it <- names( COMPOSITES[[ k ]] ); sgn <- COMPOSITES[[ k ]]
    if ( length( it ) > 1 ) {
      Zr <- Zm_of( ref, it )
      f1 <- fa( Zr, nfactors = 1, fm = "ml", scores = "regression" )
      w <- f1$weights[ , 1 ]; ld <- unclass( f1$loadings )[ , 1 ]
      if ( sum( ld * sgn ) < 0 ) { w <- -w; ld <- -ld }
      al <- psych::alpha( sweep( Zr, 2, sgn, `*` ), warnings = FALSE, check.keys = FALSE )$total$raw_alpha
      rel[[ k ]] <- data.table( composite = k, item = it, sign = sgn, loading = ld, weight = w,
                                alpha = al, var_explained = mean( ld^2 ) )
    } else {
      w <- setNames( 1, it )
      rel[[ k ]] <- data.table( composite = k, item = it, sign = sgn, loading = 1, weight = 1,
                                alpha = NA_real_, var_explained = 1 )
    }
    wts[[ k ]] <- w
  }
  rel <- rbindlist( rel ); fwrite( r3( rel ), file.path( CO, "p3_composite_weights.csv" ) ); print( r3( rel ) )

  ## score with fixed weights: factor (F_) and additive signed-z mean (A_)
  ## (both linear, so scoring SAMI-form items gives the SAMI of the composite)
  for ( k in names( COMPOSITES ) ) {
    it <- names( COMPOSITES[[ k ]] ); Zm <- Zm_of( S, it )
    set( S, j = paste0( "F_", k ), value = as.numeric( Zm %*% wts[[ k ]][ it ] ) )
    set( S, j = paste0( "A_", k ), value = as.numeric( Zm %*% COMPOSITES[[ k ]][ it ] ) / length( it ) )
  }
  FV <- paste0( "F_", SET_MAIN ); AV <- paste0( "A_", SET_MAIN ); AV2 <- paste0( "A_", SET_ASSN )

  Wc <- copy( S ); dmc <- demean( as.matrix( S[ , FV, with = FALSE ] ), f = S[ , .( cbsa, year ) ] )
  for ( j in seq_along( FV ) ) set( Wc, j = FV[ j ], value = dmc[ , j ] )
  Rc <- cor( S[ year %in% POOL, FV, with = FALSE ] )
  fwrite( data.table( var = rownames( Rc ), round( Rc, 3 ) ), file.path( CO, "p3_composite_cor.csv" ) )
  print( round( Rc, 2 ) )
  cvif <- rbindlist( list(
    data.table( set = "composites_sami", var = FV, vif = vif_tab( S[ year %in% POOL, FV, with = FALSE ] )$vif ),
    data.table( set = "composites_within", var = FV, vif = vif_tab( Wc[ , FV, with = FALSE ] )$vif ),
    data.table( set = "items_sami", var = ALL_ITEMS, vif = vif_tab( S[ year %in% POOL, ALL_ITEMS, with = FALSE ] )$vif ),
    data.table( set = "items_within", var = ALL_ITEMS, vif = vif_tab( W[ , ALL_ITEMS, with = FALSE ] )$vif ) ) )
  fwrite( r3( cvif ), file.path( CO, "p3_vif_models.csv" ) )
  print( r3( cvif[ , .( max_vif = max( vif ), median_vif = median( vif ) ), by = set ] ) )
  print( r3( cvif[ set == "composites_within" ] ) )

  ## outcomes: year-specific SAMI of log y
  for ( y in OUTV ) set( S, j = paste0( "s_", y ),
                         value = resid_logP( suppressWarnings( log( S[[ y ]] ) ), S$logP, S$year ) )
  MODELS <- list( default = DEFAULT, items = ALL_ITEMS, items_assn = ITEMS_ASSN,
                  factor = FV, additive = AV, additive_assn = AV2 )
  BSETS <- list( items = SET_MAIN, items_assn = SET_ASSN )
  bo <- rbindlist( lapply( names( COMPOSITES ), function( k ) data.table( v = names( COMPOSITES[[ k ]] ), b = k ) ) )
  bo <- bo[ !( v == "turnout" & b == "civic_assn" ) ]
  block_of <- c( setNames( bo$b, bo$v ), setNames( names( COMPOSITES ), paste0( "F_", names( COMPOSITES ) ) ),
                 setNames( names( COMPOSITES ), paste0( "A_", names( COMPOSITES ) ) ) )
  IDS <- list( cs2019 = list( yrs = 2019, fe = "0", vc = "hetero" ),
               pooled = list( yrs = POOL, fe = "year", vc = ~cbsa ),
               within = list( yrs = 2010:2022, fe = "cbsa + year", vc = ~cbsa ) )

  coefs <- list(); fits <- list(); walds <- list()
  for ( y in OUTV ) for ( id in names( IDS ) ) for ( mn in names( MODELS ) ) {
    xs <- MODELS[[ mn ]]; I <- IDS[[ id ]]
    m <- S[ year %in% I$yrs & is.finite( get( paste0( "s_", y ) ) ) ]
    m[ , yy := get( paste0( "s_", y ) ) ]
    ## unit-SD predictors on the estimation sample (coefficients per SD)
    for ( v in xs ) set( m, j = v, value = ( m[[ v ]] - mean( m[[ v ]] ) ) / sd( m[[ v ]] ) )
    r <- feols( as.formula( paste( "yy ~", paste( xs, collapse = " + " ), "|", I$fe ) ), data = m, vcov = I$vc )
    ct <- coeftable( r )
    coefs[[ length( coefs ) + 1 ]] <- data.table( outcome = y, ident = id, model = mn, term = rownames( ct ),
      block = block_of[ rownames( ct ) ], est = ct[ , 1 ], se = ct[ , 2 ], t = ct[ , 3 ], p = ct[ , 4 ] )
    fits[[ length( fits ) + 1 ]] <- data.table( outcome = y, ident = id, model = mn, k = length( xs ),
      n = nobs( r ), r2 = as.numeric( r2( r, "r2" ) ),
      wr2 = if ( id == "within" ) as.numeric( r2( r, "wr2" ) ) else NA_real_,
      ar2 = as.numeric( r2( r, "ar2" ) ), bic = BIC( r ), n_sig05 = sum( ct[ , 4 ] < 0.05 ),
      median_se = median( ct[ , 2 ] ) )
    ## block Wald tests in the item model (does the block matter jointly?)
    if ( mn %in% names( BSETS ) ) for ( b in BSETS[[ mn ]] ) {
      it <- intersect( names( COMPOSITES[[ b ]] ), rownames( ct ) )
      wt <- if ( length( it ) > 1 ) wald( r, keep = paste0( "^(", paste( it, collapse = "|" ), ")$" ), print = FALSE ) else NULL
      walds[[ length( walds ) + 1 ]] <- data.table( outcome = y, ident = id, model = mn, block = b, n_items = length( it ),
        items_sig05 = sum( ct[ it, 4 ] < 0.05 ), min_item_p = min( ct[ it, 4 ] ),
        wald_F = if ( is.null( wt ) ) ct[ it, 3 ]^2 else wt$stat,
        wald_p = if ( is.null( wt ) ) ct[ it, 4 ] else wt$p )
    }
  }
  coefs <- rbindlist( coefs ); fits <- rbindlist( fits ); walds <- rbindlist( walds )
  fwrite( r3( coefs ), file.path( CO, "p3_coefs.csv" ) )
  fwrite( r3( fits ), file.path( CO, "p3_fit.csv" ) )
  cp <- coefs[ model %in% c( "factor", "additive", "additive_assn" ), .( outcome, ident, model, block, est, se, t, p ) ]
  cp[ model == "additive_assn", model := "additive" ]
  cp <- unique( cp, by = c( "outcome", "ident", "model", "block" ) )
  cpw <- dcast( cp, outcome + ident + block ~ model, value.var = c( "est", "se", "p" ) )
  bw <- merge( walds[ model == "items" | block == "civic_assn" ], cpw, by = c( "outcome", "ident", "block" ) )
  fwrite( r3( bw ), file.path( CO, "p3_block_vs_composite.csv" ) )
  options( width = 220 )
  print( r3( fits ), nrows = 100 )
  print( r3( bw[ order( outcome, ident, block ) ] ), nrows = 200 )
}
