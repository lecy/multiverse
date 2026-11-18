## 58_outline_analyses.R
## Analyses for the methods-paper outline (Nov 13, 2026).
##   Part A  decision importance on run v4: marginal eta^2 of each design
##           decision (D1-D11) on the partial r of five benchmark covariates
##   Part B  Fact 6: metro size as the source of collinearity (attenuation and
##           FWL equivalence, 2022 metros)
##   Part C  D11: subsector scope -- Poisson models of the default covariates
##           fitted separately for each NCCS subsector (2022 metros)
##   Part D  explainer figures: one covariate over the grid; what eta^2 measures
##   Part E  what partial r measures (bivariate vs partial scatter, % BA) and,
##           for D9, how much of each covariate's variance lies between states
## Writes D:/MULTIVERSE/data/outline/. Single process.
## Usage: Rscript R/notes/58_outline_analyses.R [A ... M]  (J = D4 DV form, K = D5 zeros, L = D6 predictor form, M = D8 history, N = D10 money measure, O = age-size chart, P = D12 sector age, Q = Fact 7 hazards, R = revenue floor, S = spec-curve exhibit, T = fast-grower lag vs trait, U = D7 diversity default, V = spec curves with one decision broken out [V est | V all | V outcome unit ...; add exp for spending], W = summary table by covariate and decision)

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ) } )
setFixest_notes( FALSE )
OD <- file.path( BUILD, "data", "outline" ); dir.create( OD, showWarnings = FALSE )
## multiverse run used by the run-based parts (A, D, F, L, M, N, S, V, W). Set MV_RUN=run_principled_v5
## to use v5 (pct_nonwhite as the diversity default); outputs then go to outline/v5/ so v4 results stay.
RUN  <- Sys.getenv( "MV_RUN", "run_principled_v4" )
NEWRUN <- RUN != "run_principled_v4"
if ( NEWRUN ) { OD <- file.path( BUILD, "data", "outline", sub( "^run_principled_", "", RUN ) ); dir.create( OD, showWarnings = FALSE ) }
DIVT <- if ( NEWRUN ) "pct_nonwhite" else "race_hhi"                       # diversity term of the default model
DIVL <- if ( NEWRUN ) "Diversity (% nonwhite)" else "Diversity (racial HHI)"
DIVS <- if ( NEWRUN ) "% nonwhite" else "racial HHI"
DIVC <- if ( NEWRUN ) "% nonwhite" else "Racial HHI"                          # capitalized label
## cross-sectional parts: diversity control of the default model. From v5 on it is % nonwhite (D7); the column
## keeps the name race_hhi inside each part so the model code is unchanged, and labels use DIVC. Parts F and U
## compare the diversity measures and read both, so they are not switched.
divx <- function( x ) { if ( NEWRUN ) x[ , race_hhi := pct_nonwhite ]; x }
RUNL <- sub( "^run_principled_", "", RUN )                                  # "v4", "v5"
PARTS <- if ( length( commandArgs( TRUE ) ) ) commandArgs( TRUE ) else c( "A", "B", "C" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
slog <- function( x ) log( pmax( x, 0 ) + 1 )
## extensive covariates (foundation assets, government earnings, contributions, endowment)
## are logged with the per-capita floor of 40_multiverse.R from 2026-11-15 on: per-capita
## values below the 1st percentile of positive values in the model's sample are raised
## to it. slog()/log1p() put Eagle Pass, TX (zero foundation assets) ~11 SD below the
## metro mean. Outputs before 2026-11-15 (outline/, outline/v5/) used slog/log1p.
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
BENCH <- setNames( c( "% BA", "Foundation assets", DIVL, "Turnout", "Poverty rate" ), c( "pct_ba", "pf_assets", DIVT, "turnout", "poverty_rate" ) )

## ===========================================================================
## Part A: decision importance
## ===========================================================================
if ( "A" %in% PARTS ) {
  log_msg( "Part A: decision importance on ", RUNL )
  v4 <- file.path( OUT$spec, RUN )
  specs <- as.data.table( read_parquet( file.path( v4, "specs.parquet" ) ) )
  co <- as.data.table( read_parquet( file.path( v4, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co <- merge( co, specs[ , .( spec_id, ident, est ) ], by = "spec_id" )
  ## partial r with degrees of freedom counted in independent places, not
  ## place-years: panel SEs are clustered by metro (or state), so n - k would
  ## deflate every panel r by about sqrt(years). Pooled = 8 years, within = 13.
  co[ , n_units := n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) ]
  co[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
  ## state random-intercept fits in pooled strata use model-based SEs that treat
  ## metro-years as independent; their t is not comparable, so they are left out
  co <- co[ term != "logP" & !( ident == "pooled" & est == "state_re" ), .( spec_id, term, pr ) ]
  d <- merge( co, specs, by = "spec_id" )
  rm( co ); gc()

  eta2 <- function( y, g ) { g <- as.factor( g ); if ( nlevels( g ) < 2 || length( y ) < 10 ) return( NA_real_ )
    m <- tapply( y, g, mean ); n <- tapply( y, g, length )
    sum( n * ( m - mean( y ) )^2, na.rm = TRUE ) / sum( ( y - mean( y ) )^2 ) }
  ## eta^2 of `fac` within each group (by), then the mean over groups
  eta_by <- function( x, fac, by ) {
    if ( !nrow( x ) ) return( NA_real_ )
    e <- x[ , .( e = eta2( pr, get( fac ) ) ), by = by ]
    mean( e$e, na.rm = TRUE ) }
  ## benchmark value of a spec: the benchmark term, or for D7 the term that
  ## measures the same construct (racial HHI is concentration, so it is flipped)
  BLOCK_OF <- setNames( c( "need", "philanthropy", "diversity", "political", "need" ), names( BENCH ) )
  construct_val <- function( b, x ) {
    blk <- BLOCK_OF[[ b ]]
    x <- x[ op == "default" | sub( ":.*", "", op ) == blk ]
    alt_terms <- switch( blk,
      need = if ( b == "pct_ba" ) "pct_ba" else c( "poverty_rate", "unemp_rate" ),
      philanthropy = c( "pf_assets", "n_pf", "contrib_total", "contrib_per_return", "contrib_share_agi" ),
      diversity = c( "race_hhi", "pct_nonwhite", "pct_foreign_born", "gini", "age_sd" ),
      political = c( "turnout", "dem_margin" ) )
    x <- x[ term %in% alt_terms ]
    if ( blk == "political" ) x <- x[ !( op == "political:both" & term == "dem_margin" ) ]
    x[ term == "race_hhi", pr := -pr ]
    x[ , measure := paste( op ) ]
    x }

  GROUPS <- list( counts = quote( construct == "count" & hu == "incl" ),
                  spending = quote( construct == "exp" & hu == "excl" ) )
  base <- d[ drop == "none" ]
  res <- list()
  for ( gname in names( GROUPS ) ) for ( b in names( BENCH ) ) {
    x  <- base[ eval( GROUPS[[ gname ]] ) & ident != "within" ]
    xd <- x[ op == "default" & term == b ]
    row <- function( dec, val ) res[[ length( res ) + 1 ]] <<- data.table( group = gname, bench = b, decision = dec, eta2 = val )
    ## D1 outcome population
    if ( gname == "counts" ) row( "D1", eta_by( xd, "outcome", "ident" ) )                      # filers vs BMF registrations
    else { z <- base[ hu == "excl" & ident != "within" & op == "default" & term == b &          # counts vs spending
                      outcome %in% c( "n_filer_c3_nohu", "exp_c3_nohu" ) ]
           row( "D1", eta_by( z, "construct", "ident" ) ) }
    row( "D2", eta_by( xd, "unit", c( "ident", "outcome" ) ) )
    ## D3 identification: cs vs pooled vs within, same outcome, default model
    z <- base[ eval( GROUPS[[ gname ]] ) & op == "default" & term == b ]
    row( "D3", eta_by( z, "ident", "outcome" ) )
    row( "D4", eta_by( xd, "dv_form", c( "ident", "outcome" ) ) )
    row( "D5", eta_by( xd[ , pois := dv_form %like% "^pois" ], "pois", c( "ident", "outcome" ) ) )
    row( "D6", eta_by( xd, "pred_form", c( "ident", "outcome" ) ) )
    row( "D7", eta_by( construct_val( b, x ), "measure", c( "ident", "outcome" ) ) )
    row( "D8", eta_by( x[ op %in% c( "default", "history:pre1970" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    ## population-lag add-on (run v7 on; 82_lag_fork.R): default vs default + a lag kernel
    if ( any( d$op %like% "^lag:" ) ) row( "Lag", eta_by( x[ ( op == "default" | op %like% "^lag:" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    ## civic and religion add-on blocks (run v8): default vs default + one added covariate
    if ( any( d$op %like% "^civic:" ) ) row( "Civic", eta_by( x[ ( op == "default" | op %like% "^civic:" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    if ( any( d$op %like% "^religion:" ) ) row( "Religion", eta_by( x[ ( op == "default" | op %like% "^religion:" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    ## inequality and congregations add-on blocks (run v9; 92_ineq_cong_fork.R)
    if ( any( d$op %like% "^inequality:" ) ) row( "Inequality", eta_by( x[ ( op == "default" | op %like% "^inequality:" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    if ( any( d$op %like% "^congregations:" ) ) row( "Congregations", eta_by( x[ ( op == "default" | op %like% "^congregations:" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    ## government delivery-mode add-on blocks (run v10; 93_gov_fork.R)
    if ( any( d$op %like% "^gov_indirect:" ) ) row( "Indirect government", eta_by( x[ ( op == "default" | op %like% "^gov_indirect:" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    if ( any( d$op %like% "^gov_direct:" ) ) row( "Direct spending", eta_by( x[ ( op == "default" | op %like% "^gov_direct:" ) & term == b ], "op", c( "ident", "outcome" ) ) )
    row( "D9", eta_by( xd, "est", c( "ident", "outcome" ) ) )
    ## D10 money measure (spending only) and hospital/university exclusion
    if ( gname == "spending" ) row( "D10a", eta_by( xd, "outcome", "ident" ) )
    z <- base[ construct == ifelse( gname == "counts", "count", "exp" ) & ident != "within" & op == "default" & term == b &
               outcome %in% c( "n_filer_c3", "n_filer_c3_nohu", "exp_c3", "exp_c3_nohu" ) ]
    row( "D10b", eta_by( z, "hu", "ident" ) )
    row( "Year", eta_by( xd[ ident == "cs" ], "cs_year", "outcome" ) )
    row( "Winsorizing", eta_by( xd, "outlier", c( "ident", "outcome" ) ) )
    row( "log P on RHS", eta_by( xd, "rhs_logP", c( "ident", "outcome" ) ) )
  }
  imp <- rbindlist( res )
  fwrite( imp, file.path( OD, "decision_importance_long.csv" ) )
  w <- dcast( imp, group + decision ~ bench, value.var = "eta2" )
  setcolorder( w, c( "group", "decision", names( BENCH ) ) )
  fwrite( w, file.path( OD, "decision_importance.csv" ) )
  print( r3( w ) )
}

## ===========================================================================
## Part B: Fact 6 -- size and collinearity
## ===========================================================================
if ( "B" %in% PARTS ) {
  log_msg( "Part B: size and collinearity" )
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m[ , logP := log( population ) ]
  m[ , s_n := resid( lm( log( n_filer_c3 ) ~ logP ) ) ]
  X <- list( "Foundation assets" = flog( m$pf_assets, m$population ), "S&L govt earnings" = flog( m$gov_earn_sl, m$population ),
             "Itemized contributions" = flog( m$contrib_total, m$population ), "No. of foundations" = flog( m$n_pf, m$population ),
             "% BA" = m$pct_ba, "Turnout" = m$turnout, "Racial HHI" = m$race_hhi, "Poverty rate" = m$poverty_rate ); names( X )[ names( X ) == "Racial HHI" ] <- DIVC
  pr <- function( ct, df ) ct[ 3 ] / sqrt( ct[ 3 ]^2 + df )
  att <- rbindlist( lapply( names( X ), function( k ) {
    x <- X[[ k ]]; ok <- is.finite( x ); dd <- m[ ok ]; x <- x[ ok ]
    r2 <- summary( lm( x ~ dd$logP ) )$r.squared
    sx <- resid( lm( x ~ dd$logP ) )
    a <- coeftable( feols( s_n ~ x, data.frame( s_n = dd$s_n, x = x ), vcov = "hetero" ) )[ "x", ]                # SAMI on raw (logged) X
    b <- coeftable( feols( s_n ~ sx, data.frame( s_n = dd$s_n, sx = sx ), vcov = "hetero" ) )[ "sx", ]             # SAMI on SAMI_X
    cc <- coeftable( feols( ly ~ logP + x, data.frame( ly = log( dd$n_filer_c3 ), logP = dd$logP, x = x ), vcov = "hetero" ) )[ "x", ]  # log N on log P + X
    nn <- coeftable( feols( ly ~ x, data.frame( ly = log( dd$n_filer_c3 ), x = x ), vcov = "hetero" ) )[ "x", ]    # log N on X, no size control
    data.table( covariate = k, r_logP = sign( cor( x, dd$logP ) ) * sqrt( r2 ), r2_logP = r2,
                b_no_size = nn[ 1 ], r_no_size = pr( nn, nrow( dd ) - 2 ),
                b_sami_on_raw = a[ 1 ], b_sami_on_sami = b[ 1 ], b_loglog = cc[ 1 ],
                attenuation = a[ 1 ] / cc[ 1 ], expected_attenuation = 1 - r2,
                r_sami_on_raw = pr( a, nrow( dd ) - 2 ), r_sami_on_sami = pr( b, nrow( dd ) - 2 ), r_loglog = pr( cc, nrow( dd ) - 3 ) )
  } ) )
  print( r3( att ) ); fwrite( att, file.path( OD, "fact6_size_attenuation.csv" ) )
  ## the full default model in three arms: VIF and the government-earnings coefficient
  m[ , `:=`( l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ) ) ]
  for ( v in c( "l_pf", "l_gov", "race_hhi", "turnout", "poverty_rate", "pct_ba" ) ) m[ , ( paste0( "s_", v ) ) := resid( lm( get( v ) ~ logP ) ) ]
  vifs <- function( Xm ) { Xm <- as.matrix( Xm ); sapply( seq_len( ncol( Xm ) ), function( j ) 1 / ( 1 - summary( lm( Xm[ , j ] ~ Xm[ , -j ] ) )$r.squared ) ) }
  armL <- m[ , .( logP, l_pf, l_gov, race_hhi, turnout, poverty_rate, pct_ba ) ]
  armS <- m[ , .( s_l_pf, s_l_gov, s_race_hhi, s_turnout, s_poverty_rate, s_pct_ba ) ]
  armN <- m[ , .( l_pf, l_gov, race_hhi, turnout, poverty_rate, pct_ba ) ]
  vt <- rbind( data.table( arm = "log Y on log P + logged covariates", term = names( armL ), vif = vifs( armL ) ),
               data.table( arm = "SAMI on SAMI-form covariates", term = sub( "^s_", "", names( armS ) ), vif = vifs( armS ) ),
               data.table( arm = "SAMI on logged covariates (no size control)", term = names( armN ), vif = vifs( armN ) ) )
  print( r3( dcast( vt, term ~ arm, value.var = "vif" ) ) ); fwrite( vt, file.path( OD, "fact6_vif.csv" ) )
  fm <- list( "log Y ~ log P + logged" = feols( log( n_filer_c3 ) ~ logP + l_pf + l_gov + race_hhi + turnout + poverty_rate + pct_ba, m, vcov = "hetero" ),
              "SAMI ~ SAMI-form" = feols( s_n ~ s_l_pf + s_l_gov + s_race_hhi + s_turnout + s_poverty_rate + s_pct_ba, m, vcov = "hetero" ),
              "SAMI ~ logged (no size)" = feols( s_n ~ l_pf + l_gov + race_hhi + turnout + poverty_rate + pct_ba, m, vcov = "hetero" ),
              "log Y ~ logged (no size)" = feols( log( n_filer_c3 ) ~ l_pf + l_gov + race_hhi + turnout + poverty_rate + pct_ba, m, vcov = "hetero" ) )
  mm <- rbindlist( lapply( names( fm ), function( k ) { ct <- coeftable( fm[[ k ]] ); dfr <- degrees_freedom( fm[[ k ]], "resid" )
    data.table( model = k, term = sub( "^s_", "", rownames( ct ) ), est = ct[ , 1 ], t = ct[ , 3 ], r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + dfr ) ) } ) )
  print( r3( dcast( mm[ !term %in% c( "(Intercept)" ) ], term ~ model, value.var = "r" ) ) )
  fwrite( mm, file.path( OD, "fact6_models.csv" ) )
}

## ===========================================================================
## Part C: D11 -- subsector scope
## ===========================================================================
if ( "C" %in% PARTS ) {
  log_msg( "Part C: subsector models" )
  GRP <- c( ART = "Arts", EDU = "Education", ENV = "Environment/animals", HEL = "Health", HMS = "Human services",
            IFA = "International", PSB = "Public/societal benefit", REL = "Religion-related", MMB = "Mutual/membership",
            HOS = "Hospitals", UNI = "Universities" )
  sub <- fread( file.path( BUILD, "data", "followup", "p3_subsector_metro_year.csv" ), colClasses = list( character = "cbsa" ) )[ year == 2022 ]
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m[ , `:=`( logP = log( population ), l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ) ) ]
  cv <- c( "race_hhi", "l_pf", "turnout", "l_gov", "poverty_rate", "pct_ba" )
  tot <- sub[ grp != "UNU", .( n = sum( n ), exp = sum( exp ) ), by = cbsa ][ , grp := "ALL" ]
  sub <- rbind( sub[ , .( cbsa, grp, n, exp ) ], tot )
  x <- merge( sub, m[ , c( "cbsa", "logP", cv ), with = FALSE ], by = "cbsa" )
  ## covariates at unit SD so coefficients compare across subsectors
  for ( v in cv ) x[ , ( v ) := ( get( v ) - mean( get( v ) ) ) / sd( get( v ) ) ]
  f <- as.formula( paste( "n ~ logP +", paste( cv, collapse = " + " ) ) )
  fe <- as.formula( paste( "exp ~ logP +", paste( cv, collapse = " + " ) ) )
  one <- function( g, form, what ) {
    z <- x[ grp == g ]; r <- fepois( form, z, vcov = "hetero" ); ct <- coeftable( r )
    data.table( grp = g, label = c( ALL = "All 501(c)(3) filers", GRP )[[ g ]], outcome = what, term = rownames( ct ),
                est = ct[ , 1 ], se = ct[ , 2 ], z = ct[ , 3 ], p = ct[ , 4 ],
                r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + nobs( r ) - nrow( ct ) - 1 ), orgs = sum( z$n ), zeros = sum( z$n == 0 ) ) }
  res <- rbindlist( c( lapply( c( "ALL", names( GRP ) ), one, form = f, what = "count" ),
                       lapply( c( "ALL", names( GRP ) ), one, form = fe, what = "spending" ) ) )
  fwrite( res, file.path( OD, "d11_subsector_models.csv" ) )
  wc <- dcast( res[ outcome == "count" ], label + orgs + zeros ~ term, value.var = "est" )
  print( r3( wc[ order( -orgs ) ] ) )
  wr <- dcast( res[ outcome == "count" & term != "logP" ], label ~ term, value.var = "r" )
  print( r3( wr ) )
  ## how many subsectors share the full-sector sign, and are significant, per covariate
  allsign <- res[ outcome == "count" & grp == "ALL", .( term, s_all = sign( est ) ) ]
  agree <- merge( res[ outcome == "count" & grp != "ALL" ], allsign, by = "term" )[
    , .( same_sign = sum( sign( est ) == s_all ), sig_same = sum( sign( est ) == s_all & p < .05 ),
         sig_opposite = sum( sign( est ) != s_all & p < .05 ), subsectors = .N ), by = term ]
  print( agree ); fwrite( agree, file.path( OD, "d11_sign_agreement.csv" ) )
  agree_e <- merge( res[ outcome == "spending" & grp != "ALL" ], res[ outcome == "spending" & grp == "ALL", .( term, s_all = sign( est ) ) ], by = "term" )[
    , .( same_sign = sum( sign( est ) == s_all ), sig_opposite = sum( sign( est ) != s_all & p < .05 ), subsectors = .N ), by = term ]
  print( agree_e ); fwrite( agree_e, file.path( OD, "d11_sign_agreement_spending.csv" ) )
}
## ===========================================================================
## Part D: explainer figures -- one covariate over the grid, and eta^2
## ===========================================================================
if ( "D" %in% PARTS ) {
  log_msg( "Part D: explainer figures" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  v4 <- file.path( OUT$spec, RUN )
  specs <- as.data.table( read_parquet( file.path( v4, "specs.parquet" ) ) )[ construct == "count" & hu == "incl" & ident == "cs" ]
  co <- as.data.table( read_parquet( file.path( v4, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )[ term == DIVT ]
  x <- merge( co, specs, by = "spec_id" )
  x[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
  x[ , grp := fcase( p < .05 & pr > 0, "Positive, p < .05", p < .05 & pr < 0, "Negative, p < .05", default = "Not significant" ) ]
  x <- x[ order( pr ) ][ , rank := .I / .N ]
  ink <- "grey15"; C1 <- "#2a78d6"; C2 <- "#eb6834"
  thm <- theme_minimal( base_size = 10 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot" )

  ## Figure A: specification curve + where each option sits along it
  lab <- list(
    outcome = c( n_filer_c3 = "Filers", n_bmf_c3pc = "Registrations (BMF)" ),
    dv_form = c( pc = "Y/P", logpc = "log(Y/P)", loglog = "log-log", sami = "SAMI", pois_off = "Poisson, offset", pois_free = "Poisson, free" ),
    pred_form = c( raw = "Raw", pc = "Per capita", log = "Log", sami = "Residualized" ),
    rhs_logP = c( `FALSE` = "No", `TRUE` = "Yes" ),
    unit = c( metro = "Metro", cbsa = "Metro + micro" ),
    est = c( ols = "OLS", div_fe = "Division FE", state_fe = "State FE", state_re = "State RE" ),
    cs_year = c( `2022` = "2022", `2019` = "2019" ),
    outlier = c( none = "None", winsor1 = "1%" ) )
  fork_name <- c( outcome = "Outcome", dv_form = "DV form", pred_form = "Predictor form", rhs_logP = "log P on right",
                  unit = "Unit", est = "Estimator", cs_year = "Year", outlier = "Winsorizing", cov = "Covariate set" )
  x[ , cov := fcase( op == "default" & drop == "none", "Default", drop != "none", "Drop a block", op == "history:pre1970", "Add endowment", op %like% "^lag:", "Add population lag", op %like% "^civic:", "Add civic measure", op %like% "^religion:", "Add religion measure", op %like% "^inequality:", "Add inequality", op %like% "^congregations:", "Add congregations", op %like% "^gov_indirect:", "Add indirect government", op %like% "^gov_direct:", "Add direct spending", default = "Swap a measure" ) ]
  NB <- 60
  x[ , bin := pmin( NB, ceiling( rank * NB ) ) ]
  strips <- rbindlist( lapply( names( fork_name ), function( f ) {
    v <- as.character( x[[ f ]] ); labs <- if ( f == "cov" ) c( Default = "Default", `Drop a block` = "Drop a block", `Swap a measure` = "Swap a measure", `Add endowment` = "Add endowment", `Add population lag` = "Add population lag", `Add civic measure` = "Add civic measure", `Add religion measure` = "Add religion measure", `Add inequality` = "Add inequality", `Add congregations` = "Add congregations", `Add indirect government` = "Add indirect government", `Add direct spending` = "Add direct spending" ) else lab[[ f ]]
    z <- data.table( bin = x$bin, opt = labs[ v ] )[ , .N, by = .( bin, opt ) ][ , share := N / sum( N ), by = bin ]
    z[ , `:=`( fork = fork_name[[ f ]], opt = factor( opt, rev( unname( labs ) ) ) ) ] } ) )
  strips[ , fork := factor( fork, unname( fork_name ) ) ]
  strips[ , row := paste( fork, opt, sep = ": " ) ]
  ord <- strips[ , .( fork, opt ) ][ , unique( .SD ) ][ order( fork, -as.integer( opt ) ) ][ , paste( fork, opt, sep = ": " ) ]
  strips[ , row := factor( row, rev( ord ) ) ]
  strips[ , label := as.character( opt ) ]
  medr <- median( x$pr ); ss <- mean( sign( x$pr ) == sign( medr ) )
  pA <- ggplot( x, aes( rank, pr, colour = grp ) ) + geom_hline( yintercept = 0, colour = "grey60" ) +
    geom_point( size = 0.35, alpha = 0.6, stroke = 0 ) +
    scale_colour_manual( values = c( `Positive, p < .05` = C1, `Negative, p < .05` = C2, `Not significant` = "grey70" ), name = NULL ) +
    guides( colour = guide_legend( override.aes = list( size = 2.5, alpha = 1 ) ) ) +
    scale_x_continuous( labels = scales::percent, expand = c( 0.005, 0 ) ) +
    labs( title = sprintf( "%s across all %s specifications in one stratum (counts, cross-section)", DIVC, format( nrow( x ), big.mark = "," ) ),
          subtitle = sprintf( "Median partial r %.3f; %d%% share its sign; %d%% positive and significant, %d%% negative and significant",
                              medr, round( 100 * ss ), round( 100 * mean( x$grp == "Positive, p < .05" ) ), round( 100 * mean( x$grp == "Negative, p < .05" ) ) ),
          x = NULL, y = "Partial r" ) + thm + theme( legend.position = "top", axis.text.x = element_blank() )
  pB <- ggplot( strips, aes( bin / NB, row, fill = share ) ) + geom_tile( height = 0.9 ) +
    scale_fill_gradient( low = "#f3f6fb", high = "#104281", limits = c( 0, 1 ), name = "Share of specs\nin that part of\nthe curve", labels = scales::percent ) +
    scale_y_discrete( labels = function( z ) sub( ".*: ", "", z ) ) +
    facet_grid( fork ~ ., scales = "free_y", space = "free_y", switch = "y" ) +
    scale_x_continuous( labels = scales::percent, expand = c( 0.005, 0 ) ) +
    labs( x = "Specifications ranked from most negative to most positive partial r", y = NULL ) + thm +
    theme( strip.placement = "outside", strip.text.y.left = element_text( angle = 0, hjust = 1, face = "bold" ),
           panel.spacing.y = unit( 2, "pt" ), axis.text.y = element_text( size = 8 ), panel.grid = element_blank() )
  ggsave( file.path( OD, "fig_spec_curve_race_hhi.png" ), pA / pB + plot_layout( heights = c( 1, 2.4 ) ), width = 10, height = 9.5, dpi = 170, bg = "white" )

  ## Figure B: what eta^2 measures -- one stratum, one outcome, default model
  y <- x[ outcome == "n_filer_c3" & op == "default" & drop == "none" ]
  eta <- function( v, g ) { m <- tapply( v, g, mean ); n <- tapply( v, g, length ); sum( n * ( m - mean( v ) )^2 ) / sum( ( v - mean( v ) )^2 ) }
  e_est <- eta( y$pr, y$est ); e_win <- eta( y$pr, y$outlier )
  mk <- function( f, labs, e, ttl ) {
    z <- copy( y )[ , opt := factor( labs[ as.character( get( f ) ) ], unname( labs ) ) ]
    mu <- z[ , .( m = mean( pr ), n = .N ), by = opt ]
    gm <- mean( z$pr )
    ggplot( z, aes( pr, opt ) ) +
      geom_vline( xintercept = gm, colour = "grey45", linetype = 2 ) +
      geom_jitter( height = 0.18, width = 0, size = 0.6, alpha = 0.35, colour = "grey45", stroke = 0 ) +
      geom_segment( data = mu, aes( x = gm, xend = m, y = opt, yend = opt ), colour = C2, linewidth = 1.1,
                    arrow = arrow( length = unit( 0.12, "cm" ) ) ) +
      geom_point( data = mu, aes( m, opt ), colour = C2, size = 3 ) +
      annotate( "text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.3, size = 9, fontface = "bold", colour = C2,
                label = sprintf( "η² = %.2f", e ) ) +
      labs( title = ttl, x = paste( "Partial r of", DIVS ), y = NULL ) + thm +
      coord_cartesian( xlim = range( y$pr ) + c( -0.01, 0.06 ) ) }
  p1 <- mk( "est", c( ols = "OLS", div_fe = "Division FE", state_fe = "State FE", state_re = "State RE" ), e_est,
            "A decision that matters: estimator" )
  p2 <- mk( "outlier", c( none = "No winsorizing", winsor1 = "Winsorize 1%" ), e_win, "A decision that doesn't: winsorizing" )
  cap <- sprintf( paste0( "Filer counts, cross-section, default covariates (%s specifications; each grey dot is one). Dashed line = mean of all specifications;\n",
                          "orange dot = mean of one option; orange line = how far that option pulls away from the overall mean.\n",
                          "η² is the share of the total spread that the option means account for. The table averages this over strata and outcomes." ),
                  format( nrow( y ), big.mark = "," ) )
  cap <- sub( "η² is the share", "η² = between-option variance / total variance: the share", cap )
  ggsave( file.path( OD, "fig_eta2_explainer.png" ), ( p1 | p2 ) + plot_annotation( caption = cap, theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 12, height = 4.6, dpi = 170, bg = "white" )
  fwrite( data.table( specs_curve = nrow( x ), median_r = medr, sign_stab = ss, specs_eta = nrow( y ), eta_est = e_est, eta_win = e_win,
                      est_means = paste( y[ , .( m = round( mean( pr ), 3 ) ), by = est ][ , paste( est, m ) ], collapse = "; " ) ),
          file.path( OD, "explainer_numbers.csv" ) )
  print( fread( file.path( OD, "explainer_numbers.csv" ) ) )
}
## ===========================================================================
## Part E: partial r explainer (% BA) and between-state variance for D9
## ===========================================================================
if ( "E" %in% PARTS ) {
  log_msg( "Part E: partial r explainer and between-state variance" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m[ , `:=`( logP = log( population ), name = paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ) ) ]
  m[ , y := resid( lm( log( n_filer_c3 ) ~ logP ) ) ]                       # filer-count SAMI
  m[ , `:=`( s_pf = resid( lm( flog( pf_assets, population ) ~ logP ) ), s_gov = resid( lm( flog( gov_earn_sl, population ) ~ logP ) ) ) ]
  Z <- c( "race_hhi", "s_pf", "turnout", "s_gov", "poverty_rate", "logP" )   # the other covariates (+ size)
  fz <- function( v ) as.formula( paste( v, "~", paste( Z, collapse = " + " ) ) )
  m[ , `:=`( ex = resid( lm( fz( "pct_ba" ), m ) ), ey = resid( lm( fz( "y" ), m ) ) ) ]
  m[ , `:=`( x_adj = ex + mean( pct_ba ), y_adj = ey + mean( y ) ) ]
  b_biv <- coef( lm( y ~ pct_ba, m ) )[ 2 ]; r_biv <- cor( m$y, m$pct_ba )
  b_par <- coef( lm( ey ~ ex, m ) )[ 2 ]; r_par <- cor( m$ex, m$ey ); r_semi <- cor( m$y, m$ex )
  b_full <- coef( lm( as.formula( paste( "log( n_filer_c3 ) ~ pct_ba +", paste( Z, collapse = " + " ) ) ), m ) )[ "pct_ba" ]
  nums <- data.table( b_bivariate = b_biv, r_bivariate = r_biv, b_partial = b_par, b_full_model = b_full,
                      partial_r = r_par, semipartial_r = r_semi, sd_x = sd( m$pct_ba ), sd_ex = sd( m$ex ), sd_y = sd( m$y ), sd_ey = sd( m$ey ) )
  print( r3( nums ) ); fwrite( nums, file.path( OD, "partial_r_example.csv" ) )
  xl <- range( m$pct_ba ); yl <- range( m$y )
  lab_ids <- unique( c( m[ order( -y ) ][ 1:2, cbsa ], m[ order( y ) ][ 1:2, cbsa ], m[ order( -pct_ba ) ][ 1, cbsa ], m[ order( pct_ba ) ][ 1, cbsa ] ) )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  pa <- ggplot( m, aes( pct_ba, y ) ) +
    geom_point( colour = "#2a78d6", alpha = 0.45, size = 1.4, stroke = 0 ) +
    geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "#eb6834", linewidth = 0.9 ) +
    geom_text_repel( data = m[ cbsa %in% lab_ids ], aes( label = name ), size = 2.7, colour = "grey25", seed = 3, min.segment.length = 0 ) +
    scale_x_continuous( labels = scales::percent_format( accuracy = 1 ) ) + coord_cartesian( xlim = xl, ylim = yl ) +
    labs( title = "A. Bivariate: r", subtitle = sprintf( "B = %.2f per 10 points of %% BA   ·   r = %.2f", b_biv / 10, r_biv ),
          x = "% of adults with a BA or more", y = "Filer-count SAMI (log points)" ) + thm
  pb <- ggplot( m, aes( x_adj, y_adj ) ) +
    geom_point( colour = "#2a78d6", alpha = 0.45, size = 1.4, stroke = 0 ) +
    geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "#eb6834", linewidth = 0.9 ) +
    geom_text_repel( data = m[ cbsa %in% lab_ids ], aes( label = name ), size = 2.7, colour = "grey25", seed = 3, min.segment.length = 0 ) +
    scale_x_continuous( labels = scales::percent_format( accuracy = 1 ) ) + coord_cartesian( xlim = xl, ylim = yl ) +
    labs( title = "B. Net of the other covariates: partial r",
          subtitle = sprintf( "B = %.2f per 10 points   ·   partial r = %.2f   ·   semi-partial r = %.2f", b_par / 10, r_par, r_semi ),
          x = "% BA, residual + mean", y = "Filer-count SAMI, residual + mean" ) + thm
  cap <- paste0( "387 metros, 2022. Panel B removes from both axes what ", DIVS, ", foundation assets, turnout, government earnings, poverty and log population predict\n",
                 "(Frisch-Waugh-Lovell), then adds back each variable's mean so both panels share one scale. Its slope equals the % BA coefficient in the full model.\n",
                 "Partial r = correlation of the two residuals (what the tables report). Semi-partial r = correlation of the raw outcome with the residualized % BA." )
  ggsave( file.path( OD, "fig_partial_r_explainer.png" ), ( pa | pb ) + plot_annotation( caption = cap, theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 11, height = 5.2, dpi = 170, bg = "white" )

  ## D9: share of each covariate's variance that lies between states / divisions (2022 metros)
  DIVISION <- c( "09" = 1, "23" = 1, "25" = 1, "33" = 1, "44" = 1, "50" = 1, "34" = 2, "36" = 2, "42" = 2, "17" = 3, "18" = 3, "26" = 3, "39" = 3, "55" = 3,
    "19" = 4, "20" = 4, "27" = 4, "29" = 4, "31" = 4, "38" = 4, "46" = 4, "10" = 5, "11" = 5, "12" = 5, "13" = 5, "24" = 5, "37" = 5, "45" = 5, "51" = 5, "54" = 5,
    "01" = 6, "21" = 6, "28" = 6, "47" = 6, "05" = 7, "22" = 7, "40" = 7, "48" = 7, "04" = 8, "08" = 8, "16" = 8, "30" = 8, "32" = 8, "35" = 8, "49" = 8, "56" = 8,
    "02" = 9, "06" = 9, "15" = 9, "41" = 9, "53" = 9 )
  m[ , division := DIVISION[ state_fips ] ]
  m[ , s_contrib := resid( lm( flog( contrib_total, population ) ~ logP ) ) ]
  V <- c( y = "Filer-count SAMI (outcome)", pct_ba = "% BA", s_pf = "Foundation assets (SAMI)", race_hhi = DIVC, turnout = "Turnout",
          poverty_rate = "Poverty rate", s_gov = "S&L govt earnings (SAMI)", contrib_share_agi = "Contributions / AGI", dem_margin = "Dem. margin" )
  bs <- rbindlist( lapply( names( V ), function( v ) { z <- m[ is.finite( get( v ) ) ]
    data.table( var = v, label = V[[ v ]], between_state = summary( lm( as.formula( paste( v, "~ factor( state_fips )" ) ), z ) )$r.squared,
                between_division = summary( lm( as.formula( paste( v, "~ factor( division )" ) ), z ) )$r.squared ) } ) )
  print( r3( bs ) ); fwrite( bs, file.path( OD, "d9_between_state_share.csv" ) )
}

## ===========================================================================
## Part F: Fact 1 (scaling and per capita) and D7 (measures by block)
## ===========================================================================
if ( "F" %in% PARTS ) {
  log_msg( "Part F: Fact 1 and D7 figures" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  ## ---- Fact 1: log-log scaling and what per capita does ----------------------
  m <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m[ , `:=`( P = population, n = n_filer_c3, e = exp_c3_nohu ) ]
  fit <- function( v ) coef( lm( log( get( v ) ) ~ log( P ), m[ get( v ) > 0 ] ) )
  bn <- fit( "n" ); be <- fit( "e" )
  pc_slope <- function( v ) coef( lm( log( get( v ) / P ) ~ log( P ), m[ get( v ) > 0 ] ) )[ 2 ]
  ## share of the per-capita spread explained by size
  r2pc <- function( v ) summary( lm( log( get( v ) / P ) ~ log( P ), m[ get( v ) > 0 ] ) )$r.squared
  nums <- data.table( beta_count = bn[ 2 ], beta_exp = be[ 2 ], pc_slope_count = pc_slope( "n" ), pc_slope_exp = pc_slope( "e" ),
                      r2_pc_count = r2pc( "n" ), r2_pc_exp = r2pc( "e" ),
                      ## per-capita ratio of a 2M metro to a 200k metro implied by beta
                      ratio_10x_count = 10^( bn[ 2 ] - 1 ), ratio_10x_exp = 10^( be[ 2 ] - 1 ),
                      r_sami_vs_pc_count = cor( resid( lm( log( n ) ~ log( P ), m ) ), log( m$n / m$P ) ),
                      r_sami_vs_pc_exp = cor( resid( lm( log( e ) ~ log( P ), m[ e > 0 ] ) ), log( m[ e > 0 ]$e / m[ e > 0 ]$P ) ) )
  print( r3( nums ) ); fwrite( nums, file.path( OD, "fact1_numbers.csv" ) )
  mk_ll <- function( v, b, ttl, ylab ) {
    z <- m[ get( v ) > 0 ]; xm <- mean( log( z$P ) ); ym <- mean( log( z[[ v ]] ) )
    ggplot( z, aes( P, get( v ) ) ) + geom_point( colour = "#2a78d6", alpha = 0.45, size = 1.3, stroke = 0 ) +
      geom_abline( intercept = ( ym - xm ) / log( 10 ), slope = 1, linetype = 2, colour = "grey45" ) +
      geom_abline( intercept = ( b[ 1 ] ) / log( 10 ), slope = b[ 2 ], colour = "#eb6834", linewidth = 0.9 ) +
      annotate( "text", x = 6e4, y = Inf, vjust = 1.4, hjust = 0, size = 5.2, fontface = "bold", colour = "#eb6834", label = sprintf( "β = %.2f", b[ 2 ] ) ) +
      annotate( "text", x = 6e4, y = Inf, vjust = 3.6, hjust = 0, size = 3.2, colour = "grey35", label = "dashed: proportional (β = 1)" ) +
      scale_x_log10( labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
      scale_y_log10( labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
      labs( title = ttl, x = "Population (log scale)", y = ylab ) + thm }
  mk_pc <- function( v, s, ttl, ylab, mult ) {
    z <- m[ get( v ) > 0 ]
    ggplot( z, aes( P, get( v ) / P * mult ) ) + geom_point( colour = "#2a78d6", alpha = 0.45, size = 1.3, stroke = 0 ) +
      geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "#eb6834", linewidth = 0.9 ) +
      annotate( "text", x = 6e4, y = Inf, vjust = 1.4, hjust = 0, size = 4.2, fontface = "bold", colour = "#eb6834",
                label = sprintf( "%+.0f%% per capita per\ntenfold population", 100 * ( 10^s - 1 ) ) ) +
      scale_x_log10( labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
      scale_y_log10( labels = scales::label_comma() ) +
      labs( title = ttl, x = "Population (log scale)", y = ylab ) + thm }
  p1 <- mk_ll( "n", bn, "A. Filers scale in proportion to population", "501(c)(3) filers" )
  p2 <- mk_ll( "e", be, "B. Spending scales faster than population", "Expenses excl. hospitals, universities ($)" )
  p3 <- mk_pc( "n", pc_slope( "n" ), "C. So filers per capita are size-neutral", "Filers per 10,000 residents", 1e4 )
  p4 <- mk_pc( "e", pc_slope( "e" ), "D. But spending per capita rises with size", "Expenses per resident ($)", 1 )
  cap <- "387 metros, 2022. Orange: OLS fit. A per-capita measure assumes β = 1; where β > 1, per-capita values rank large metros above small ones for size alone."
  ggsave( file.path( OD, "fig_fact1_scaling.png" ), ( ( p1 | p2 ) / ( p3 | p4 ) ) + plot_annotation( caption = cap, theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 11, height = 8.4, dpi = 170, bg = "white" )

  ## ---- D7: every measure option, by block (counts, cross-section) -------------
  source( "R/lib/40_multiverse.R" ); source( "R/lib/41_composites.R" )
  cmp <- file.path( OUT$spec, "run_principled_cmp_merged" )
  sp <- as.data.table( read_parquet( file.path( cmp, "specs.parquet" ) ) )[ construct == "count" & hu == "incl" & ident == "cs" & drop == "none" ]
  co <- as.data.table( read_parquet( file.path( cmp, "coefs.parquet" ), col_select = c( "spec_id", "term", "stat", "p", "n" ) ) )[ spec_id %in% sp$spec_id ]
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
  d <- merge( co[ term != "logP" ], sp[ , .( spec_id, op ) ], by = "spec_id" )
  d[ , blk := fifelse( op == "default", NA_character_, sub( ":.*", "", op ) ) ]
  defs <- unlist( lapply( names( BLOCKS ), function( b ) setNames( rep( b, length( BLOCKS[[ b ]]$default ) ), BLOCKS[[ b ]]$default ) ) )
  ## a term belongs to the measured block of its spec: default specs -> every default term;
  ## alternative specs -> only the terms of the swapped block
  d[ , term_block := fifelse( term %in% names( defs ), defs[ term ], NA_character_ ) ]
  alt_terms <- function( b, a ) BLOCKS[[ b ]]$alt[[ a ]]
  d[ !is.na( blk ), keep := mapply( function( o, t ) { b <- sub( ":.*", "", o ); a <- sub( "^[^:]+:", "", o ); t %in% alt_terms( b, a ) }, op, term ) ]
  d[ is.na( blk ), keep := !is.na( term_block ) ]
  d <- d[ keep == TRUE ]
  d[ is.na( blk ), blk := term_block ]
  d <- d[ blk %in% c( "diversity", "philanthropy", "political", "government", "need" ) ]
  LBL <- c( race_hhi = "Racial HHI (reversed)", pct_nonwhite = "% non-white", pct_foreign_born = "% foreign-born", gini = "Income Gini",
            age_sd = "Age dispersion", cmp_diversity = "Diversity index", pf_assets = "Foundation assets", n_pf = "No. of foundations",
            contrib_total = "Contributions, total", contrib_per_return = "Contributions per return", contrib_share_agi = "Contributions / AGI",
            cmp_foundations = "Foundation index", cmp_giving = "Giving index", turnout = "Turnout", dem_margin = "Dem. margin",
            cmp_civic = "Civic index", cmp_relcons = "Religious-conservative index", gov_earn_sl = "S&L govt earnings",
            gov_emp_sl = "S&L govt employment", gov_emp_share = "Govt employment share", cmp_government = "Government index",
            poverty_rate = "Poverty rate", pct_ba = "% BA", pcpi = "Per capita income", unemp_rate = "Unemployment", pct_65plus = "% 65+",
            cmp_affluence = "Affluence index", cmp_age = "Age structure index" )
  d[ term == "race_hhi", pr := -pr ]
  BNM <- c( diversity = "Diversity", philanthropy = "Philanthropy", political = "Political culture", government = "Government", need = "Need and affluence" )
  sm <- d[ , { med <- median( pr ); .( specs = .N, med = med, q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
                                      sign_stab = mean( sign( pr ) == sign( med ) ) ) }, by = .( blk, term ) ]
  sm[ , `:=`( label = LBL[ term ], composite = grepl( "^cmp_", term ), is_default = term %in% names( defs ), block = factor( BNM[ blk ], unname( BNM ) ) ) ]
  sm[ , label := factor( label, rev( unique( sm[ order( block, -is_default, composite, -med ), label ] ) ) ) ]
  fwrite( sm, file.path( OD, "d7_measures_by_block.csv" ) ); print( r3( sm[ order( block, -med ) ][ , .( block, label, specs, med, q25, q75, sign_stab ) ] ) )
  pd <- ggplot( sm, aes( y = label ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_segment( aes( x = q25, xend = q75, yend = label, colour = composite ), linewidth = 1.1 ) +
    geom_point( aes( x = med, colour = composite, shape = is_default ), size = 2.8 ) +
    geom_text( aes( x = pmax( q75, med ) + 0.015, label = sprintf( "%d%%", round( 100 * sign_stab ) ) ), hjust = 0, size = 2.8, colour = "grey35" ) +
    facet_grid( block ~ ., scales = "free_y", space = "free_y", switch = "y" ) +
    scale_colour_manual( values = c( `FALSE` = "#2a78d6", `TRUE` = "#eb6834" ), labels = c( "Single measure", "Composite index" ), name = NULL ) +
    scale_shape_manual( values = c( `FALSE` = 16, `TRUE` = 15 ), labels = c( "Alternative", "Default measure" ), name = NULL ) +
    labs( title = "Each measure of a construct, counts cross-section (default model or one swap)",
          subtitle = "Dot = median partial r; bar = interquartile range across specifications; number = share with the median's sign",
          x = "Partial r (racial HHI reversed so that all diversity measures point the same way)", y = NULL ) + thm +
    theme( strip.placement = "outside", strip.text.y.left = element_text( angle = 0, hjust = 1, face = "bold" ), legend.position = "top",
           panel.spacing.y = unit( 6, "pt" ) )
  ggsave( file.path( OD, "fig_d7_measures.png" ), pd, width = 10, height = 8.6, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part G: D1 -- outcome population (filers, grassroots, registered, spending)
## ===========================================================================
if ( "G" %in% PARTS ) {
  log_msg( "Part G: outcome populations" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m[ , `:=`( logP = log( population ), l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ),
             grass = n_bmf_c3pc - n_filer_c3,
             name = paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ) ) ]
  cat( "metros with grassroots <= 0:", m[ grass <= 0, .N ], "\n" )
  OUTC <- c( n_filer_c3 = "Filers", grass = "Grassroots (registered, not filing)", n_bmf_c3pc = "Registered stock (all)", exp_c3_nohu = "Spending excl. hospitals, universities" )
  for ( v in names( OUTC ) ) m[ , ( paste0( "s_", v ) ) := { z <- rep( NA_real_, .N ); ok <- get( v ) > 0; z[ ok ] <- resid( lm( log( get( v )[ ok ] ) ~ logP[ ok ] ) ); z } ]
  sv <- paste0( "s_", names( OUTC ) )
  cm <- cor( m[ , ..sv ], use = "pairwise.complete.obs" ); dimnames( cm ) <- list( OUTC, OUTC )
  print( round( cm, 3 ) ); fwrite( data.table( outcome = OUTC, cm ), file.path( OD, "d1_sami_correlations.csv" ) )
  beta <- sapply( names( OUTC ), function( v ) coef( lm( log( get( v ) ) ~ logP, m[ get( v ) > 0 ] ) )[ 2 ] )
  sdv <- sapply( sv, function( v ) sd( m[[ v ]], na.rm = TRUE ) )
  print( round( rbind( beta = beta, sd_sami = sdv ), 3 ) )
  ## the default cross-section model on each outcome
  CV <- c( race_hhi = DIVC, l_pf = "Foundation assets (log)", turnout = "Turnout", l_gov = "S&L govt earnings (log)", poverty_rate = "Poverty rate", pct_ba = "% BA" )
  res <- rbindlist( lapply( names( OUTC ), function( v ) {
    r <- feols( as.formula( paste( "log(", v, ") ~ logP +", paste( names( CV ), collapse = " + " ) ) ), m[ get( v ) > 0 ], vcov = "hetero" )
    ct <- coeftable( r ); dfr <- degrees_freedom( r, "resid" ); tt <- ct[ , 3 ]
    pr <- function( t ) t / sqrt( t^2 + dfr )
    data.table( outcome = OUTC[[ v ]], term = rownames( ct ), est = ct[ , 1 ], t = tt, p = ct[ , 4 ], r = pr( tt ), lo = pr( tt - 1.96 ), hi = pr( tt + 1.96 ) ) } ) )[ term %in% names( CV ) ]
  res[ , `:=`( label = factor( CV[ term ], rev( unname( CV ) ) ), outcome = factor( outcome, unname( OUTC ) ) ) ]
  fwrite( res, file.path( OD, "d1_outcome_models.csv" ) )
  print( dcast( res, label ~ outcome, value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  ## figure: A scatter filers vs grassroots SAMI; B covariate partial r by outcome
  a <- m[ is.finite( s_n_filer_c3 ) & is.finite( s_grass ) ]
  a[ , gap := s_n_filer_c3 - s_grass ]
  lab <- a[ order( -abs( gap ) ) ][ 1:8, cbsa ]
  pA <- ggplot( a, aes( s_grass, s_n_filer_c3 ) ) + geom_abline( slope = 1, intercept = 0, linetype = 2, colour = "grey55" ) +
    geom_hline( yintercept = 0, colour = "grey80" ) + geom_vline( xintercept = 0, colour = "grey80" ) +
    geom_point( colour = "#2a78d6", alpha = 0.5, size = 1.4, stroke = 0 ) +
    geom_text_repel( data = a[ cbsa %in% lab ], aes( label = name ), size = 2.7, colour = "grey25", seed = 4, min.segment.length = 0 ) +
    annotate( "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, size = 5, fontface = "bold", colour = "#eb6834",
              label = sprintf( "r = %.2f", cor( a$s_n_filer_c3, a$s_grass ) ) ) +
    labs( title = "A. Two populations, two rankings", subtitle = "Dashed line: equal density in both groups",
          x = "Grassroots SAMI (registered, not filing)", y = "Filer SAMI" ) + thm
  cols <- c( "#2a78d6", "#1baf7a", "#8a8f98", "#eb6834" ); names( cols ) <- unname( OUTC )
  pB <- ggplot( res, aes( r, label, colour = outcome ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.7 ), linewidth = 0.8 ) +
    geom_point( position = position_dodge( width = 0.7 ), size = 2.2 ) +
    scale_colour_manual( values = cols, name = NULL, guide = guide_legend( ncol = 2, reverse = FALSE ) ) +
    labs( title = "B. The same model, four outcomes", subtitle = "Partial r with 95% interval; log-log model with log population, 2022",
          x = "Partial r", y = NULL ) + thm + theme( legend.position = "bottom" )
  cap <- "387 metros, 2022. Grassroots = registered 501(c)(3) public charities that do not file a 990/990-EZ above the $50k real floor (postcard filers, small filers, registered churches, dormant registrations)."
  ggsave( file.path( OD, "fig_d1_outcomes.png" ), ( pA | pB ) + plot_layout( widths = c( 1, 1.1 ) ) +
            plot_annotation( caption = cap, theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 12, height = 6, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part H: D2 -- unit of analysis; Fact 4 -- composition of the registered stock
## ===========================================================================
if ( "H" %in% PARTS ) {
  log_msg( "Part H: units of analysis and composition" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  cy <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "county_year.parquet" ) ) )[ year == 2022 ]
  sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "csa" ) ) )
  cy <- merge( cy[ , !c( "cbsa", "cbsa_type" ) ], sp[ , .( geoid, cbsa, cbsa_type, csa ) ], by = "geoid" )
  RC <- c( "n_nhwhite", "n_nhblack", "n_nhaian", "n_nhasian", "n_nhnhpi", "n_nhtwo", "n_hisp" )
  ## aggregate counties to any grouping; rates recomputed from numerators and denominators
  agg <- function( dt, key ) {
    a <- dt[ , c( list( population = sum( population ), n = sum( n_filer_c3 ), e = sum( exp_c3_nohu ), pf = sum( pf_assets ), gov = sum( gov_earn_sl ),
                        ba = sum( edu_ba_num ) / sum( edu_den ), pov = sum( pov_n, na.rm = TRUE ) / sum( pov_universe, na.rm = TRUE ),
                        turnout = sum( turnout * pop_18plus, na.rm = TRUE ) / sum( pop_18plus[ !is.na( turnout ) ] ),
                        race_den = sum( race_den ), counties = .N ), lapply( .SD, sum ) ), by = key, .SDcols = RC ]
    a[ , race_hhi := if ( NEWRUN ) 1 - n_nhwhite / race_den else Reduce( `+`, lapply( RC, function( v ) ( get( v ) / race_den )^2 ) ) ]
    a[] }
  ## county rows themselves, same definitions
  cty <- copy( cy )[ , `:=`( n = n_filer_c3, e = exp_c3_nohu, pf = pf_assets, gov = gov_earn_sl, ba = edu_ba_num / edu_den,
                             pov = pov_n / pov_universe, race_hhi = if ( NEWRUN ) 1 - n_nhwhite / race_den else Reduce( `+`, lapply( RC, function( v ) ( get( v ) / race_den )^2 ) ) ) ]
  units <- list(
    "County (all)" = cty,
    "Metro CBSA" = agg( cy[ cbsa_type == "metro" ], "cbsa" ),
    "Metro + micro CBSA" = agg( cy[ cbsa_type %in% c( "metro", "micro" ) ], "cbsa" ),
    "Micropolitan only" = agg( cy[ cbsa_type == "micro" ], "cbsa" ),
    "CSA or metro (metro areas)" = agg( cy[ cbsa_type == "metro" ][ , grp := fifelse( !is.na( csa ) & csa != "", paste0( "csa", csa ), cbsa ) ], "grp" ) )
  rhs <- "log( population ) + race_hhi + lpf + turnout + lgov + pov + ba"
  CV <- c( race_hhi = DIVC, lpf = "Foundation assets (log)", turnout = "Turnout", lgov = "S&L govt earnings (log)", pov = "Poverty rate", ba = "% BA" )
  fitu <- function( z, u, y, fe = NULL ) {
    zz <- z[ get( y ) > 0 & is.finite( turnout ) & is.finite( pov ) ]
    zz[ , `:=`( lpf = flog( pf, population ), lgov = flog( gov, population ) ) ]     # floor set within each unit's sample
    f <- as.formula( paste( "log(", y, ") ~", rhs, if ( !is.null( fe ) ) paste( "|", fe ) ) )
    r <- feols( f, zz, vcov = if ( is.null( fe ) ) "hetero" else as.formula( paste( "~", fe ) ) )
    ct <- coeftable( r ); dfr <- if ( is.null( fe ) ) degrees_freedom( r, "resid" ) else uniqueN( zz[[ fe ]] ) - nrow( ct ) - 1
    pr <- function( t ) t / sqrt( t^2 + dfr )
    b0 <- coef( lm( as.formula( paste( "log(", y, ") ~ log( population )" ) ), zz ) )[ 2 ]
    data.table( unit = u, outcome = y, n_units = nrow( zz ), zeros = z[ get( y ) <= 0, .N ], beta_bivariate = b0,
                term = rownames( ct ), est = ct[ , 1 ], r = pr( ct[ , 3 ] ), lo = pr( ct[ , 3 ] - 1.96 ), hi = pr( ct[ , 3 ] + 1.96 ) ) }
  res <- rbindlist( c( lapply( names( units ), function( u ) fitu( units[[ u ]], u, "n" ) ),
                       lapply( names( units ), function( u ) fitu( units[[ u ]], u, "e" ) ),
                       list( fitu( cty[ cbsa_type == "metro" ], "Counties within metros (metro FE)", "n", fe = "cbsa" ),
                             fitu( cty[ cbsa_type == "metro" ], "Counties within metros (metro FE)", "e", fe = "cbsa" ) ) ) )
  res[ term == "log(population)", term := "logP" ]
  fwrite( res, file.path( OD, "d2_units.csv" ) )
  ulev <- c( names( units ), "Counties within metros (metro FE)" )
  print( unique( res[ , .( unit, outcome, n_units, zeros, beta_bivariate = round( beta_bivariate, 3 ) ) ] ) )
  print( dcast( res[ term %in% c( names( CV ), "logP" ) ], outcome + term ~ factor( unit, ulev ), value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  pd <- res[ outcome == "n" & term %in% names( CV ) ][ , `:=`( label = factor( CV[ term ], rev( unname( CV ) ) ), unit = factor( unit, ulev ) ) ]
  cols <- c( "#8a8f98", "#2a78d6", "#1baf7a", "#eda100", "#6250d6", "#eb6834" ); names( cols ) <- ulev
  p <- ggplot( pd, aes( r, label, colour = unit ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.75 ), linewidth = 0.8 ) +
    geom_point( position = position_dodge( width = 0.75 ), size = 2.1 ) +
    scale_colour_manual( values = cols, name = NULL, guide = guide_legend( ncol = 2, reverse = TRUE ) ) +
    labs( title = "The same filer-count model at six geographic units, 2022",
          subtitle = "Partial r with 95% interval; log-log model with log population; covariates rebuilt from county counts for every unit",
          x = "Partial r", y = NULL ) + thm + theme( legend.position = "bottom" )
  ggsave( file.path( OD, "fig_d2_units.png" ), p, width = 9.5, height = 6.4, dpi = 170, bg = "white" )

  ## Fact 4: composition of the registered stock over time (387 metros)
  pnl <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ cbsa_type == "metro" ]
  ts <- pnl[ , .( filers = sum( n_filer_c3 ), registered = sum( n_bmf_c3pc ) ), keyby = year ][ , grass := registered - filers ][ , share_filing := filers / registered ]
  fwrite( ts, file.path( OD, "fact4_composition.csv" ) ); print( ts[ year %in% c( 2000, 2005, 2010, 2011, 2015, 2019, 2020, 2022, 2024 ) ] )
  tl <- melt( ts[ , .( year, Filers = filers, Grassroots = grass ) ], id.vars = "year" )
  pa <- ggplot( tl, aes( year, value / 1e3, fill = variable ) ) + geom_area( alpha = 0.85, colour = "white", linewidth = 0.3 ) +
    geom_vline( xintercept = c( 2011, 2020 ), linetype = 3, colour = "grey40" ) +
    annotate( "text", x = c( 2011, 2020 ), y = Inf, vjust = 1.4, hjust = -0.05, size = 2.8, colour = "grey30", label = c( "CORE to SOI", "SOI to e-file" ) ) +
    scale_fill_manual( values = c( Filers = "#2a78d6", Grassroots = "#1baf7a" ), name = NULL ) +
    labs( title = "A. Registered 501(c)(3) public charities in 387 metros", x = NULL, y = "Organizations (thousands)" ) + thm + theme( legend.position = "top" )
  pb <- ggplot( ts, aes( year, share_filing ) ) + geom_line( colour = "#2a78d6", linewidth = 1 ) + geom_point( colour = "#2a78d6", size = 1.4 ) +
    geom_vline( xintercept = c( 2011, 2020 ), linetype = 3, colour = "grey40" ) +
    scale_y_continuous( labels = scales::percent_format( accuracy = 1 ), limits = c( 0, NA ) ) +
    labs( title = "B. Share that file above the $50k floor", x = NULL, y = "Filers / registered" ) + thm
  ggsave( file.path( OD, "fig_fact4_composition.png" ), ( pa | pb ) + plot_layout( widths = c( 1.4, 1 ) ) +
            plot_annotation( caption = paste0( "Grassroots = registered minus filers: postcard and small filers, registered churches, dormant registrations.\n",
                                                "Dotted lines mark source changes in the filer series. The 2011 drop in the registered stock reflects the IRS automatic revocations;\n",
                                                "the 2016-18 BMF snapshots omit some newly ruled organizations." ),
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 11, height = 4.6, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part I: D3 -- identification (cross-section, pooled, within) and within variance
## ===========================================================================
if ( "I" %in% PARTS ) {
  log_msg( "Part I: identification" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  ## A: median partial r by identification, v1 run, df counted in metros, pooled RE excluded
  v1 <- file.path( OUT$spec, "run_principled_v1" )
  specs <- as.data.table( read_parquet( file.path( v1, "specs.parquet" ) ) )[ op == "default" & drop == "none" &
             ( ( construct == "count" & hu == "incl" ) | ( construct == "exp" & hu == "excl" ) ) ]
  co <- as.data.table( read_parquet( file.path( v1, "coefs.parquet" ) ) )
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  ## (panel A reads run v1, whose diversity term is racial HHI; panel B uses the current default measure)
  d <- merge( co, specs, by = "spec_id" )[ !( ident == "pooled" & est == "state_re" ) ]
  d[ , pr := stat / sqrt( stat^2 + pmax( n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) - k - 1, 1 ) ) ]
  CV <- c( race_hhi = "Racial HHI", pf_assets = "Foundation assets", turnout = "Turnout", gov_earn_sl = "S&L govt earnings", poverty_rate = "Poverty rate", pct_ba = "% BA" )
  s <- d[ term %in% names( CV ), { med <- median( pr ); .( med = med, q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ), ss = mean( sign( pr ) == sign( med ) ), specs = .N ) },
          by = .( construct, ident, term ) ]
  s[ , `:=`( label = factor( CV[ term ], rev( unname( CV ) ) ), ident = factor( c( cs = "Cross-section", pooled = "Pooled, year FE", within = "Within metro (metro + year FE)" )[ ident ],
                                                                                c( "Cross-section", "Pooled, year FE", "Within metro (metro + year FE)" ) ),
             construct = factor( c( count = "Counts (filers, registrations)", exp = "Spending excl. hospitals, universities" )[ construct ] ) ) ]
  fwrite( s, file.path( OD, "d3_identification.csv" ) )
  print( dcast( s, construct + label ~ ident, value.var = "med" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  pA <- ggplot( s, aes( med, label, colour = ident ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = q25, xmax = q75 ), position = position_dodge( width = 0.7 ), linewidth = 0.8 ) +
    geom_point( position = position_dodge( width = 0.7 ), size = 2.2 ) + facet_wrap( ~ construct, ncol = 1 ) +
    scale_colour_manual( values = c( "#2a78d6", "#eb6834", "#1baf7a" ), name = NULL, guide = guide_legend( ncol = 1 ) ) +
    labs( title = "A. Pooling changes little; within-metro estimates shrink", subtitle = "Median partial r and interquartile range across specifications",
          x = "Partial r", y = NULL ) + thm + theme( legend.position = "bottom", strip.text = element_text( face = "bold", hjust = 0 ) )
  ## B: share of variance within metros, 2010-2022 (after removing year means)
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ cbsa_type == "metro" & year %in% 2010:2022 ]
  m[ , logP := log( population ) ]
  rs <- function( y, lp ) { z <- rep( NA_real_, length( y ) ); ok <- is.finite( y ); z[ ok ] <- resid( lm( y[ ok ] ~ lp[ ok ] ) ); z }
  m[ , `:=`( s_n = rs( log( n_filer_c3 ), logP ), s_e = rs( log( pmax( exp_c3_nohu, 1 ) ), logP ), s_g = rs( log( n_bmf_c3pc - n_filer_c3 ), logP ),
             s_pf = rs( flog( pf_assets, population ), logP ), s_gov = rs( flog( gov_earn_sl, population ), logP ) ), by = year ]
  m <- m[ order( cbsa, year ) ][ , g_pop := logP - shift( logP, 3 ), by = cbsa ]
  V <- c( s_n = "Filer SAMI", s_g = "Grassroots SAMI", s_e = "Spending SAMI", race_hhi = DIVC, pct_ba = "% BA", poverty_rate = "Poverty rate",
          turnout = "Turnout", s_pf = "Foundation assets (SAMI form)", s_gov = "S&L govt earnings (SAMI form)", pcpi = "Per capita income", g_pop = "3-year population growth" )
  ws <- rbindlist( lapply( names( V ), function( v ) { z <- m[ is.finite( get( v ) ), .( cbsa, year, x = get( v ) ) ]
    z[ , x := x - mean( x ), by = year ]; tot <- var( z$x ); z[ , w := x - mean( x ), by = cbsa ]
    data.table( var = v, label = V[[ v ]], within_share = var( z$w ) / tot, kind = fifelse( v %in% c( "s_n", "s_g", "s_e" ), "Outcome", "Covariate" ) ) } ) )
  print( ws[ , .( label, within_share = round( within_share, 3 ) ) ] ); fwrite( ws, file.path( OD, "d3_within_share.csv" ) )
  ws[ , label := factor( label, ws[ order( within_share ), label ] ) ]
  pB <- ggplot( ws, aes( within_share, label, fill = kind ) ) + geom_col( width = 0.7 ) +
    geom_text( aes( label = scales::percent( within_share, accuracy = 0.1 ) ), hjust = -0.15, size = 3, colour = "grey25" ) +
    scale_fill_manual( values = c( Outcome = "#eb6834", Covariate = "#2a78d6" ), name = NULL ) +
    scale_x_continuous( labels = scales::percent_format( accuracy = 1 ), expand = expansion( mult = c( 0, 0.25 ) ) ) +
    labs( title = "B. Little variation is left within metros", subtitle = "Share of variance within metros, 2010-2022, after removing year means",
          x = "Within-metro share of variance", y = NULL ) + thm + theme( legend.position = "bottom" )
  ggsave( file.path( OD, "fig_d3_identification.png" ), ( pA | pB ) + plot_layout( widths = c( 1.15, 1 ) ) +
            plot_annotation( caption = "Panel A: principled run v1, default model; degrees of freedom counted in metros; pooled random-intercept fits omitted. Panel B: 387 metros; extensive covariates as SAMIs.",
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 12, height = 6.6, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part J: D4 -- dependent-variable form (per capita, log per capita, log-log, SAMI)
## ===========================================================================
if ( "J" %in% PARTS ) {
  log_msg( "Part J: DV form" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m[ , logP := log( population ) ]
  RATES <- c( "race_hhi", "turnout", "poverty_rate", "pct_ba" )
  m <- m[ is.finite( turnout ) & is.finite( poverty_rate ) ]
  m[ , `:=`( pf_l = flog( pf_assets, population ), gov_l = flog( gov_earn_sl, population ),
             pf_lpc = flog( pf_assets, population ) - log( population ), gov_lpc = flog( gov_earn_sl, population ) - log( population ),
             pf_pc = pf_assets / population, gov_pc = gov_earn_sl / population ) ]
  for ( v in c( "pf_l", "gov_l", RATES ) ) m[ , ( paste0( "S_", v ) ) := resid( lm( get( v ) ~ logP ) ) ]
  Y <- c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. hospitals, universities" )
  FORMS <- c( loglog = "Log-log: log Y on log P + covariates", sami = "SAMI on SAMI-form covariates",
              logpcP = "Log per capita + log P on the right", logpc = "Log per capita, no size control",
              pc = "Per capita (linear), no size control" )
  fitf <- function( y, form ) {
    d <- copy( m )[ get( y ) > 0 ]
    d[ , `:=`( lY = log( get( y ) ), lYpc = log( get( y ) / population ), Ypc = get( y ) / population ) ]
    d[ , sY := resid( lm( lY ~ logP ) ) ]
    rr <- paste( RATES, collapse = " + " )
    f <- switch( form,
      loglog = paste( "lY ~ logP + pf_l + gov_l +", rr ),
      sami   = paste( "sY ~ S_pf_l + S_gov_l +", paste( paste0( "S_", RATES ), collapse = " + " ) ),
      logpcP = paste( "lYpc ~ logP + pf_lpc + gov_lpc +", rr ),
      logpc  = paste( "lYpc ~ pf_lpc + gov_lpc +", rr ),
      pc     = paste( "Ypc ~ pf_pc + gov_pc +", rr ) )
    r <- feols( as.formula( f ), d, vcov = "hetero" ); ct <- coeftable( r ); dfr <- degrees_freedom( r, "resid" )
    pr <- function( t ) t / sqrt( t^2 + dfr )
    data.table( outcome = y, form = form, term = sub( "^S_", "", rownames( ct ) ), est = ct[ , 1 ], t = ct[ , 3 ],
                r = pr( ct[ , 3 ] ), lo = pr( ct[ , 3 ] - 1.96 ), hi = pr( ct[ , 3 ] + 1.96 ) ) }
  res <- rbindlist( lapply( names( Y ), function( y ) rbindlist( lapply( names( FORMS ), function( f ) fitf( y, f ) ) ) ) )
  res[ , term := fcase( term %like% "^pf_", "pf", term %like% "^gov_", "gov", default = term ) ]
  CV <- c( race_hhi = DIVC, pf = "Foundation assets", turnout = "Turnout", gov = "S&L govt earnings", poverty_rate = "Poverty rate", pct_ba = "% BA" )
  res <- res[ term %in% names( CV ) ]
  fwrite( res, file.path( OD, "d4_forms.csv" ) )
  print( dcast( res, outcome + term ~ form, value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  ## size correlation of each covariate (as entered in the per capita model)
  sz <- data.table( term = names( CV ), r_logP = c( cor( m$race_hhi, m$logP ), cor( m$pf_lpc, m$logP ), cor( m$turnout, m$logP ),
                                                    cor( m$gov_lpc, m$logP ), cor( m$poverty_rate, m$logP ), cor( m$pct_ba, m$logP ) ) )
  print( sz[ , .( term, r_logP = round( r_logP, 3 ) ) ] ); fwrite( sz, file.path( OD, "d4_size_cor.csv" ) )
  ## scaling and the per capita tilt
  tilt <- rbindlist( lapply( names( Y ), function( y ) { d <- m[ get( y ) > 0 ]
    a <- lm( log( get( y ) / population ) ~ logP, d ); b <- lm( log( get( y ) ) ~ logP, d )
    d[ , `:=`( pc = get( y ) / population, sami = resid( b ) ) ]
    data.table( outcome = y, beta = coef( b )[ 2 ], pc_slope = coef( a )[ 2 ], pc_r2 = summary( a )$r.squared,
                rank_rho = cor( d$pc, d$sami, method = "spearman" ),
                top10_pc_pctile = d[ order( -population ) ][ 1:10, mean( frank( d$pc )[ match( cbsa, d$cbsa ) ] / nrow( d ) ) ],
                top10_sami_pctile = d[ order( -population ) ][ 1:10, mean( frank( d$sami )[ match( cbsa, d$cbsa ) ] / nrow( d ) ) ] ) } ) )
  print( r3( tilt ) ); fwrite( tilt, file.path( OD, "d4_tilt.csv" ) )
  ## the ten largest metros: percentile under per capita vs SAMI, spending
  d <- m[ exp_c3_nohu > 0 ]; d[ , `:=`( pcp = frank( exp_c3_nohu / population ) / .N, sp = frank( resid( lm( log( exp_c3_nohu ) ~ logP ) ) ) / .N ) ]
  big <- d[ order( -population ) ][ 1:10, .( name = sub( ",.*", "", cbsa_title ), pc_pctile = round( 100 * pcp ), sami_pctile = round( 100 * sp ) ) ]
  print( big ); fwrite( big, file.path( OD, "d4_big_metros.csv" ) )
  ## figure
  pd <- res[ form != "sami" ][ , `:=`( label = factor( CV[ term ], rev( unname( CV ) ) ), outcome = factor( Y[ outcome ], Y ),
                                     form = factor( FORMS[ form ], FORMS[ c( "loglog", "logpcP", "logpc", "pc" ) ] ) ) ]
  pA <- ggplot( pd, aes( r, label, colour = form ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.75 ), linewidth = 0.7 ) +
    geom_point( position = position_dodge( width = 0.75 ), size = 2 ) + facet_wrap( ~ outcome, ncol = 1 ) +
    scale_colour_manual( values = c( "#2a78d6", "#1baf7a", "#eda100", "#eb6834" ), name = NULL, guide = guide_legend( ncol = 1 ) ) +
    labs( title = "A. Without log P on the right, per capita forms change the answer", subtitle = "Partial r with 95% interval, default covariates, 387 metros, 2022",
          x = "Partial r", y = NULL ) + thm + theme( legend.position = "bottom", strip.text = element_text( face = "bold", hjust = 0 ) )
  sc <- rbindlist( lapply( names( Y ), function( y ) m[ get( y ) > 0, .( outcome = Y[[ y ]], name = sub( "[-,].*", "", cbsa_title ), population, logP,
                                                                     lpc = log( get( y ) / population ) ) ] ) )
  sc[ , outcome := factor( outcome, Y ) ]
  sc[ , lpc_c := lpc - mean( lpc ), by = outcome ]
  lab <- sc[ , .SD[ order( -population ) ][ 1:6 ], by = outcome ]
  ann <- tilt[ , .( outcome = factor( Y[ outcome ], Y ), txt = sprintf( "slope = %.2f (β − 1)\nR² = %.2f", pc_slope, pc_r2 ) ) ]
  pB <- ggplot( sc, aes( population, lpc_c ) ) + geom_point( colour = "#2a78d6", alpha = 0.45, size = 1.3 ) +
    geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "#eb6834", linewidth = 0.9 ) +
    geom_text_repel( data = lab, aes( label = name ), size = 2.8, colour = "grey20", min.segment.length = 0, seed = 1 ) +
    geom_text( data = ann, aes( x = 6e4, y = Inf, label = txt ), hjust = 0, vjust = 1.3, size = 3.2, inherit.aes = FALSE ) +
    facet_wrap( ~ outcome, ncol = 1 ) + scale_x_log10( labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
    labs( title = "B. Per capita spending rises with metro size", subtitle = "Log per capita value (centered) against population, log scale",
          x = "Population (log scale)", y = "Log per capita, centered" ) + thm + theme( strip.text = element_text( face = "bold", hjust = 0 ) )
  ggsave( file.path( OD, "fig_d4_dv_form.png" ), ( pA | pB ) + plot_layout( widths = c( 1.1, 1 ) ) +
            plot_annotation( caption = "Log-log and SAMI models give identical partial r (FWL); the SAMI arm is not drawn. Per capita models use per capita foundation assets and government earnings.",
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 12, height = 7, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part K: D5 -- zeros: OLS on logs vs Poisson (counties, NCCS groups, 2022)
## ===========================================================================
if ( "K" %in% PARTS ) {
  log_msg( "Part K: zeros and Poisson" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  GL <- c( ALL = "Full sector", ART = "Arts", EDU = "Education", UNI = "Universities", ENV = "Environment/animals",
           HEL = "Health", HOS = "Hospitals", HMS = "Human services", IFA = "International",
           PSB = "Public/societal benefit", REL = "Religion-related", MMB = "Mutual/membership" )
  ## org-years as in R/74 (e-file filers above the real floor), 2022 only
  pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )
  sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "csa", "state_fips" ) ) )
  ef <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ),
          col_select = c( "ein", "year", "subsec_group", "rev", "exp", "ind", "geoid" ) ) )[ year == 2022 & subsec_group == "c3" & !is.na( geoid ) ]
  ef <- merge( ef, pce[ , .( year, floor_nom ) ], by = "year" )[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
  bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "ntee3" ),
                colClasses = list( character = c( "EIN", "ntee3" ) ) )[ !is.na( ntee3 ) & ntee3 != "" ]
  bmf <- unique( bmf[ order( EIN, -year ) ], by = "EIN" )[ , .( ein = EIN, ntee_bmf = toupper( substr( ntee3, 1, 3 ) ) ) ]
  ef <- merge( ef, bmf, by = "ein", all.x = TRUE )
  map_ntee <- function( n ) { a <- substr( n, 1, 1 )
    fcase( n %in% NTEE_UNIV, "UNI", n %in% NTEE_HOSP, "HOS", a == "A", "ART", a == "B", "EDU",
           a %in% c( "C", "D" ), "ENV", a %in% c( "E", "F", "G", "H" ), "HEL", a %in% LETTERS[ 9:16 ], "HMS",
           a == "Q", "IFA", a %in% LETTERS[ 18:23 ], "PSB", a == "X", "REL", a == "Y", "MMB", default = "UNU" ) }
  ef[ , grp := fifelse( is.na( ind ) | ind == "UNU", map_ntee( ntee_bmf ), ind ) ]
  ef <- ef[ geoid %in% sp$geoid ]
  cn <- rbind( ef[ grp %in% names( GL ), .( n = .N ), by = .( geoid, grp ) ], ef[ , .( grp = "ALL", n = .N ), by = geoid ] )
  cn <- CJ( geoid = sp$geoid, grp = names( GL ) )[ cn, on = .( geoid, grp ), n := i.n ][ is.na( n ), n := 0L ]
  ## county covariates (same definitions as Part H)
  cy <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "county_year.parquet" ) ) )[ year == 2022 ]
  RC <- c( "n_nhwhite", "n_nhblack", "n_nhaian", "n_nhasian", "n_nhnhpi", "n_nhtwo", "n_hisp" )
  cv <- cy[ , .( geoid, population, logP = log( population ), race_hhi = if ( NEWRUN ) 1 - n_nhwhite / race_den else Reduce( `+`, lapply( RC, function( v ) ( get( v ) / race_den )^2 ) ),
                 l_pf = flog( pf_assets, population ), turnout, l_gov = flog( gov_earn_sl, population ), pov = pov_n / pov_universe, ba = edu_ba_num / edu_den ) ]
  CVN <- c( "race_hhi", "l_pf", "turnout", "l_gov", "pov", "ba" )
  cv <- cv[ complete.cases( cv ) & population > 0 ]
  ## covariates in SAMI form (residualized on log P), then standardized, so the
  ## log P coefficient is the scaling exponent and comparable to 1
  for ( v in CVN ) cv[ , ( v ) := resid( lm( get( v ) ~ logP ) ) ][ , ( v ) := get( v ) / sd( get( v ) ) ]
  x <- merge( cn, cv, by = "geoid" )
  rhs <- paste( "logP +", paste( CVN, collapse = " + " ) )
  EST <- c( ols = "OLS on log(n), zeros dropped", ols1 = "OLS on log(n + 1)", pois = "Poisson (PML), zeros kept" )
  fitk <- function( g, e ) { z <- x[ grp == g ]
    r <- switch( e,
      ols  = feols( as.formula( paste( "log( n ) ~", rhs ) ), z[ n > 0 ], vcov = "hetero" ),
      ols1 = feols( as.formula( paste( "log( n + 1 ) ~", rhs ) ), z, vcov = "hetero" ),
      pois = fepois( as.formula( paste( "n ~", rhs ) ), z, vcov = "hetero" ) )
    ct <- coeftable( r )
    data.table( grp = g, label = GL[[ g ]], est = e, term = rownames( ct ), b = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ],
                n_used = nobs( r ), counties = nrow( z ), zero_share = mean( z$n == 0 ) ) }
  res <- rbindlist( lapply( names( GL ), function( g ) rbindlist( lapply( names( EST ), function( e ) fitk( g, e ) ) ) ) )
  fwrite( res, file.path( OD, "d5_county_models.csv" ) )
  print( dcast( res[ term == "logP" ], label + zero_share ~ est, value.var = "b" )[ order( zero_share ) ][ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  print( dcast( res[ grp %in% c( "ALL", "HOS", "MMB", "UNI", "IFA" ) & term != "(Intercept)" ], label + term ~ est, value.var = "b" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  ## sign disagreements between OLS (zeros dropped) and Poisson among significant covariate effects
  w <- dcast( res[ !term %in% c( "(Intercept)", "logP" ) ], grp + term ~ est, value.var = c( "b", "p" ) )
  dis <- w[ , .( flips = sum( sign( b_ols ) != sign( b_pois ) & ( p_ols < .05 | p_pois < .05 ) ),
                 sig_pois_only = sum( p_pois < .05 & p_ols >= .05 ), sig_ols_only = sum( p_ols < .05 & p_pois >= .05 ),
                 mean_abs_gap = mean( abs( b_ols - b_pois ) ), mean_abs_pois = mean( abs( b_pois ) ) ), by = grp ]
  print( r3( merge( dis, unique( res[ , .( grp, zero_share ) ] ), by = "grp" )[ order( zero_share ) ] ) )
  fwrite( dis, file.path( OD, "d5_disagreement.csv" ) )
  ## metro level, no zeros in the full sector: OLS vs Poisson weighting
  mm <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" & is.finite( turnout ) & is.finite( poverty_rate ) ]
  mm[ , `:=`( logP = log( population ), l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ), pov = poverty_rate, ba = pct_ba ) ]
  for ( v in CVN ) mm[ , ( v ) := resid( lm( get( v ) ~ logP ) ) ][ , ( v ) := get( v ) / sd( get( v ) ) ]
  mres <- rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y ) {
    o <- coeftable( feols( as.formula( paste( "log(", y, ") ~", rhs ) ), mm[ get( y ) > 0 ], vcov = "hetero" ) )
    p <- coeftable( fepois( as.formula( paste( y, "~", rhs ) ), mm, vcov = "hetero" ) )
    q <- coeftable( fepois( as.formula( paste( y, "~", rhs ) ), mm[ population < quantile( population, .95 ) ], vcov = "hetero" ) )
    data.table( outcome = y, term = rownames( o ), ols = o[ , 1 ], ols_se = o[ , 2 ], pois = p[ , 1 ], pois_se = p[ , 2 ], pois_no_top5 = q[ , 1 ] ) } ) )
  print( r3( mres ) ); fwrite( mres, file.path( OD, "d5_metro_ols_pois.csv" ) )
  ## figure A: OLS - Poisson gap in B against zero share, all units, counts 2022
  B <- fread( file.path( BUILD, "data", "scaling_agg", "scaling_B_by_unit.csv" ) )[ year == 2022 & measure == "count" & unit != "county_metro" & grp != "UNU" ]
  B[ , `:=`( zero_share = zeros / units, gap = B_ols - B_pois ) ]
  ul <- c( county = "Counties", cbsa_metro = "Metro CBSAs", cbsa_all = "Metro + micro CBSAs", csa = "CSAs", metro_area = "Metro areas" )
  B[ , unit := factor( ul[ unit ], ul ) ]
  B[ , lab := fifelse( unit == "Counties" & zero_share > 0.2, paste( sub( "/.*", "", grp_label ), "(counties)" ), NA_character_ ) ]
  fwrite( B, file.path( OD, "d5_gap_vs_zeros.csv" ) )
  print( B[ , .( r = cor( zero_share, gap ) ) ] )
  pA <- ggplot( B, aes( zero_share, gap, colour = unit ) ) + geom_hline( yintercept = 0, colour = "grey60" ) +
    geom_smooth( aes( group = 1 ), method = "lm", formula = y ~ x, se = FALSE, colour = "grey45", linewidth = 0.6, linetype = 2 ) +
    geom_point( size = 2.2, alpha = 0.85 ) +
    geom_text_repel( aes( label = lab ), size = 2.7, colour = "grey20", na.rm = TRUE, min.segment.length = 0, seed = 2, max.overlaps = 30 ) +
    scale_x_continuous( labels = scales::percent_format( accuracy = 1 ) ) +
    scale_colour_manual( values = c( "#eb6834", "#2a78d6", "#1baf7a", "#6250d6", "#eda100" ), name = NULL ) +
    labs( title = "A. Dropping zeros flattens the scaling slope", subtitle = "β from OLS (zeros dropped) minus β from Poisson
Filer counts, 12 groups × 5 geographic units, 2022",
          x = "Share of units with no filer in the group", y = "β OLS − β Poisson" ) + thm + theme( legend.position = "bottom" )
  pdk <- res[ term == "logP" ][ , `:=`( est = factor( EST[ est ], EST ) ) ]
  ord <- unique( pdk[ , .( label, zero_share ) ] )[ order( zero_share ) ]
  pdk[ , lab := factor( sprintf( "%s (%.0f%% zero)", label, 100 * zero_share ), sprintf( "%s (%.0f%% zero)", ord$label, 100 * ord$zero_share ) ) ]
  pB <- ggplot( pdk, aes( b, lab, colour = est ) ) + geom_vline( xintercept = 1, colour = "grey60" ) +
    geom_linerange( aes( xmin = b - 1.96 * se, xmax = b + 1.96 * se ), position = position_dodge( width = 0.6 ), linewidth = 0.7 ) +
    geom_point( position = position_dodge( width = 0.6 ), size = 2 ) +
    scale_colour_manual( values = c( "#eb6834", "#eda100", "#2a78d6" ), name = NULL, guide = guide_legend( ncol = 1 ) ) +
    labs( title = "B. Only Poisson keeps β stable as zeros rise", subtitle = "β on log population with default covariates (SAMI form)
3,100+ counties, 2022",
          x = "Scaling exponent β", y = NULL ) + thm + theme( legend.position = "bottom" )
  ggsave( file.path( OD, "fig_d5_zeros.png" ), ( pA | pB ) + plot_layout( widths = c( 1.1, 1 ) ) +
            plot_annotation( caption = "501(c)(3) e-filers above the $50k real floor; NCCS groups. Panel A from R/74 (bivariate β); panel B adds the six default covariates.",
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 12.5, height = 6.6, dpi = 170, bg = "white" )
}

## shared helper for L and M: cross-section partial r from the v4 run
cs_v4 <- function( keep_ops ) {
  v4 <- file.path( OUT$spec, RUN )
  s <- as.data.table( read_parquet( file.path( v4, "specs.parquet" ) ) )[ op %in% keep_ops & drop == "none" &
         outcome %in% c( "n_filer_c3", "exp_c3_nohu" ) & ident %in% c( "cs", "pooled" ) & !( ident == "pooled" & est == "state_re" ) ]
  co <- as.data.table( read_parquet( file.path( v4, "coefs.parquet" ) ) )[ spec_id %in% s$spec_id ]
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
  co <- merge( co, s, by = "spec_id" )
  co[ , pr := stat / sqrt( stat^2 + pmax( n / fifelse( ident == "pooled", 8, 1 ) - k - 1, 1 ) ) ]
  if ( NEWRUN ) co[ term == DIVT, term := "race_hhi" ]   # v5: % nonwhite under the old key, labelled with DIVC
  co[] }

## ===========================================================================
## Part L: D6 -- predictor form for extensive covariates (raw, per capita, log, SAMI)
## ===========================================================================
if ( "L" %in% PARTS ) {
  log_msg( "Part L: predictor form" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  d <- cs_v4( "default" )[ ident == "cs" & !dv_form %like% "^pois" ]
  CV <- c( pf_assets = "Foundation assets", gov_earn_sl = "S&L govt earnings", pct_ba = "% BA", turnout = "Turnout",
           race_hhi = DIVC, poverty_rate = "Poverty rate" )
  PF <- c( raw = "Raw dollars", pc = "Per capita", log = "Logged total", sami = "SAMI form (log, residualized on log P)" )
  s <- d[ term %in% names( CV ), { md <- median( pr ); .( med = md, q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ), pos = mean( pr > 0 ), specs = .N ) },
          by = .( outcome, rhs_logP, pred_form, term ) ]
  fwrite( s, file.path( OD, "d6_pred_form.csv" ) )
  print( dcast( s, outcome + term ~ pred_form + rhs_logP, value.var = "med" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  ## eta^2 of predictor form within the other forks, per covariate
  eta <- function( v, g ) { m <- tapply( v, g, mean ); n <- tapply( v, g, length ); sum( n * ( m - mean( v ) )^2 ) / sum( ( v - mean( v ) )^2 ) }
  e <- d[ term %in% names( CV ), .( e = eta( pr, pred_form ) ), by = .( outcome, term ) ][
    , .( eta2 = mean( e, na.rm = TRUE ) ), by = .( outcome, term ) ]
  print( r3( dcast( e, term ~ outcome, value.var = "eta2" ) ) ); fwrite( e, file.path( OD, "d6_eta2.csv" ) )
  ## mechanism: correlation of each form with log P, and concentration of raw dollars, 2022 metros
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m[ , logP := log( population ) ]
  mech <- rbindlist( lapply( c( "pf_assets", "gov_earn_sl" ), function( v ) { x <- m[[ v ]]
    f <- list( raw = x, pc = x / m$population, log = flog( x, m$population ), sami = resid( lm( flog( x, m$population ) ~ m$logP ) ) )
    data.table( term = v, pred_form = names( f ), r_logP = sapply( f, function( z ) cor( z, m$logP ) ),
                skew = sapply( f, function( z ) mean( ( z - mean( z ) )^3 ) / sd( z )^3 ),
                top5_share = sapply( f, function( z ) if ( all( z >= 0 ) ) sum( sort( z, TRUE )[ 1:5 ] ) / sum( z ) else NA_real_ ) ) } ) )
  print( r3( mech ) ); fwrite( mech, file.path( OD, "d6_mechanism.csv" ) )
  ## figure A: median partial r by predictor form, with / without log P
  pd <- s[ term %in% c( "pf_assets", "gov_earn_sl", "pct_ba", "turnout" ) ]
  pd[ , `:=`( label = factor( CV[ term ], rev( CV[ c( "pf_assets", "gov_earn_sl", "pct_ba", "turnout" ) ] ) ),
              pred_form = factor( PF[ pred_form ], PF ),
              panel = factor( paste0( c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending" )[ outcome ], " | ",
                                      fifelse( rhs_logP, "log P on the right", "no log P on the right" ) ),
                              c( "Filer counts | log P on the right", "Filer counts | no log P on the right",
                                 "Spending | log P on the right", "Spending | no log P on the right" ) ) ) ]
  pA <- ggplot( pd, aes( med, label, colour = pred_form ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = q25, xmax = q75 ), position = position_dodge( width = 0.7 ), linewidth = 0.8 ) +
    geom_point( position = position_dodge( width = 0.7 ), size = 2.1 ) + facet_wrap( ~ panel, ncol = 2 ) +
    scale_colour_manual( values = c( "#eb6834", "#eda100", "#6250d6", "#2a78d6" ), name = NULL, guide = guide_legend( nrow = 2 ) ) +
    labs( title = "A. How extensive covariates enter changes their own result and their neighbors'",
          subtitle = paste0( "Median partial r and interquartile range across specifications, cross-section, ", RUNL ),
          x = "Partial r", y = NULL ) + thm + theme( legend.position = "bottom", strip.text = element_text( face = "bold", hjust = 0 ) )
  mb <- copy( mech )[ , `:=`( label = c( pf_assets = "Foundation assets", gov_earn_sl = "S&L govt earnings" )[ term ], pred_form = factor( PF[ pred_form ], rev( PF ) ) ) ]
  pB <- ggplot( mb, aes( r_logP, pred_form, fill = pred_form ) ) + geom_col( width = 0.65, show.legend = FALSE ) +
    geom_text( aes( label = sprintf( "%.2f", r_logP ), hjust = ifelse( r_logP >= 0, -0.15, 1.15 ) ), size = 3.1, colour = "grey25" ) +
    facet_wrap( ~ label, ncol = 1 ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    scale_fill_manual( values = rev( c( "#eb6834", "#eda100", "#6250d6", "#2a78d6" ) ) ) +
    scale_x_continuous( limits = c( -0.35, 1.08 ), breaks = seq( -0.25, 1, 0.25 ) ) +
    labs( title = "B. Raw and logged totals are size measures", subtitle = "Correlation with log population, 387 metros, 2022",
          x = "r with log P", y = NULL ) + thm + theme( strip.text = element_text( face = "bold", hjust = 0 ) )
  ggsave( file.path( OD, "fig_d6_pred_form.png" ), ( pA | pB ) + plot_layout( widths = c( 1.6, 1 ) ) +
            plot_annotation( caption = paste0( "Panel A: ", RUNL, " run, cross-section, default covariate set, log-scale and per capita outcomes (Poisson excluded); filers incl. hospitals and universities, spending excl." ),
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 13, height = 7.2, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part M: D8 -- historical controls: pre-1970 endowment and nonprofit market share
## ===========================================================================
if ( "M" %in% PARTS ) {
  log_msg( "Part M: historical controls" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  CV <- c( race_hhi = DIVC, pf = "Foundation assets", turnout = "Turnout", gov = "S&L govt earnings", poverty_rate = "Poverty rate", pct_ba = "% BA",
           endow = "Pre-1970 endowment", share = "Nonprofit market share" )
  ## direct models: 2022 cross-section, SAMI form; share = mean BLS share 2013-2022
  pnl <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ cbsa_type == "metro" ]
  sh <- pnl[ year %in% 2013:2022 & !( bls_delin_flag %in% TRUE ) & bls_pct_npo > 0, .( share = mean( bls_pct_npo ), yrs = .N ), by = cbsa ][ yrs >= 5 ]
  en <- fread( file.path( OUT$cov, "cov_endowment.csv" ), select = c( "cbsa", "hist_pre1970" ), colClasses = list( character = "cbsa" ) )
  m <- merge( merge( pnl[ year == 2022 ], sh, by = "cbsa" ), en, by = "cbsa" )
  m <- m[ is.finite( hist_pre1970 ) & is.finite( turnout ) & is.finite( poverty_rate ) ]
  m[ , logP := log( population ) ]
  rz <- function( x ) { z <- resid( lm( x ~ m$logP ) ); z / sd( z ) }
  m[ , `:=`( race_hhi = rz( race_hhi ), pf = rz( flog( pf_assets, population ) ), turnout = rz( turnout ), gov = rz( flog( gov_earn_sl, population ) ),
             poverty_rate = rz( poverty_rate ), pct_ba = rz( pct_ba ), endow = rz( flog( hist_pre1970, population ) ), share = rz( log( share ) ),
             s_n = resid( lm( log( n_filer_c3 ) ~ logP ) ), s_e = resid( lm( log( exp_c3_nohu ) ~ logP ) ) ) ]
  ARMS <- c( base = "Default model", endow = "+ pre-1970 endowment", share = "+ market share", both = "+ both" )
  base <- c( "race_hhi", "pf", "turnout", "gov", "poverty_rate", "pct_ba" )
  fitm <- function( y, arm ) {
    xs <- c( base, if ( arm %in% c( "endow", "both" ) ) "endow", if ( arm %in% c( "share", "both" ) ) "share" )
    r <- feols( as.formula( paste( y, "~", paste( xs, collapse = " + " ) ) ), m, vcov = "hetero" )
    ct <- coeftable( r )[ -1, , drop = FALSE ]; dfr <- degrees_freedom( r, "resid" ); pr <- function( t ) t / sqrt( t^2 + dfr )
    data.table( outcome = y, arm = arm, term = rownames( ct ), b = ct[ , 1 ], r = pr( ct[ , 3 ] ), lo = pr( ct[ , 3 ] - 1.96 ), hi = pr( ct[ , 3 ] + 1.96 ),
                r2 = r2( r, "r2" ), n = nobs( r ) ) }
  res <- rbindlist( lapply( c( "s_n", "s_e" ), function( y ) rbindlist( lapply( names( ARMS ), function( a ) fitm( y, a ) ) ) ) )
  fwrite( res, file.path( OD, "d8_models.csv" ) )
  print( dcast( res, outcome + term ~ arm, value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  print( unique( res[ , .( outcome, arm, r2 = round( r2, 3 ), n ) ] ) )
  print( round( cor( m[ , .( endow, share, pf, gov, pct_ba, turnout, race_hhi, poverty_rate ) ] )[ 1:2, ], 2 ) )
  ## multiverse: matched default vs +endowment specs (cs and pooled), change in partial r
  d <- cs_v4( c( "default", "history:pre1970" ) )
  key <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year", "outlier", "term" )
  w <- merge( d[ op == "default", c( key, "pr" ), with = FALSE ], d[ op == "history:pre1970", c( key, "pr" ), with = FALSE ], by = key, suffixes = c( "_def", "_end" ) )
  dl <- w[ , .( med_def = median( pr_def ), med_end = median( pr_end ), d_med = median( pr_end - pr_def ), d_q25 = quantile( pr_end - pr_def, .25 ),
                d_q75 = quantile( pr_end - pr_def, .75 ), pairs = .N ), by = .( outcome, term ) ]
  print( r3( dl ) ); fwrite( dl, file.path( OD, "d8_multiverse_delta.csv" ) )
  own <- d[ op == "history:pre1970" & term == "hist_pre1970", .( med = median( pr ), pos = mean( pr > 0 ), sig_pos = mean( pr > 0 & p < .05 ), specs = .N ), by = .( outcome, ident ) ]
  print( r3( own ) ); fwrite( own, file.path( OD, "d8_endowment_own.csv" ) )
  ## figure
  OUTL <- c( s_n = "Filer counts", s_e = "Spending excl. hospitals, universities" )
  pd <- res[ , `:=`( label = factor( CV[ term ], rev( unname( CV ) ) ), arm = factor( ARMS[ arm ], ARMS ), outcome = factor( OUTL[ outcome ], OUTL ) ) ]
  pA <- ggplot( pd, aes( r, label, colour = arm ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.75 ), linewidth = 0.7 ) +
    geom_point( position = position_dodge( width = 0.75 ), size = 2 ) + facet_wrap( ~ outcome, ncol = 2 ) +
    scale_colour_manual( values = c( "#8a8f98", "#2a78d6", "#eb6834", "#6250d6" ), name = NULL ) +
    labs( title = "A. Both history measures absorb philanthropy and government; share also absorbs turnout",
          subtitle = sprintf( "Partial r with 95%% interval, SAMI form, %d metros, 2022 (share = mean 2013-2022)", nrow( m ) ),
          x = "Partial r", y = NULL ) + thm + theme( legend.position = "bottom", strip.text = element_text( face = "bold", hjust = 0 ) )
  pb <- dl[ term %in% c( "race_hhi", "pf_assets", "turnout", "gov_earn_sl", "poverty_rate", "pct_ba" ) ]
  CV2 <- c( race_hhi = DIVC, pf_assets = "Foundation assets", turnout = "Turnout", gov_earn_sl = "S&L govt earnings", poverty_rate = "Poverty rate", pct_ba = "% BA" )
  pb[ , `:=`( label = factor( CV2[ term ], rev( unname( CV2 ) ) ), outcome = factor( c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. hospitals, universities" )[ outcome ],
                                                                               c( "Filer counts", "Spending excl. hospitals, universities" ) ) ) ]
  pB <- ggplot( pb, aes( d_med, label, colour = outcome ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = d_q25, xmax = d_q75 ), position = position_dodge( width = 0.55 ), linewidth = 0.8 ) +
    geom_point( position = position_dodge( width = 0.55 ), size = 2.2 ) +
    scale_colour_manual( values = c( "#2a78d6", "#eb6834" ), name = NULL, guide = guide_legend( ncol = 1 ) ) +
    labs( title = "B. Adding the endowment across the multiverse", subtitle = "Change in partial r, matched specifications (cross-section and pooled)",
          x = "Change in partial r (with endowment − without)", y = NULL ) + thm + theme( legend.position = "bottom" )
  ggsave( file.path( OD, "fig_d8_history.png" ), ( pA | pB ) + plot_layout( widths = c( 1.7, 1 ) ) +
            plot_annotation( caption = "Covariates residualized on log P and standardized. Endowment: 501(c)(3) public charities in the 2000 BMF ruled before 1970; group-exemption artifacts dropped. Market share: BLS 501(c)(3) share of private employment.",
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 13.5, height = 6.8, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part N: D10 -- money measure (expenses / revenue, gross / net of pass-through)
##         and hospital / university exclusion, 2022 metros
## ===========================================================================
if ( "N" %in% PARTS ) {
  log_msg( "Part N: money measure and exclusions" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m <- m[ is.finite( turnout ) & is.finite( poverty_rate ) ]
  m[ , logP := log( population ) ]
  OUTS <- c( exp_c3 = "Expenses", exp_c3_net = "Expenses, net", rev_c3 = "Revenue", rev_c3_net = "Revenue, net",
             exp_c3_nohu = "Expenses", exp_c3_nohu_net = "Expenses, net", rev_c3_nohu = "Revenue", rev_c3_nohu_net = "Revenue, net",
             n_filer_c3 = "Filer counts", n_filer_c3_nohu = "Filer counts" )
  HU <- function( o ) fifelse( o %like% "nohu", "Excl. hospitals, universities", "All 501(c)(3)" )
  rz <- function( x ) { z <- resid( lm( x ~ m$logP ) ); z / sd( z ) }
  X <- m[ , .( race_hhi = rz( race_hhi ), pf = rz( flog( pf_assets, population ) ), turnout = rz( turnout ), gov = rz( flog( gov_earn_sl, population ) ),
               poverty_rate = rz( poverty_rate ), pct_ba = rz( pct_ba ) ) ]
  CV <- c( race_hhi = DIVC, pf = "Foundation assets", turnout = "Turnout", gov = "S&L govt earnings", poverty_rate = "Poverty rate", pct_ba = "% BA" )
  S <- list(); B <- list()
  res <- rbindlist( lapply( names( OUTS ), function( o ) {
    ok <- m[[ o ]] > 0
    b <- lm( log( m[[ o ]][ ok ] ) ~ m$logP[ ok ] )
    s <- rep( NA_real_, nrow( m ) ); s[ ok ] <- resid( b ); S[[ o ]] <<- s
    B[[ o ]] <<- data.table( outcome = o, beta = coef( b )[ 2 ], se = sqrt( diag( vcov( b ) ) )[ 2 ], sd_sami = sd( s, na.rm = TRUE ), total = sum( m[[ o ]] ) )
    dd <- cbind( data.table( s = s ), X )[ ok ]
    r <- feols( s ~ race_hhi + pf + turnout + gov + poverty_rate + pct_ba, dd, vcov = "hetero" )
    ct <- coeftable( r )[ -1, ]; dfr <- degrees_freedom( r, "resid" ); pr <- function( t ) t / sqrt( t^2 + dfr )
    data.table( outcome = o, measure = OUTS[[ o ]], hu = HU( o ), term = rownames( ct ), b = ct[ , 1 ], r = pr( ct[ , 3 ] ),
                lo = pr( ct[ , 3 ] - 1.96 ), hi = pr( ct[ , 3 ] + 1.96 ), r2 = r2( r, "r2" ) ) } ) )
  fwrite( res, file.path( OD, "d10_models.csv" ) )
  bt <- rbindlist( B ); print( r3( bt ) ); fwrite( bt, file.path( OD, "d10_beta.csv" ) )
  print( dcast( res, term ~ outcome, value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  ## SAMI correlations among measures
  SM <- as.data.table( S ); cm <- cor( SM, use = "pairwise.complete.obs" )
  pairs <- data.table( a = c( "exp_c3", "exp_c3_nohu", "exp_c3_nohu", "exp_c3_nohu", "n_filer_c3", "exp_c3_nohu" ),
                       b = c( "exp_c3_nohu", "exp_c3_nohu_net", "rev_c3_nohu", "rev_c3_nohu_net", "n_filer_c3_nohu", "n_filer_c3_nohu" ) )
  pairs[ , r := mapply( function( x, y ) cm[ x, y ], a, b ) ]; print( r3( pairs ) ); fwrite( pairs, file.path( OD, "d10_sami_cor.csv" ) )
  ## hospital and university share of spending, and the metros that move most
  m[ , `:=`( hu_share = 1 - exp_c3_nohu / exp_c3, s_all = S$exp_c3, s_nohu = S$exp_c3_nohu,
             name = sub( "[-,/].*", "", cbsa_title ) ) ]
  m[ , `:=`( p_all = frank( s_all ) / .N, p_nohu = frank( s_nohu ) / .N ) ]
  hs <- m[ , .( national = 1 - sum( exp_c3_nohu ) / sum( exp_c3 ), median = median( hu_share ), p10 = quantile( hu_share, .1 ), p90 = quantile( hu_share, .9 ),
                net_cut_nohu = 1 - sum( exp_c3_nohu_net ) / sum( exp_c3_nohu ), net_cut_all = 1 - sum( exp_c3_net ) / sum( exp_c3 ) ) ]
  print( r3( hs ) ); fwrite( hs, file.path( OD, "d10_hu_share.csv" ) )
  mv <- m[ , .( name, population, hu_share = round( hu_share, 2 ), pctile_all = round( 100 * p_all ), pctile_nohu = round( 100 * p_nohu ), shift = round( 100 * ( p_nohu - p_all ) ) ) ]
  print( mv[ order( shift ) ][ c( 1:8, ( .N - 7 ):.N ) ] ); fwrite( mv, file.path( OD, "d10_movers.csv" ) )
  print( m[ , .( r_hu_share_ba = cor( hu_share, pct_ba ), r_hu_share_sall = cor( hu_share, s_all ), r_hu_share_snohu = cor( hu_share, s_nohu ) ) ] )
  ## multiverse: median partial r by hu and by money measure, cross-section and pooled
  d <- cs_v4( "default" )[ construct == "exp" ]
  d <- rbind( d, { z <- as.data.table( read_parquet( file.path( OUT$spec, RUN, "specs.parquet" ) ) )[ op == "default" & drop == "none" & construct == "exp" &
                     ident %in% c( "cs", "pooled" ) & !outcome %in% c( "exp_c3_nohu" ) & !( ident == "pooled" & est == "state_re" ) ]
                   co <- as.data.table( read_parquet( file.path( OUT$spec, RUN, "coefs.parquet" ) ) )[ spec_id %in% z$spec_id ]
                   co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" ); co <- merge( co, z, by = "spec_id" )
                   co[ , pr := stat / sqrt( stat^2 + pmax( n / fifelse( ident == "pooled", 8, 1 ) - k - 1, 1 ) ) ]; co } )
  if ( NEWRUN ) d[ term == DIVT, term := "race_hhi" ]   # v5: % nonwhite is the default diversity term; keep the key
  mv2 <- d[ term %in% c( "race_hhi", "pf_assets", "turnout", "gov_earn_sl", "poverty_rate", "pct_ba" ),
            .( med = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ), specs = .N ), by = .( outcome, term ) ]
  print( r3( dcast( mv2, term ~ outcome, value.var = "med" ) ) ); fwrite( mv2, file.path( OD, "d10_multiverse.csv" ) )
  ## figure A: partial r, four expense measures plus revenue
  keep <- c( "exp_c3", "exp_c3_nohu", "exp_c3_nohu_net", "rev_c3_nohu" )
  LAB <- c( exp_c3 = "Expenses, all 501(c)(3)", exp_c3_nohu = "Expenses excl. hospitals, universities",
            exp_c3_nohu_net = "… and net of pass-through grants", rev_c3_nohu = "Revenue excl. hospitals, universities" )
  pd <- res[ outcome %in% keep ][ , `:=`( label = factor( CV[ term ], rev( unname( CV ) ) ), outcome = factor( LAB[ outcome ], LAB ) ) ]
  pA <- ggplot( pd, aes( r, label, colour = outcome ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.75 ), linewidth = 0.7 ) +
    geom_point( position = position_dodge( width = 0.75 ), size = 2.1 ) +
    scale_colour_manual( values = c( "#eb6834", "#2a78d6", "#1baf7a", "#6250d6" ), name = NULL, guide = guide_legend( ncol = 1 ) ) +
    labs( title = "A. Excluding hospitals and universities matters;
the money measure barely does",
          subtitle = sprintf( "Partial r with 95%% interval, SAMI form, %d metros, 2022", nrow( m ) ), x = "Partial r", y = NULL ) +
    thm + theme( legend.position = "bottom" )
  lab <- rbind( m[ order( p_nohu - p_all ) ][ 1:8 ], m[ order( -( p_nohu - p_all ) ) ][ c( 1, 3, 5, 7 ) ] )
  pB <- ggplot( m, aes( 100 * p_all, 100 * p_nohu ) ) + geom_abline( slope = 1, intercept = 0, colour = "grey60" ) +
    geom_point( aes( colour = hu_share ), size = 1.7, alpha = 0.85 ) +
    geom_text_repel( data = lab, aes( label = name ), size = 2.8, colour = "grey15", min.segment.length = 0, seed = 3, max.overlaps = 30 ) +
    scale_colour_gradient( low = "#c9dcf3", high = "#c2410c", labels = scales::percent_format( accuracy = 1 ), name = "Hospital and\nuniversity share" ) +
    labs( title = "B. Where hospitals and universities dominate,
rankings move",
          subtitle = "Spending SAMI percentile, all 501(c)(3) vs. excluding hospitals and universities",
          x = "Percentile, all 501(c)(3)", y = "Percentile, excl. hospitals, universities" ) + coord_equal() + thm + theme( legend.position = "right" )
  ggsave( file.path( OD, "fig_d10_money.png" ), ( pA | pB ) + plot_layout( widths = c( 1, 1.05 ) ) +
            plot_annotation( caption = "Expenses and revenue of 501(c)(3) filers above the $50k real floor. Net = less grants paid to domestic organizations and governments (Form 990 Part IX line 1).",
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 13, height = 6.6, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part O: Fact 5 add-on -- simplified age vs size chart (filers, 2022)
##         organization table built as in R/notes/77_nonprofit_age.R
## ===========================================================================
if ( "O" %in% PARTS ) {
  log_msg( "Part O: age vs size" )
  suppressPackageStartupMessages( library( ggplot2 ) )
  YR <- 2022L
  floor22 <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )[ year == YR, floor_nom ]
  b <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "subsec", "pf", "ryear", "ntee3" ) )
  b <- unique( b[ year == YR & subsec == 3 & pf == FALSE & is.finite( ryear ) & ryear >= 1800 & ryear <= YR ], by = "EIN" )[ , age := YR - ryear ]
  e <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ), col_select = c( "ein", "year", "subsec_group", "rev", "exp", "ntee3", "ind" ) ) )[ year == YR & subsec_group == "c3" ]
  e[ , `:=`( ein = as.integer( ein ), size = pmax( rev, exp, na.rm = TRUE ) ) ]
  e <- e[ order( -size ) ][ !duplicated( ein ), .( ein, exp, size, ntee3_e = ntee3, ind ) ]
  f <- merge( b, e, by.x = "EIN", by.y = "ein" )[ size >= floor22 & exp > 0 ]
  f[ , hosp_univ := ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) | ntee3_e %in% c( NTEE_HOSP, NTEE_UNIV ) | ind %in% c( "HOS", "UNI" ) ]
  BANDS <- c( "0-2", "3-5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81+" )
  f[ , band := cut( age, c( -1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 200 ), labels = BANDS ) ]
  sz <- rbind( f[ , .( group = "All filers", orgs = .N, med = median( exp ), q25 = quantile( exp, .25 ), q75 = quantile( exp, .75 ), hu = mean( hosp_univ ) ), keyby = band ],
               f[ hosp_univ == FALSE, .( group = "Excluding hospitals and universities", orgs = .N, med = median( exp ), q25 = quantile( exp, .25 ), q75 = quantile( exp, .75 ), hu = 0 ), keyby = band ] )
  print( sz ); fwrite( sz, file.path( OD, "fact5_age_size.csv" ) )
  lab <- sz[ group == "All filers", .( band, xl = sprintf( "%s\n%s filers\n%s H&U", band, ifelse( orgs >= 1000, paste0( round( orgs / 1000 ), "k" ), orgs ),
                                                        ifelse( hu < 0.1, sprintf( "%.1f%%", 100 * hu ), sprintf( "%.0f%%", 100 * hu ) ) ) ) ]
  money <- scales::label_dollar( accuracy = 1, scale_cut = scales::cut_short_scale() )
  fm <- function( v ) ifelse( v >= 1e6, sprintf( "$%.1fM", v / 1e6 ), sprintf( "$%.0fK", v / 1e3 ) )
  ## dumbbell: age bands on the y axis (oldest at top), median with and without H&U,
  ## dots sized by filers in the band, hospital/university share in a column at right
  sz[ , band := factor( band, BANDS ) ]
  wd <- dcast( sz, band ~ group, value.var = c( "med", "hu" ) )
  setnames( wd, c( "band", "all", "nohu", "hu", "hu0" ) )
  XR <- 2.6e7                                                  # x position of the right-hand column
  g <- ggplot( sz, aes( med, band ) ) +
    geom_segment( data = wd, aes( x = nohu, xend = all, y = band, yend = band ), colour = "grey75", linewidth = 2, lineend = "round" ) +
    geom_point( aes( colour = group, size = orgs ), alpha = 0.9 ) +
    geom_text( data = wd, aes( x = pmax( all, nohu ), y = band, label = fm( all ) ), hjust = 0, nudge_x = 0.07, size = 3.1, colour = "#1f5fae" ) +
    geom_text( data = wd[ all / nohu > 1.15 ], aes( x = pmin( all, nohu ), y = band, label = fm( nohu ) ), hjust = 1, nudge_x = -0.07, size = 3.1, colour = "#5f646c" ) +
    geom_text( data = wd, aes( x = XR, y = band, label = ifelse( hu < 0.1, sprintf( "%.1f%%", 100 * hu ), sprintf( "%.0f%%", 100 * hu ) ) ),
               hjust = 1, size = 3.2, colour = "grey25" ) +
    annotate( "text", x = XR, y = length( BANDS ) + 0.75, label = "Hospitals and\nuniversities", hjust = 1, vjust = 0, size = 3, colour = "grey35", lineheight = 0.9 ) +
    scale_x_log10( labels = money, breaks = c( 1e5, 3e5, 1e6, 3e6, 1e7 ), expand = expansion( mult = c( 0.06, 0.02 ) ) ) +
    scale_y_discrete( labels = function( b ) paste( b, "years" ), expand = expansion( add = c( 0.6, 1.3 ) ) ) +
    scale_size_area( max_size = 9, breaks = c( 5e3, 2e4, 5e4 ), labels = c( "5k", "20k", "50k" ), name = "Filers in band" ) +
    scale_colour_manual( values = c( "All filers" = "#2a78d6", "Excluding hospitals and universities" = "#8a8f98" ), name = NULL ) +
    guides( colour = guide_legend( order = 1, override.aes = list( size = 3.4 ) ), size = guide_legend( order = 2, override.aes = list( colour = "#2a78d6" ) ) ) +
    coord_cartesian( clip = "off" ) +
    labs( title = "Older nonprofits are larger",
          subtitle = "Median expenditures by years since IRS ruling, with and without hospitals and universities; 501(c)(3) filers, 2022",
          y = NULL, x = "Median expenditures (log scale)",
          caption = "Age = 2022 minus IRS ruling year (recognition, not founding; resets on reinstatement).
Elasticity of expenditures with respect to age + 1: .59 (.53 excluding hospitals and universities)." ) +
    theme_minimal( base_size = 11 ) + theme( panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), plot.title.position = "plot",
                                             plot.subtitle = element_text( colour = "grey30" ), legend.position = "top", legend.justification = "left",
                                             axis.text.y = element_text( size = 10, hjust = 1 ), legend.box = "horizontal",
                                             plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ), plot.caption.position = "plot" )
  ggsave( file.path( OD, "fig_fact5_age_size.png" ), g, width = 9, height = 7.2, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part P: D12 -- sector age as a control (metro ages from R/77, c12_age_by_metro)
## ===========================================================================
if ( "P" %in% PARTS ) {
  log_msg( "Part P: sector age" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  CU <- file.path( BUILD, "data", "catchup" )
  a <- fread( file.path( CU, "c12_age_by_metro.csv" ), colClasses = list( character = "cbsa" ) )
  gr <- fread( file.path( CU, "c9_metro_1970_2024.csv" ), colClasses = list( character = "cbsa" ) )
  en <- fread( file.path( OUT$cov, "cov_endowment.csv" ), select = c( "cbsa", "hist_pre1970" ), colClasses = list( character = "cbsa" ) )
  m <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ year == 2022 & cbsa_type == "metro" ]
  m <- merge( m, a[ , .( cbsa, region, mean_age_filers, median_age_all, dollar_weighted_age_nohu ) ], by = "cbsa" )
  m <- merge( m, gr[ , .( cbsa, g7000, g0024, s0n = s0_n_filer_c3, s1n = s1_n_filer_c3, s0g = s0_n_grass_c3pc, s1g = s1_n_grass_c3pc ) ], by = "cbsa" )
  m <- merge( m, en, by = "cbsa" )[ is.finite( hist_pre1970 ) & is.finite( turnout ) & is.finite( poverty_rate ) & is.finite( dollar_weighted_age_nohu ) ]
  m[ , logP := log( population ) ]
  rz <- function( x ) { z <- resid( lm( x ~ m$logP ) ); z / sd( z ) }
  m[ , `:=`( race_hhi = rz( race_hhi ), pf = rz( flog( pf_assets, population ) ), turnout = rz( turnout ), gov = rz( flog( gov_earn_sl, population ) ),
             poverty_rate = rz( poverty_rate ), pct_ba = rz( pct_ba ), endow = rz( flog( hist_pre1970, population ) ),
             age_f = rz( mean_age_filers ), age_r = rz( median_age_all ), age_d = rz( dollar_weighted_age_nohu ), grow = rz( g7000 ),
             s_n = resid( lm( log( n_filer_c3 ) ~ logP ) ), s_g = resid( lm( log( n_bmf_c3pc - n_filer_c3 ) ~ logP ) ), s_e = resid( lm( log( exp_c3_nohu ) ~ logP ) ),
             d_n = s1n - s0n, d_g = s1g - s0g ) ]
  ## how age relates to growth, history and size
  cr <- m[ , .( var = c( "mean_age_filers", "median_age_all", "dollar_weighted_age_nohu" ) ) ][
    , c( list( var = var ), lapply( list( logP = m$logP, g7000 = m$g7000, g0024 = m$g0024, endow = m$endow, s_n = m$s_n, s_g = m$s_g, s_e = m$s_e, d_n = m$d_n, d_g = m$d_g ),
                                    function( z ) sapply( var, function( v ) cor( m[[ v ]], z ) ) ) ) ]
  print( r3( cr ) ); fwrite( cr, file.path( OD, "d12_age_cor.csv" ) )
  CV <- c( race_hhi = DIVC, pf = "Foundation assets", turnout = "Turnout", gov = "S&L govt earnings", poverty_rate = "Poverty rate", pct_ba = "% BA",
           age_f = "Mean filer age", endow = "Pre-1970 endowment", grow = "Population growth 1970-2000" )
  ARMS <- list( base = character( 0 ), age = "age_f", endow = "endow", age_endow = c( "age_f", "endow" ), age_endow_grow = c( "age_f", "endow", "grow" ) )
  ARML <- c( base = "Default model", age = "+ mean filer age", endow = "+ pre-1970 endowment", age_endow = "+ age + endowment", age_endow_grow = "+ age + endowment + 1970-2000 growth" )
  base <- c( "race_hhi", "pf", "turnout", "gov", "poverty_rate", "pct_ba" )
  fitp <- function( y, arm, extra = character( 0 ) ) {
    xs <- c( extra, base, ARMS[[ arm ]] )
    r <- feols( as.formula( paste( y, "~", paste( xs, collapse = " + " ) ) ), m, vcov = "hetero" )
    ct <- coeftable( r )[ -1, , drop = FALSE ]; dfr <- degrees_freedom( r, "resid" ); pr <- function( t ) t / sqrt( t^2 + dfr )
    data.table( outcome = y, arm = arm, term = rownames( ct ), b = ct[ , 1 ], r = pr( ct[ , 3 ] ), lo = pr( ct[ , 3 ] - 1.96 ), hi = pr( ct[ , 3 ] + 1.96 ), r2 = r2( r, "r2" ), n = nobs( r ) ) }
  res <- rbindlist( lapply( c( "s_n", "s_g", "s_e" ), function( y ) rbindlist( lapply( names( ARMS ), function( a ) fitp( y, a ) ) ) ) )
  fwrite( res, file.path( OD, "d12_levels.csv" ) )
  print( dcast( res, outcome + term ~ arm, value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  print( unique( res[ , .( outcome, arm, r2 = round( r2, 3 ), n ) ] ) )
  ## alternative age measures in the filer levels model
  alt <- rbindlist( lapply( c( age_f = "Mean filer age", age_r = "Median age, all registered", age_d = "Dollar-weighted age excl. H&U" ), function( z ) NULL ) )
  alt <- rbindlist( lapply( c( "age_f", "age_r", "age_d" ), function( v ) { ARMS$tmp <<- v; z <- fitp( "s_n", "tmp" )[ term %in% c( v, base ) ]; z[ , measure := v ] } ) )
  print( dcast( alt, term ~ measure, value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  fwrite( alt, file.path( OD, "d12_alt_measures.csv" ) )
  ## change model: filer and grassroots SAMI change 2000-02 to 2022-24, start level and 2000-2024 growth held fixed
  m[ , g24 := rz( g0024 ) ]
  chg <- rbindlist( lapply( c( "d_n", "d_g" ), function( y ) { s0 <- if ( y == "d_n" ) "s0n" else "s0g"
    rbindlist( lapply( c( "base", "age" ), function( a ) { z <- fitp( y, a, extra = c( s0, "g24" ) ); z } ) ) } ) )
  print( r3( chg[ term %in% c( "age_f", "g24", base ) , .( outcome, arm, term, r, r2 ) ] ) ); fwrite( chg, file.path( OD, "d12_change.csv" ) )
  ## figure
  OUTL <- c( s_n = "Filer counts", s_e = "Spending excl. hospitals, universities" )
  pd <- res[ outcome %in% names( OUTL ) & arm %in% c( "base", "age", "endow", "age_endow" ) ][
    , `:=`( label = factor( CV[ term ], rev( unname( CV[ 1:8 ] ) ) ), arm = factor( ARML[ arm ], ARML[ 1:4 ] ), outcome = factor( OUTL[ outcome ], OUTL ) ) ]
  pA <- ggplot( pd, aes( r, label, colour = arm ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_linerange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.75 ), linewidth = 0.7 ) +
    geom_point( position = position_dodge( width = 0.75 ), size = 2 ) + facet_wrap( ~ outcome, ncol = 2 ) +
    scale_colour_manual( values = c( "#8a8f98", "#eda100", "#2a78d6", "#6250d6" ), name = NULL, guide = guide_legend( nrow = 2 ) ) +
    labs( title = "A. Sector age does the endowment's work in levels models",
          subtitle = sprintf( "Partial r with 95%% interval, SAMI form, %d metros, 2022", nrow( m ) ), x = "Partial r", y = NULL ) +
    thm + theme( legend.position = "bottom", strip.text = element_text( face = "bold", hjust = 0 ) )
  m[ , name := sub( "[-,/].*", "", cbsa_title ) ]
  lab <- rbind( m[ order( d_n ) ][ 1:4 ], m[ order( -d_n ) ][ 1:3 ], m[ order( mean_age_filers ) ][ 1:3 ], m[ order( -mean_age_filers ) ][ 1:3 ] )
  RC <- c( Northeast = "#2a78d6", Midwest = "#1baf7a", South = "#eb6834", West = "#eda100" )
  rr <- m[ , cor( mean_age_filers, d_n ) ]
  pB <- ggplot( m, aes( mean_age_filers, d_n ) ) + geom_hline( yintercept = 0, colour = "grey60" ) +
    geom_point( aes( colour = region, size = population ), alpha = 0.75 ) +
    geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "grey25", linewidth = 0.7 ) +
    geom_text_repel( data = unique( lab ), aes( label = name ), size = 2.8, colour = "grey15", min.segment.length = 0, seed = 4, max.overlaps = 30 ) +
    annotate( "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.4, size = 3.3, label = sprintf( "r = %.2f", rr ) ) +
    scale_colour_manual( values = RC, name = NULL ) + scale_size_area( max_size = 7, guide = "none" ) +
    labs( title = "B. Metros that added filers have younger sectors",
          subtitle = "Mean filer age in 2022 against the change in filer SAMI, 2000-02 to 2022-24",
          x = "Mean age of filers, 2022 (years since IRS ruling)", y = "Change in filer SAMI (log points)" ) + thm + theme( legend.position = "bottom" )
  ggsave( file.path( OD, "fig_d12_age.png" ), ( pA | pB ) + plot_layout( widths = c( 1.5, 1 ) ) +
            plot_annotation( caption = "Covariates and age measures residualized on log P and standardized. Age = 2022 minus IRS ruling year (R/notes/77_nonprofit_age.R). Endowment: charities ruled before 1970 in the 2000 BMF.",
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 13.5, height = 7, dpi = 170, bg = "white" )
}

## ===========================================================================
## Part Q: Fact 7 -- data hazards (splices, interpolation, tax-law break, group exemptions)
## ===========================================================================
if ( "Q" %in% PARTS ) {
  log_msg( "Part Q: data hazards" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot",
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  d <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ cbsa_type == "metro" ]
  ## A: national totals, indexed to 2010 (the last CORE year)
  nat <- d[ , .( `Filers ($50k real floor)` = sum( n_filer_c3 ), `Filers, no floor` = sum( n_filer_c3_nofloor, na.rm = TRUE ),
                 `Registered stock (BMF)` = sum( n_bmf_c3pc ), `Spending excl. H&U` = sum( exp_c3_nohu ),
                 `Itemized contributions` = sum( contrib_total, na.rm = TRUE ) ), by = year ]
  nl <- melt( nat, id.vars = "year", variable.name = "series" )[ value > 0 ]
  nl[ , idx := 100 * value / value[ year == 2010 ], by = series ]
  jumps <- nl[ order( series, year ) ][ , .( year, pct = 100 * ( value / shift( value ) - 1 ) ), by = series ][ year %in% c( 2011, 2018, 2020, 2021 ) ]
  print( dcast( jumps, series ~ year, value.var = "pct" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 1 ) else z ) ] )
  fwrite( nl, file.path( OD, "fact7_national.csv" ) ); fwrite( jumps, file.path( OD, "fact7_jumps.csv" ) )
  ## B: year-to-year SAMI correlation by year (from 50_followups part 2)
  p2 <- fread( file.path( BUILD, "data", "followup", "p2_persistence_lag1_by_year.csv" ) )
  print( dcast( p2[ year %in% c( 2010:2012, 2016:2021 ) ], label ~ year, value.var = "r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  ## C: share of metro-years in which each covariate is observed rather than interpolated or carried
  oc <- grep( "_obs$", names( d ), value = TRUE )
  ob <- rbindlist( lapply( oc, function( v ) data.table( var = sub( "_obs$", "", v ), observed = mean( d[[ v ]] %in% c( TRUE, 1 ), na.rm = TRUE ),
                                                       years = paste( range( d[ d[[ v ]] %in% c( TRUE, 1 ), year ] ), collapse = "-" ) ) ) )
  print( ob[ order( observed ) ] ); fwrite( ob, file.path( OD, "fact7_observed.csv" ) )
  ## D: group exemptions -- registered stock vs filers, 2022
  m <- d[ year == 2022 ][ , logP := log( population ) ]
  m[ , `:=`( s_f = resid( lm( log( n_filer_c3 ) ~ logP ) ), s_b = resid( lm( log( n_bmf_c3pc ) ~ logP ) ), name = sub( "[-,/].*", "", cbsa_title ) ) ]
  m[ , gap := s_b - s_f ]
  print( m[ order( -gap ) ][ 1:8, .( name, s_f = round( s_f, 2 ), s_b = round( s_b, 2 ), gap = round( gap, 2 ) ) ] )
  ## figures
  SPL <- data.frame( x = c( 2010.5, 2019.5 ), lab = c( "CORE → SOI", "SOI → e-file" ) )
  era <- geom_vline( data = SPL, aes( xintercept = x ), linetype = 2, colour = "grey55" )
  pA <- ggplot( nl, aes( year, idx, colour = series ) ) + era + geom_hline( yintercept = 100, colour = "grey80" ) +
    geom_line( linewidth = 0.8 ) + geom_point( size = 1.1 ) +
    geom_vline( xintercept = 2017.5, colour = "#c2410c", linetype = 3 ) +
    annotate( "text", x = c( 2010.4, 2019.4, 2017.4 ), y = c( 205, 205, 60 ), label = c( "CORE → SOI", "SOI → e-file", "2018 tax law" ),
              hjust = 1, size = 2.9, colour = c( "grey35", "grey35", "#c2410c" ) ) +
    scale_colour_manual( values = c( "#2a78d6", "#8fb8ea", "#1baf7a", "#eb6834", "#c2410c" ), name = NULL ) +
    labs( title = "A. Totals jump at source changes", subtitle = "387 metros summed, index 2010 = 100", x = NULL, y = "Index" ) +
    thm + theme( legend.position = "bottom" ) + guides( colour = guide_legend( ncol = 2 ) )
  p2[ , label := factor( label ) ]
  pB <- ggplot( p2, aes( year, r, colour = label ) ) + era + geom_line( linewidth = 0.8 ) + geom_point( size = 1.1 ) +
    scale_colour_manual( values = c( "#2a78d6", "#eb6834", "#1baf7a", "#6250d6", "#eda100", "#8a8f98", "#c2410c" ), name = NULL ) +
    labs( title = "B. Spending rankings break at the splices; counts do not", subtitle = "Correlation of each metro's SAMI with the year before", x = NULL, y = "r, year t vs. t − 1" ) +
    thm + theme( legend.position = "bottom" ) + guides( colour = guide_legend( ncol = 2 ) )
  OBL <- c( turnout = "Turnout", dem_margin = "Democratic margin", pct_ba = "% BA", gini = "Gini", pct_foreign_born = "% foreign born",
            contrib_total = "Itemized contributions", contrib_per_return = "Contributions per return", contrib_share_agi = "Contributions / AGI",
            rel_hhi = "Religious HHI", pct_adherent = "% religious adherents", socap_assn_n = "Associations (CBP)", socap_respn = "Census response",
            socap_sk = "Social capital index", socap_sk2 = "Social capital index 2" )
  pc <- ob[ var %in% names( OBL ) ][ , label := factor( OBL[ var ], OBL[ var ][ order( observed ) ] ) ]
  pC <- ggplot( pc, aes( observed, label ) ) + geom_col( fill = "#2a78d6", width = 0.65 ) +
    geom_text( aes( label = scales::percent( observed, accuracy = 1 ) ), hjust = -0.15, size = 3, colour = "grey25" ) +
    scale_x_continuous( labels = scales::percent_format(), limits = c( 0, 1.12 ), breaks = c( 0, .25, .5, .75, 1 ) ) +
    labs( title = "C. Most covariate-years are filled in", subtitle = "Share of metro-years observed, not interpolated or carried forward, 2000-2024", x = NULL, y = NULL ) + thm
  lab <- unique( rbind( m[ order( -abs( gap ) ) ][ 1:8 ], m[ name == "Mankato" ] ) )
  pD <- ggplot( m, aes( s_f, s_b ) ) + geom_abline( slope = 1, intercept = 0, colour = "grey70" ) +
    geom_point( colour = "#2a78d6", alpha = 0.5, size = 1.4 ) + geom_point( data = lab, colour = "#c2410c", size = 2 ) +
    geom_text_repel( data = lab, aes( label = name ), size = 2.8, colour = "grey15", min.segment.length = 0, seed = 5 ) +
    labs( title = "D. Group exemptions and registration habits", subtitle = "Registered-stock SAMI against filer SAMI, 2022", x = "Filer SAMI", y = "Registered-stock SAMI" ) + thm
  ggsave( file.path( OD, "fig_fact7_hazards.png" ), ( pA | pB ) / ( pC | pD ) +
            plot_annotation( caption = "Sources: NCCS CORE 2000-2010, IRS SOI extracts 2011-2019, IRS e-file 2020-2024; IRS SOI county income (contributions); _obs flags in cbsa_year.parquet.",
                             theme = theme( plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ),
          width = 13, height = 11, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part R: open gap -- the $50k real revenue floor vs no floor (filer counts)
## ===========================================================================
if ( "R" %in% PARTS ) {
  log_msg( "Part R: revenue floor" )
  d <- divx( as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) ) )[ cbsa_type == "metro" ]
  d[ , logP := log( population ) ]
  ## splice jumps, floor vs no floor
  nat <- d[ , .( floor = sum( n_filer_c3 ), nofloor = sum( n_filer_c3_nofloor, na.rm = TRUE ) ), by = year ][ order( year ) ]
  nat[ , `:=`( j_floor = 100 * ( floor / shift( floor ) - 1 ), j_nofloor = 100 * ( nofloor / shift( nofloor ) - 1 ), share_below = 1 - floor / nofloor ) ]
  print( r3( nat ) ); fwrite( nat, file.path( OD, "gap_floor_national.csv" ) )
  ## 2022 cross-section: beta, SAMI correlation, default model partial r
  m <- d[ year == 2022 & is.finite( turnout ) & is.finite( poverty_rate ) ]
  rz <- function( x ) { z <- resid( lm( x ~ m$logP ) ); z / sd( z ) }
  X <- m[ , .( race_hhi = rz( race_hhi ), pf = rz( flog( pf_assets, population ) ), turnout = rz( turnout ), gov = rz( flog( gov_earn_sl, population ) ), poverty_rate = rz( poverty_rate ), pct_ba = rz( pct_ba ) ) ]
  one <- function( y ) { b <- lm( log( m[[ y ]] ) ~ m$logP ); s <- resid( b )
    r <- feols( s ~ race_hhi + pf + turnout + gov + poverty_rate + pct_ba, cbind( data.table( s = s ), X ), vcov = "hetero" )
    ct <- coeftable( r )[ -1, ]; dfr <- degrees_freedom( r, "resid" )
    list( beta = coef( b )[ 2 ], sami = s, pr = data.table( outcome = y, term = rownames( ct ), r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + dfr ) ) ) }
  a <- one( "n_filer_c3" ); b <- one( "n_filer_c3_nofloor" )
  out <- merge( a$pr, b$pr, by = "term", suffixes = c( "_floor", "_nofloor" ) )
  print( r3( out ) ); fwrite( out, file.path( OD, "gap_floor_models.csv" ) )
  cat( "beta floor", round( a$beta, 3 ), " nofloor", round( b$beta, 3 ), " SAMI r", round( cor( a$sami, b$sami ), 3 ),
       " share below floor 2022", round( 1 - sum( m$n_filer_c3 ) / sum( m$n_filer_c3_nofloor ), 3 ), "\n" )
  ## persistence of each, 2012 vs 2022
  w <- dcast( d[ year %in% c( 2012, 2022 ) ], cbsa + logP ~ year, value.var = c( "n_filer_c3", "n_filer_c3_nofloor", "population" ) )
  sm <- function( y, p ) resid( lm( log( y ) ~ log( p ) ) )
  cat( "SAMI r 2012-2022: floor", round( cor( sm( w$n_filer_c3_2012, w$population_2012 ), sm( w$n_filer_c3_2022, w$population_2022 ) ), 3 ),
       " nofloor", round( cor( sm( w$n_filer_c3_nofloor_2012, w$population_2012 ), sm( w$n_filer_c3_nofloor_2022, w$population_2022 ) ), 3 ), "\n" )
}

## ===========================================================================
## Part S: summary exhibit -- specification curves for the five benchmarks (counts),
##         colored by the decision with the largest eta^2 among default-model forks
## ===========================================================================
if ( "S" %in% PARTS ) {
  log_msg( "Part S: specification curves" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  v4 <- file.path( OUT$spec, RUN )
  s <- as.data.table( read_parquet( file.path( v4, "specs.parquet" ) ) )[ op == "default" & drop == "none" & !( ident == "pooled" & est == "state_re" ) ]
  co <- as.data.table( read_parquet( file.path( v4, "coefs.parquet" ) ) )[ spec_id %in% s$spec_id ]
  co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" ); co <- merge( co, s, by = "spec_id" )
  co[ , pr := stat / sqrt( stat^2 + pmax( n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) - k - 1, 1 ) ) ]
  imp <- fread( file.path( OD, "decision_importance_long.csv" ) )
  COL <- c( D1 = "outcome", D2 = "unit", D3 = "ident", D4 = "dv_form", D5 = "pois", D6 = "pred_form", D9 = "est" )
  DL <- c( D1 = "Outcome population (D1)", D2 = "Unit (D2)", D3 = "Identification (D3)", D4 = "DV form (D4)", D5 = "Poisson vs. OLS (D5)",
           D6 = "Predictor form (D6)", D9 = "Estimator / FE (D9)" )
  OPT <- c( n_filer_c3 = "Filers", n_bmf_c3pc = "Registrations", cs = "Cross-section", pooled = "Pooled", within = "Within metro",
            ols = "OLS", state_fe = "State FE", state_re = "State RE", div_fe = "Division FE", raw = "Raw", pc = "Per capita", log = "Logged", sami = "SAMI form",
            loglog = "Log-log", logpc = "Log per capita", pois_free = "Poisson", pois_off = "Poisson offset", metro = "Metro", cbsa = "Metro + micro",
            `TRUE` = "Poisson", `FALSE` = "OLS" )
  BEN <- BENCH
  mk <- function( grp, cons, h ) {
    x <- co[ construct == cons & hu == h ][ , pois := dv_form %like% "^pois" ]
    ## within the spending group, D1 is the counts-vs-spending contrast, which is not a column here
    okdec <- if ( grp == "spending" ) setdiff( names( COL ), "D1" ) else names( COL )
    thm_s <- theme_minimal( base_size = 9.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text( face = "bold" ),
                                                       plot.subtitle = element_text( colour = "grey30", size = 8.5 ), axis.text.x = element_blank(), axis.title.x = element_blank() )
    plots <- lapply( names( BEN ), function( b ) {
      dom <- imp[ group == grp & bench == b & decision %in% okdec ][ which.max( eta2 ) ]
      z <- x[ term == b ][ order( pr ) ][ , rank := .I ]
      z[ , opt := OPT[ as.character( get( COL[[ dom$decision ]] ) ) ] ]
      md <- median( z$pr ); ss <- mean( sign( z$pr ) == sign( md ) )
      ## options ordered by their mean rank, so the strip reads from low to high
      lev <- z[ , .( m = mean( rank ) ), by = opt ][ order( m ), opt ]
      z[ , opt := factor( opt, rev( lev ) ) ]
      pc <- ggplot( z, aes( rank, pr ) ) + geom_hline( yintercept = 0, colour = "grey55" ) +
        geom_point( size = 0.4, colour = "#2a78d6" ) +
        labs( title = BEN[[ b ]], subtitle = sprintf( "median r = %.2f; %d%% share its sign\n%s specifications", md, round( 100 * ss ), format( nrow( z ), big.mark = "," ) ),
              y = "Partial r" ) + thm_s
      ps <- ggplot( z, aes( rank, opt ) ) + geom_point( shape = "|", size = 2.6, colour = "#c2410c", alpha = 0.35 ) +
        labs( subtitle = sprintf( "%s, η² = %.2f", DL[[ dom$decision ]], dom$eta2 ), y = NULL, x = "Specifications, ordered by partial r" ) +
        thm_s + theme( panel.grid.major.y = element_blank(), axis.title.x = element_text( size = 8.5 ), axis.text.y = element_text( size = 8 ),
                       plot.subtitle = element_text( size = 8.5, colour = "#c2410c", face = "bold" ) )
      pc / ps + plot_layout( heights = c( 3, 1.25 ) ) } )
    wrap_plots( plots, nrow = 1 ) }
  g <- mk( "counts", "count", "incl" ) / mk( "spending", "exp", "excl" ) +
    plot_annotation( title = "The five benchmark covariates across the multiverse",
                     subtitle = paste0( "Top: counts (filers and registrations). Bottom: spending excluding hospitals and universities. Default covariate set, all strata, run ", RUNL, "." ),
                     caption = "Partial r with degrees of freedom counted in metros; pooled random-intercept fits omitted. The strip under each curve marks where each option of the most influential decision (largest η² among the default-model forks) falls along the curve.",
                     theme = theme( plot.title = element_text( size = 14 ), plot.subtitle = element_text( colour = "grey30" ), plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) )
  ggsave( file.path( OD, "fig_exhibit_spec_curves.png" ), g, width = 17, height = 11, dpi = 160, bg = "white" )
  sm <- co[ term %in% names( BEN ) & ( ( construct == "count" & hu == "incl" ) | ( construct == "exp" & hu == "excl" ) ),
            { md <- median( pr ); .( median = md, same_sign = mean( sign( pr ) == sign( md ) ), sig = mean( abs( stat ) > 1.96 ), specs = .N ) }, by = .( construct, term ) ]
  print( r3( sm ) ); fwrite( sm, file.path( OD, "exhibit_spec_summary.csv" ) )
}

## ===========================================================================
## Part T: open gap -- is the fast growers' deficit a lag or a trait?
##   Stock-flow accounting, 2012-13 -> 2018-19 (one source, no splice).
##   dN/dt = f P - d N  =>  steady-state density D* = f / ( d + g ).
##   log D*_m - log D*_ref splits exactly into
##     entry    log( f_m / f_ref )                       (trait: fewer entrants per resident)
##     exit    -log( ( d_m + g_m ) / ( d_ref + g_m ) )     (trait: organizations leave faster)
##     growth  -log( ( d_ref + g_m ) / ( d_ref + g_ref ) ) (lag: the stock trails a growing population)
##   and log( D_m / D*_m ) says whether a metro sits below its own steady state (still catching up)
##   or at it (its flows sustain the deficit). Reference = mean over metros (logs).
## ===========================================================================
if ( "T" %in% PARTS ) {
  log_msg( "Part T: fast-grower lag vs trait" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  CU <- file.path( BUILD, "data", "catchup" )
  W0 <- 2012:2013; W1 <- 2018:2019; SPAN <- 6
  sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )[ , .( geoid, cbsa ) ]
  x9 <- fread( file.path( CU, "c9_metro_1970_2024.csv" ), colClasses = list( character = "cbsa" ) )
  x9[ , fast_pre := gq == "Q5" ][ , post_half := fifelse( r_post >= median( r_post[ fast_pre ] ), "kept growing fast", "slowed down" ), by = fast_pre ]
  x9[ , group := fifelse( fast_pre, post_half, "all other metros" ) ]
  pop <- fread( file.path( OUT$pop, "cbsa_pop_2000_2025.csv" ), colClasses = list( character = "cbsa" ) )
  pw <- pop[ , .( P00 = population[ year == 2000 ], P0 = mean( population[ year %in% W0 ] ), P1 = mean( population[ year %in% W1 ] ) ), by = cbsa ]
  ## a stock's members in each window; an organization counts in the metro of its latest record in the window
  flows <- function( d, lab ) {
    a0 <- unique( d[ year %in% W0 ][ order( id, -year ) ], by = "id" )[ , .( id, c0 = cbsa ) ]
    a1 <- unique( d[ year %in% W1 ][ order( id, -year ) ], by = "id" )[ , .( id, c1 = cbsa ) ]
    m <- merge( a0, a1, by = "id", all = TRUE )
    out <- merge( m[ !is.na( c0 ), .( N0 = .N, stay = sum( !is.na( c1 ) & c1 == c0 ) ), by = .( cbsa = c0 ) ],
                  m[ !is.na( c1 ), .( N1 = .N, new = sum( is.na( c0 ) ), moved_in = sum( !is.na( c0 ) & c0 != c1 ) ), by = .( cbsa = c1 ) ], by = "cbsa" )
    out[ , stock := lab ] }
  ## filers: SOI 990/990-EZ extracts, public charities above the real revenue floor
  pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )
  s <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_soi.parquet" ), col_select = c( "ein", "year", "subsec_group", "rev", "exp", "geoid" ) ) )[
    year %in% c( W0, W1 ) & subsec_group == "c3" & !is.na( geoid ) ]
  s <- merge( s, pce[ , .( year, floor_nom ) ], by = "year" )[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
  s[ , `:=`( id = as.integer( ein ), geoid = sprintf( "%05d", as.integer( geoid ) ) ) ]
  s <- merge( s, sp, by = "geoid" )
  ## registered stock: BMF public charities
  b <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "subsec", "pf" ) )[ year %in% c( W0, W1 ) & subsec == 3 & pf == FALSE ]
  b[ , `:=`( id = EIN, geoid = sprintf( "%05d", as.integer( geoid ) ) ) ]
  b <- merge( b, sp, by = "geoid" )
  fl <- rbind( flows( s, "Filers" ), flows( b, "Registered" ) )
  rm( s, b ); invisible( gc() )
  fl <- merge( fl, pw, by = "cbsa" )
  fl <- merge( fl, x9[ , .( cbsa, name, region, group, g7000, r_pre, r_post ) ], by = "cbsa" )
  ## rates per year (continuous time): entry per resident, exit per organization, population growth
  fl[ , `:=`( f = ( N1 - stay ) / sqrt( P0 * P1 ) / SPAN, d = -log( stay / N0 ) / SPAN, g = log( P1 / P0 ) / SPAN,
              g0012 = log( P0 / P00 ) / 12.5, D0 = N0 / P0 ) ]
  fl[ , `:=`( f_new = new / sqrt( P0 * P1 ) / SPAN, moved_share = moved_in / ( N1 - stay ) ) ]
  fl <- fl[ is.finite( f ) & is.finite( d ) & f > 0 & d > 0 ]
  fl[ , `:=`( f_ref = exp( mean( log( f ) ) ), d_ref = exp( mean( log( d ) ) ), g_ref = mean( g ), D_ref = exp( mean( log( D0 ) ) ) ), by = stock ]
  fl[ , `:=`( entry = log( f / f_ref ), exit = -log( ( d + g ) / ( d_ref + g ) ), growth = -log( ( d_ref + g ) / ( d_ref + g_ref ) ),
              actual = log( D0 / D_ref ), Dstar = f / ( d + g ) ) ]
  fl[ , implied := entry + exit + growth ][ , off_ss := log( D0 / Dstar ) ][ , off_ss := off_ss - mean( off_ss ), by = stock ]
  ref <- fl[ , .( f_ref = 1000 * f_ref[ 1 ], d_ref = d_ref[ 1 ], g_ref = g_ref[ 1 ], D_ref = 1000 * D_ref[ 1 ], metros = .N,
                  half_life = log( 2 ) / ( d_ref[ 1 ] + g_ref[ 1 ] ) ), by = stock ]
  print( r3( ref ) ); fwrite( ref, file.path( OD, "gap_lag_reference.csv" ) )
  ## group means (log points) with SEs; "within South and West" compares with other South/West metros only
  GRP <- c( "kept growing fast", "slowed down", "all other metros" )
  gm <- function( dd, lab ) dd[ , c( list( metros = .N, growth_7000 = mean( exp( g7000 ) - 1 ), growth_pa = mean( g ), entry_rate = 1000 * exp( mean( log( f ) ) ),
                                        exit_rate = exp( mean( log( d ) ) ), moved_share = mean( moved_share ) ),
                                   lapply( .SD, mean ) ), by = .( stock, group ), .SDcols = c( "actual", "implied", "entry", "exit", "growth", "off_ss" ) ][ , sample := lab ]
  tab <- rbind( gm( fl, "all metros" ), gm( fl[ region %in% c( "South", "West" ) ], "South and West only" ) )
  tab[ , group := factor( group, GRP ) ]; setorder( tab, sample, stock, group )
  print( r3( tab ) ); fwrite( tab, file.path( OD, "gap_lag_groups.csv" ) )
  ## differences from other metros, with SEs (OLS on group dummies; region FE in the second row)
  dif <- rbindlist( lapply( c( "Filers", "Registered" ), function( st ) rbindlist( lapply( c( "actual", "entry", "exit", "growth", "off_ss" ), function( v ) {
    dd <- fl[ stock == st ][ , group := relevel( factor( group ), "all other metros" ) ]
    rbindlist( lapply( list( none = NULL, region = "region" ), function( fe ) {
      m <- if ( is.null( fe ) ) feols( as.formula( paste( v, "~ group" ) ), dd, vcov = "hetero" ) else feols( as.formula( paste( v, "~ group | region" ) ), dd, vcov = "hetero" )
      ct <- coeftable( m ); data.table( stock = st, component = v, fe = if ( is.null( fe ) ) "none" else fe, term = sub( "group", "", rownames( ct ) ), est = ct[ , 1 ], se = ct[ , 2 ] ) } ) ) } ) ) ) )
  print( r3( dif ) ); fwrite( dif, file.path( OD, "gap_lag_differences.csv" ) )
  ## integration lag vs trait in entry: does entry per resident track recent growth or 1970-2000 growth?
  ent <- rbindlist( lapply( c( "Filers", "Registered" ), function( st ) {
    dd <- fl[ stock == st ]
    m1 <- feols( entry ~ g0012 + g + g7000 | region, dd, vcov = "hetero" )
    m2 <- feols( exit ~ g0012 + g + g7000 | region, dd, vcov = "hetero" )
    rbind( data.table( stock = st, outcome = "entry", term = names( coef( m1 ) ), est = coef( m1 ), se = se( m1 ), r2 = r2( m1, "wr2" ) ),
           data.table( stock = st, outcome = "exit", term = names( coef( m2 ) ), est = coef( m2 ), se = se( m2 ), r2 = r2( m2, "wr2" ) ) ) } ) )
  print( r3( ent ) ); fwrite( ent, file.path( OD, "gap_lag_entry_regressions.csv" ) )
  ## how well the steady state reproduces observed density, and what each component predicts
  fit <- fl[ , .( r_implied_actual = cor( implied, actual ), r_entry = cor( entry, actual ), r_exit = cor( exit, actual ), r_growth = cor( growth, actual ),
                  sd_actual = sd( actual ), sd_entry = sd( entry ), sd_exit = sd( exit ), sd_growth = sd( growth ) ), by = stock ]
  print( r3( fit ) ); fwrite( fit, file.path( OD, "gap_lag_fit.csv" ) )
  fwrite( fl[ , .( stock, cbsa, name, region, group, g7000, g, f = 1000 * f, d, actual, implied, entry, exit, growth, off_ss ) ], file.path( OD, "gap_lag_metro.csv" ) )

  ## ---- figure -------------------------------------------------------------------------------
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text( face = "bold" ),
                                                    plot.subtitle = element_text( colour = "grey30" ), strip.text = element_text( face = "bold", hjust = 0 ) )
  GL <- c( "kept growing fast" = "Fast growers,\nstill growing (39)", "slowed down" = "Fast growers,\nslowed down (38)", "all other metros" = "All other\nmetros (305)" )
  CC <- c( "Entry per resident" = "#2a78d6", "Exit rate" = "#eb6834", "Population growth" = "#8a8f98" )
  ## A: steady-state decomposition by group (all metros)
  ta <- tab[ sample == "all metros" ]
  la <- melt( ta[ , .( stock, group, `Entry per resident` = entry, `Exit rate` = exit, `Population growth` = growth ) ], id.vars = c( "stock", "group" ), variable.name = "part" )
  la[ , `:=`( grp = factor( GL[ as.character( group ) ], rev( GL ) ), part = factor( part, names( CC ) ) ) ]
  ta[ , grp := factor( GL[ as.character( group ) ], rev( GL ) ) ]
  pa <- ggplot( la, aes( value, grp ) ) + geom_vline( xintercept = 0, colour = "grey55" ) +
    geom_col( aes( fill = part ), width = 0.62, position = position_stack() ) +
    geom_point( data = ta, aes( implied, grp, shape = "Steady state implied by the flows" ), size = 2.6, colour = "black", fill = "white", stroke = 0.9 ) +
    geom_point( data = ta, aes( actual, grp, shape = "Observed density, 2012-13" ), size = 2.6, colour = "black" ) +
    facet_wrap( ~ stock, ncol = 1 ) +
    scale_fill_manual( values = CC, name = NULL ) + scale_shape_manual( values = c( "Steady state implied by the flows" = 21, "Observed density, 2012-13" = 18 ), name = NULL ) +
    guides( fill = guide_legend( order = 1, nrow = 1 ), shape = guide_legend( order = 2, nrow = 1 ) ) +
    labs( title = "A. Growth, not low entry, holds down the still-growing",
          subtitle = "Density vs. the average metro, split into its flows, 2012-13 to 2018-19",
          x = "Log points (≈ % difference in density)", y = NULL ) + thm + theme( legend.position = "bottom", legend.box = "vertical", legend.justification = "left" )
  ## B: distance below the metro's own steady state, difference from other metros
  tb <- dif[ component == "off_ss" & fe == "none" & term != "(Intercept)" ]
  tb[ , grp := factor( GL[ term ], rev( GL[ 1:2 ] ) ) ]
  pb <- ggplot( tb, aes( est, grp, colour = stock ) ) + geom_vline( xintercept = 0, colour = "grey55" ) +
    geom_pointrange( aes( xmin = est - 1.96 * se, xmax = est + 1.96 * se ), position = position_dodge( width = 0.5 ), size = 0.45 ) +
    scale_colour_manual( values = c( Filers = "#1f5fae", Registered = "#1baf7a" ), name = NULL ) +
    labs( title = "B. The slowed growers sit below the density their own flows sustain",
          subtitle = "Observed minus steady-state density, vs. other metros (95% CI).\nBelow 0 = still catching up; 0 = the flows sustain the gap",
          x = "Log points", y = NULL ) + thm + theme( legend.position = "bottom", legend.justification = "left" )
  ## C: entry per resident vs 1970-2000 growth, net of recent growth and region (filers)
  fc <- fl[ stock == "Filers" ]
  fc[ , `:=`( e_r = resid( feols( entry ~ g0012 + g | region, fc ) ), x_r = resid( feols( g7000 ~ g0012 + g | region, fc ) ) ) ]
  bc <- ent[ stock == "Filers" & outcome == "entry" & term == "g7000" ]
  pc <- ggplot( fc, aes( x_r, e_r ) ) + geom_hline( yintercept = 0, colour = "grey55" ) +
    geom_point( aes( colour = group == "all other metros" ), size = 1.4, alpha = 0.7 ) +
    geom_smooth( method = "lm", formula = y ~ x, colour = "black", linewidth = 0.7, se = TRUE, fill = "grey80" ) +
    geom_text_repel( data = fc[ group != "all other metros" ][ order( -abs( e_r ) ) ][ 1:8 ], aes( label = sub( ",.*", "", name ) ), size = 2.7, colour = "grey25", min.segment.length = 0, seed = 3 ) +
    scale_colour_manual( values = c( `FALSE` = "#c2410c", `TRUE` = "#9aa0a8" ), labels = c( `FALSE` = "Fast growers (top fifth, 1970-2000)", `TRUE` = "Other metros" ), name = NULL ) +
    labs( title = "C. Entry per resident shows no trace of 1970-2000 growth",
          subtitle = sprintf( "Filers; both axes net of 2000-12 and 2012-19 growth and region.\nSlope %.2f (SE %.2f) per log point of 1970-2000 growth", bc$est, bc$se ),
          x = "1970-2000 growth (log points, residual)", y = "Entry per resident (log, residual)" ) + thm + theme( legend.position = "bottom", legend.justification = "left" )
  g <- ( pa | ( pb / pc + plot_layout( heights = c( 1, 1.5 ) ) ) ) + plot_layout( widths = c( 1.1, 1 ) ) +
    plot_annotation( title = "Fast growers' density gap is a lag in the stock, not a shortfall in entry",
                     caption = "Stock-flow accounting: dN/dt = fP − dN, so steady-state density is f / (d + g). Filers: 990/990-EZ public charities above the real revenue floor (SOI extracts). Registered: BMF public charities.
Fast growers = top fifth of 1970-2000 population growth, split at the median of 2000-24 growth. Entry includes new organizations, moves in and (filers) crossings of the floor.",
                     theme = theme( plot.title = element_text( size = 14, face = "bold" ), plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) )
  ggsave( file.path( OD, "fig_gap_lag_trait.png" ), g, width = 14, height = 8.6, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part U: D7 diversity default -- racial HHI (fractionalization) vs % nonwhite
##   vs the diversity index (mean z of % nonwhite, % foreign-born, -HHI).
##   2022 metros, default covariates, log-log with log P on the right.
## ===========================================================================
if ( "U" %in% PARTS ) {
  log_msg( "Part U: diversity default" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ); library( ggrepel ) } )
  thm <- theme_minimal( base_size = 10.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text( face = "bold" ),
                                                    plot.subtitle = element_text( colour = "grey30" ) )
  m <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )[ cbsa_type == "metro" & year == 2022L ]
  m <- m[ , .( cbsa, cbsa_title, st = sub( ".*, ([A-Z]{2}).*", "\\1", cbsa_title ), population, logP = log( population ), n_filer_c3, n_bmf_c3pc, exp_c3_nohu,
               pct_ba, poverty_rate, turnout, pf_assets, gov_earn_sl, race_hhi, pct_nonwhite, pct_foreign_born,
               pct_hisp = n_hisp / race_den, pct_black = n_nhblack / race_den ) ]
  m[ , frac := 1 - race_hhi ]
  z <- function( v ) ( v - mean( v, na.rm = TRUE ) ) / sd( v, na.rm = TRUE )
  m[ , index := ( z( pct_nonwhite ) + z( pct_foreign_born ) + z( frac ) ) / 3 ]
  m[ , majmin := pct_nonwhite > 0.5 ]
  MEAS <- c( frac = "Fractionalization (1 − HHI)", pct_nonwhite = "% nonwhite", index = "Diversity index" )
  ## how the measures relate, raw and net of log P
  cr <- rbindlist( lapply( list( raw = function( v ) v, net_logP = function( v ) resid( lm( v ~ m$logP ) ) ), function( tf ) {
    x <- sapply( c( names( MEAS ), "pct_foreign_born", "logP" ), function( v ) tf( m[[ v ]] ) ); as.data.table( round( cor( x ), 3 ), keep.rownames = "var" ) } ), idcol = "form" )
  print( cr ); fwrite( cr, file.path( OD, "d7_diversity_correlations.csv" ) )
  cat( "majority-minority metros:", m[ majmin == TRUE, .N ], "\n" ); print( m[ majmin == TRUE ][ order( race_hhi ), .( cbsa_title, pct_nonwhite, pct_hisp, pct_black, frac ) ] )
  ## default model partial r for each measure: outcome x estimator x sample
  OUTS <- c( n_filer_c3 = "Filers", n_bmf_c3pc = "Registrations", exp_c3_nohu = "Spending (excl. H&U)" )
  pr <- rbindlist( lapply( names( OUTS ), function( y ) rbindlist( lapply( names( MEAS ), function( d ) rbindlist( lapply( c( "ols", "state_fe" ), function( est ) rbindlist( lapply( c( "all", "no_majmin" ), function( smp ) {
    dd <- m[ get( y ) > 0 & is.finite( turnout ) ]; if ( smp == "no_majmin" ) dd <- dd[ majmin == FALSE ]
    dd[ , `:=`( Y = log( get( y ) ), D = get( d ), lpf = flog( pf_assets, population ), lgov = flog( gov_earn_sl, population ) ) ]
    f <- if ( est == "ols" ) Y ~ logP + D + pct_ba + poverty_rate + turnout + lpf + lgov else Y ~ logP + D + pct_ba + poverty_rate + turnout + lpf + lgov | st
    fm <- feols( f, dd, vcov = "hetero" ); tt <- tstat( fm )[[ "D" ]]
    if ( est == "ols" ) dfr <- nobs( fm ) - length( coef( fm ) ) - 1 else dfr <- nobs( fm ) - length( coef( fm ) ) - uniqueN( dd$st )
    data.table( outcome = OUTS[[ y ]], measure = MEAS[[ d ]], est = est, sample = smp, n = nobs( fm ), t = tt, pr = tt / sqrt( tt^2 + dfr ) ) } ) ) ) ) ) ) ) )
  print( r3( dcast( pr, outcome + est + sample ~ measure, value.var = "pr" ) ) ); fwrite( pr, file.path( OD, "d7_diversity_partial_r.csv" ) )
  ## the multiverse view (part F output): median partial r and sign stability
  mv <- fread( file.path( OD, "d7_measures_by_block.csv" ) )[ blk == "diversity" & term %in% c( "race_hhi", "pct_nonwhite", "cmp_diversity", "pct_foreign_born" ) ]
  print( mv[ , .( term, label, med, sign_stab ) ] )
  ## figure A: fractionalization against % nonwhite
  lab <- m[ majmin == TRUE & ( pct_nonwhite > 0.8 | frac < 0.45 ) | cbsa_title %like% "^(Honolulu|Houston|New York|Los Angeles|Vallejo|Miami)" ]
  pa <- ggplot( m, aes( pct_nonwhite, frac ) ) + geom_vline( xintercept = 0.5, colour = "grey70", linetype = 2 ) +
    geom_point( aes( colour = majmin, size = population ), alpha = 0.6 ) +
    geom_text_repel( data = lab, aes( label = sub( "[-,].*", "", cbsa_title ) ), size = 2.8, colour = "grey25", min.segment.length = 0, seed = 2, max.overlaps = 30 ) +
    scale_colour_manual( values = c( `FALSE` = "#2a78d6", `TRUE` = "#c2410c" ), labels = c( `FALSE` = "White majority", `TRUE` = "Majority-minority" ), name = NULL ) +
    scale_size_area( max_size = 7, guide = "none" ) + scale_x_continuous( labels = scales::percent ) +
    labs( title = "A. The two measures agree until whites are a minority",
          subtitle = sprintf( "%d metros, 2022. r = %.2f overall; %.2f among white-majority metros", nrow( m ), m[ , cor( frac, pct_nonwhite ) ], m[ majmin == FALSE, cor( frac, pct_nonwhite ) ] ),
          x = "% nonwhite (incl. Hispanic)", y = "Fractionalization (1 − racial HHI)" ) + thm + theme( legend.position = "bottom", legend.justification = "left" )
  ## figure B: partial r by measure, outcome, estimator and sample
  pb_d <- copy( pr )[ , `:=`( measure = factor( measure, rev( MEAS ) ), lab = paste0( fifelse( est == "ols", "OLS", "State FE" ), fifelse( sample == "all", "", ", no majority-minority" ) ) ) ]
  pb <- ggplot( pb_d, aes( pr, measure, colour = lab, shape = lab ) ) + geom_vline( xintercept = 0, colour = "grey55" ) +
    geom_point( size = 2.4, position = position_dodge( width = 0.6 ) ) + facet_wrap( ~ outcome, ncol = 1 ) +
    scale_colour_manual( values = c( "OLS" = "#2a78d6", "OLS, no majority-minority" = "#93b9ea", "State FE" = "#c2410c", "State FE, no majority-minority" = "#eda27a" ), name = NULL ) +
    scale_shape_manual( values = c( "OLS" = 16, "OLS, no majority-minority" = 1, "State FE" = 17, "State FE, no majority-minority" = 2 ), name = NULL ) +
    labs( title = "B. Default-model partial r for each measure", subtitle = "2022 metros; log-log with log P and the default covariates",
          x = "Partial r", y = NULL ) + thm + theme( legend.position = "bottom", legend.justification = "left", strip.text = element_text( face = "bold", hjust = 0 ) ) +
    guides( colour = guide_legend( nrow = 2 ), shape = guide_legend( nrow = 2 ) )
  g <- ( pa | pb ) + plot_layout( widths = c( 1.15, 1 ) ) +
    plot_annotation( title = "Racial fractionalization and % nonwhite measure different things",
                     caption = "Fractionalization = 1 − Σ s² over seven groups (non-Hispanic white, Black, AIAN, Asian, NHPI, two or more; Hispanic). Diversity index = mean z-score of % nonwhite, % foreign-born and fractionalization.
Partial r with heteroskedasticity-robust SEs; state FE models count states in the degrees of freedom. Majority-minority = % nonwhite above 50%.",
                     theme = theme( plot.title = element_text( size = 14, face = "bold" ), plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) )
  ggsave( file.path( OD, "fig_d7_diversity.png" ), g, width = 13, height = 7.6, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part V: exhibit -- specification curves for the five benchmarks (top) with the
##   eta^2 break-out of one decision (bottom), counts. Default decision: estimator
##   (D9). Within-metro specs are left out because they have only OLS, so both rows
##   show the same cross-section + pooled specifications.
##   Usage: Rscript R/notes/58_outline_analyses.R V [est | all | decision columns...]; all = est outcome unit ident dv_form pois pred_form.
##   Non-estimator decisions use all strata (within-metro included).
## ===========================================================================
if ( "V" %in% PARTS ) {
  log_msg( "Part V: spec curves with one decision broken out" )
  suppressPackageStartupMessages( { library( ggplot2 ); library( patchwork ) } )
  ALLDEC <- c( "est", "outcome", "unit", "ident", "dv_form", "pois", "pred_form" )
  DECS <- if ( length( commandArgs( TRUE ) ) > 1 ) commandArgs( TRUE )[ -1 ] else "est"
  ## "exp" anywhere in the arguments switches to spending (excl. hospitals and universities)
  SPEND <- "exp" %in% DECS; DECS <- setdiff( DECS, "exp" ); if ( !length( DECS ) ) DECS <- "est"
  if ( identical( DECS, "all" ) ) DECS <- ALLDEC
  CONS <- if ( SPEND ) "exp" else "count"; HU <- if ( SPEND ) "excl" else "incl"; SFX <- if ( SPEND ) "_exp" else ""
  GRPL <- if ( SPEND ) "Spending, excluding hospitals and universities" else "Counts (filers and registrations)"
  v4 <- file.path( OUT$spec, RUN )
  s0 <- as.data.table( read_parquet( file.path( v4, "specs.parquet" ) ) )[ op == "default" & drop == "none" & !( ident == "pooled" & est == "state_re" ) &
                                                                            construct == CONS & hu == HU ]
  co0 <- as.data.table( read_parquet( file.path( v4, "coefs.parquet" ) ) )[ spec_id %in% s0$spec_id ]
  co0 <- merge( co0, co0[ , .( k = .N ), by = spec_id ], by = "spec_id" ); co0 <- merge( co0, s0, by = "spec_id" )
  co0[ , dfm := pmax( n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) - k - 1, 1 ) ]
  co0[ , `:=`( pr = stat / sqrt( stat^2 + dfm ), sig = 2 * pt( -abs( stat ), dfm ) < 0.05 ) ]
  co0[ , pois := fifelse( dv_form %like% "^pois", "Poisson", "OLS (no zeros)" ) ]
  ## option order, bottom to top, the same in every row
  LEV <- list( est = c( "OLS", "Division FE", "State FE", "State RE" ),
               outcome = c( "Filers", "Human services filers", "Registrations", "Expenses", "Expenses, net", "Revenue", "Revenue, net" ),
               unit = c( "Metro", "Metro + micro" ), ident = c( "Within metro", "Pooled", "Cross-section" ),
               dv_form = c( "Per capita", "Log per capita", "Log-log", "SAMI form", "Poisson offset", "Poisson" ),
               pois = c( "OLS (no zeros)", "Poisson" ), pred_form = c( "Raw", "Per capita", "Logged", "SAMI form" ) )
  for ( DEC in DECS ) {
  log_msg( "  decision: ", DEC )
  ## within-metro specs are OLS only, so they are dropped when the estimator is the decision
  co <- if ( DEC == "est" ) co0[ ident != "within" ] else co0
  BEN <- BENCH
  OPT <- c( ols = "OLS", div_fe = "Division FE", state_fe = "State FE", state_re = "State RE", raw = "Raw", pc = "Per capita", log = "Logged", sami = "SAMI form",
            loglog = "Log-log", logpc = "Log per capita", pois_free = "Poisson", pois_off = "Poisson offset", cs = "Cross-section", pooled = "Pooled",
            metro = "Metro", cbsa = "Metro + micro", n_filer_c3 = "Filers", n_bmf_c3pc = "Registrations", n_filer_c3_hs = "Human services filers", within = "Within metro",
            exp_c3_nohu = "Expenses", exp_c3_nohu_net = "Expenses, net", rev_c3_nohu = "Revenue", rev_c3_nohu_net = "Revenue, net" )
  ## within the spending group the outcome column is the money measure (D10), not the outcome population
  DECL <- c( est = "Estimator / FE (D9)", pred_form = "Predictor form (D6)", pois = "Poisson vs. OLS (D5)", dv_form = "DV form (D4)", ident = "Identification (D3)",
             unit = "Unit (D2)", outcome = if ( SPEND ) "Money measure (D10)" else "Outcome population (D1)" )
  x <- co[ term %in% names( BEN ) ][ , opt := OPT[ as.character( get( DEC ) ) ] ]
  x[ is.na( opt ), opt := as.character( get( DEC ) ) ]
  lev <- intersect( LEV[[ DEC ]], unique( x$opt ) ); lev <- c( lev, setdiff( unique( x$opt ), lev ) )
  x[ , opt := factor( opt, lev ) ]
  ## eta^2 of the decision on this spec set, per covariate
  et <- x[ , { mu <- mean( pr ); .( eta2 = sum( tapply( pr, opt, function( v ) length( v ) * ( mean( v ) - mu )^2 ), na.rm = TRUE ) / sum( ( pr - mu )^2 ),
                                   median = median( pr ), same_sign = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( sig ), specs = .N, mu = mu ) }, by = term ]
  om <- x[ , .( m = mean( pr ), n = .N, sig = mean( sig ) ), by = .( term, opt ) ]
  print( r3( et ) ); print( r3( dcast( om, opt ~ term, value.var = "m" ) ) )
  fwrite( merge( om, et[ , .( term, eta2 ) ], by = "term" ), file.path( OD, sprintf( "exhibit_breakout_%s%s.csv", DEC, SFX ) ) )
  xr <- range( x$pr )
  thm_v <- theme_minimal( base_size = 9.5 ) + theme( panel.grid.minor = element_blank(), plot.title.position = "plot", plot.title = element_text( face = "bold", size = 11 ),
                                                     plot.subtitle = element_text( colour = "grey30", size = 8.5 ) )
  ## one row per covariate: left = specification curve (partial r on x, specs ordered up the y),
  ## right = the decision's options on the same x scale
  BW <- 0.02                                                                   # bin width for pooled dots
  TXT <- 5.6                                                                   # in-panel stat labels (r, % sig, eta^2)
  XB <- seq( -1, 1, 0.2 )
  NMAX <- x[ , .N, by = .( term, b = floor( pr / BW ), sig ) ][ , max( N ) ]
  rows <- lapply( names( BEN ), function( b ) {
    z <- x[ term == b ][ order( pr ) ][ , rank := .I ]; e <- et[ term == b ]; o <- om[ term == b ]
    zb <- z[ , .( pr = mean( pr ), rank = mean( rank ), n = .N ), by = .( bin = floor( pr / BW ), sig ) ]
    last <- b == names( BEN )[ length( BEN ) ]
    pl <- ggplot( z, aes( pr, rank ) ) +
      annotate( "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "grey92" ) +
      geom_vline( xintercept = 0, colour = "grey55", linewidth = 0.4 ) +
      geom_path( colour = "grey75", linewidth = 0.4 ) +
      geom_point( data = zb, aes( colour = sig, size = n ), alpha = 0.85 ) +
      scale_colour_manual( values = c( `TRUE` = "black", `FALSE` = "#9aa0a6" ), labels = c( `TRUE` = "Significant (p < .05)", `FALSE` = "Not significant" ), name = NULL ) +
      scale_size_area( max_size = 6, limits = c( 0, NMAX ), breaks = c( 10, 50, 150 ), name = "Specifications per dot" ) +
      guides( colour = guide_legend( order = 1, override.aes = list( size = 3.5 ) ), size = guide_legend( order = 2, override.aes = list( colour = "grey40" ) ) ) +
      scale_x_continuous( limits = xr, breaks = XB ) +
      annotate( "label", x = xr[ 1 ] + 0.755 * diff( xr ), y = 0.3 * nrow( z ), label = sprintf( "r = %.2f\n%d%% sig", e$median, round( 100 * e$sig ) ),
                hjust = 0, vjust = 0.5, size = TXT, fontface = "bold", colour = "black", fill = NA, border.colour = NA,
                label.padding = unit( 0.45, "lines" ), lineheight = 0.95 ) +
      labs( title = BEN[[ b ]], x = if ( last ) "Partial r" else NULL, y = "Specifications, ordered" ) +
      thm_v + theme( axis.text.y = element_blank(), panel.grid.major.y = element_blank(), axis.title.y = element_text( size = 8, colour = "grey40" ),
                     plot.title = element_text( face = "bold", size = 15 ) )
    pr_ <- ggplot( z, aes( pr, opt ) ) +
      annotate( "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "grey92" ) +
      geom_vline( xintercept = 0, colour = "grey55", linewidth = 0.4 ) +
      geom_point( position = position_jitter( height = 0.2, width = 0, seed = 1 ), size = 0.35, colour = "grey60", alpha = 0.45 ) +
      geom_vline( xintercept = e$mu, colour = "black", linewidth = 0.5 ) +
      geom_segment( data = o, aes( x = e$mu, xend = m, y = opt, yend = opt ), colour = "black", linewidth = 1.9 ) +
      geom_point( data = o, aes( m, opt ), colour = "black", size = 4.2 ) +
      scale_x_continuous( limits = xr, breaks = XB ) +
      scale_y_discrete( expand = expansion( add = c( 0.5, 0.6 ) ) ) +
      labs( title = sprintf( "η² = %.2f", e$eta2 ), x = if ( last ) "Partial r" else NULL, y = NULL ) +
      thm_v + theme( panel.grid.major.y = element_blank(), axis.text.y = element_text( size = 9 ),
                     plot.title.position = "panel", plot.title = element_text( face = "bold", size = 16, hjust = 0.5 ) )
    list( pl, pr_ ) } )
  g <- wrap_plots( unlist( rows, recursive = FALSE ), ncol = 2, guides = "collect" ) +
    plot_annotation( title = sprintf( "The five benchmark covariates across the multiverse, broken out by %s", DECL[[ DEC ]] ),
                     subtitle = sprintf( "%s, %s, default covariates, run %s; %s specifications per covariate.", GRPL,
                                         if ( DEC == "est" ) "cross-section and pooled" else "cross-section, pooled and within-metro", RUNL, format( et$specs[ 1 ], big.mark = "," ) ),
                     caption = sprintf( "Left: specification curve turned on its side; specifications are ordered from most negative (bottom) to most positive (top). Specifications are pooled into dots within %.2f bins of partial r, sized by count;
black = significant at p < .05, grey = not. Lower right: r = median partial r across specifications; %% sig = share significant. Shaded area = negative estimates.
Partial r and p use degrees of freedom counted in metros; pooled random-intercept fits are left out.
Right: grey dots = the same specifications by option; black line = mean of all; black dot = option mean; bar = how far it pulls from the overall mean. η² = between-option variance / total variance.
%s%s", BW,
                                        if ( DEC == "est" ) "Within-metro specifications are omitted because they are OLS only. " else "",
                                        if ( NEWRUN ) "Diversity is % nonwhite, the default from run v5 on (D7)." else "Diversity is racial HHI in run v4; final runs use % nonwhite (D7)." ),
                     theme = theme( plot.title = element_text( size = 14, face = "bold" ), plot.subtitle = element_text( colour = "grey30" ),
                                    plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ) ) ) &
    theme( legend.position = "bottom", legend.justification = "left" )
  ggsave( file.path( OD, sprintf( "fig_exhibit_breakout_%s%s.png", DEC, SFX ) ), g, width = 11, height = 15, dpi = 160, bg = "white" )
  }
}

## ===========================================================================
## Part W: summary table -- per covariate and decision: median r over the decision's
##   specifications, r under the card's recommended option, % of recommended specs that
##   share that sign, % of recommended specs significant, and the decision's eta^2.
##   Same spec sets as part V (estimator: cross-section + pooled; others: all strata).
##   Writes summary_table_{count,exp}.csv and summary_table.md (markdown for the doc).
## ===========================================================================
if ( "W" %in% PARTS ) {
  log_msg( "Part W: summary table" )
  v4 <- file.path( OUT$spec, RUN )
  sp <- as.data.table( read_parquet( file.path( v4, "specs.parquet" ) ) )[ op == "default" & drop == "none" & !( ident == "pooled" & est == "state_re" ) ]
  cf <- as.data.table( read_parquet( file.path( v4, "coefs.parquet" ) ) )[ spec_id %in% sp$spec_id ]
  cf <- merge( cf, cf[ , .( k = .N ), by = spec_id ], by = "spec_id" ); cf <- merge( cf, sp, by = "spec_id" )
  cf[ , dfm := pmax( n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) - k - 1, 1 ) ]
  cf[ , `:=`( pr = stat / sqrt( stat^2 + dfm ), sig = 2 * pt( -abs( stat ), dfm ) < 0.05 ) ]
  cf[ , pois := fifelse( dv_form %like% "^pois", "pois", "ols" ) ]
  BEN <- BENCH
  ## recommended option of each card, as a filter on the spec table
  REC <- list(
    outcome   = list( lab = "D1 Outcome population", rec = "filers", f = quote( outcome == "n_filer_c3" ) ),
    money     = list( lab = "D10 Money measure", rec = "expenses", f = quote( outcome == "exp_c3_nohu" ) ),
    unit      = list( lab = "D2 Unit", rec = "metro", f = quote( unit == "metro" ) ),
    ident     = list( lab = "D3 Identification", rec = "cross-section", f = quote( ident == "cs" ) ),
    dv_form   = list( lab = "D4 DV form", rec = "log-log or SAMI", f = quote( dv_form %in% c( "loglog", "sami" ) | ( dv_form == "logpc" & rhs_logP ) ) ),
    pois      = list( lab = "D5 Poisson vs. OLS", rec = "OLS, no zeros", f = quote( pois == "ols" ) ),
    pred_form = list( lab = "D6 Predictor form", rec = "SAMI or logged + log P", f = quote( pred_form == "sami" | ( pred_form == "log" & rhs_logP ) ) ),
    est       = list( lab = "D9 Estimator / FE", rec = "OLS, division or state FE", f = quote( est != "state_re" ) ) )
  DCOL <- c( outcome = "outcome", money = "outcome", unit = "unit", ident = "ident", dv_form = "dv_form", pois = "pois", pred_form = "pred_form", est = "est" )
  eta2 <- function( v, g ) { mu <- mean( v ); sum( tapply( v, g, function( z ) length( z ) * ( mean( z ) - mu )^2 ) ) / sum( ( v - mu )^2 ) }
  one <- function( cons, h ) {
    base <- cf[ construct == cons & hu == h & term %in% names( BEN ) ]
    decs <- if ( cons == "exp" ) c( "money", "unit", "ident", "dv_form", "pois", "pred_form", "est" ) else c( "outcome", "unit", "ident", "dv_form", "pois", "pred_form", "est" )
    out <- rbindlist( lapply( decs, function( d ) {
      x <- if ( d == "est" ) base[ ident != "within" ] else base
      x[ , .( decision = REC[[ d ]]$lab, recommended = REC[[ d ]]$rec, median_r = median( pr ),
              rec_r = median( pr[ eval( REC[[ d ]]$f ) ] ),
              agree = { rr <- pr[ eval( REC[[ d ]]$f ) ]; mean( sign( rr ) == sign( median( rr ) ) ) },
              sig = mean( sig[ eval( REC[[ d ]]$f ) ] ), eta2 = eta2( pr, get( DCOL[[ d ]] ) ), specs = .N, rec_specs = sum( eval( REC[[ d ]]$f ) ) ), by = term ] } ) )
    ## every recommendation at once (cross-section, so the estimator rule applies)
    allf <- Reduce( function( a, b ) call( "&", a, b ), lapply( decs, function( d ) REC[[ d ]]$f ) )
    al <- base[ eval( allf ), .( decision = sprintf( "All recommendations together (%d specifications)", .N ), recommended = "", median_r = NA_real_, rec_r = median( pr ),
                                 agree = mean( sign( pr ) == sign( median( pr ) ) ), sig = mean( sig ), eta2 = NA_real_, specs = NA_integer_, rec_specs = .N ), by = term ]
    av <- out[ , .( decision = "Average across decisions", recommended = "", median_r = mean( median_r ), rec_r = mean( rec_r ), agree = NA_real_, sig = NA_real_,
                    eta2 = NA_real_, specs = NA_integer_, rec_specs = NA_integer_ ), by = term ]
    res <- rbind( out, av, al )[ , construct := cons ]
    res[ , term := factor( term, names( BEN ) ) ]; setorder( res, term ); res }
  tabs <- list( count = one( "count", "incl" ), exp = one( "exp", "excl" ) )
  for ( k in names( tabs ) ) { print( r3( tabs[[ k ]] ), nrows = 100 ); fwrite( tabs[[ k ]], file.path( OD, sprintf( "summary_table_%s.csv", k ) ) ) }
  ## markdown for the doc: one table per construct, a section row per covariate
  fr <- function( v ) ifelse( is.na( v ), "", sub( "^(-?)0\\.", "\\1.", sprintf( "%.2f", round( v, 2 ) + 0 ) ) |> gsub( pattern = "^-", replacement = "−" ) )   # + 0 drops negative zero
  fp <- function( v ) ifelse( is.na( v ), "", sprintf( "%d%%", round( 100 * v ) ) )
  md <- unlist( lapply( names( tabs ), function( k ) {
    t <- tabs[[ k ]]
    c( sprintf( "%s", if ( k == "count" ) "COUNTS" else "SPENDING" ),
       "| Decision (recommended option) | Median r | Recommended r | % agree | % significant | η² |", "|---|---|---|---|---|---|",
       unlist( lapply( names( BEN ), function( b ) {
         s <- t[ term == b ]
         c( sprintf( "| **%s** | | | | | |", BEN[[ b ]] ),
            s[ , sprintf( "| %s | %s | %s | %s | %s | %s |",
                          ifelse( recommended == "", ifelse( decision %like% "^Average", paste0( "*", decision, "*" ), paste0( "*", decision, "*" ) ), sprintf( "%s (%s)", decision, recommended ) ),
                          fr( median_r ), fr( rec_r ), fp( agree ), fp( sig ), fr( eta2 ) ) ] ) } ) ), "" ) } ) )
  writeLines( md, file.path( OD, "summary_table.md" ), useBytes = TRUE )
}
log_msg( "done" )
