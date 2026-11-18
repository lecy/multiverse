## 79_kernel_lag.R
## Distributed-lag (partial-adjustment) models of nonprofit density on population.
## Each year's population change opens a gap between the organization stock and its
## size-expected level; the kernel says how that gap closes. Kernels on log population
## change, built over a grid of half-lives h (lambda = 1 - 0.5^(1/h), a = 1 - lambda):
##   G[t] = a * G[t-1] + dlogP[t]      geometric: weight a^k on dlogP[t-k]
##   H[t] = a * H[t-1] + G[t]          Pascal (order 2): weight (k+1) a^k -- with G it
##                                     allows a deficit that grows before it closes
## Specs (metro + year FE unless noted; "free beta" = log N on log P with the long-run
## within-metro elasticity estimated, instead of the SAMI with the cross-sectional b imposed):
##   A   SAMI ~ G                        (b imposed)
##   B   log N ~ log P + G               free beta, geometric
##   P   log N ~ log P + G + H           free beta, Pascal mixture (hump allowed), one rate
##   M   log N ~ log P + G(h1) + G(h2)   free beta, two rates
##   C   SAMI ~ G+(h1) + G-(h2)          gains vs losses, b imposed
##   Cb  log N ~ log P + G+(h1) + G-(h2) gains vs losses, free beta
##   D1  SAMI ~ G | year                 across metros
##   D2  SAMI ~ G(h1) + G(h2) | year     across metros, two rates
##   T   B plus metro-specific linear trends (the level analogue of the within-metro LPs,
##       which put metro FE on a differenced outcome); also the free windows with trends
## (A free-starting-gap version on all 387 metros -- metro loadings on a^(t - 2000) -- was
## tried and dropped: at slow rates the loading is a near-linear trend collinear with log P.)
## Checks: kernel-implied impulse responses vs the local projections (72 parts 2 and 5),
## free growth windows vs the kernel weights, fit against single-lag specs.
## Half-lives are profiled on the grid; 95% CIs from a metro cluster bootstrap that
## re-profiles every draw (duplicated metros enter as frequency weights).
## Population history: 1970/1980/1990 census counts (c9_metro_1970_2024.csv from
## 73_pre2000_growth.R) interpolated log-linearly to the panel's 2000 value (382 metros;
## CT and incomplete-coverage metros are only in spec E).
## Writes D:/MULTIVERSE/data/catchup/c14_*, fig_c14_*.
##   Rscript R/79_kernel_lag.R [B]     B = bootstrap draws (default 300)

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( ggplot2 ) } )
setFixest_notes( FALSE )
CU <- file.path( BUILD, "data", "catchup" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
options( width = 220 )
theme_c <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"; C5 <- "#7c5cd6"
args <- commandArgs( TRUE ); NB <- if ( length( args ) ) as.integer( args[ 1 ] ) else 300L
set.seed( 20261115 )
pct <- function( b, shift ) 100 * ( exp( b * shift ) - 1 )
q <- function( z, p ) as.numeric( quantile( z, p, na.rm = TRUE, type = 7 ) )

## ---- panel and year-specific SAMIs (as in 72_catchup_lag.R) ---------------------------
d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
m <- d[ cbsa_type == "metro" ]; rm( d )
m[ , cbsa := as.character( cbsa ) ]; setorder( m, cbsa, year ); m[ , logP := log( population ) ]
m[ , n_grass_c3pc := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]
OUTC <- c( n_filer_c3 = "Filers", n_grass_c3pc = "Grassroots", exp_c3_nohu = "Expenditures" )
BS <- c()
for ( v in names( OUTC ) ) {
  m[ , c( "a_", "b_" ) := { ok <- get( v ) > 0; cf <- coef( lm( log( get( v )[ ok ] ) ~ logP[ ok ] ) ); list( cf[ 1 ], cf[ 2 ] ) }, by = year ]
  m[ , ( paste0( "s_", v ) ) := fifelse( get( v ) > 0, log( get( v ) ) - a_ - b_ * logP, NA_real_ ) ]
  m[ , ( paste0( "l_", v ) ) := fifelse( get( v ) > 0, log( get( v ) ), NA_real_ ) ]
  BS[ v ] <- mean( unique( m[ , .( year, b_ ) ] )$b_ )
  m[ , c( "a_", "b_" ) := NULL ]
}
cat( "metros", uniqueN( m$cbsa ), " years", paste( range( m$year ), collapse = "-" ), " rows", nrow( m ), "\n" )
print( round( BS, 3 ) )

## ---- population history 1970-2024 -------------------------------------------------------
h70 <- fread( file.path( CU, "c9_metro_1970_2024.csv" ), colClasses = list( character = "cbsa" ) )[ , .( cbsa, p1970, p1980, p1990 ) ]
h70 <- merge( h70, m[ year == 2000, .( cbsa, l2000 = logP ) ], by = "cbsa" )
pre <- h70[ , .( year = 1970:1999, logP = approx( c( 1970, 1980, 1990, 2000 ), c( log( c( p1970, p1980, p1990 ) ), l2000 ),
                                                   xout = 1970:1999 )$y ), by = cbsa ]
full <- rbind( pre, m[ cbsa %in% h70$cbsa, .( cbsa, year, logP ) ] ); setorder( full, cbsa, year )
full[ , row := .I ]
cat( "metros with 1970-2000 history:", uniqueN( full$cbsa ), "\n" )

## ---- kernels on a half-life grid ------------------------------------------------------------
HL <- c( seq( 0.5, 20, 0.5 ), 25, 30, 40, 60, 100 ); NH <- length( HL )
lam <- function( h ) 1 - 0.5^( 1 / h )
rec <- function( x, g, a ) ave( x, g, FUN = function( z ) as.numeric( stats::filter( z, a, method = "recursive" ) ) )
kern <- function( dt, f = identity ) {
  x <- f( dt[ , c( 0, diff( logP ) ), by = cbsa ]$V1 ); g <- dt$cbsa
  sapply( HL, function( h ) rec( x, g, 1 - lam( h ) ) ) }
Gs <- kern( full ); Gp <- kern( full, function( z ) pmax( z, 0 ) ); Gn <- kern( full, function( z ) pmin( z, 0 ) )
Hs <- sapply( seq_len( NH ), function( k ) rec( Gs[ , k ], full$cbsa, 1 - lam( HL[ k ] ) ) )
E <- merge( full[ year >= 2000, .( cbsa, year, row ) ], m[ , c( "cbsa", "year", "logP", paste0( "s_", names( OUTC ) ), paste0( "l_", names( OUTC ) ) ), with = FALSE ],
            by = c( "cbsa", "year" ) )
setorder( E, cbsa, year )
XK <- cbind( Gs[ E$row, ], Gp[ E$row, ], Gn[ E$row, ], Hs[ E$row, ], E$logP )
IS <- 1:NH; IP <- NH + 1:NH; IN <- 2 * NH + 1:NH; IH <- 3 * NH + 1:NH; IL <- 4 * NH + 1      # column sets in XK

## ---- closed-form fits on demeaned cross-products ---------------------------------------------------
## C = cross-products of [y, X] after (weighted) demeaning; column 1 = y, column j + 1 = XK[ , j ]
cross <- function( y, X, cl, yr, w = NULL, two = TRUE ) {
  Z <- fixest::demean( cbind( y, X ), f = if ( two ) list( cl, yr ) else list( yr ), weights = w )
  if ( !is.null( w ) ) Z <- Z * sqrt( w )
  crossprod( Z ) }
sweepL <- function( C ) { p <- IL + 1; cp <- C[ , p ]; C - outer( cp, cp ) / C[ p, p ] }     # partial out log P (FWL)
betaL <- function( C, J, g ) ( C[ IL + 1, 1 ] - sum( g * C[ IL + 1, J + 1 ] ) ) / C[ IL + 1, IL + 1 ]
one <- function( C, J ) { j <- J + 1; sxy <- C[ 1, j ]; list( ssr = C[ 1, 1 ] - sxy^2 / diag( C )[ j ], b = sxy / diag( C )[ j ] ) }
two <- function( C, A, B ) { a <- diag( C )[ A + 1 ]; dd <- diag( C )[ B + 1 ]; bb <- C[ A + 1, B + 1, drop = FALSE ]
  ya <- C[ 1, A + 1 ]; yb <- C[ 1, B + 1 ]; nA <- length( A ); nB <- length( B ); det <- outer( a, dd ) - bb^2
  list( ssr = C[ 1, 1 ] - ( outer( ya^2, dd ) - 2 * outer( ya, yb ) * bb + outer( a, yb^2 ) ) / det,
        bA = ( outer( ya, dd ) - bb * matrix( yb, nA, nB, byrow = TRUE ) ) / det,
        bB = ( outer( a, yb ) - bb * matrix( ya, nA, nB ) ) / det ) }
row1 <- function( spec, type, h1, g1, h2 = NA_real_, g2 = NA_real_, beta = NA_real_, ssr )
  data.table( spec = spec, type = type, h1 = h1, g1 = g1, h2 = h2, g2 = g2, beta = beta, ssr = ssr )
pair_best <- function( t, upper = FALSE ) { s <- t$ssr; if ( upper ) s[ lower.tri( s, diag = TRUE ) ] <- Inf
  s[ !is.finite( s ) ] <- Inf; arrayInd( which.min( s ), dim( s ) ) }
## all specs from the three cross-product matrices: Cw (SAMI, two-way), Cl (log N, two-way), Ca (SAMI, year FE)
## free-elasticity specs: kernels slower than HMAXF are near-trends, collinear with log P under metro FE
## (the fit trades them off against beta), so their rates are capped
HMAXF <- 20; capv <- function( s ) { s[ HL > HMAXF ] <- Inf; s }
capm <- function( s ) { s[ HL > HMAXF, ] <- Inf; s[ , HL > HMAXF ] <- Inf; s }
fit_all <- function( Cw, Cl, Ca, keep = FALSE ) {
  r <- list(); pr <- list(); Sl <- sweepL( Cl )
  o <- one( Cw, IS ); k <- which.min( o$ssr ); r$A <- row1( "A", "geo1", HL[ k ], o$b[ k ], ssr = o$ssr[ k ] ); pr$A <- o$ssr
  o <- one( Sl, IS ); o$ssr <- capv( o$ssr ); k <- which.min( o$ssr )
  r$B <- row1( "B", "geo1", HL[ k ], o$b[ k ], beta = betaL( Cl, IS[ k ], o$b[ k ] ), ssr = o$ssr[ k ] ); pr$B <- o$ssr
  t <- two( Sl, IS, IH ); s <- capv( diag( t$ssr ) ); k <- which.min( s )
  r$P <- row1( "P", "pascal", HL[ k ], t$bA[ k, k ], HL[ k ], t$bB[ k, k ], betaL( Cl, c( IS[ k ], IH[ k ] ), c( t$bA[ k, k ], t$bB[ k, k ] ) ), s[ k ] ); pr$P <- s
  t <- two( Sl, IS, IS ); t$ssr <- capm( t$ssr ); k <- pair_best( t, TRUE )
  r$M <- row1( "M", "geo2", HL[ k[ 1 ] ], t$bA[ k ], HL[ k[ 2 ] ], t$bB[ k ], betaL( Cl, IS[ k ], c( t$bA[ k ], t$bB[ k ] ) ), t$ssr[ k ] )
  t <- two( Cw, IP, IN ); k <- pair_best( t ); r$C <- row1( "C", "asym", HL[ k[ 1 ] ], t$bA[ k ], HL[ k[ 2 ] ], t$bB[ k ], ssr = t$ssr[ k ] )
  t <- two( Sl, IP, IN ); t$ssr <- capm( t$ssr ); k <- pair_best( t ); sCb <- t$ssr
  r$Cb <- row1( "Cb", "asym", HL[ k[ 1 ] ], t$bA[ k ], HL[ k[ 2 ] ], t$bB[ k ], betaL( Cl, c( IP[ k[ 1 ] ], IN[ k[ 2 ] ] ), c( t$bA[ k ], t$bB[ k ] ) ), t$ssr[ k ] )
  o <- one( Ca, IS ); k <- which.min( o$ssr ); r$D1 <- row1( "D1", "geo1", HL[ k ], o$b[ k ], ssr = o$ssr[ k ] ); pr$D1 <- o$ssr
  t <- two( Ca, IS, IS ); k <- pair_best( t, TRUE ); r$D2 <- row1( "D2", "geo2", HL[ k[ 1 ] ], t$bA[ k ], HL[ k[ 2 ] ], t$bB[ k ], ssr = t$ssr[ k ] )
  out <- rbindlist( r )
  if ( keep ) attr( out, "prof" ) <- rbindlist( lapply( names( pr ), function( s ) data.table( spec = s, h = HL, ssr = pr[[ s ]] ) ) )
  if ( keep ) attr( out, "surf" ) <- data.table( h_gain = rep( HL, NH ), h_loss = rep( HL, each = NH ), ssr = as.vector( sCb ) )
  out }

## ---- deficit path implied by a fitted kernel -----------------------------------------------------
## D[k] = effect on log N of a unit log population change k years ago, beyond the long-run elasticity
wts <- function( type, h1, g1, h2, g2, K, dir = "growth" ) {
  a1 <- 1 - lam( h1 ); a2 <- if ( is.na( h2 ) ) 0 else 1 - lam( h2 )
  switch( type, geo1 = g1 * a1^K, geo2 = g1 * a1^K + g2 * a2^K, pascal = ( g1 + g2 * ( K + 1 ) ) * a1^K,
          asym = if ( dir == "growth" ) g1 * a1^K else g2 * a2^K ) }
## shares of the long-run response missing: at impact, at the peak, and when the deficit halves from its peak
summ <- function( type, h1, g1, h2, g2, beta, b, dir = "growth" ) {
  K <- 0:100; D <- wts( type, h1, g1, h2, g2, K, dir ); den <- if ( is.na( beta ) ) b else beta
  pk <- which.max( abs( D ) ); kh <- which( abs( D ) <= 0.5 * abs( D[ pk ] ) & K > K[ pk ] )
  list( short0 = -D[ 1 ] / den, peak_lag = K[ pk ], short_peak = -D[ pk ] / den,
        half_lag = if ( length( kh ) ) as.numeric( K[ min( kh ) ] ) else NA_real_, short10 = -D[ 11 ] / den ) }
addsumm <- function( x, by ) {
  x[ , b := BS[ outcome ] ]
  x[ , c( "short0", "peak_lag", "short_peak", "half_lag", "short10" ) := summ( type, h1, g1, h2, g2, beta, b ), by = by ]
  x[ , c( "short0_loss", "half_lag_loss", "short10_loss" ) := { if ( type == "asym" ) { z <- summ( type, h1, g1, h2, g2, beta, b, "decline" )
    list( z$short0, z$half_lag, z$short10 ) } else list( NA_real_, NA_real_, NA_real_ ) }, by = by ]
  x }

## ---- point estimates, profiles, bootstrap -------------------------------------------------------
res <- list(); prof <- list(); surf <- list(); boot <- list()
for ( v in names( OUTC ) ) {
  log_msg( "kernel fits: ", v )
  sv <- paste0( "s_", v ); lv <- paste0( "l_", v )
  ok <- which( is.finite( E[[ sv ]] ) ); Ev <- E[ ok ]; X <- XK[ ok, ]
  f0 <- fit_all( cross( Ev[[ sv ]], X, Ev$cbsa, Ev$year ), cross( Ev[[ lv ]], X, Ev$cbsa, Ev$year ),
                 cross( Ev[[ sv ]], X, Ev$cbsa, Ev$year, two = FALSE ), keep = TRUE )
  prof[[ v ]] <- attr( f0, "prof" )[ , outcome := v ]; surf[[ v ]] <- attr( f0, "surf" )[ , outcome := v ]
  res[[ v ]] <- f0[ , outcome := v ]
  cl <- unique( Ev$cbsa ); bl <- vector( "list", NB )
  for ( bi in seq_len( NB ) ) {
    dr <- table( sample( cl, length( cl ), replace = TRUE ) )
    j <- which( Ev$cbsa %in% names( dr ) ); w <- as.numeric( dr[ Ev$cbsa[ j ] ] ); Xj <- X[ j, ]
    bl[[ bi ]] <- fit_all( cross( Ev[[ sv ]][ j ], Xj, Ev$cbsa[ j ], Ev$year[ j ], w ), cross( Ev[[ lv ]][ j ], Xj, Ev$cbsa[ j ], Ev$year[ j ], w ),
                           cross( Ev[[ sv ]][ j ], Xj, Ev$cbsa[ j ], Ev$year[ j ], w, two = FALSE ) )[ , rep := bi ]
  }
  boot[[ v ]] <- rbindlist( bl )[ , outcome := v ]
}
res <- addsumm( rbindlist( res ), c( "outcome", "spec" ) ); boot <- addsumm( rbindlist( boot ), c( "outcome", "spec", "rep" ) )
prof <- rbindlist( prof ); surf <- rbindlist( surf )
CIV <- c( "h1", "h2", "beta", "short0", "peak_lag", "short_peak", "half_lag", "short10", "short0_loss", "half_lag_loss", "short10_loss" )
bci <- boot[ , c( setNames( lapply( CIV, function( z ) q( get( z ), .025 ) ), paste0( CIV, "_lo" ) ),
                  setNames( lapply( CIV, function( z ) q( get( z ), .975 ) ), paste0( CIV, "_hi" ) ) ), by = .( outcome, spec ) ]
tab <- merge( res, bci, by = c( "outcome", "spec" ) )
SPECL <- c( A = "A SAMI, geometric (b imposed)", B = "B log N, geometric, free beta", P = "P log N, Pascal (hump), free beta",
            M = "M log N, two rates, free beta", C = "C SAMI, gains vs losses (b imposed)", Cb = "Cb log N, gains vs losses, free beta",
            D1 = "D1 across metros, geometric", D2 = "D2 across metros, two rates" )
tab[ , label := SPECL[ spec ] ]; setorder( tab, outcome, spec )
print( r3( tab[ , .( outcome, spec, h1, h2, beta, short0, short0_lo, short0_hi, peak_lag, short_peak, half_lag, half_lag_lo, half_lag_hi,
                     short10, short10_lo, short10_hi, short0_loss, half_lag_loss, half_lag_loss_lo, half_lag_loss_hi, ssr ) ] ), nrows = 100 )
fwrite( tab, file.path( CU, "c14_kernel_estimates.csv" ) ); fwrite( prof, file.path( CU, "c14_profile.csv" ) )
fwrite( surf, file.path( CU, "c14_asym_surface.csv" ) ); fwrite( boot, file.path( CU, "c14_bootstrap.csv" ) )


## ---- impulse responses vs local projections ------------------------------------------------------
## LP: s[t+h] - s[t-3] on growth t-3 -> t (metro + year FE). Kernel equivalent for growth g spread over
## t-2..t: SAMI effect at t+h = g * [ (beta - b) + mean( D[h + 0:2] ) ]; (beta - b) = 0 for SAMI specs.
irf_k <- function( type, h1, g1, h2, g2, beta, b, dir, H = 0:12 ) {
  lr <- if ( is.na( beta ) ) 0 else beta - b
  sapply( H, function( z ) lr + mean( wts( type, h1, g1, h2, g2, z + 0:2, dir ) ) ) }
lp2 <- fread( file.path( CU, "c2_local_projection.csv" ) )[ fe == "metro_year" & h <= 12 ]
lp5 <- fread( file.path( CU, "c5_local_projection_sign.csv" ) )[ fe == "metro_year" & term %in% c( "pop_growth", "pop_decline" ) ]
IRS <- list( c( "A", "growth" ), c( "B", "growth" ), c( "P", "growth" ), c( "M", "growth" ), c( "Cb", "growth" ), c( "Cb", "decline" ) )
irf <- rbindlist( lapply( names( OUTC ), function( v ) rbindlist( lapply( IRS, function( z ) {
  sp <- z[ 1 ]; dir <- z[ 2 ]; sh <- if ( dir == "growth" ) log( 1.1 ) else log( 0.9 )
  r <- res[ outcome == v & spec == sp ]; pt <- r[ , irf_k( type, h1, g1, h2, g2, beta, b, dir ) ]
  bm <- boot[ outcome == v & spec == sp, .( H = 0:12, e = irf_k( type, h1, g1, h2, g2, beta, b, dir ) ), by = rep ]
  bq <- bm[ , .( lo = q( e, .025 ), hi = q( e, .975 ) ), keyby = H ]
  data.table( outcome = v, model = sp, dir = dir, H = 0:12, eff = pct( pt, sh ),
              lo = pmin( pct( bq$lo, sh ), pct( bq$hi, sh ) ), hi = pmax( pct( bq$lo, sh ), pct( bq$hi, sh ) ) ) } ) ) ) )
lpd <- rbind( lp2[ , .( outcome, model = "LP", dir = "growth", H = h, est = theta, se, sh = log( 1.1 ) ) ],
              lp5[ , .( outcome, model = "LP sign", dir = fifelse( term == "pop_growth", "growth", "decline" ), H = h, est, se,
                        sh = fifelse( term == "pop_growth", log( 1.1 ), log( 0.9 ) ) ) ] )
lpd <- lpd[ , .( outcome, model, dir, H, eff = pct( est, sh ), lo = pmin( pct( est - 1.96 * se, sh ), pct( est + 1.96 * se, sh ) ),
                 hi = pmax( pct( est - 1.96 * se, sh ), pct( est + 1.96 * se, sh ) ) ) ]
irf_all <- rbind( irf, lpd ); fwrite( irf_all, file.path( CU, "c14_irf_kernel_vs_lp.csv" ) )
## distance from the LPs (mean absolute gap in % points, h = 0..12)
lpref <- irf_all[ model %in% c( "LP", "LP sign" ) & !( model == "LP" & dir == "decline" ) ]
lpref <- rbind( lpref[ model == "LP" ], lpref[ model == "LP sign" & dir == "decline" ] )[ , .( outcome, dir, H, lp = eff ) ]
gap <- merge( irf[ , .( outcome, model, dir, H, eff ) ], lpref, by = c( "outcome", "dir", "H" ) )[ , .( mean_abs_gap = mean( abs( eff - lp ) ) ), by = .( outcome, model, dir ) ]
print( r3( dcast( irf_all[ H %in% c( 0, 2, 4, 6, 8, 10, 12 ) ], outcome + dir + H ~ model, value.var = "eff" ) ), nrows = 200 )
print( r3( gap ) ); fwrite( gap, file.path( CU, "c14_irf_gap.csv" ) )

## ---- free growth windows vs kernel weights -------------------------------------------------------
setkey( full, cbsa, year ); lagP <- function( k ) full[ .( E$cbsa, E$year - k ), logP ]
WIN <- list( c( 0, 3 ), c( 3, 6 ), c( 6, 10 ), c( 10, 15 ), c( 15, 20 ), c( 20, 30 ) )
for ( i in seq_along( WIN ) ) E[ , ( paste0( "w", i ) ) := lagP( WIN[[ i ]][ 1 ] ) - lagP( WIN[[ i ]][ 2 ] ) ]
win <- rbindlist( lapply( names( OUTC ), function( v ) {
  r <- feols( as.formula( paste0( "l_", v, " ~ logP + ", paste0( "w", seq_along( WIN ), collapse = " + " ), " | cbsa + year" ) ), E, cluster = ~cbsa )
  ct <- coeftable( r )
  out <- data.table( outcome = v, window = sapply( WIN, paste, collapse = "-" ), lag_mid = sapply( WIN, function( z ) mean( z[ 1 ]:( z[ 2 ] - 1 ) ) ),
                     est = ct[ paste0( "w", seq_along( WIN ) ), 1 ], se = ct[ paste0( "w", seq_along( WIN ) ), 2 ], beta_free = ct[ "logP", 1 ] )
  for ( sp in c( "B", "P", "M" ) ) { rr <- res[ outcome == v & spec == sp ]
    out[ , ( paste0( "k_", sp ) ) := sapply( WIN, function( z ) rr[ , mean( wts( type, h1, g1, h2, g2, z[ 1 ]:( z[ 2 ] - 1 ) ) ) ] ) ] }
  out } ) )
print( r3( win ) ); fwrite( win, file.path( CU, "c14_windows.csv" ) )

## ---- T: metro-specific linear trends ---------------------------------------------------------------
## The within-metro LPs put metro FE on a differenced outcome (s[t+h] - s[t-3]), which is equivalent to a
## metro-specific linear trend in levels. T adds those trends to B and to the free windows: if the slow
## tail in the level models is metro-specific secular drift correlated with old growth, it goes away here.
log_msg( "spec T: metro-specific trends" )
E[ , tt := year - 2012 ]; HT <- HL[ HL <= HMAXF ]
specT <- rbindlist( lapply( names( OUTC ), function( v ) {
  lv <- paste0( "l_", v )
  pr <- rbindlist( lapply( HT, function( h ) { E[ , g := Gs[ row, match( h, HL ) ] ]
    r <- feols( as.formula( paste( lv, "~ logP + g | cbsa[ tt ] + year" ) ), E, cluster = ~cbsa )
    data.table( outcome = v, h = h, ssr = sum( resid( r )^2 ), beta = coef( r )[[ "logP" ]], se_beta = se( r )[[ "logP" ]],
                gamma = coef( r )[[ "g" ]], se = se( r )[[ "g" ]], n = nobs( r ) ) } ) )
  pr[ , best := ssr == min( ssr ) ] } ) )
specT[ , short0 := -gamma / beta ]
print( r3( specT[ best == TRUE ] ) ); fwrite( specT, file.path( CU, "c14_specT_profile.csv" ) )
winT <- rbindlist( lapply( names( OUTC ), function( v ) {
  r <- feols( as.formula( paste0( "l_", v, " ~ logP + ", paste0( "w", seq_along( WIN ), collapse = " + " ), " | cbsa[ tt ] + year" ) ), E, cluster = ~cbsa )
  ct <- coeftable( r )
  data.table( outcome = v, window = sapply( WIN, paste, collapse = "-" ), lag_mid = sapply( WIN, function( z ) mean( z[ 1 ]:( z[ 2 ] - 1 ) ) ),
              est = ct[ paste0( "w", seq_along( WIN ) ), 1 ], se = ct[ paste0( "w", seq_along( WIN ) ), 2 ], beta_free = ct[ "logP", 1 ] ) } ) )
print( r3( winT ) ); fwrite( winT, file.path( CU, "c14_windows_trends.csv" ) )
irfT <- rbindlist( lapply( names( OUTC ), function( v ) { r <- specT[ outcome == v & best == TRUE ]
  data.table( outcome = v, model = "T", dir = "growth", H = 0:12, lo = NA_real_, hi = NA_real_,
              eff = pct( irf_k( "geo1", r$h, r$gamma, NA, NA, r$beta, BS[ v ], "growth" ), log( 1.1 ) ) ) } ) )
irf_all <- rbind( irf_all, irfT, use.names = TRUE ); fwrite( irf_all, file.path( CU, "c14_irf_kernel_vs_lp.csv" ) )
gapT <- merge( irfT[ , .( outcome, dir, H, eff ) ], lpref, by = c( "outcome", "dir", "H" ) )[ , .( model = "T", mean_abs_gap = mean( abs( eff - lp ) ) ), by = .( outcome, dir ) ]
gap <- rbind( gap, gapT, use.names = TRUE ); print( r3( gap[ dir == "growth" ] ) ); fwrite( gap, file.path( CU, "c14_irf_gap.csv" ) )

## ---- fit against single-lag and one-window specs (log N, metro + year FE) ----------------------------
log_msg( "spec comparison" )
for ( k in 1:10 ) E[ , ( paste0( "pl", k ) ) := lagP( k ) ]
cmp <- rbindlist( lapply( names( OUTC ), function( v ) {
  lv <- paste0( "l_", v ); r <- res[ outcome == v ]; ix <- function( sp, w ) match( r[ spec == sp, get( w ) ], HL )
  E[ , `:=`( gB = Gs[ row, ix( "B", "h1" ) ], gP1 = Gs[ row, ix( "P", "h1" ) ], gP2 = Hs[ row, ix( "P", "h1" ) ],
             gM1 = Gs[ row, ix( "M", "h1" ) ], gM2 = Gs[ row, ix( "M", "h2" ) ],
             gCp = Gp[ row, ix( "Cb", "h1" ) ], gCn = Gn[ row, ix( "Cb", "h2" ) ], d5 = logP - pl5 ) ]
  fs <- c( list( "log P[t]" = "logP" ), setNames( as.list( paste0( "pl", 1:10 ) ), paste0( "log P[t-", 1:10, "]" ) ),
           list( "log P[t] + (log P[t] - log P[t-5])" = "logP + d5", "B geometric" = "logP + gB", "P Pascal (hump)" = "logP + gP1 + gP2",
                 "M two rates" = "logP + gM1 + gM2", "Cb gains vs losses" = "logP + gCp + gCn",
                 "log P[t] + 6 growth windows" = paste( "logP +", paste0( "w", 1:6, collapse = " + " ) ) ) )
  np <- c( 1, rep( 1, 10 ), 2, 3, 4, 5, 5, 7 )
  rbindlist( lapply( seq_along( fs ), function( i ) {
    f <- feols( as.formula( paste( lv, "~", fs[[ i ]], "| cbsa + year" ) ), E )
    data.table( outcome = v, model = names( fs )[ i ], params = np[ i ], n = nobs( f ), wr2 = fitstat( f, "wr2" )[[ 1 ]],
                rmse = sqrt( mean( resid( f )^2 ) ), bic = nobs( f ) * log( mean( resid( f )^2 ) ) + np[ i ] * log( nobs( f ) ) ) } ) )
} ) )
cmp[ , dbic := bic - min( bic ), by = outcome ]
print( r3( cmp ), nrows = 100 ); fwrite( cmp, file.path( CU, "c14_spec_comparison.csv" ) )

## ---- figures ----------------------------------------------------------------------------------------
OL <- c( n_filer_c3 = "Filers", n_grass_c3pc = "Grassroots" )
## 1 profiles
SL <- c( A = "SAMI, b imposed (A)", B = "log N, free elasticity (B)", P = "Pascal / hump (P)", D1 = "Across metros (D1)" )
pf <- prof[ spec %in% names( SL ) & outcome %in% names( OL ) & is.finite( ssr ) ][ , rel := 100 * ( ssr / min( ssr ) - 1 ), by = .( outcome, spec ) ]
ci <- tab[ spec %in% names( SL ) & outcome %in% names( OL ) ]
g <- ggplot( pf[ h <= 40 ], aes( h, rel, colour = spec ) ) + geom_line( linewidth = 0.8 ) +
  geom_point( data = ci[ h1 <= 40 ], aes( h1, 0, colour = spec ), size = 2.2 ) +
  facet_wrap( ~ OL[ outcome ], scales = "free_y" ) + coord_cartesian( ylim = c( 0, 6 ) ) +
  scale_colour_manual( values = c( A = "grey55", B = C1, P = C3, D1 = C2 ), labels = SL, name = NULL ) +
  labs( title = "Fit of each kernel rate", subtitle = "Residual sum of squares relative to the best rate for that spec (%); dot = best. Within metro unless noted.",
        x = "Kernel half-life parameter (years)", y = "Extra residual SS (%)" ) + theme_c
ggsave( file.path( CU, "fig_c14_profile.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )

## 2 impulse responses
ML <- c( A = "Geometric, b imposed (A)", B = "Geometric, free elasticity (B)", P = "Pascal / hump (P)", Cb = "Gains vs losses (Cb)",
         LP = "Local projection", "LP sign" = "Local projection" )
## T is left out of the figure: with metro trends its long-run elasticity is not identified (see c14_specT_profile.csv)
pi_ <- irf_all[ outcome %in% names( OL ) & ( ( dir == "growth" & model %in% c( "A", "B", "P", "LP" ) ) | ( dir == "decline" & model %in% c( "Cb", "LP sign" ) ) ) ]
pi_[ , panel := factor( paste0( OL[ outcome ], ": after 10% ", dir ), levels = paste0( rep( OL, each = 2 ), ": after 10% ", c( "growth", "decline" ) ) ) ]
KC <- c( A = "grey55", B = C1, P = C3, Cb = C5, T = C2, LP = "grey20", "LP sign" = "grey20" )
g <- ggplot( pi_, aes( H, eff, colour = model, fill = model ) ) + geom_hline( yintercept = 0, colour = "grey50" ) +
  geom_ribbon( data = pi_[ model %in% c( "B", "P", "Cb" ) ], aes( ymin = lo, ymax = hi ), alpha = 0.10, colour = NA ) +
  geom_line( data = pi_[ !model %like% "LP" ], linewidth = 0.85 ) +
  geom_pointrange( data = pi_[ model %like% "LP" ], aes( ymin = lo, ymax = hi ), size = 0.2, linewidth = 0.4 ) +
  facet_wrap( ~ panel, nrow = 1, scales = "free_y" ) +
  scale_colour_manual( values = KC, labels = ML[ c( "A", "B", "P", "Cb", "LP" ) ], name = NULL, breaks = c( "A", "B", "P", "Cb", "LP" ) ) +
  scale_fill_manual( values = KC, guide = "none" ) +
  labs( title = "Kernel models against the local projections",
        subtitle = "Effect on density (%) of a 3-year population change, within metro. Lines: kernels (95% bootstrap band). Points: local projections (72_catchup_lag.R).",
        x = "Years after the 3-year episode", y = "Effect on density (%)" ) + theme_c
ggsave( file.path( CU, "fig_c14_irf.png" ), g, width = 12.5, height = 4.4, dpi = 160, bg = "white" )

## 3 free windows vs kernels
pw <- melt( win[ outcome %in% names( OL ) ], id.vars = c( "outcome", "window", "lag_mid", "est", "se", "beta_free" ), variable.name = "kernel", value.name = "w" )
KL <- c( k_B = "Geometric (B)", k_P = "Pascal / hump (P)", k_M = "Two rates (M)" )
g <- ggplot( pw, aes( lag_mid ) ) + geom_hline( yintercept = 0, colour = "grey60" ) +
  geom_pointrange( data = unique( pw[ , .( outcome, lag_mid, est, se ) ] ), aes( y = est, ymin = est - 1.96 * se, ymax = est + 1.96 * se ), colour = "grey25", size = 0.3 ) +
  geom_line( aes( y = w, colour = kernel ), linewidth = 0.85 ) +
  facet_wrap( ~ OL[ outcome ], scales = "free_y" ) +
  scale_colour_manual( values = c( k_B = C1, k_P = C3, k_M = C4 ), labels = KL, name = NULL ) +
  labs( title = "Free growth windows against the kernel weights",
        subtitle = "log N on log P and log growth in six windows (metro + year FE). Grey: free estimates (95% CI). Lines: deficit weights implied by each kernel.",
        x = "Years before t (window midpoint)", y = "Effect per log point of growth" ) + theme_c
ggsave( file.path( CU, "fig_c14_windows.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )

## 4 gains vs losses surface (free beta)
ps <- surf[ outcome %in% names( OL ) & h_gain <= 20 & h_loss <= 20 & is.finite( ssr ) ][ , rel := 100 * ( ssr / min( ssr ) - 1 ), by = outcome ]
bc <- res[ spec == "Cb" & outcome %in% names( OL ) & h1 <= 20 & h2 <= 20 ]
g <- ggplot( ps, aes( h_gain, h_loss, fill = pmin( rel, 3 ) ) ) + geom_raster() +
  geom_point( data = bc, aes( h1, h2 ), inherit.aes = FALSE, shape = 4, size = 3, stroke = 1.2 ) +
  facet_wrap( ~ OL[ outcome ] ) + scale_fill_viridis_c( name = "Extra residual SS (%), capped at 3", direction = -1 ) +
  labs( title = "Half-lives after population gains vs losses", subtitle = "log N, free elasticity, metro + year FE. Fit loss relative to the best pair (x).",
        x = "Half-life after gains (years)", y = "Half-life after losses (years)" ) + theme_c + theme( legend.key.width = unit( 1.5, "cm" ) )
ggsave( file.path( CU, "fig_c14_asym_surface.png" ), g, width = 9, height = 4.6, dpi = 160, bg = "white" )
log_msg( "done: ", CU, "/c14_*" )
