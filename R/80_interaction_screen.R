## 80_interaction_screen.R
## Should the multiverse carry interaction terms? A screen of the five benchmark
## covariates (% BA, foundation assets, turnout, % nonwhite, poverty).
##
##   Part 1  interaction matrix, 2022 metros, default model (log Y + log P, which
##           equals the SAMI model by FWL), filer counts and spending excl.
##           hospitals/universities. Each pair is added ONE AT A TIME to the full
##           main-effects model. Cells: naive cor(x*z, y) on raw products,
##           cor on centered products, and the interaction's partial r / t / p
##           (HC1), with and without x^2 and z^2 controls (Ganzach 1997: an
##           interaction between correlated predictors can stand in for curvature).
##           Diagonal = x^2 (curvature); extra row = x * log P (size dependence,
##           the planned "mechanism arm", Data Construction Plan C3).
##   Part 2  all 10 pairwise interactions at once: joint Wald test, adj R2, and
##           10-fold cross-validated RMSE vs the main-effects model.
##   Part 3  mini-multiverse: each interaction one at a time over identification
##           (cs 2022, cs 2019, pooled 2015-22), unit, estimator and outliers, for
##           3 count and 2 spending outcomes: share with the median sign, share
##           significant -- the same summary as the main-effects table.
## Predictors are z-scored in each estimation sample, so an interaction coef is
## the change in x's effect (log points per SD) per SD of z.
## Writes D:/MULTIVERSE/data/interactions/. Usage: Rscript R/80_interaction_screen.R

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( ggplot2 ); library( patchwork ) } )
setFixest_notes( FALSE )
## extensive covariates are logged with the per-capita floor of 40_multiverse.R (flog, run v6 on);
## "Rscript R/80_interaction_screen.R slog" reproduces the first run (log(x + 1)) in interactions/slog/
OLDLOG <- "slog" %in% commandArgs( TRUE )
OD <- file.path( BUILD, "data", "interactions", if ( OLDLOG ) "slog" else "" ); dir.create( OD, recursive = TRUE, showWarnings = FALSE )
slog <- function( x ) log( pmax( x, 0 ) + 1 )
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
lg <- function( x, pop ) if ( OLDLOG ) slog( x ) else flog( x, pop )
winsor <- function( x, p = 0.01 ) { q <- quantile( x, c( p, 1 - p ), na.rm = TRUE ); pmin( pmax( x, q[1] ), q[2] ) }
z <- function( x ) ( x - mean( x ) ) / sd( x )
rs_by <- function( y, logP, yr ) { out <- y; for ( t in unique( yr ) ) { i <- yr == t; out[ i ] <- resid( lm( y[ i ] ~ logP[ i ] ) ) }; out }

FIVE <- c( pct_ba = "% BA", pf = "Foundation assets", turnout = "Turnout", pct_nonwhite = "% nonwhite", poverty_rate = "Poverty" )
X5 <- names( FIVE )
CTRL <- c( "gov", "logP" )                               # government earnings (SAMI) and size
OUTL <- c( n_filer_c3 = "Filer counts", n_filer_c3_nohu = "Filers excl. hosp/univ", n_bmf_c3pc = "Registered stock",
           exp_c3_nohu = "Spending excl. hosp/univ", exp_c3 = "Spending (all)" )

panel <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
panel[ , logP := log( population ) ]
DIVISION <- c( "09" = 1, "23" = 1, "25" = 1, "33" = 1, "44" = 1, "50" = 1, "34" = 2, "36" = 2, "42" = 2,
  "17" = 3, "18" = 3, "26" = 3, "39" = 3, "55" = 3, "19" = 4, "20" = 4, "27" = 4, "29" = 4,
  "31" = 4, "38" = 4, "46" = 4, "10" = 5, "11" = 5, "12" = 5, "13" = 5, "24" = 5, "37" = 5,
  "45" = 5, "51" = 5, "54" = 5, "01" = 6, "21" = 6, "28" = 6, "47" = 6, "05" = 7, "22" = 7,
  "40" = 7, "48" = 7, "04" = 8, "08" = 8, "16" = 8, "30" = 8, "32" = 8, "35" = 8, "49" = 8,
  "56" = 8, "02" = 9, "06" = 9, "15" = 9, "41" = 9, "53" = 9 )
panel[ , division := unname( DIVISION[ state_fips ] ) ]

## estimation sample: default model, SAMI-form extensive predictors (by year)
## sample "no_ep" drops Eagle Pass, TX (cbsa 20580): zero foundation assets every year, so under slog() its
## log-asset SAMI is -16.6 (about 8 SD below the mean) -- a single leverage point
make_data <- function( outcome, years = 2022L, unit = "metro", outlier = "none", sample = "all" ) {
  d <- panel[ year %in% years ]
  if ( sample == "no_ep" ) d <- d[ cbsa != "20580" ]
  if ( unit == "metro" ) d <- d[ cbsa_type == "metro" ]
  d <- d[ , c( "cbsa", "year", "state_fips", "division", "logP", "population", outcome, "pct_ba", "pf_assets", "turnout",
               "pct_nonwhite", "poverty_rate", "gov_earn_sl" ), with = FALSE ]
  d <- d[ complete.cases( d ) & get( outcome ) > 0 ]
  d[ , ly := log( get( outcome ) ) ]
  d[ , sami_y := rs_by( ly, logP, year ) ]
  d[ , pf := rs_by( lg( pf_assets, population ), logP, year ) ]
  d[ , gov := rs_by( lg( gov_earn_sl, population ), logP, year ) ]
  if ( outlier == "winsor1" ) for ( v in c( "ly", "sami_y", X5, "gov" ) ) set( d, j = v, value = winsor( d[[ v ]] ) )
  d[ , raw_pf := pf ]                                    # SAMI pf is already centered; keep a copy for the naive product
  for ( v in c( X5, CTRL ) ) set( d, j = paste0( "z_", v ), value = z( d[[ v ]] ) )
  d[]
}

MAIN <- paste( paste0( "z_", c( X5, CTRL ) ), collapse = " + " )
fe_of <- function( est, pooled ) {
  f <- switch( est, ols = "", state_fe = "state_fips", div_fe = "division" )
  if ( pooled ) f <- paste( c( if ( nzchar( f ) ) f, "year" ), collapse = " + " )
  if ( nzchar( f ) ) paste( "|", f ) else ""
}
vc_of <- function( est, pooled ) if ( est != "ols" ) ~state_fips else if ( pooled ) ~cbsa else "hetero"

## one interaction term (x:z, or x^2 when x == z) added to the full model
fit_int <- function( d, x, w, est = "ols", pooled = FALSE, squares = FALSE ) {
  d <- copy( d )
  zx <- d[[ paste0( "z_", x ) ]]; zw <- d[[ paste0( "z_", w ) ]]
  d[ , iv := zx * zw ]; d[ , sqx := zx^2 ]; d[ , sqw := zw^2 ]
  extra <- if ( squares && x != w ) " + sqx + sqw" else ""
  f <- as.formula( paste( "ly ~", MAIN, "+ iv", extra, fe_of( est, pooled ) ) )
  r <- tryCatch( feols( f, data = d, vcov = vc_of( est, pooled ) ), error = function( e ) NULL )
  if ( is.null( r ) ) return( NULL )
  ct <- coeftable( r ); k <- which( rownames( ct ) == "iv" )
  if ( !length( k ) ) return( NULL )
  df_res <- ( if ( pooled ) uniqueN( d$cbsa ) else nobs( r ) ) - length( coef( r ) ) - 1
  t <- ct[ k, 3 ]
  data.table( estimate = ct[ k, 1 ], se = ct[ k, 2 ], t = t, p = ct[ k, 4 ], pr = t / sqrt( t^2 + max( df_res, 1 ) ), n = nobs( r ) )
}

PAIRS <- rbind( as.data.table( t( combn( X5, 2 ) ) )[ , kind := "pair" ],
                data.table( V1 = X5, V2 = "logP", kind = "x_logP" ),
                data.table( V1 = X5, V2 = X5, kind = "square" ) )
setnames( PAIRS, c( "V1", "V2" ), c( "x", "w" ) )

## ===========================================================================
## Part 1: interaction matrix, 2022 metros
## ===========================================================================
log_msg( "Part 1: interaction matrix" )
P1 <- rbindlist( lapply( c( "all", "no_ep" ), function( smp ) rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( o ) {
  d <- make_data( o, sample = smp )
  rbindlist( lapply( seq_len( nrow( PAIRS ) ), function( i ) {
    x <- PAIRS$x[ i ]; w <- PAIRS$w[ i ]
    a <- fit_int( d, x, w ); b <- if ( PAIRS$kind[ i ] == "pair" ) fit_int( d, x, w, squares = TRUE ) else NULL
    rawx <- d[[ x ]]; raww <- d[[ w ]]
    data.table( sample = smp, outcome = o, x = x, w = w, kind = PAIRS$kind[ i ],
                cor_naive = cor( rawx * raww, d$sami_y ),                      # the proposed cell: cor(x*z, y)
                cor_centered = cor( d[[ paste0( "z_", x ) ]] * d[[ paste0( "z_", w ) ]], d$sami_y ),
                cor_naive_with_x = cor( rawx * raww, rawx ),                   # why the naive cell misleads
                b = a$estimate, se = a$se, t = a$t, p = a$p, pr = a$pr, n = a$n,
                b_sq = if ( is.null( b ) ) NA_real_ else b$estimate, p_sq = if ( is.null( b ) ) NA_real_ else b$p )
  } ) )
} ) ) ) )
P1[ , `:=`( p_bh = p.adjust( p, "BH" ), p_holm = p.adjust( p, "holm" ) ), by = .( sample, outcome ) ]   # 20 tests per outcome
fwrite( P1, file.path( OD, "p1_interaction_matrix.csv" ) )
print( P1[ , .( sample, outcome, x, w, kind, cor_naive = round( cor_naive, 2 ), cor_c = round( cor_centered, 2 ),
                b = round( b, 3 ), pr = round( pr, 3 ), p = signif( p, 2 ), p_bh = signif( p_bh, 2 ), p_sq = signif( p_sq, 2 ) ) ] )

## ===========================================================================
## Part 2: all ten pairwise interactions at once
## ===========================================================================
log_msg( "Part 2: joint test and cross-validation" )
cv_rmse <- function( d, f, K = 10, reps = 20 ) {
  mean( vapply( seq_len( reps ), function( r ) {
    set.seed( r ); fold <- sample( rep( 1:K, length.out = nrow( d ) ) )
    e <- unlist( lapply( 1:K, function( k ) { m <- lm( f, d[ fold != k ] ); d$ly[ fold == k ] - predict( m, d[ fold == k ] ) } ) )
    sqrt( mean( e^2 ) ) }, numeric( 1 ) ) )
}
PW <- paste( apply( combn( X5, 2 ), 2, function( p ) sprintf( "z_%s:z_%s", p[1], p[2] ) ), collapse = " + " )
LP <- paste( sprintf( "z_%s:z_logP", X5 ), collapse = " + " )
P2 <- rbindlist( lapply( c( "all", "no_ep" ), function( smp ) rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( o ) {
  d <- make_data( o, sample = smp )
  fm <- list( main = paste( "ly ~", MAIN ), pairwise = paste( "ly ~", MAIN, "+", PW ),
              x_logP = paste( "ly ~", MAIN, "+", LP ), both = paste( "ly ~", MAIN, "+", PW, "+", LP ) )
  m0 <- feols( as.formula( fm$main ), d, vcov = "hetero" )
  rbindlist( lapply( names( fm ), function( k ) {
    m <- feols( as.formula( fm[[ k ]] ), d, vcov = "hetero" )
    added <- setdiff( names( coef( m ) ), names( coef( m0 ) ) )
    wt <- if ( length( added ) ) wald( m, keep = paste0( "^\\Q", added, "\\E$", collapse = "|" ), print = FALSE ) else NULL
    data.table( sample = smp, outcome = o, model = k, k_added = length( added ), r2 = r2( m, "r2" ), adj_r2 = r2( m, "ar2" ),
                wald_F = if ( is.null( wt ) ) NA_real_ else wt$stat, wald_p = if ( is.null( wt ) ) NA_real_ else wt$p,
                cv_rmse = cv_rmse( d, as.formula( fm[[ k ]] ) ), n = nobs( m ) )
  } ) )
} ) ) ) )
P2[ , cv_change_pct := 100 * ( cv_rmse / cv_rmse[ model == "main" ] - 1 ), by = .( sample, outcome ) ]
fwrite( P2, file.path( OD, "p2_joint_cv.csv" ) ); print( P2[ , lapply( .SD, function( v ) if ( is.numeric( v ) ) round( v, 4 ) else v ) ] )

## ===========================================================================
## Part 3: mini-multiverse stability, one interaction at a time
## ===========================================================================
log_msg( "Part 3: stability across forks" )
G <- CJ( outcome = names( OUTL ), ident = c( "cs2022", "cs2019", "pooled" ), unit = c( "metro", "cbsa" ),
         est = c( "ols", "state_fe", "div_fe" ), outlier = c( "none", "winsor1" ), sample = c( "all", "no_ep" ) )
P3 <- rbindlist( lapply( seq_len( nrow( G ) ), function( g ) {
  s <- G[ g ]
  yrs <- switch( s$ident, cs2022 = 2022L, cs2019 = 2019L, pooled = 2015:2022 )
  d <- make_data( s$outcome, yrs, s$unit, s$outlier, s$sample )
  rbindlist( lapply( seq_len( nrow( PAIRS ) ), function( i ) {
    a <- fit_int( d, PAIRS$x[ i ], PAIRS$w[ i ], s$est, pooled = s$ident == "pooled" )
    if ( is.null( a ) ) NULL else cbind( s, PAIRS[ i ], a )
  } ) )
} ) )
P3[ , construct := fifelse( grepl( "^exp", outcome ), "spending", "counts" ) ]
fwrite( P3, file.path( OD, "p3_stability.csv" ) )
S3 <- P3[ , { ms <- sign( median( pr ) )
  .( specs = .N, median_pr = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
     same_sign = mean( sign( pr ) == ms ), signif = mean( p < .05 ), signif_same = mean( p < .05 & sign( pr ) == ms ) ) },
  by = .( sample, construct, x, w, kind ) ]
fwrite( S3, file.path( OD, "p3_stability_summary.csv" ) )
print( S3[ order( sample, construct, -abs( median_pr ) ), lapply( .SD, function( v ) if ( is.numeric( v ) ) round( v, 3 ) else v ) ] )

## ===========================================================================
## Figure: interaction matrices (partial r of the added term, 2022 metros)
## ===========================================================================
LAB <- c( FIVE, logP = "log population" )
fig <- copy( P1 )
fig[ , `:=`( col = factor( LAB[ x ], levels = LAB ), row = factor( LAB[ w ], levels = rev( LAB ) ) ) ]
fig[ , star := fcase( p_bh < .05, "**", p < .05, "*", default = "" ) ]
fig[ , lab := sprintf( "%.2f%s", pr, star ) ]
fig[ , panel := factor( c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. hospitals, universities" )[ outcome ],
                        levels = c( "Filer counts", "Spending excl. hospitals, universities" ) ) ]
fig[ , smp := factor( c( all = "All 387 metros", no_ep = "Without Eagle Pass, TX" )[ sample ],
                      levels = c( "All 387 metros", "Without Eagle Pass, TX" ) ) ]
lim <- max( abs( fig$pr ), na.rm = TRUE )
p <- ggplot( fig, aes( col, row, fill = pr ) ) +
  geom_tile( colour = "#fcfcfb", linewidth = 1.5 ) +
  geom_text( aes( label = lab ), size = 3.1, colour = "grey15" ) +
  scale_fill_gradient2( low = "#2a78d6", mid = "#f0efec", high = "#e34948", midpoint = 0, limits = c( -lim, lim ),
                        name = "Partial r of\nadded term" ) +
  facet_grid( smp ~ panel ) + coord_equal() +
  labs( title = "Interaction screen: each term added one at a time to the full 2022 model",
        subtitle = "Off-diagonal: x × z; diagonal: x²; bottom row: x × log population. * p < .05 (HC1); ** survives Benjamini-Hochberg over 20 tests",
        x = NULL, y = NULL ) +
  theme_minimal( base_size = 10.5 ) +
  theme( panel.grid = element_blank(), axis.text.x = element_text( angle = 35, hjust = 1 ),
         plot.title.position = "plot", plot.subtitle = element_text( colour = "grey30", size = 8.5 ),
         strip.text = element_text( face = "bold" ), plot.background = element_rect( fill = "#fcfcfb", colour = NA ) )
ggsave( file.path( OD, "fig_interaction_matrix.png" ), p, width = 10.5, height = 10, dpi = 200 )
log_msg( "done: ", OD )
