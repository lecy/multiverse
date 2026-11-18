## 87_poverty_size_figure.R
## Figure for the size heterogeneity in the poverty term (D2 Evidence 6; 86 part B).
## 86 split metros at 250k and found poverty negative for filers below, positive above.
## Two panels:
##   A. Size bins: the 387 metros split at 250k (as in 86 B) and each half split again at its
##      median, giving four separate bins of 96-97 metros (bins 1-2 = under 250k, 3-4 = 250k and
##      over). In each bin the default-model metro specs (cs 2022 + pooled; OLS and division FE
##      only: state FE and state RE spend too many df on ~97 metros; no winsorizing) are re-fit
##      and the poverty partial r summarized (median and interquartile range across specs).
##      (Replaced on 2026-11-16 a rolling-window version with 150-metro windows, whose overlap
##      and window-median axis made it hard to read; its fits stay in c_rolling.csv.)
##   B. Added-variable plot for one representative spec (filers, cs 2022): filer SAMI vs poverty,
##      both residualized on the other default covariates separately within each size group,
##      with each group's fitted line.
## Outputs D:/MULTIVERSE/data/sensitivity/fig_poverty_size{,_bins,_coef,_window_coef}.{png,pdf}, c_bins.csv, c_local_coef.csv,
## c_split_coef.csv, c_rolling_grass.csv, c_group_coef.csv.
## Usage: Rscript R/notes/87_poverty_size_figure.R [fit] [workers]   ("fit" re-runs panel A's fits)

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( ggplot2 ); library( patchwork ) } )
args <- commandArgs( TRUE )
WORKERS <- suppressWarnings( as.integer( args[ grepl( "^[0-9]+$", args ) ] ) ); if ( !length( WORKERS ) ) WORKERS <- 12L
OD <- file.path( BUILD, "data", "sensitivity" ); dir.create( OD, showWarnings = FALSE )
OUTS <- c( n_filer_c3 = "Filing organizations", exp_c3_nohu = "Spending (excl. hospitals, universities)",
           n_bmf_c3pc = "Registered organizations" )

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
panel <- load_panel()
p22 <- panel[ year == 2022 & cbsa_type == "metro" ][ order( population ) ]

## ---- A: size-bin fits -------------------------------------------------------------------
p22[ , half := fifelse( population < 2.5e5, 1L, 2L ) ]
p22[ , bin := ( half - 1L ) * 2L + fifelse( seq_len( .N ) <= ceiling( .N / 2 ), 1L, 2L ), by = half ]
bins <- p22[ , .( metros = .N, pop_lo = min( population ), pop_hi = max( population ) ), keyby = bin ]
fmt <- function( x ) scales::label_number( accuracy = 1, scale_cut = scales::cut_short_scale() )( signif( x, 2 ) )
bins[ , label := sprintf( "%s-%s\n%d metros", fmt( pop_lo ), fmt( pop_hi ), metros ) ]
print( bins )
fa <- file.path( OD, "c_bins.csv" )
if ( "fit" %in% args || !file.exists( fa ) ) {
  sp <- grid[ drop == "none" & op == "default" & outlier == "none" & outcome %in% names( OUTS ) & cs_year == 2022L &
              unit == "metro" & ident != "within" & est %in% c( "ols", "div_fe" ) ]
  keepsets <- split( p22$cbsa, p22$bin )
  jobs <- CJ( spec_id = sp$spec_id, w = seq_along( keepsets ) )
  log_msg( "A: ", nrow( sp ), " specs x ", length( keepsets ), " bins = ", nrow( jobs ), " fits" )
  run_job <- function( i ) {
    j <- jobs[ i ]; s <- grid[ spec_id == j$spec_id ]
    r <- tryCatch( run_spec( s, panel[ cbsa %in% keepsets[[ j$w ]] ] ), error = function( e ) NULL )
    if ( is.null( r ) ) return( NULL )
    r[ , `:=`( spec_id = j$spec_id, w = j$w, k = .N ) ][ term == "poverty_rate" ] }
  environment( run_job ) <- globalenv()
  assign( "jobs", jobs, envir = globalenv() ); assign( "keepsets", keepsets, envir = globalenv() )
  assign( "run_job", run_job, envir = globalenv() )
  cl <- makePSOCKcluster( WORKERS )
  invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
    setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
    source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
  clusterExport( cl, c( "panel", "grid", "jobs", "keepsets", "run_job" ), envir = globalenv() )
  t0 <- proc.time()
  r <- rbindlist( parLapplyLB( cl, seq_len( nrow( jobs ) ), run_job, chunk.size = 100 ), fill = TRUE )
  stopCluster( cl ); log_msg( "A: fits in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min" )
  r <- merge( r, grid[ , .( spec_id, ident, outcome ) ], by = "spec_id" )
  r[ , n_units := n / fifelse( ident == "pooled", 8, 1 ) ]
  r[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
  roll <- r[ , .( specs = .N, med = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
                  share_pos_sig = mean( pr > 0 & p < .05 ), share_neg_sig = mean( pr < 0 & p < .05 ) ),
             keyby = .( outcome, ident, w ) ]
  setnames( roll, "w", "bin" )
  fwrite( roll, fa )
}
roll <- merge( fread( fa ), bins, by = "bin" )
roll[ , outcome := factor( OUTS[ outcome ], levels = OUTS ) ]
roll[ , label := factor( label, levels = bins$label ) ]

## ---- B: added-variable plot, one representative spec ---------------------------------------
## Spec chosen as the one closest to the median poverty partial r in both size groups (86 part B,
## b_coefs.parquet): SAMI DV, "log" predictors (poverty and other rates as levels, dollar totals
## logged with the floor), no log P on the right, OLS. Everything is built within each size group,
## as run_spec does on a subset (the SAMI first stage and the flog floor are sample-specific).
s0 <- grid[ drop == "none" & op == "default" & outlier == "none" & outcome == "n_filer_c3" & ident == "cs" &
            cs_year == 2022L & unit == "metro" & est == "ols" & dv_form == "sami" & pred_form == "log" & rhs_logP == FALSE ]
stopifnot( nrow( s0 ) == 1 )
xs <- covars_for( as.list( s0 ) )
d <- p22[ , c( "cbsa", "population", "logP", "year", "n_filer_c3", xs ), with = FALSE ]
d <- d[ complete.cases( d ) & n_filer_c3 > 0 ]
d[ , size := factor( fifelse( population < 2.5e5, "Under 250k", "250k and over" ), levels = c( "Under 250k", "250k and over" ) ) ]
d <- rbindlist( lapply( split( d, by = "size" ), function( g ) {
  g[ , y := resid_logP( log( n_filer_c3 ), logP, year ) ]
  for ( v in xs ) set( g, j = paste0( "x_", v ), value = if ( COVS[ var == v, type ] == "B" ) flog( g[[ v ]], g$population ) else g[[ v ]] )
  others <- paste0( "x_", setdiff( xs, "poverty_rate" ) )
  g[ , `:=`( y_res = resid( lm( reformulate( others, "y" ), data = g ) ),
             pov_res = resid( lm( reformulate( others, "x_poverty_rate" ), data = g ) ) ) ]
  g } ) )
## check against run_spec on each subset: the slope of y_res on pov_res equals the poverty coefficient
chk <- rbindlist( lapply( levels( d$size ), function( g ) {
  rr <- run_spec( s0, panel[ cbsa %in% d[ size == g, cbsa ] ] )[ term == "poverty_rate" ]
  ft <- lm( y_res ~ pov_res, data = d[ size == g ] )
  data.table( size = g, n = d[ size == g, .N ], run_spec_b = rr$estimate, avplot_b = coef( ft )[[ 2 ]],
              pr = rr$stat / sqrt( rr$stat^2 + rr$n - length( xs ) - 1 ) ) } ) )
cat( "\n== B: added-variable check (run_spec_b should equal avplot_b) ==\n" ); print( chk )
strip_lab <- setNames( chk[ , sprintf( "%s: %d metros, partial r %+.2f", size, n, pr ) ], chk$size )

## ---- A (main figure): component-plus-residual plot over all metros ---------------------------
## Same spec as B, fit once on all 387 metros. y = filer SAMI with the fitted contribution of every
## other default covariate removed (model residual + b_poverty * centred poverty), x = poverty rate
## itself, so the linear slope is the full-sample poverty coefficient and a lowess shows any bend.
## Points are coloured by the four size bins of the bin panel; thin lines are lowess fits per bin.
da <- p22[ , c( "cbsa", "population", "logP", "year", "bin", "n_filer_c3", xs ), with = FALSE ]
da <- da[ complete.cases( da ) & n_filer_c3 > 0 ]
da[ , y := resid_logP( log( n_filer_c3 ), logP, year ) ]
for ( v in xs ) set( da, j = paste0( "x_", v ), value = if ( COVS[ var == v, type ] == "B" ) flog( da[[ v ]], da$population ) else da[[ v ]] )
fa_lm <- lm( reformulate( paste0( "x_", xs ), "y" ), data = da )
b_pov <- coef( fa_lm )[[ "x_poverty_rate" ]]
da[ , y_cr := resid( fa_lm ) + b_pov * ( x_poverty_rate - mean( x_poverty_rate ) ) ]
rr_all <- run_spec( s0, panel[ cbsa %in% da$cbsa ] )[ term == "poverty_rate" ]
pr_all <- rr_all$stat / sqrt( rr_all$stat^2 + rr_all$n - length( xs ) - 1 )
cat( sprintf( "\n== A: all metros: n %d, b (lm) %.3f, b (run_spec) %.3f, partial r %+.3f ==\n", nrow( da ), b_pov, rr_all$estimate, pr_all ) )
da[ , bin_lab := factor( bins$label[ bin ], levels = bins$label ) ]
da[ , bin_lab := factor( sub( "\n", ", ", bin_lab ), levels = sub( "\n", ", ", bins$label ) ) ]

## ---- figure -----------------------------------------------------------------------------
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRIDC <- "#e6e5e1"
COL3 <- c( "Filing organizations" = "#2a78d6", "Spending (excl. hospitals, universities)" = "#eb6834",
           "Registered organizations" = "#1baf7a" )
COL2 <- c( "Under 250k" = "#2a78d6", "250k and over" = "#2a78d6" )   # both panels are filers: keep the filer colour from A
th <- theme_minimal( base_size = 10, base_family = "sans" ) +
  theme( panel.grid.minor = element_blank(), panel.grid.major = element_line( colour = GRIDC, linewidth = .3 ),
         axis.text = element_text( colour = INK2 ), axis.title = element_text( colour = INK2, size = 9 ),
         plot.title = element_text( colour = INK, face = "bold", size = 10.5 ),
         plot.subtitle = element_text( colour = INK2, size = 8.5 ), legend.position = "top",
         legend.title = element_blank(), legend.text = element_text( colour = INK2, size = 8.5 ),
         strip.text = element_text( colour = INK, hjust = 0, face = "bold", size = 9 ),
         plot.background = element_rect( fill = "white", colour = NA ) )

dg <- position_dodge( width = .6 )
pa <- ggplot( roll, aes( label, med, colour = outcome ) ) +
  geom_hline( yintercept = 0, colour = INK2, linewidth = .4 ) +
  geom_linerange( aes( ymin = q25, ymax = q75 ), position = dg, linewidth = .9, alpha = .45 ) +
  geom_point( position = dg, size = 2.4 ) +
  geom_vline( xintercept = 2.5, colour = INK2, linewidth = .3, linetype = "22" ) +
  annotate( "text", x = 2.5, y = Inf, label = "250k", hjust = -.15, vjust = 1.4, size = 2.8, colour = INK2 ) +
  facet_wrap( ~ ident, labeller = as_labeller( c( cs = "Cross-section 2022", pooled = "Pooled 2015-22" ) ) ) +
  scale_colour_manual( values = COL3 ) +
  labs( x = "Metros in four size bins (2022 population)",
        y = "Partial r of poverty rate",
        title = "A. Poverty turns negative for filers only, and only in smaller metros",
        subtitle = "Each bin fit on its own metros with all default covariates. Dot: median across specs (OLS, division FE); bar: interquartile range" ) +
  th + theme( panel.grid.major.x = element_blank(), axis.text.x = element_text( size = 7.5 ) )

pb <- ggplot( d, aes( pov_res, y_res, colour = size ) ) +
  geom_hline( yintercept = 0, colour = GRIDC ) + geom_vline( xintercept = 0, colour = GRIDC ) +
  geom_point( size = 1.4, alpha = .55, stroke = 0 ) +
  geom_smooth( method = "lm", formula = y ~ x, se = TRUE, linewidth = .8, alpha = .15, aes( fill = size ) ) +

  facet_wrap( ~ size, labeller = as_labeller( strip_lab ) ) +
  scale_colour_manual( values = COL2, guide = "none" ) + scale_fill_manual( values = COL2, guide = "none" ) +
  labs( x = "Poverty rate (share), net of the other default covariates",
        y = "Filer density (SAMI), net of the same",
        title = "B. The same model fit separately: filers vs poverty, 2022",
        subtitle = "Added-variable plot for the spec nearest both groups' medians (SAMI outcome, OLS); fit within each group" ) + th

SEQ4 <- setNames( c( "#86b6ef", "#3987e5", "#1c5cab", "#0d366b" ), levels( da$bin_lab ) )   # sequential blue: small -> large
pc <- ggplot( da, aes( x_poverty_rate, y_cr ) ) +
  geom_hline( yintercept = 0, colour = GRIDC ) +
  geom_point( aes( colour = bin_lab ), size = 1.5, alpha = .8, stroke = 0 ) +
  geom_smooth( aes( colour = bin_lab ), method = "loess", formula = y ~ x, span = .9, method.args = list( degree = 1 ),
               se = FALSE, linewidth = .55 ) +
  geom_smooth( method = "loess", formula = y ~ x, span = .75, method.args = list( degree = 1 ),
               colour = INK, fill = INK2, alpha = .12, linewidth = 1 ) +
  scale_colour_manual( values = SEQ4, name = "2022 population" ) +
  scale_x_continuous( labels = scales::label_percent( accuracy = 1 ) ) +
  guides( colour = guide_legend( override.aes = list( linewidth = .8, size = 2 ) ) ) +
  labs( x = "Poverty rate, 2022", y = "Filer density (SAMI), net of the other default covariates",
        title = sprintf( "A. Filer density vs poverty, all %d metros (partial r %+.2f)", nrow( da ), pr_all ),
        subtitle = "Component-plus-residual plot. Black: lowess on all metros (band: 95% CI); thin lines: lowess within each size bin" ) +
  th + theme( legend.title = element_text( colour = INK2, size = 8.5 ) )

fig <- pc / pb + plot_layout( heights = c( 1.1, 1 ) )
ggsave( file.path( OD, "fig_poverty_size.png" ), fig, width = 8, height = 8.5, dpi = 200, device = ragg::agg_png, bg = "white" )
ggsave( file.path( OD, "fig_poverty_size.pdf" ), fig, width = 8, height = 8.5 )
## the size-bin version (bins on top), kept as its own figure
fig2 <- pa / pb + plot_layout( heights = c( 1, 1 ) )
ggsave( file.path( OD, "fig_poverty_size_bins.png" ), fig2, width = 8, height = 8.5, dpi = 200, device = ragg::agg_png, bg = "white" )
ggsave( file.path( OD, "fig_poverty_size_bins.pdf" ), fig2, width = 8, height = 8.5 )

## ---- coefficient version (fig_poverty_size_coef) ------------------------------------------
## Regression coefficients are in different units across forks (pc vs log outcomes, raw vs SAMI
## predictors), so they cannot be summarized across specs the way partial r is. This version uses
## the one spec of panel B (SAMI outcome; poverty and other rates as levels, dollar totals logged
## with the floor; no log P on the right; OLS; 2022) and lets every coefficient vary smoothly with
## size: at each target population p0 the model is re-fit with Gaussian kernel weights in log
## population (bandwidth H), including the SAMI first stage (weighted regression of log y on log P),
## as the 250k split does with 0/1 weights. SEs: heteroskedasticity-robust, ignoring the first stage
## (as in run_spec). Outcomes: filers, grassroots (registered public charities that do not file =
## BMF stock - filers; 58 part G) and spending excl. hospitals and universities.
## Bottom: added-variable plot on all metros (y and poverty residualized on the other covariates in
## the full sample), lowess, and the two split-model slopes drawn through each group centre.
H <- 0.5
OUTC <- c( n_filer_c3 = "Filing organizations", n_grass = "Grassroots (registered, not filing)",
           exp_c3_nohu = "Spending (excl. hospitals, universities)" )
q <- p22[ , c( "cbsa", "population", "logP", "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu", xs ), with = FALSE ]
q[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
q <- q[ complete.cases( q ) ]
for ( v in xs ) set( q, j = paste0( "x_", v ), value = if ( COVS[ var == v, type ] == "B" ) flog( q[[ v ]], q$population ) else q[[ v ]] )
XV <- paste0( "x_", xs ); XO <- setdiff( XV, "x_poverty_rate" )
cat( "\nmetros with grassroots <= 0 (dropped for that outcome):", q[ n_grass <= 0, .N ], "\n" )
local_fit <- function( yv, w, keep ) {
  keep <- keep & q[[ yv ]] > 0 & w > 0
  dd <- q[ keep ]; dd[ , w := w[ keep ] ]
  dd[ , ly := log( dd[[ yv ]] ) ]
  dd[ , sami := resid( lm( ly ~ logP, data = dd, weights = w ) ) ]
  f <- feols( reformulate( XV, "sami" ), data = dd, weights = ~w, vcov = "hetero" )
  ct <- coeftable( f )[ "x_poverty_rate", ]
  data.table( b = ct[[ 1 ]], se = ct[[ 2 ]], n_eff = sum( dd$w )^2 / sum( dd$w^2 ) )
}
g0 <- seq( log( 7e4 ), log( 2.5e6 ), length.out = 60 )   # above ~2.5M the effective n falls below ~55 metros
cf <- rbindlist( lapply( names( OUTC ), function( yv ) rbindlist( lapply( g0, function( g ) {
  cbind( outcome = yv, p0 = exp( g ), local_fit( yv, dnorm( ( q$logP - g ) / H ), rep( TRUE, nrow( q ) ) ) ) } ) ) ) )
sp2 <- rbindlist( lapply( names( OUTC ), function( yv ) rbindlist( lapply( c( "Under 250k", "250k and over" ), function( s ) {
  k <- if ( s == "Under 250k" ) q$population < 2.5e5 else q$population >= 2.5e5
  cbind( outcome = yv, size = s, lo = min( q$population[ k ] ), hi = max( q$population[ k ] ), local_fit( yv, rep( 1, nrow( q ) ), k ) ) } ) ) ) )
cat( "\n== coefficient version: split fits (b per unit poverty share; filers should be close to the B check) ==\n" ); print( sp2 )
cat( "\n== effective n of the kernel at selected sizes ==\n" ); print( cf[ outcome == "n_filer_c3" ][ c( 1, 15, 30, 45, 60 ), .( p0 = round( p0 ), n_eff = round( n_eff ) ) ] )
fwrite( cf, file.path( OD, "c_local_coef.csv" ) ); fwrite( sp2, file.path( OD, "c_split_coef.csv" ) )

COLO <- c( "Filing organizations" = "#2a78d6", "Grassroots (registered, not filing)" = "#1baf7a",
           "Spending (excl. hospitals, universities)" = "#eb6834" )
pan <- function( o ) factor( fifelse( o == "n_filer_c3", "Filing organizations", "Grassroots and spending" ) )
cf[ , `:=`( lab = factor( OUTC[ outcome ], levels = OUTC ), panel = pan( outcome ), est = b / 10, lo95 = ( b - 1.96 * se ) / 10, hi95 = ( b + 1.96 * se ) / 10 ) ]
sp2[ , `:=`( lab = factor( OUTC[ outcome ], levels = OUTC ), panel = pan( outcome ), est = b / 10 ) ]
rug <- rbind( q[ , .( population, panel = factor( "Filing organizations" ) ) ], q[ , .( population, panel = factor( "Grassroots and spending" ) ) ] )
pt <- ggplot( cf, aes( p0, est ) ) +
  geom_hline( yintercept = 0, colour = INK2, linewidth = .4 ) +
  geom_vline( xintercept = 2.5e5, colour = INK2, linewidth = .3, linetype = "22" ) +
  geom_ribbon( aes( ymin = lo95, ymax = hi95, fill = lab ), alpha = .14 ) +
  geom_segment( data = sp2, aes( x = lo, xend = hi, y = est, yend = est, colour = lab ), linewidth = .5, linetype = "42" ) +
  geom_line( aes( colour = lab ), linewidth = .9 ) +
  geom_rug( data = rug, aes( x = population ), inherit.aes = FALSE, sides = "b", colour = INK2, alpha = .35, length = unit( .02, "npc" ) ) +
  facet_wrap( ~ panel ) +
  scale_x_log10( breaks = c( 1e5, 2.5e5, 1e6, 4e6, 2e7 ), labels = c( "100K", "250K", "1M", "4M", "20M" ) ) +
  scale_colour_manual( values = COLO ) + scale_fill_manual( values = COLO ) +
  labs( x = "Metro population, 2022 (log scale; ticks along the bottom are metros; curves stop at 2.5M, where few metros remain)",
        y = "Change in log density per\n10 points more poverty",
        title = "A. Poverty coefficient by metro size, 2022",
        subtitle = sprintf( "Solid: model re-fit with metros weighted by closeness in size (Gaussian, bandwidth %.1f log points); band: 95%% CI.\nDashed: the same model fit separately below and above 250k", H ) ) + th

af <- copy( q[ n_filer_c3 > 0 ] )
af[ , y := resid( lm( log( n_filer_c3 ) ~ logP ) ) ]
af[ , `:=`( y_res = resid( lm( reformulate( XO, "y" ), data = af ) ), pov_res = resid( lm( reformulate( XO, "x_poverty_rate" ), data = af ) ) ) ]
af[ , size := factor( fifelse( population < 2.5e5, "Under 250k", "250k and over" ), levels = c( "Under 250k", "250k and over" ) ) ]
b_full <- coef( lm( y_res ~ pov_res, data = af ) )[[ 2 ]]
gl <- merge( af[ , .( xc = mean( pov_res ), yc = mean( y_res ), x1 = quantile( pov_res, .025 ), x2 = quantile( pov_res, .975 ) ), by = size ],
             sp2[ outcome == "n_filer_c3", .( size, b ) ], by = "size" )
gl[ , `:=`( y1 = yc + b * ( x1 - xc ), y2 = yc + b * ( x2 - xc ) ) ]
gl[ , size := factor( size, levels = levels( af$size ) ) ]
COLS <- c( "Under 250k" = "#6da7ec", "250k and over" = "#104281" )
leg <- setNames( gl[ order( size ), sprintf( "%s metros: separate fit, %+.2f per 10 pts", size, b / 10 ) ], levels( af$size ) )
pb2 <- ggplot( af, aes( pov_res * 100, y_res ) ) +
  geom_hline( yintercept = 0, colour = GRIDC ) + geom_vline( xintercept = 0, colour = GRIDC ) +
  geom_point( aes( colour = size ), size = 1.5, alpha = .7, stroke = 0 ) +
  geom_smooth( method = "loess", formula = y ~ x, span = .75, method.args = list( degree = 1 ),
               colour = INK, fill = INK2, alpha = .12, linewidth = .9 ) +
  geom_segment( data = gl, aes( x = x1 * 100, xend = x2 * 100, y = y1, yend = y2, colour = size ), linewidth = 1.3 ) +
  scale_colour_manual( values = COLS, labels = leg ) +
  guides( colour = guide_legend( override.aes = list( linewidth = 1.2, size = 2 ) ) ) +
  labs( x = "Poverty rate (percentage points), net of the other default covariates",
        y = "Filer density (SAMI), net of the same",
        title = sprintf( "B. Filers vs poverty, all %d metros: lowess (black) and the two split models", nrow( af ) ),
        subtitle = sprintf( "Added-variable plot fit on all metros (slope %+.3f per 10 pts). Coloured lines: slopes of the separate fits, through each group centre", b_full / 10 ) ) + th
fig3 <- pt / pb2 + plot_layout( heights = c( 1, 1.1 ) )
ggsave( file.path( OD, "fig_poverty_size_coef.png" ), fig3, width = 8.5, height = 9, dpi = 200, device = ragg::agg_png, bg = "white" )
ggsave( file.path( OD, "fig_poverty_size_coef.pdf" ), fig3, width = 8.5, height = 9 )


## ---- window + coefficient-plot version (fig_poverty_size_window_coef) -------------------------
## Top left: the rolling-window partial r of the first version (windows of 150 adjacent metros,
## step 10; median and IQR across the default-model metro specs), cross-section 2022 only, for
## filers, grassroots (registered, not filing) and spending. Filers and spending come from
## c_rolling.csv (first version); grassroots is fit here on the registered-stock specs with the
## outcome swapped (c_rolling_grass.csv). Top right: poverty coefficient (per 10 points) of the
## panel-B spec fit on each size group (four bins, the 250k halves, all metros), 95% CI.
## Bottom: the combined added-variable plot (pb2).
WIN <- 150L; STEP <- 10L
starts <- unique( c( seq( 1L, nrow( p22 ) - WIN + 1L, by = STEP ), nrow( p22 ) - WIN + 1L ) )
wins <- data.table( w = seq_along( starts ), pop_median = sapply( starts, function( s ) median( p22$population[ s:( s + WIN - 1L ) ] ) ),
                    pop_lo = p22$population[ starts ], pop_hi = p22$population[ starts + WIN - 1L ] )
fg <- file.path( OD, "c_rolling_grass.csv" )
if ( "fit" %in% args || !file.exists( fg ) ) {
  panel[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
  specs <- grid[ drop == "none" & op == "default" & outlier == "none" & outcome == "n_bmf_c3pc" & cs_year == 2022L &
                 unit == "metro" & ident == "cs" ]
  specs[ , outcome := "n_grass" ]
  keepsets <- lapply( starts, function( s ) p22$cbsa[ s:( s + WIN - 1L ) ] )
  jobs <- CJ( spec_id = specs$spec_id, w = wins$w )
  log_msg( "grassroots windows: ", nrow( specs ), " specs x ", nrow( wins ), " windows = ", nrow( jobs ), " fits" )
  run_job2 <- function( i ) {
    j <- jobs[ i ]; s <- specs[ spec_id == j$spec_id ]
    r <- tryCatch( run_spec( s, panel[ cbsa %in% keepsets[[ j$w ]] ] ), error = function( e ) NULL )
    if ( is.null( r ) ) return( NULL )
    r[ , `:=`( spec_id = j$spec_id, w = j$w, k = .N ) ][ term == "poverty_rate" ] }
  environment( run_job2 ) <- globalenv()
  for ( nm in c( "jobs", "keepsets", "specs", "run_job2" ) ) assign( nm, get( nm ), envir = globalenv() )
  cl <- makePSOCKcluster( WORKERS )
  invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
    setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
    source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
  clusterExport( cl, c( "panel", "specs", "jobs", "keepsets", "run_job2" ), envir = globalenv() )
  rg <- rbindlist( parLapplyLB( cl, seq_len( nrow( jobs ) ), run_job2, chunk.size = 50 ), fill = TRUE )
  stopCluster( cl )
  rg[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
  roll_g <- rg[ , .( specs = .N, med = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
                     share_pos_sig = mean( pr > 0 & p < .05 ), share_neg_sig = mean( pr < 0 & p < .05 ) ), keyby = w ]
  roll_g[ , `:=`( outcome = "n_grass", ident = "cs" ) ]
  fwrite( merge( roll_g, wins, by = "w" ), fg )
}
rw <- rbind( fread( file.path( OD, "c_rolling.csv" ) )[ ident == "cs" & outcome %in% c( "n_filer_c3", "exp_c3_nohu" ) ],
             fread( fg ), fill = TRUE )
rw[ , lab := factor( OUTC[ outcome ], levels = OUTC ) ]
print( rw[ , .( specs = specs[ 1 ], first = round( med[ which.min( pop_median ) ], 3 ), last = round( med[ which.max( pop_median ) ], 3 ) ), by = lab ] )

SHORT <- c( "Filing organizations", "Grassroots (not filing)", "Spending (excl. hospitals, universities)" )
pl <- ggplot( rw, aes( pop_median, med, colour = lab, fill = lab ) ) +
  geom_hline( yintercept = 0, colour = INK2, linewidth = .4 ) +
  geom_vline( xintercept = 2.5e5, colour = INK2, linewidth = .3, linetype = "22" ) +
  annotate( "text", x = 2.5e5, y = Inf, label = "250k split", hjust = -.08, vjust = 1.4, size = 2.8, colour = INK2 ) +
  geom_ribbon( aes( ymin = q25, ymax = q75 ), alpha = .15, colour = NA ) +
  geom_line( linewidth = .7 ) + geom_point( size = 1.1 ) +
  scale_x_log10( breaks = c( 1.5e5, 2e5, 3e5, 5e5, 8e5 ), labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
  scale_colour_manual( values = COLO, labels = SHORT ) + scale_fill_manual( values = COLO, labels = SHORT ) +
  labs( x = sprintf( "Median 2022 population of a window of %d adjacent metros (log scale)", WIN ),
        y = "Partial r of poverty rate",
        title = "A. Partial r of poverty across metro size, 2022",
        subtitle = "Line: median across default-model specs\nfit in each window; band: interquartile range" ) + th +
  guides( colour = guide_legend( nrow = 2 ), fill = guide_legend( nrow = 2 ) ) + theme( legend.justification = "left" )

## coefficient plot: the panel-B spec fit on each size group
q[ , bin := p22$bin[ match( cbsa, p22$cbsa ) ] ]
binlab <- sub( "\n", ", ", bins$label )
grp <- c( setNames( lapply( 1:4, function( b ) q$bin == b ), binlab ),
          list( "Under 250k, 194 metros" = q$population < 2.5e5, "250k and over, 193 metros" = q$population >= 2.5e5,
                "All metros, 387" = rep( TRUE, nrow( q ) ) ) )
cp <- rbindlist( lapply( names( OUTC ), function( yv ) rbindlist( lapply( names( grp ), function( g )
  cbind( outcome = yv, group = g, local_fit( yv, rep( 1, nrow( q ) ), grp[[ g ]] ) ) ) ) ) )
cp[ , `:=`( lab = factor( OUTC[ outcome ], levels = OUTC ), est = b / 10, lo95 = ( b - 1.96 * se ) / 10, hi95 = ( b + 1.96 * se ) / 10 ) ]
cp[ , set := factor( fifelse( group %in% binlab, "Four size bins", fifelse( group %like% "^All", "All", "Split at 250k" ) ),
                     levels = c( "Four size bins", "Split at 250k", "All" ) ) ]
cp[ , group := factor( group, levels = rev( names( grp ) ) ) ]
fwrite( cp, file.path( OD, "c_group_coef.csv" ) )
cat( "\n== coefficient plot: poverty coefficient per 10 points by group ==\n" )
print( dcast( cp, group ~ outcome, value.var = "est" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
dv <- position_dodge( width = .65 )
## the figure shows the four size bins only (the halves and all-metro rows stay in c_group_coef.csv)
cpb <- droplevels( cp[ set == "Four size bins" ] )
pr_ <- ggplot( cpb, aes( est, group, colour = lab ) ) +
  geom_vline( xintercept = 0, colour = INK2, linewidth = .5 ) +
  geom_linerange( aes( xmin = lo95, xmax = hi95 ), position = dv, linewidth = .8, alpha = .55 ) +
  geom_point( position = dv, size = 2 ) +
  scale_colour_manual( values = COLO, guide = "none" ) +
  labs( x = "Change in log density per 10 points more poverty", y = NULL,
        title = "B. Poverty coefficient by metro size, 2022",
        subtitle = "One spec (SAMI outcome, OLS) fit on each size bin;\ndot: estimate, bar: 95% CI" ) +
  th + theme( panel.grid.major.y = element_blank(), axis.text.y = element_text( size = 8 ) )

## bottom: the original split added-variable plots (pb), relettered
pc2 <- pb + labs( title = "C. The same model fit separately: filers vs poverty, 2022" )
fig4 <- ( ( pl | pr_ ) + plot_layout( widths = c( 1.15, 1 ) ) ) / pc2 + plot_layout( heights = c( 1, 1 ) )
ggsave( file.path( OD, "fig_poverty_size_window_coef.png" ), fig4, width = 10, height = 9.5, dpi = 200, device = ragg::agg_png, bg = "white" )
ggsave( file.path( OD, "fig_poverty_size_window_coef.pdf" ), fig4, width = 10, height = 9.5 )

r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
print( r3( roll[ , .( outcome, ident, bin, pop_lo, pop_hi, specs, med, q25, q75, share_pos_sig, share_neg_sig ) ] ), nrows = 200 )
log_msg( "done" )
