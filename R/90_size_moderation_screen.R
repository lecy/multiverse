## 90_size_moderation_screen.R
## Does metro size moderate the benchmark effects, and is the moderation curved? 80_interaction_screen.R
## tested only a linear x * log P term (in its own z-scored mini-model); the size figures (87, 88) suggest
## curvature for poverty (filers) and foundation assets (spending). Here every default-model spec of the
## multiverse (run_spec's design: all DV and predictor forms, log P on the right or not, units, OLS /
## state FE / division FE, winsorizing; cross-sections 2022 and 2019 and pooled 2015-22; state RE and the
## within stratum left out) is re-fit with size terms for one focal covariate at a time:
##   lin  : + c + x:c                      c = log population, centred in the estimation sample
##   quad : + c + c^2 + x:c + x:c^2
## (when log P is already on the right, c replaces it: the same model.) Per spec and focal covariate:
## t and p of x:c in lin; t and p of x:c^2 and the joint Wald p of (x:c, x:c^2) in quad; and the t of the
## marginal effect of x at 100k, 250k, 1M and 4M (quad). SEs as in run_spec (HC1 cs; metro-clustered
## pooled; state-clustered with state or division FE). t-statistics are compared across specs because the
## coefficients' units differ by form.
## Outcomes: filers, grassroots (registered, not filing; the registered-stock specs with the outcome
## swapped) and spending excl. hospitals and universities. Focal covariates: the five benchmarks.
## Outputs D:/MULTIVERSE/data/interactions/size_moderation/. Usage: Rscript R/90_size_moderation_screen.R [workers]

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( ggplot2 ) } )
args <- commandArgs( TRUE )
WORKERS <- suppressWarnings( as.integer( args[ grepl( "^[0-9]+$", args ) ] ) ); if ( !length( WORKERS ) ) WORKERS <- 12L
## with MV_MODEL=lean (the 13-covariate default of run v11) results go to size_moderation_v11/
OD <- file.path( BUILD, "data", "interactions", if ( MV_MODEL == "lean" ) "size_moderation_v11" else "size_moderation" )
dir.create( OD, recursive = TRUE, showWarnings = FALSE )
FOCAL <- c( poverty_rate = "Poverty", pct_ba = "% BA", pf_assets = "Foundation assets", turnout = "Turnout", pct_nonwhite = "% nonwhite" )
OUTC <- c( n_filer_c3 = "Filers", n_grass = "Grassroots", exp_c3_nohu = "Spending" )
SIZES <- c( "100k" = 1e5, "250k" = 2.5e5, "1M" = 1e6, "4M" = 4e6 )

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
panel <- load_panel()
panel[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
base <- grid[ drop == "none" & op == "default" & ident %in% c( "cs", "pooled" ) & est != "state_re" ]
specs <- rbind( base[ outcome %in% c( "n_filer_c3", "exp_c3_nohu" ) ], copy( base[ outcome == "n_bmf_c3pc" ] )[ , outcome := "n_grass" ] )
specs[ , job := .I ]
log_msg( "specs: ", nrow( specs ), " (", paste( specs[ , .N, by = outcome ][ , paste( outcome, N ) ], collapse = ", " ), ")" )

## run_spec's design (40_multiverse.R), returned before fitting
design <- function( s, panel ) {
  s <- as.list( s ); xs <- covars_for( s )
  yrs <- switch( s$ident, cs = s$cs_year, pooled = 2015:2022 )
  d <- panel[ year %in% yrs ]
  if ( s$unit == "metro" ) d <- d[ cbsa_type == "metro" ]
  d <- d[ , c( "cbsa", "year", "state_fips", "division", "logP", "population", s$outcome, xs ), with = FALSE ]
  d <- d[ complete.cases( d ) & population > 0 ]
  y <- d[[ s$outcome ]]
  if ( s$dv_form %in% c( "logpc", "loglog", "sami" ) ) { keep <- y > 0; d <- d[ keep ]; y <- y[ keep ] }
  yv <- switch( s$dv_form, pc = y / d$population, logpc = log( y / d$population ), loglog = log( y ),
                sami = resid_logP( log( y ), d$logP, d$year ), pois_off = y, pois_free = y )
  if ( s$outlier == "winsor1" && !s$dv_form %in% c( "pois_off", "pois_free" ) ) yv <- winsor( yv )
  X <- list()
  for ( v in xs ) {
    x <- d[[ v ]]; ty <- COVS[ var == v, type ]
    xv <- if ( ty == "B" ) switch( s$pred_form, raw = x, pc = x / d$population, log = flog( x, d$population ),
                                   sami = resid_logP( flog( x, d$population ), d$logP, d$year ) )
          else if ( s$pred_form == "sami" ) resid_logP( x, d$logP, d$year ) else x
    if ( s$outlier == "winsor1" ) xv <- winsor( xv )
    X[[ v ]] <- xv
  }
  m <- data.table( y = yv, as.data.table( X ), logP = d$logP, cbsa = d$cbsa, year = d$year,
                   state = d$state_fips, division = d$division, lpop = log( d$population ) )
  m <- m[ is.finite( y ) & Reduce( `&`, lapply( xs, function( v ) is.finite( m[[ v ]] ) ) ) ]
  for ( v in xs ) { s0 <- sd( m[[ v ]] ); if ( is.finite( s0 ) && s0 > 0 ) set( m, j = v, value = m[[ v ]] / s0 ) }
  m[ , c := logP - mean( logP ) ]; m[ , c2 := c^2 ]
  list( m = m, xs = xs, mc = mean( m$logP ) )
}

screen_spec <- function( i ) {
  s <- as.list( specs[ job == i ] )
  D <- tryCatch( design( s, panel ), error = function( e ) NULL )
  if ( is.null( D ) || nrow( D$m ) < 30 ) return( NULL )
  m <- D$m; xs <- D$xs
  fe <- switch( s$ident, cs = switch( s$est, state_fe = "state", div_fe = "division", "0" ),
                pooled = switch( s$est, state_fe = "state + year", div_fe = "division + year", "year" ) )
  vc <- switch( s$ident, cs = switch( s$est, ols = "hetero", ~state ), pooled = switch( s$est, ols = ~cbsa, ~state ) )
  fitf <- function( rhs ) {
    f <- as.formula( paste( "y ~", paste( rhs, collapse = " + " ), "|", fe ) )
    tryCatch( if ( s$dv_form == "pois_off" ) fepois( f, data = m, offset = ~lpop, vcov = vc )
              else if ( s$dv_form == "pois_free" ) fepois( f, data = m, vcov = vc )
              else feols( f, data = m, vcov = vc ), error = function( e ) NULL )
  }
  rbindlist( lapply( intersect( names( FOCAL ), xs ), function( fx ) {
    m[ , ic := m[[ fx ]] * c ]; m[ , ic2 := m[[ fx ]] * c2 ]
    r_lin <- fitf( c( xs, "c", "ic" ) )
    r_quad <- fitf( c( xs, "c", "c2", "ic", "ic2" ) )
    if ( is.null( r_lin ) || is.null( r_quad ) ) return( NULL )
    cl <- coeftable( r_lin ); cq <- coeftable( r_quad )
    if ( !all( c( "ic" ) %in% rownames( cl ) ) || !all( c( fx, "ic", "ic2" ) %in% rownames( cq ) ) ) return( NULL )
    w <- tryCatch( wald( r_quad, keep = "^ic2?$", print = FALSE ), error = function( e ) NULL )
    b <- coef( r_quad )[ c( fx, "ic", "ic2" ) ]; V <- vcov( r_quad )[ c( fx, "ic", "ic2" ), c( fx, "ic", "ic2" ) ]
    me_t <- vapply( SIZES, function( p ) { a <- log( p ) - D$mc; g <- c( 1, a, a^2 )
      sum( g * b ) / sqrt( drop( t( g ) %*% V %*% g ) ) }, numeric( 1 ) )
    data.table( job = i, focal = fx, n = nobs( r_quad ),
                t_lin = cl[ "ic", 3 ], p_lin = cl[ "ic", 4 ],
                t_c = cq[ "ic", 3 ], t_c2 = cq[ "ic2", 3 ], p_c2 = cq[ "ic2", 4 ],
                p_joint = if ( is.null( w ) ) NA_real_ else w$p, t_main_center = cq[ fx, 3 ],
                me_t_100k = me_t[[ 1 ]], me_t_250k = me_t[[ 2 ]], me_t_1M = me_t[[ 3 ]], me_t_4M = me_t[[ 4 ]] )
  } ) )
}

fr <- file.path( OD, "screen_specs.csv" )
environment( screen_spec ) <- globalenv(); environment( design ) <- globalenv()
for ( nm in c( "specs", "screen_spec", "design", "FOCAL", "SIZES" ) ) assign( nm, get( nm ), envir = globalenv() )
cl <- makePSOCKcluster( WORKERS )
invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
  setwd( "C:/Users/jdlec/Dropbox (Personal)/04 - PAPERS/01 - In Progress/38 - resource environments/multiverse" )
  source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
clusterExport( cl, c( "panel", "specs", "screen_spec", "design", "FOCAL", "SIZES" ), envir = globalenv() )
t0 <- proc.time()
res <- rbindlist( parLapplyLB( cl, specs$job, screen_spec, chunk.size = 20 ), fill = TRUE )
stopCluster( cl ); log_msg( "fits in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min; rows ", nrow( res ) )
res <- merge( res, specs[ , .( job, spec_id, outcome, ident, cs_year, dv_form, pred_form, rhs_logP, unit, est, outlier ) ], by = "job" )
fwrite( res, fr )

## ---- summaries --------------------------------------------------------------------------------
res[ , idg := fifelse( ident == "pooled", "pooled", paste0( "cs", cs_year ) ) ]
smry <- function( dt, by ) dt[ , .(
  specs = .N,
  lin_sig = mean( p_lin < .05 ), quad_joint_sig = mean( p_joint < .05, na.rm = TRUE ), sq_sig = mean( p_c2 < .05 ),
  med_t_lin = median( t_lin ), med_t_c2 = median( t_c2 ), sq_neg = mean( t_c2 < 0 ),
  me_100k = median( me_t_100k ), me_250k = median( me_t_250k ), me_1M = median( me_t_1M ), me_4M = median( me_t_4M ),
  neg_sig_100k = mean( me_t_100k < -1.96 ), pos_sig_1M = mean( me_t_1M > 1.96 ),
  inv_u = mean( me_t_250k > me_t_100k & me_t_1M > me_t_4M ) ), keyby = by ]
s1 <- smry( res, c( "focal", "outcome" ) )
s2 <- smry( res, c( "focal", "outcome", "idg" ) )
s3 <- smry( res[ focal %in% c( "poverty_rate", "pf_assets" ) ], c( "focal", "outcome", "pred_form" ) )
fwrite( s1, file.path( OD, "summary_focal_outcome.csv" ) ); fwrite( s2, file.path( OD, "summary_by_ident.csv" ) )
fwrite( s3, file.path( OD, "summary_by_predform.csv" ) )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 2 ) else z ) ]
options( width = 250 )
cat( "\n== by focal x outcome (shares: p < .05; me_*: median t of the marginal effect at that size) ==\n" ); print( r3( s1 ), nrows = 100 )
cat( "\n== by identification ==\n" ); print( r3( s2[ focal %in% c( "poverty_rate", "pf_assets" ) ] ), nrows = 100 )
cat( "\n== poverty and foundation assets by predictor form ==\n" ); print( r3( s3 ), nrows = 100 )

## ---- figure: median t of the marginal effect across size ------------------------------------------
fl <- melt( res, id.vars = c( "focal", "outcome", "spec_id" ), measure.vars = patterns( "^me_t_" ), variable.name = "size", value.name = "t" )
fl[ , size := factor( sub( "me_t_", "", size ), levels = names( SIZES ) ) ]
fs <- fl[ , .( med = median( t ), q25 = quantile( t, .25 ), q75 = quantile( t, .75 ) ), by = .( focal, outcome, size ) ]
fs[ , `:=`( focal = factor( FOCAL[ focal ], levels = FOCAL ), outcome = factor( OUTC[ outcome ], levels = OUTC ) ) ]
COLO <- c( Filers = "#2a78d6", Grassroots = "#1baf7a", Spending = "#eb6834" )
p <- ggplot( fs, aes( size, med, colour = outcome, group = outcome ) ) +
  geom_hline( yintercept = c( -1.96, 1.96 ), colour = "#c3c2b7", linewidth = .3, linetype = "22" ) +
  geom_hline( yintercept = 0, colour = "#52514e", linewidth = .4 ) +
  geom_linerange( aes( ymin = q25, ymax = q75 ), position = position_dodge( width = .4 ), linewidth = .8, alpha = .5 ) +
  geom_line( position = position_dodge( width = .4 ), linewidth = .6 ) + geom_point( position = position_dodge( width = .4 ), size = 1.8 ) +
  facet_wrap( ~ focal, nrow = 1 ) + scale_colour_manual( values = COLO ) +
  labs( x = "Metro population at which the effect is evaluated", y = "t of the covariate's effect at that size",
        title = "Effect of each benchmark covariate by metro size, from a quadratic size interaction",
        subtitle = sprintf( "Default-model specs (%d per outcome; cs 2022, cs 2019, pooled); dot: median t across specs, bar: IQR; dashed: |t| = 1.96",
                            nrow( specs ) / 3 ) ) +
  theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), legend.position = "top", legend.title = element_blank(),
         strip.text = element_text( face = "bold", hjust = 0 ), plot.background = element_rect( fill = "white", colour = NA ) )
ggsave( file.path( OD, "fig_size_moderation.png" ), p, width = 11, height = 4.2, dpi = 200, device = ragg::agg_png, bg = "white" )
log_msg( "done: ", OD )
