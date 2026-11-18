## 88_covariate_size_figures.R
## The size-heterogeneity figure of 87 (fig_poverty_size_window_coef) for any default covariate:
## poverty rate (reproduces 87 as a check), % BA and foundation assets.
##   A. Rolling-window partial r, cross-section 2022: metros ordered by 2022 population, windows of
##      150 adjacent metros (step 10); in each window the default-model metro specs (93 per outcome)
##      are re-fit; line = median, band = IQR across specs. Outcomes: filers, grassroots (registered
##      public charities that do not file = BMF stock - filers; fit on the registered-stock specs with
##      the outcome swapped) and spending excl. hospitals and universities.
##   B. Coefficient of the covariate in four size bins (the 387 metros split at 250k, each half split
##      at its median; 96-97 metros each), one representative spec, 95% CI (heteroskedasticity-robust).
##   C. Added-variable plots for filers in the two 250k halves, same spec, fit within each half.
## Representative spec per covariate (same rule as 87): among filer cross-section 2022 OLS specs with a
## SAMI outcome, the one whose partial r in the two 250k halves is nearest the median across all filer
## cross-section specs in each half (86 part B, b_coefs.parquet). Foundation assets: SAMI predictor form
## (no spec is near both medians; see below), so the coefficient reads per doubling.
## Outputs D:/MULTIVERSE/data/sensitivity/fig_<term>_size_window_coef.{png,pdf}, c_rolling_terms.csv,
## c_group_coef_terms.csv, c_rep_specs.csv. Usage: Rscript R/notes/88_covariate_size_figures.R [fit] [workers]

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( ggplot2 ); library( patchwork ); library( arrow ) } )
args <- commandArgs( TRUE )
WORKERS <- suppressWarnings( as.integer( args[ grepl( "^[0-9]+$", args ) ] ) ); if ( !length( WORKERS ) ) WORKERS <- 12L
OD <- file.path( BUILD, "data", "sensitivity" ); dir.create( OD, showWarnings = FALSE )
TERMS <- c( poverty_rate = "poverty rate", pct_ba = "% BA", pf_assets = "foundation assets" )
OUTC <- c( n_filer_c3 = "Filing organizations", n_grass = "Grassroots (not filing)",
           exp_c3_nohu = "Spending (excl. hospitals, universities)" )
WIN <- 150L; STEP <- 10L

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
panel <- load_panel()
panel[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
p22 <- panel[ year == 2022 & cbsa_type == "metro" ][ order( population ) ]
xs <- covars_for( as.list( grid[ op == "default" & drop == "none" ][ 1 ] ) )

## size bins and windows (as in 87)
p22[ , half := fifelse( population < 2.5e5, 1L, 2L ) ]
p22[ , bin := ( half - 1L ) * 2L + fifelse( seq_len( .N ) <= ceiling( .N / 2 ), 1L, 2L ), by = half ]
bins <- p22[ , .( metros = .N, pop_lo = min( population ), pop_hi = max( population ) ), keyby = bin ]
fmt <- function( x ) scales::label_number( accuracy = 1, scale_cut = scales::cut_short_scale() )( signif( x, 2 ) )
bins[ , label := sprintf( "%s-%s, %d metros", fmt( pop_lo ), fmt( pop_hi ), metros ) ]
starts <- unique( c( seq( 1L, nrow( p22 ) - WIN + 1L, by = STEP ), nrow( p22 ) - WIN + 1L ) )
wins <- data.table( w = seq_along( starts ), pop_median = sapply( starts, function( s ) median( p22$population[ s:( s + WIN - 1L ) ] ) ),
                    pop_lo = p22$population[ starts ], pop_hi = p22$population[ starts + WIN - 1L ] )

## ---- A: rolling-window fits, all three covariates ---------------------------------------------
fr <- file.path( OD, "c_rolling_terms.csv" )
if ( "fit" %in% args || !file.exists( fr ) ) {
  base <- grid[ drop == "none" & op == "default" & outlier == "none" & cs_year == 2022L & unit == "metro" & ident == "cs" ]
  specs <- rbind( base[ outcome %in% c( "n_filer_c3", "exp_c3_nohu" ) ],
                  copy( base[ outcome == "n_bmf_c3pc" ] )[ , outcome := "n_grass" ] )
  specs[ , job_spec := .I ]
  keepsets <- lapply( starts, function( s ) p22$cbsa[ s:( s + WIN - 1L ) ] )
  jobs <- CJ( job_spec = specs$job_spec, w = wins$w )
  log_msg( "A: ", nrow( specs ), " specs x ", nrow( wins ), " windows = ", nrow( jobs ), " fits" )
  run_job3 <- function( i ) {
    j <- jobs[ i ]; s <- specs[ job_spec == j$job_spec ]
    r <- tryCatch( run_spec( s, panel[ cbsa %in% keepsets[[ j$w ]] ] ), error = function( e ) NULL )
    if ( is.null( r ) ) return( NULL )
    r[ , `:=`( job_spec = j$job_spec, w = j$w, k = .N ) ][ term %in% names( TERMS ) ] }
  environment( run_job3 ) <- globalenv()
  for ( nm in c( "jobs", "keepsets", "specs", "run_job3" ) ) assign( nm, get( nm ), envir = globalenv() )
  cl <- makePSOCKcluster( WORKERS )
  invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
    setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
    source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
  clusterExport( cl, c( "panel", "specs", "jobs", "keepsets", "run_job3", "TERMS" ), envir = globalenv() )
  t0 <- proc.time()
  r <- rbindlist( parLapplyLB( cl, seq_len( nrow( jobs ) ), run_job3, chunk.size = 100 ), fill = TRUE )
  stopCluster( cl ); log_msg( "A: fits in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min" )
  r <- merge( r, specs[ , .( job_spec, outcome ) ], by = "job_spec" )
  r[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
  roll <- r[ , .( specs = .N, med = median( pr ), q25 = quantile( pr, .25 ), q75 = quantile( pr, .75 ),
                  share_pos_sig = mean( pr > 0 & p < .05 ), share_neg_sig = mean( pr < 0 & p < .05 ) ),
             keyby = .( term, outcome, w ) ]
  fwrite( merge( roll, wins, by = "w" ), fr )
}
roll <- fread( fr )
## check: poverty reproduces 87's window fits
old <- rbind( fread( file.path( OD, "c_rolling.csv" ) )[ ident == "cs" & outcome %in% c( "n_filer_c3", "exp_c3_nohu" ), .( outcome, w, med ) ],
              fread( file.path( OD, "c_rolling_grass.csv" ) )[ , .( outcome, w, med ) ] )
cmp <- merge( old, roll[ term == "poverty_rate", .( outcome, w, med_new = med ) ], by = c( "outcome", "w" ) )
cat( sprintf( "\n== check vs 87 window fits (poverty): %d rows, max |diff| %.2g ==\n", nrow( cmp ), max( abs( cmp$med - cmp$med_new ) ) ) )

## ---- representative spec per covariate --------------------------------------------------------
bc <- as.data.table( read_parquet( file.path( OD, "b_coefs.parquet" ) ) )[ outcome == "n_filer_c3" & ident == "cs" & term %in% names( TERMS ) ]
w2 <- dcast( bc, spec_id + term ~ variant, value.var = "pr" )
w2[ , `:=`( m_lt = median( lt250k ), m_ge = median( ge250k ) ), by = term ]
w2 <- merge( w2, grid[ , .( spec_id, dv_form, pred_form, rhs_logP, est ) ], by = "spec_id" )
## Foundation assets: no spec sits near both medians, because the size pattern depends on the
## predictor form (filer cs specs, median partial r <250k / >=250k: log or SAMI .39-.41 / .27-.44,
## per capita .09-.11 / .42-.45, raw totals .10 / .06-.07); the medians fall between the modes.
## The SAMI form is used: it matches the SAMI outcome and its coefficient reads per doubling.
cand <- w2[ est == "ols" & dv_form == "sami" & !( term == "pf_assets" & pred_form != "sami" ) ]
cand[ , dist := abs( lt250k - m_lt ) + abs( ge250k - m_ge ) ]
rep <- cand[ order( dist ), .SD[ 1 ], by = term ]
fwrite( rep, file.path( OD, "c_rep_specs.csv" ) )
cat( "\n== representative specs ==\n" ); print( rep[ , .( term, spec_id, pred_form, rhs_logP, lt250k = round( lt250k, 3 ), m_lt = round( m_lt, 3 ), ge250k = round( ge250k, 3 ), m_ge = round( m_ge, 3 ) ) ] )

## ---- model pieces for B and C: run_spec's design, built within a subset of metros --------------
build <- function( s, yv, cb ) {
  d <- p22[ cbsa %in% cb, c( "cbsa", "population", "logP", "year", yv, xs ), with = FALSE ]
  d <- d[ complete.cases( d ) & d[[ yv ]] > 0 ]
  d[ , y := resid_logP( log( d[[ yv ]] ), logP, year ) ]
  for ( v in xs ) {
    x <- d[[ v ]]; ty <- COVS[ var == v, type ]
    xv <- if ( ty == "B" ) switch( s$pred_form, raw = x, pc = x / d$population, log = flog( x, d$population ),
                                   sami = resid_logP( flog( x, d$population ), d$logP, d$year ) )
          else if ( s$pred_form == "sami" ) resid_logP( x, d$logP, d$year ) else x
    set( d, j = paste0( "x_", v ), value = xv )
  }
  d[]
}
rhs_of <- function( s ) c( paste0( "x_", xs ), if ( isTRUE( s$rhs_logP ) ) "logP" )
fit_group <- function( s, yv, cb, tm ) {
  d <- build( s, yv, cb )
  f <- feols( reformulate( rhs_of( s ), "y" ), data = d, vcov = "hetero" )
  ct <- coeftable( f )[ paste0( "x_", tm ), ]
  data.table( b = ct[[ 1 ]], se = ct[[ 2 ]], t = ct[[ 3 ]], n = nobs( f ), k = length( rhs_of( s ) ) )
}

## ---- figure pieces ------------------------------------------------------------------------------
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRIDC <- "#e6e5e1"
COLO <- setNames( c( "#2a78d6", "#1baf7a", "#eb6834" ), OUTC )
th <- theme_minimal( base_size = 10, base_family = "sans" ) +
  theme( panel.grid.minor = element_blank(), panel.grid.major = element_line( colour = GRIDC, linewidth = .3 ),
         axis.text = element_text( colour = INK2 ), axis.title = element_text( colour = INK2, size = 9 ),
         plot.title = element_text( colour = INK, face = "bold", size = 10.5 ),
         plot.subtitle = element_text( colour = INK2, size = 8.5 ), legend.position = "top",
         legend.title = element_blank(), legend.text = element_text( colour = INK2, size = 8.5 ),
         strip.text = element_text( colour = INK, hjust = 0, face = "bold", size = 9 ),
         plot.background = element_rect( fill = "white", colour = NA ) )
## display units of the coefficient: rates per 10 points; foundation assets (logged) per doubling
UNIT <- list( poverty_rate = list( mult = .1, lab = "10 points more poverty", x = "Poverty rate (share)" ),
              pct_ba = list( mult = .1, lab = "10 points more BA holders", x = "% BA (share)" ),
              pf_assets = list( mult = log( 2 ), lab = "doubling of foundation assets", x = "Foundation assets (log, SAMI form)" ) )
TITLE <- c( poverty_rate = "poverty", pct_ba = "% BA", pf_assets = "foundation assets" )

grp_all <- c( setNames( lapply( 1:4, function( b ) p22[ bin == b, cbsa ] ), bins$label ) )
halves <- list( "Under 250k" = p22[ population < 2.5e5, cbsa ], "250k and over" = p22[ population >= 2.5e5, cbsa ] )
gc_all <- list()
for ( tm in names( TERMS ) ) {
  s <- as.list( grid[ spec_id == rep[ term == tm, spec_id ] ] )
  u <- UNIT[[ tm ]]
  ## check: within-group fit equals run_spec on the subset (filers)
  for ( g in names( halves ) ) {
    a <- fit_group( s, "n_filer_c3", halves[[ g ]], tm )$b
    b <- run_spec( s, panel[ cbsa %in% halves[[ g ]] ] )[ term == tm, estimate ]
    if ( abs( a - b ) > 1e-8 * max( 1, abs( b ) ) ) stop( "fit_group differs from run_spec: ", tm, " ", g, " ", a, " vs ", b )
  }
  ## A
  ra <- roll[ term == tm ][ , lab := factor( OUTC[ outcome ], levels = OUTC ) ]
  pl <- ggplot( ra, aes( pop_median, med, colour = lab, fill = lab ) ) +
    geom_hline( yintercept = 0, colour = INK2, linewidth = .4 ) +
    geom_vline( xintercept = 2.5e5, colour = INK2, linewidth = .3, linetype = "22" ) +
    annotate( "text", x = 2.5e5, y = Inf, label = "250k split", hjust = -.08, vjust = 1.4, size = 2.8, colour = INK2 ) +
    geom_ribbon( aes( ymin = q25, ymax = q75 ), alpha = .15, colour = NA ) +
    geom_line( linewidth = .7 ) + geom_point( size = 1.1 ) +
    scale_x_log10( breaks = c( 1.5e5, 2e5, 3e5, 5e5, 8e5 ), labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
    scale_colour_manual( values = COLO ) + scale_fill_manual( values = COLO ) +
    labs( x = sprintf( "Median 2022 population of a window of %d adjacent metros (log scale)", WIN ),
          y = sprintf( "Partial r of %s", TERMS[[ tm ]] ),
          title = sprintf( "A. Partial r of %s across metro size, 2022", TITLE[[ tm ]] ),
          subtitle = "Line: median across default-model specs\nfit in each window; band: interquartile range" ) + th +
    guides( colour = guide_legend( nrow = 2 ), fill = guide_legend( nrow = 2 ) ) + theme( legend.justification = "left" )
  ## B
  gc <- rbindlist( lapply( names( OUTC ), function( yv ) rbindlist( lapply( names( grp_all ), function( g )
    cbind( term = tm, outcome = yv, group = g, fit_group( s, yv, grp_all[[ g ]], tm ) ) ) ) ) )
  gc[ , `:=`( lab = factor( OUTC[ outcome ], levels = OUTC ), est = b * u$mult, lo95 = ( b - 1.96 * se ) * u$mult, hi95 = ( b + 1.96 * se ) * u$mult,
              group = factor( group, levels = rev( names( grp_all ) ) ) ) ]
  gc_all[[ tm ]] <- gc
  pr_ <- ggplot( gc, aes( est, group, colour = lab ) ) +
    geom_vline( xintercept = 0, colour = INK2, linewidth = .5 ) +
    geom_linerange( aes( xmin = lo95, xmax = hi95 ), position = position_dodge( width = .65 ), linewidth = .8, alpha = .55 ) +
    geom_point( position = position_dodge( width = .65 ), size = 2 ) +
    scale_colour_manual( values = COLO, guide = "none" ) +
    labs( x = sprintf( "Change in log density per %s", u$lab ), y = NULL,
          title = "B. Coefficient by metro size, 2022",
          subtitle = sprintf( "One spec (SAMI outcome, %s predictors%s, OLS) fit on\neach size bin; dot: estimate, bar: 95%% CI",
                              fifelse( s$pred_form == "sami", "SAMI", s$pred_form ), if ( isTRUE( s$rhs_logP ) ) ", log P" else "" ) ) +
    th + theme( panel.grid.major.y = element_blank(), axis.text.y = element_text( size = 8 ) )
  ## C: filers, except foundation assets, where the grassroots outcome is shown (user choice, 2026-11-16)
  cout <- if ( tm == "pf_assets" ) "n_grass" else "n_filer_c3"
  cname <- c( n_filer_c3 = "filers", n_grass = "grassroots organizations" )[[ cout ]]
  ccol <- COLO[[ OUTC[[ cout ]] ]]
  others <- setdiff( rhs_of( s ), paste0( "x_", tm ) )
  d <- rbindlist( lapply( names( halves ), function( g ) {
    dd <- build( s, cout, halves[[ g ]] )
    dd[ , `:=`( size = g, y_res = resid( lm( reformulate( others, "y" ), data = dd ) ),
                x_res = resid( lm( reformulate( others, paste0( "x_", tm ) ), data = dd ) ) ) ] } ) )
  d[ , size := factor( size, levels = names( halves ) ) ]
  hc <- rbindlist( lapply( names( halves ), function( g ) cbind( size = g, fit_group( s, cout, halves[[ g ]], tm ) ) ) )
  hc[ , pr := t / sqrt( t^2 + n - k - 1 ) ]
  strip_lab <- setNames( hc[ , sprintf( "%s: %d metros, partial r %+.2f", size, n, pr ) ], hc$size )
  pc <- ggplot( d, aes( x_res, y_res ) ) +
    geom_hline( yintercept = 0, colour = GRIDC ) + geom_vline( xintercept = 0, colour = GRIDC ) +
    geom_point( size = 1.4, alpha = .55, stroke = 0, colour = ccol ) +
    geom_smooth( method = "lm", formula = y ~ x, se = TRUE, linewidth = .8, alpha = .15, colour = ccol, fill = ccol ) +
    facet_wrap( ~ size, labeller = as_labeller( strip_lab ) ) +
    labs( x = sprintf( "%s, net of the other default covariates", u$x ),
          y = sprintf( "%s density (SAMI), net of the same", if ( cout == "n_grass" ) "Grassroots" else "Filer" ),
          title = sprintf( "C. The same model fit separately: %s vs %s, 2022", cname, TITLE[[ tm ]] ),
          subtitle = if ( tm == "pf_assets" ) "Added-variable plot, SAMI outcome and SAMI predictors, OLS (no spec is near both medians: the predictor forms disagree); fit within each group" else "Added-variable plot for the spec nearest both groups' medians (SAMI outcome, OLS); fit within each group" ) + th
  fig <- ( ( pl | pr_ ) + plot_layout( widths = c( 1.15, 1 ) ) ) / pc + plot_layout( heights = c( 1, 1 ) )
  fn <- file.path( OD, sprintf( "fig_%s_size_window_coef", tm ) )
  ggsave( paste0( fn, ".png" ), fig, width = 10, height = 9.5, dpi = 200, device = ragg::agg_png, bg = "white" )
  ggsave( paste0( fn, ".pdf" ), fig, width = 10, height = 9.5 )
  cat( sprintf( "\n== %s (spec %d) ==\n", tm, s$spec_id ) )
  print( dcast( gc, group ~ outcome, value.var = "est" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
  print( hc[ , .( size, b = round( b * u$mult, 3 ), pr = round( pr, 3 ) ) ] )
  print( ra[ , .( first = round( med[ which.min( pop_median ) ], 3 ), last = round( med[ which.max( pop_median ) ], 3 ),
                  min = round( min( med ), 3 ), max = round( max( med ), 3 ) ), by = outcome ] )
}
fwrite( rbindlist( gc_all ), file.path( OD, "c_group_coef_terms.csv" ) )
log_msg( "done" )
