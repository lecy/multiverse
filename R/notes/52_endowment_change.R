## 52_endowment_change.R
## Levels ("initial endowment") vs change in nonprofit density, 387 metros.
##   Part A  SAMI as a % deviation from the size-expected value
##   Part B  rank change 2000-02 -> 2022-24: risers, fallers, what they share
##   Part C  endowment models: convergence (OLS and IV), sigma-convergence,
##           historical endowment (pre-1970 rulings in the 2000 BMF stock),
##           hybrid / correlated-random-effects model, growth-curve model
## Writes to D:/MULTIVERSE/data/followup/ (p7_*, p8_*, p9_*, fig_p7-9_*).

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( ggplot2 ); library( lme4 ) } )
setFixest_notes( FALSE )
FU <- file.path( BUILD, "data", "followup" ); dir.create( FU, showWarnings = FALSE )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
## log of an extensive covariate with a per-capita floor (as flog() in 40_multiverse.R, run v6 on;
## switched here 2026-11-16). log(x + 1) put zero-asset metros (Eagle Pass, TX) ~11 SD below the mean.
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
theme_fu <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"

d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
m <- d[ cbsa_type == "metro" ]
REGION <- c( "09" = "Northeast", "23" = "Northeast", "25" = "Northeast", "33" = "Northeast", "44" = "Northeast",
  "50" = "Northeast", "34" = "Northeast", "36" = "Northeast", "42" = "Northeast",
  "17" = "Midwest", "18" = "Midwest", "26" = "Midwest", "39" = "Midwest", "55" = "Midwest", "19" = "Midwest",
  "20" = "Midwest", "27" = "Midwest", "29" = "Midwest", "31" = "Midwest", "38" = "Midwest", "46" = "Midwest",
  "10" = "South", "11" = "South", "12" = "South", "13" = "South", "24" = "South", "37" = "South", "45" = "South",
  "51" = "South", "54" = "South", "01" = "South", "21" = "South", "28" = "South", "47" = "South", "05" = "South",
  "22" = "South", "40" = "South", "48" = "South",
  "04" = "West", "08" = "West", "16" = "West", "30" = "West", "32" = "West", "35" = "West", "49" = "West",
  "56" = "West", "02" = "West", "06" = "West", "15" = "West", "41" = "West", "53" = "West" )
m[ , region := unname( REGION[ state_fips ] ) ]
short <- function( t ) sub( ",.*", "", sub( "-.*?,", ",", t ) )
m[ , name := paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ) ]

## year-specific SAMIs with their scaling line (to recover expected values)
fit_year <- function( y, P ) { ok <- y > 0 & P > 0; cf <- coef( lm( log( y[ ok ] ) ~ log( P[ ok ] ) ) )
  list( a = cf[ 1 ], b = cf[ 2 ] ) }
m[ , n_grass_c3pc := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]   # grassroots: registered, not filing
for ( v in c( "n_filer_c3", "n_bmf_c3pc", "n_grass_c3pc", "exp_c3_nohu" ) ) {
  m[ , c( paste0( "a_", v ), paste0( "b_", v ) ) := fit_year( get( v ), population ), by = year ]
  m[ , ( paste0( "s_", v ) ) := fifelse( get( v ) > 0, log( get( v ) ) - get( paste0( "a_", v ) ) - get( paste0( "b_", v ) ) * log( population ), NA_real_ ) ]
}

## ===========================================================================
## Part A: SAMI as % deviation from expected
## ===========================================================================
log_msg( "Part A: % deviation" )
x <- m[ year == 2022 ]
x[ , `:=`( expected_n = exp( a_n_filer_c3 + b_n_filer_c3 * log( population ) ),
           pct_dev_n = 100 * ( exp( s_n_filer_c3 ) - 1 ),
           pct_dev_e = 100 * ( exp( s_exp_c3_nohu ) - 1 ) ) ]
x[ , excess_n := n_filer_c3 - expected_n ]
q <- c( .05, .10, .25, .50, .75, .90, .95 )
pctA <- data.table( quantile = q, sami_n = quantile( x$s_n_filer_c3, q ), pct_n = quantile( x$pct_dev_n, q ),
                    sami_e = quantile( x$s_exp_c3_nohu, q, na.rm = TRUE ), pct_e = quantile( x$pct_dev_e, q, na.rm = TRUE ) )
print( r3( pctA ) ); fwrite( pctA, file.path( FU, "p7_pct_deviation_quantiles.csv" ) )
ex <- x[ order( -population ) ][ 1:15 ][ , .( name, population, n_filer_c3, expected_n = round( expected_n ), excess_n = round( excess_n ),
                                                sami = s_n_filer_c3, pct_dev_n, pct_dev_e ) ]
ext <- rbind( x[ order( -s_n_filer_c3 ) ][ 1:5 ], x[ order( s_n_filer_c3 ) ][ 1:5 ] )[
  , .( name, population, n_filer_c3, expected_n = round( expected_n ), excess_n = round( excess_n ), sami = s_n_filer_c3, pct_dev_n, pct_dev_e ) ]
print( r3( ex ) ); print( r3( ext ) )
fwrite( rbind( ex[ , grp := "largest" ], ext[ , grp := "extreme" ] ), file.path( FU, "p7_pct_deviation_examples.csv" ) )
fwrite( x[ , .( cbsa, name, population, n_filer_c3, expected_n, excess_n, s_n_filer_c3, pct_dev_n, s_exp_c3_nohu, pct_dev_e ) ],
        file.path( FU, "p7_pct_deviation_2022.csv" ) )

## ===========================================================================
## Part B: rank change, smoothed endpoints
## ===========================================================================
log_msg( "Part B: rank change" )
end_avg <- function( v, yrs ) m[ year %in% yrs, .( s = mean( get( v ), na.rm = TRUE ) ), by = cbsa ]
ch <- Reduce( function( a, b ) merge( a, b, by = "cbsa" ), list(
  setnames( end_avg( "s_n_filer_c3", 2000:2002 ), "s", "s0" ),
  setnames( end_avg( "s_n_filer_c3", 2022:2024 ), "s", "s1" ),
  setnames( end_avg( "s_n_filer_c3", 2011:2013 ), "s", "s_mid" ),
  setnames( end_avg( "s_n_bmf_c3pc", 2000:2002 ), "s", "b0" ),
  setnames( end_avg( "s_n_bmf_c3pc", 2022:2024 ), "s", "b1" ) ) )
st0 <- m[ year == 2000, .( cbsa, name, region, state_fips, pop0 = population, n0 = n_filer_c3,
                           pct_ba0 = pct_ba, pcpi0 = pcpi, pov0 = poverty_rate, old0 = pct_65plus,
                           nonwhite0 = pct_nonwhite, fb0 = pct_foreign_born, gini0 = gini, adh0 = pct_adherent ) ]
st1 <- m[ year == 2024, .( cbsa, pop1 = population, n1 = n_filer_c3, pct_ba1 = pct_ba, pcpi1 = pcpi,
                           old1 = pct_65plus, nonwhite1 = pct_nonwhite ) ]
ch <- merge( merge( ch, st0, by = "cbsa" ), st1, by = "cbsa" )
ch[ , `:=`( ds = s1 - s0, ds_late = s1 - s_mid, db = b1 - b0,
            pr0 = 100 * ( rank( s0 ) - 0.5 ) / .N, pr1 = 100 * ( rank( s1 ) - 0.5 ) / .N,
            gpop = log( pop1 / pop0 ), gn = log( n1 / n0 ), pct0 = 100 * ( exp( s0 ) - 1 ), pct1 = 100 * ( exp( s1 ) - 1 ) ) ]
ch[ , dpr := pr1 - pr0 ]
ch[ , `:=`( d_ba = pct_ba1 - pct_ba0, d_old = old1 - old0, d_nonwhite = nonwhite1 - nonwhite0, d_lpcpi = log( pcpi1 / pcpi0 ) ) ]
ch[ , grp := fcase( ds >= quantile( ds, .9 ), "Risers (top 10%)", ds <= quantile( ds, .1 ), "Fallers (bottom 10%)", default = "Middle 80%" ) ]

## historical endowment: 501(c)(3) public charities in the 2000 BMF stock ruled before 1970
log_msg( "historical endowment from BMF ruling years" )
bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "subsec", "pf", "ryear" ),
              colClasses = list( character = c( "EIN", "geoid", "subsec" ) ) )
cat( "BMF years:", paste( range( bmf$year ), collapse = "-" ), "\n" )
b0 <- bmf[ year == min( year ) & subsec %in% c( "03", "3" ) & pf == FALSE & !is.na( geoid ) ]
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )
b0 <- merge( b0, sp[ , .( geoid, cbsa ) ], by = "geoid" )
cat( "ruling-year coverage in base-year stock:", round( mean( !is.na( b0$ryear ) & b0$ryear > 1900 ), 3 ),
     "; share ruled before 1970:", round( mean( b0$ryear < 1970, na.rm = TRUE ), 3 ), "\n" )
hist <- b0[ , .( n_bmf0 = .N, n_pre1970 = sum( ryear < 1970, na.rm = TRUE ), n_pre1960 = sum( ryear < 1960, na.rm = TRUE ) ), by = cbsa ]
ch <- merge( ch, hist, by = "cbsa", all.x = TRUE )
ch[ , old_share := n_pre1970 / n_bmf0 ]
## metros where most of the stock predates 1970 are group-exemption artifacts
## (subordinate units registered at a headquarters address: Mankato 82%, Memphis 72%)
ch[ , s_hist := { ok <- n_pre1970 > 0 & old_share <= 0.6; z <- rep( NA_real_, .N )
                  z[ ok ] <- resid( lm( log( n_pre1970[ ok ] ) ~ log( pop0[ ok ] ) ) ); z } ]
cat( "metros excluded from the endowment measure:", ch[ old_share > 0.6, paste( name, collapse = "; " ) ], "\n" )
fwrite( ch, file.path( FU, "p8_rank_change.csv" ) )

## top risers and fallers
top <- rbind( ch[ order( -ds ) ][ 1:15 ][ , side := "Rising most" ], ch[ order( ds ) ][ 1:15 ][ , side := "Falling most" ] )
print( top[ , .( side, name, pop0 = round( pop0 / 1e3 ), gpop = round( 100 * ( exp( gpop ) - 1 ) ), pr0 = round( pr0 ), pr1 = round( pr1 ),
                 pct0 = round( pct0 ), pct1 = round( pct1 ), db = round( db, 2 ), region ) ] )
fwrite( top, file.path( FU, "p8_top_movers.csv" ) )

## who changes: group profiles
prof <- ch[ , .( metros = .N, ds = median( ds ), start_pctile = median( pr0 ), pop2000_k = median( pop0 ) / 1e3,
                 pop_growth_pct = median( 100 * ( exp( gpop ) - 1 ) ), org_growth_pct = median( 100 * ( exp( gn ) - 1 ) ),
                 south = mean( region == "South" ), west = mean( region == "West" ), midwest = mean( region == "Midwest" ),
                 northeast = mean( region == "Northeast" ),
                 pct_ba0 = median( pct_ba0 ), d_ba = median( d_ba ), pcpi0_k = median( pcpi0 ) / 1e3, pov0 = median( pov0 ),
                 pct65_0 = median( old0 ), d_65 = median( d_old ), nonwhite0 = median( nonwhite0 ), d_nonwhite = median( d_nonwhite ),
                 pre1970_share = median( old_share, na.rm = TRUE ), s_hist = median( s_hist, na.rm = TRUE ) ), by = grp ]
print( r3( prof ) ); fwrite( prof, file.path( FU, "p8_group_profiles.csv" ) )

## what predicts change: standardized regression
z <- function( v ) ( v - mean( v, na.rm = TRUE ) ) / sd( v, na.rm = TRUE )
cz <- copy( ch )[ , region := factor( region, c( "Northeast", "Midwest", "South", "West" ) ) ]
for ( v in c( "ds", "s0", "gpop", "pop0", "pct_ba0", "d_ba", "pcpi0", "pov0", "old0", "d_old", "nonwhite0", "d_nonwhite", "s_hist" ) )
  cz[ , ( paste0( "z_", v ) ) := z( if ( v %in% c( "pop0", "pcpi0" ) ) log( get( v ) ) else get( v ) ) ]
mods <- list(
  "Population growth only" = z_ds ~ z_gpop,
  "Start SAMI only" = z_ds ~ z_s0,
  "Start SAMI + growth" = z_ds ~ z_s0 + z_gpop,
  "Full" = z_ds ~ z_s0 + z_gpop + z_pop0 + z_pct_ba0 + z_d_ba + z_pcpi0 + z_pov0 + z_old0 + z_d_old + z_nonwhite0 + z_d_nonwhite + region )
reg <- rbindlist( lapply( names( mods ), function( k ) { r <- feols( mods[[ k ]], cz, vcov = "hetero" ); ct <- coeftable( r )
  data.table( model = k, term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ], r2 = r2( r, "r2" ), n = nobs( r ) ) } ) )
print( r3( reg[ term != "(Intercept)" ] ) ); fwrite( reg, file.path( FU, "p8_change_regression.csv" ) )
cat( "cor(ds, ds_late) =", round( cor( ch$ds, ch$ds_late ), 3 ), "; cor(ds filers, ds BMF) =", round( cor( ch$ds, ch$db ), 3 ),
     "; cor(ds, gpop) =", round( cor( ch$ds, ch$gpop ), 3 ), "; cor(ds, gn) =", round( cor( ch$ds, ch$gn ), 3 ), "\n" )

## figures: dumbbells and change vs growth
tp <- copy( top )[ , label := factor( name, rev( c( ch[ order( -ds ) ][ 1:15, name ], ch[ order( ds ) ][ 1:15, name ] ) ) ) ]
tp[ , side := factor( side, c( "Rising most", "Falling most" ) ) ]
## arrows stop short of the end dot so the head stays visible (moves under 3 points get no arrow)
tp[ , `:=`( dir = sign( pr1 - pr0 ), big_move = abs( pr1 - pr0 ) >= 3 ) ]
g1 <- ggplot( tp, aes( y = label ) ) +
  geom_segment( data = tp[ big_move == FALSE ], aes( x = pr0, xend = pr1, yend = label ), colour = "grey70", linewidth = 0.6 ) +
  geom_segment( data = tp[ big_move == TRUE ], aes( x = pr0 + 1.2 * dir, xend = pr1 - 2.2 * dir, yend = label, colour = side ),
                linewidth = 0.7, arrow = arrow( length = unit( 0.16, "cm" ), type = "closed" ) ) +
  geom_point( aes( x = pr0 ), colour = "grey55", size = 1.8 ) + geom_point( aes( x = pr1, colour = side ), size = 2.2 ) +
  scale_y_discrete( limits = function( l ) intersect( levels( tp$label ), l ) ) +
  facet_wrap( ~ side, scales = "free_y", ncol = 2 ) + scale_colour_manual( values = c( C1, C2 ), guide = "none" ) +
  scale_x_continuous( limits = c( 0, 100 ), breaks = seq( 0, 100, 25 ) ) +
  labs( title = "Largest changes in filer-count SAMI, 2000-02 to 2022-24 (387 metros)",
        subtitle = "Grey dot = percentile rank at start; coloured dot = rank at end", x = "Percentile rank of SAMI", y = NULL ) + theme_fu
ggsave( file.path( FU, "fig_p8_movers.png" ), g1, width = 10, height = 5.6, dpi = 160, bg = "white" )
g2 <- ggplot( ch, aes( 100 * ( exp( gpop ) - 1 ), ds, colour = region ) ) +
  geom_hline( yintercept = 0, colour = "grey70" ) + geom_point( size = 1.6, alpha = 0.75 ) +
  geom_smooth( aes( group = 1 ), method = "lm", formula = y ~ x, se = FALSE, colour = "grey25", linewidth = 0.5, linetype = 2 ) +
  ## robust lowess (symmetric family down-weights outliers such as Wildwood)
  geom_smooth( data = ch[ gpop < log( 2.5 ) ], aes( group = 1 ), method = "loess", formula = y ~ x, span = 0.75, method.args = list( family = "symmetric" ),
               se = TRUE, colour = "grey10", fill = "grey80", linewidth = 0.9 ) +
  ggrepel::geom_text_repel( data = top[ , .( name, gpop, ds, region ) ][ c( 1:6, 16:21 ) ], aes( label = name ),
                            size = 2.5, colour = "grey20", max.overlaps = 30, seed = 1 ) +
  geom_vline( xintercept = 0, colour = "grey70" ) +
  scale_x_continuous( breaks = c( -25, 0, 25, 50, 100, 150, 200 ) ) +
  scale_colour_manual( values = c( Northeast = C1, Midwest = C3, South = C2, West = C4 ), name = NULL ) +
  labs( title = "Change in filer-count SAMI vs population growth, 2000 to 2024",
        subtitle = "Solid: robust lowess with 95% band, fit to metros growing under 150% (leaves out Wildwood, FL); dashed: linear fit (all metros)",
        x = "Population growth, 2000-2024 (%)", y = "Change in SAMI (log points)" ) + theme_fu
ggsave( file.path( FU, "fig_p8_change_vs_growth.png" ), g2, width = 8.5, height = 5.4, dpi = 160, bg = "white" )
## the same by region, each with its own lowess and linear fit
reg_slope <- ch[ , { r <- lm( ds ~ I( 100 * ( exp( gpop ) - 1 ) ) ); ct <- summary( r )$coefficients
  .( metros = .N, slope_per_10pct = 10 * ct[ 2, 1 ], se = 10 * ct[ 2, 2 ], p = ct[ 2, 4 ], cor = cor( ds, gpop ),
     median_growth_pct = median( 100 * ( exp( gpop ) - 1 ) ), mean_ds = mean( ds ) ) }, keyby = region ]
print( r3( reg_slope ) ); fwrite( reg_slope, file.path( FU, "p8_change_vs_growth_by_region.csv" ) )
rs_lab <- reg_slope[ , .( region, txt = sprintf( "%d metros; linear slope %+.3f per 10%% growth (p = %.2f)", metros, slope_per_10pct, p ) ) ]
g2r <- ggplot( ch, aes( 100 * ( exp( gpop ) - 1 ), ds ) ) +
  geom_hline( yintercept = 0, colour = "grey70" ) + geom_vline( xintercept = 0, colour = "grey70" ) +
  geom_point( aes( colour = region ), size = 1.5, alpha = 0.75 ) +
  geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "grey25", linewidth = 0.5, linetype = 2 ) +
  geom_smooth( data = ch[ gpop < log( 2.5 ) ], method = "loess", formula = y ~ x, span = 0.9, method.args = list( family = "symmetric" ),
               se = TRUE, colour = "grey10", fill = "grey80", linewidth = 0.9 ) +
  ggrepel::geom_text_repel( data = top[ , .( name, gpop, ds, region ) ][ c( 1:6, 16:21 ) ], aes( label = name ),
                            size = 2.3, colour = "grey20", max.overlaps = 30, seed = 1 ) +
  geom_text( data = rs_lab, aes( x = -Inf, y = Inf, label = txt ), hjust = -0.03, vjust = 1.4, size = 2.5, colour = "grey30", inherit.aes = FALSE ) +
  facet_wrap( ~ factor( region, c( "Northeast", "Midwest", "South", "West" ) ), ncol = 2 ) +
  scale_x_continuous( breaks = c( -25, 0, 25, 50, 100, 150, 200 ) ) +
  scale_colour_manual( values = c( Northeast = C1, Midwest = C3, South = C2, West = C4 ), guide = "none" ) +
  labs( title = "Change in filer-count SAMI vs population growth, 2000 to 2024, by region",
        subtitle = "Solid: robust lowess with 95% band, fit to metros growing under 150% (leaves out Wildwood, FL); dashed: linear fit (all metros)",
        x = "Population growth, 2000-2024 (%)", y = "Change in SAMI (log points)" ) + theme_fu
ggsave( file.path( FU, "fig_p8_change_vs_growth_region.png" ), g2r, width = 10, height = 7.2, dpi = 160, bg = "white" )

## ===========================================================================
## Part C: endowment models
## ===========================================================================
log_msg( "Part C: endowment models" )
## C1 beta-convergence: OLS, and IV using the BMF-based start SAMI (independent
## measurement of the same endowment) to remove regression to the mean from noise
conv <- rbindlist( list(
  { r <- feols( ds ~ s0, ch, vcov = "hetero" ); data.table( model = "OLS: change on start SAMI", lambda = coef( r )[ 2 ], se = se( r )[ 2 ], r2 = r2( r, "r2" ) ) },
  { r <- feols( ds ~ 1 | s0 ~ b0, ch, vcov = "hetero" ); data.table( model = "IV: start SAMI instrumented by BMF start SAMI", lambda = coef( r )[ 2 ], se = se( r )[ 2 ], r2 = NA_real_ ) },
  { r <- feols( ds ~ s0 + gpop, ch, vcov = "hetero" ); data.table( model = "OLS + population growth", lambda = coef( r )[ 2 ], se = se( r )[ 2 ], r2 = r2( r, "r2" ) ) },
  { r <- feols( ds_late ~ s_mid, ch, vcov = "hetero" ); data.table( model = "OLS, 2011-13 to 2022-24", lambda = coef( r )[ 2 ], se = se( r )[ 2 ], r2 = r2( r, "r2" ) ) } ) )
conv[ , half_life_years := fifelse( lambda < 0, log( 0.5 ) / log( 1 + lambda ) * 23, NA_real_ ) ]
print( r3( conv ) ); fwrite( conv, file.path( FU, "p9_convergence.csv" ) )
sig <- m[ , .( sd_sami = sd( s_n_filer_c3, na.rm = TRUE ), sd_sami_exp = sd( s_exp_c3_nohu, na.rm = TRUE ), sd_bmf = sd( s_n_bmf_c3pc, na.rm = TRUE ), sd_grass = sd( s_n_grass_c3pc, na.rm = TRUE ) ), keyby = year ]
fwrite( sig, file.path( FU, "p9_sigma_convergence.csv" ) ); print( r3( sig[ year %in% c( 2000, 2005, 2010, 2015, 2020, 2024 ) ] ) )

## C2 historical endowment as a level covariate (2022 cross-section)
x22 <- merge( m[ year == 2022 ], ch[ , .( cbsa, s_hist, s0 ) ], by = "cbsa" )
x22[ , `:=`( l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ), logP = log( population ) ) ]
base <- "log( n_filer_c3 ) ~ logP + race_hhi + l_pf + turnout + l_gov + poverty_rate + pct_ba"
hm <- list( "Covariates only" = base, "+ pre-1970 endowment" = paste( base, "+ s_hist" ), "+ 2000-02 SAMI" = paste( base, "+ s0" ) )
endow <- rbindlist( lapply( names( hm ), function( k ) { r <- feols( as.formula( hm[[ k ]] ), x22, vcov = "hetero" ); ct <- coeftable( r )
  data.table( model = k, term = rownames( ct ), est = ct[ , 1 ], t = ct[ , 3 ], partial_r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + degrees_freedom( r, "resid" ) ),
              within_r2 = r2( r, "r2" ) ) } ) )
print( r3( dcast( endow[ term != "(Intercept)" ], term ~ model, value.var = "partial_r" ) ) )
print( unique( r3( endow[ , .( model, within_r2 ) ] ) ) )
cat( "cor(s_hist, s0) =", round( cor( ch$s_hist, ch$s0, use = "complete.obs" ), 3 ), "; cor(s_hist, s1) =",
     round( cor( ch$s_hist, ch$s1, use = "complete.obs" ), 3 ), "; cor(s_hist, ds) =", round( cor( ch$s_hist, ch$ds, use = "complete.obs" ), 3 ), "\n" )
fwrite( endow, file.path( FU, "p9_endowment_levels.csv" ) )

## C3 hybrid (Mundlak / correlated random effects) model, 2010-2022:
## within (deviation from metro mean) and between (metro mean) coefficients in one model
pn <- m[ year %in% 2010:2022 ]
pn[ , `:=`( x_pf = { z <- flog( pf_assets, population ); z - fitted( lm( z ~ log( population ) ) ) }, x_gov = { z <- flog( gov_earn_sl, population ); z - fitted( lm( z ~ log( population ) ) ) } ), by = year ]
XV <- c( "race_hhi", "x_pf", "turnout", "x_gov", "poverty_rate", "pct_ba" )
for ( v in XV ) pn[ , `:=`( ( paste0( "b_", v ) ), mean( get( v ) ) ), by = cbsa ][ , ( paste0( "w_", v ) ) := get( v ) - get( paste0( "b_", v ) ) ]
pn <- merge( pn, ch[ , .( cbsa, s_hist ) ], by = "cbsa" )
for ( v in c( XV, "s_n_filer_c3" ) ) pn[ , ( paste0( "zz_", v ) ) := get( v ) / sd( get( v ) ) ]
fh <- as.formula( paste( "s_n_filer_c3 ~", paste( c( paste0( "w_", XV ), paste0( "b_", XV ) ), collapse = " + " ), "+ factor( year ) + ( 1 | cbsa )" ) )
fh2 <- update( fh, . ~ . + s_hist )
cre <- rbindlist( lapply( list( "Hybrid (CRE)" = fh, "Hybrid + pre-1970 endowment" = fh2 ), function( f ) {
  r <- lmer( f, pn, REML = FALSE ); cf <- summary( r )$coefficients; vc <- as.data.frame( VarCorr( r ) )
  data.table( term = rownames( cf ), est = cf[ , 1 ], se = cf[ , 2 ], t = cf[ , 3 ],
              var_metro = vc$vcov[ vc$grp == "cbsa" ], var_resid = vc$vcov[ vc$grp == "Residual" ] ) } ), idcol = "model" )
cre <- cre[ !grepl( "year|Intercept", term ) ]
## standardize: coefficient x SD of the regressor / SD of the SAMI
sdx <- sapply( unique( cre$term ), function( v ) sd( pn[[ v ]], na.rm = TRUE ) )
cre[ , std := est * sdx[ term ] / sd( pn$s_n_filer_c3 ) ]
cre[ , part := fcase( grepl( "^w_", term ), "within", grepl( "^b_", term ), "between", default = "time-invariant" ) ]
cre[ , var := sub( "^[wb]_", "", term ) ]
print( r3( dcast( cre, model + var ~ part, value.var = "std" ) ) )
print( unique( r3( cre[ , .( model, var_metro, var_resid, icc = var_metro / ( var_metro + var_resid ) ) ] ) ) )
fwrite( cre, file.path( FU, "p9_hybrid_cre.csv" ) )

## C4 growth-curve model 2000-2024: random intercept (level) and random slope (trend) by metro;
## the endowment (pre-1970 SAMI) predicts both
gc <- merge( m[ , .( cbsa, year, s = s_n_filer_c3 ) ], ch[ , .( cbsa, s_hist ) ], by = "cbsa" )[ is.finite( s ) ]
gc[ , t := ( year - 2012 ) / 10 ]
g0 <- lmer( s ~ t + ( 1 + t | cbsa ), gc, REML = FALSE )
g1m <- lmer( s ~ t * s_hist + ( 1 + t | cbsa ), gc[ is.finite( s_hist ) ], REML = FALSE )
vc0 <- as.data.frame( VarCorr( g0 ) )
growth <- list( sd_intercept = sqrt( vc0$vcov[ 1 ] ), sd_slope_per_decade = sqrt( vc0$vcov[ 2 ] ),
                cor_intercept_slope = vc0$sdcor[ 3 ], sd_resid = sqrt( vc0$vcov[ 4 ] ) )
print( lapply( growth, round, 3 ) ); print( round( summary( g1m )$coefficients, 3 ) )
saveRDS( list( growth = growth, endow_coef = summary( g1m )$coefficients ), file.path( FU, "p9_growth_curve.rds" ) )
fwrite( data.table( stat = names( growth ), value = unlist( growth ) ), file.path( FU, "p9_growth_curve.csv" ) )
fwrite( data.table( term = rownames( summary( g1m )$coefficients ), summary( g1m )$coefficients ), file.path( FU, "p9_growth_curve_endowment.csv" ) )

## figure: endowment vs level and vs change
g3 <- ggplot( ch, aes( s_hist, s0 ) ) + geom_point( colour = C1, alpha = 0.6, size = 1.4 ) +
  geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "grey25", linewidth = 0.6 ) +
  labs( title = "A. Pre-1970 endowment vs 2000-02 SAMI", x = "SAMI of pre-1970 charities in the 2000 BMF", y = "Filer-count SAMI, 2000-02" ) + theme_fu
g4 <- ggplot( ch, aes( s0, ds ) ) + geom_hline( yintercept = 0, colour = "grey70" ) + geom_point( colour = C2, alpha = 0.6, size = 1.4 ) +
  geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = "grey25", linewidth = 0.6 ) +
  labs( title = "B. Start level vs change (convergence)", x = "Filer-count SAMI, 2000-02", y = "Change to 2022-24" ) + theme_fu
ggsave( file.path( FU, "fig_p9_endowment.png" ), patchwork::wrap_plots( g3, g4, ncol = 2 ), width = 10, height = 4.3, dpi = 160, bg = "white" )
log_msg( "done" )
