## 75_capacity_grid.R
## "Carrying capacity": population growth history (1970-2000) and nonprofit market
## share (BLS 501(c)(3) share of private employment, 2013-2022 mean) together.
##   1. grid: quintiles of growth x quintiles of market share -> mean SAMI (2013-2022)
##      per cell, and mean SAMI change 2000-02 -> 2022-24 per cell
##   2. scatter: growth vs share, coloured by SAMI
##   3. smoothed surface: loess of SAMI on growth and log share
##   4. variance decomposition (R2 of growth, share, both, interaction) and the
##      "capacity" fit: expected SAMI given growth history and share; metros furthest
##      above / below it
## Outcomes: filers, grassroots (registered public charities not filing: postcards,
## sub-$50k EZ filers, registered churches, non-filers), all registered, expenditures.
## Needs data/catchup/c9_metro_1970_2024.csv from 73_pre2000_growth.R.
## Writes D:/MULTIVERSE/data/catchup/c10_*, fig_c10_*.
##   Rscript R/notes/75_capacity_grid.R

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( ggplot2 ) } )
setFixest_notes( FALSE )
CU <- file.path( BUILD, "data", "catchup" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
options( width = 220 )
theme_c <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"

d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )[ cbsa_type == "metro" ]
d[ , logP := log( population ) ][ , n_grass_c3pc := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]
OUTC <- c( n_filer_c3 = "Filers", n_grass_c3pc = "Grassroots (registered, not filing)", exp_c3_nohu = "Expenditures" )
for ( v in names( OUTC ) )
  d[ , ( paste0( "s_", v ) ) := { ok <- get( v ) > 0; z <- rep( NA_real_, .N ); z[ ok ] <- resid( lm( log( get( v )[ ok ] ) ~ logP[ ok ] ) ); z }, by = year ]
sv <- paste0( "s_", names( OUTC ) )
lev <- d[ year %in% 2013:2022, lapply( .SD, mean, na.rm = TRUE ), by = cbsa, .SDcols = sv ]
setnames( lev, sv, paste0( "lev_", names( OUTC ) ) )
s0 <- d[ year %in% 2000:2002, lapply( .SD, mean, na.rm = TRUE ), by = cbsa, .SDcols = sv ]; setnames( s0, sv, paste0( "a_", names( OUTC ) ) )
s1 <- d[ year %in% 2022:2024, lapply( .SD, mean, na.rm = TRUE ), by = cbsa, .SDcols = sv ]; setnames( s1, sv, paste0( "b_", names( OUTC ) ) )
sh <- d[ year %in% 2013:2022 & !( bls_delin_flag %in% TRUE ) & bls_pct_npo > 0,
         .( share = mean( bls_pct_npo ), years = .N ), by = cbsa ][ years >= 5 ]
h <- fread( file.path( CU, "c9_metro_1970_2024.csv" ), colClasses = list( character = "cbsa" ) )[ , .( cbsa, name, region, p2000, p2024, g7000, r_pre, r_post ) ]
x <- Reduce( function( a, b ) merge( a, b, by = "cbsa" ), list( h, sh, lev, s0, s1 ) )
for ( v in names( OUTC ) ) set( x, j = paste0( "d_", v ), value = x[[ paste0( "b_", v ) ]] - x[[ paste0( "a_", v ) ]] )
x[ , `:=`( growth_pct_yr = 100 * r_pre, l_share = log( share ) ) ]
q5 <- function( z, lab ) cut( z, quantile( z, 0:5 / 5 ), include.lowest = TRUE, labels = lab )
x[ , `:=`( gq = q5( g7000, c( "Slowest", "Q2", "Q3", "Q4", "Fastest" ) ), sq = q5( share, c( "Lowest", "Q2", "Q3", "Q4", "Highest" ) ) ) ]
log_msg( "metros with growth history and BLS share: ", nrow( x ), "; cor(growth 1970-2000, log share) = ", round( cor( x$g7000, x$l_share ), 3 ) )
qb <- function( z ) round( quantile( z, 0:5 / 5 ), 3 )
cat( "growth quintile breaks (%/yr):", qb( x$growth_pct_yr ), "\nshare quintile breaks (%):", qb( 100 * x$share ), "\n" )
fwrite( x, file.path( CU, "c10_metro.csv" ) )

## ---- 1. grid ------------------------------------------------------------------------------
grid <- rbindlist( lapply( names( OUTC ), function( v ) x[ , .( metros = .N, sami = mean( get( paste0( "lev_", v ) ), na.rm = TRUE ),
  change = mean( get( paste0( "d_", v ) ), na.rm = TRUE ) ), keyby = .( gq, sq ) ][ , outcome := v ] ) )
grid[ , `:=`( pct_dev = 100 * ( exp( sami ) - 1 ), pct_change = 100 * ( exp( change ) - 1 ) ) ]
fwrite( grid, file.path( CU, "c10_grid.csv" ) )
for ( v in names( OUTC ) ) { cat( "\n==", OUTC[[ v ]], ": mean % deviation from size-expected, 2013-2022 (rows: growth 1970-2000; cols: market share)\n" )
  print( dcast( grid[ outcome == v ], gq ~ sq, value.var = "pct_dev", fun.aggregate = function( z ) round( z ) ) ) }
cat( "\nmetros per cell:\n" ); print( dcast( grid[ outcome == "n_filer_c3" ], gq ~ sq, value.var = "metros" ) )
for ( v in c( "n_filer_c3", "n_grass_c3pc" ) ) { cat( "\n==", OUTC[[ v ]], ": mean % change in density, 2000-02 -> 2022-24\n" )
  print( dcast( grid[ outcome == v ], gq ~ sq, value.var = "pct_change", fun.aggregate = function( z ) round( z ) ) ) }
## row and column margins
marg <- rbindlist( lapply( names( OUTC ), function( v ) rbind(
  x[ , .( axis = "growth", level = as.character( gq ), pct_dev = 100 * ( exp( mean( get( paste0( "lev_", v ) ), na.rm = TRUE ) ) - 1 ) ), keyby = gq ][ , gq := NULL ],
  x[ , .( axis = "share", level = as.character( sq ), pct_dev = 100 * ( exp( mean( get( paste0( "lev_", v ) ), na.rm = TRUE ) ) - 1 ) ), keyby = sq ][ , sq := NULL ] )[ , outcome := v ] ) )
fwrite( marg, file.path( CU, "c10_margins.csv" ) )

lims <- function( z ) { m <- max( abs( z ), na.rm = TRUE ); c( -m, m ) }
heat <- function( dt, val, ttl, sub, legend, cap ) {
  ggplot( dt, aes( sq, gq, fill = get( val ) ) ) + geom_tile( colour = "white", linewidth = 0.8 ) +
    geom_text( aes( label = sprintf( "%+.0f%%\nn=%d", get( val ), metros ) ), size = 2.5, lineheight = 0.9,
               colour = ifelse( abs( dt[[ val ]] ) > 0.6 * cap, "white", "grey15" ) ) +
    facet_wrap( ~ factor( OUTC[ outcome ], OUTC ) ) +
    scale_fill_gradient2( low = C2, mid = "#f4f4f4", high = C1, midpoint = 0, limits = c( -cap, cap ), oob = scales::squish, name = legend ) +
    labs( title = ttl, subtitle = sub, x = "Nonprofit share of private employment, 2013-2022 (quintile; BLS)",
          y = "Population growth 1970-2000 (quintile)" ) + theme_c + theme( panel.grid = element_blank() ) }
ggsave( file.path( CU, "fig_c10_grid.png" ),
  heat( grid[ outcome != "n_bmf_c3pc" ], "pct_dev", "Nonprofit density by growth history and nonprofit market share",
        paste0( "Mean SAMI 2013-2022 as % above (+) or below (-) the size-expected level; quintile breaks: growth ",
                paste( round( quantile( x$growth_pct_yr, 1:4 / 5 ), 1 ), collapse = "/" ), " %/yr; share ",
                paste( round( 100 * quantile( x$share, 1:4 / 5 ), 1 ), collapse = "/" ), "%" ),
        "% vs size-expected (colour capped at +/-60)", 60 ), width = 11, height = 4.6, dpi = 160, bg = "white" )
ggsave( file.path( CU, "fig_c10_grid_change.png" ),
  heat( grid[ outcome != "n_bmf_c3pc" ], "pct_change", "Change in nonprofit density 2000-02 to 2022-24, by growth history and market share",
        "Mean change in SAMI, as % change in the size-adjusted count", "% change (colour capped at +/-15)", 15 ), width = 11, height = 4.6, dpi = 160, bg = "white" )

## ---- 2. scatter -------------------------------------------------------------------------
lab <- c( "Las Vegas, NV", "Austin, TX", "Raleigh, NC", "Myrtle Beach, SC", "Cape Coral, FL", "Naples, FL", "Phoenix, AZ",
          "Boston, MA", "Pittsburgh, PA", "Rochester, MN", "Provo, UT", "McAllen, TX", "Charleston, SC", "Santa Rosa, CA",
          "Houston, TX", "Ocala, FL", "Burlington, VT", "Boise City, ID", "St. George, UT", "Ithaca, NY" )
sc <- melt( x[ , c( "cbsa", "name", "growth_pct_yr", "share", "p2024", paste0( "lev_", c( "n_filer_c3", "n_grass_c3pc" ) ) ), with = FALSE ],
            id.vars = c( "cbsa", "name", "growth_pct_yr", "share", "p2024" ), variable.name = "outcome", value.name = "sami" )
sc[ , outcome := factor( OUTC[ sub( "^lev_", "", outcome ) ], OUTC ) ]
g <- ggplot( sc, aes( growth_pct_yr, 100 * share ) ) +
  geom_point( aes( fill = pmax( pmin( sami, 0.8 ), -0.8 ), size = p2024 ), shape = 21, colour = "white", stroke = 0.3, alpha = 0.9 ) +
  ggrepel::geom_text_repel( data = sc[ name %in% lab ], aes( label = name ), size = 2.2, colour = "grey25", seed = 3, max.overlaps = 30 ) +
  scale_y_log10( breaks = c( 2, 5, 10, 20, 40 ) ) + scale_size_area( max_size = 7, guide = "none" ) +
  scale_fill_gradient2( low = C2, mid = "#f2f2f2", high = C1, midpoint = 0, limits = c( -0.8, 0.8 ), name = "SAMI 2013-2022" ) +
  facet_wrap( ~ outcome ) +
  labs( title = "Growth history, nonprofit market share and density",
        subtitle = "Each point is a metro (size = 2024 population); colour = mean SAMI 2013-2022 (blue above, orange below size-expected; capped at +/-0.8)",
        x = "Population growth 1970-2000 (%/yr)", y = "Nonprofit share of private employment (%, log scale)" ) + theme_c
ggsave( file.path( CU, "fig_c10_scatter.png" ), g, width = 11, height = 5.4, dpi = 160, bg = "white" )

## ---- 3. smoothed surface ------------------------------------------------------------------
surf <- rbindlist( lapply( c( "n_filer_c3", "n_grass_c3pc", "exp_c3_nohu" ), function( v ) {
  y <- x[ is.finite( get( paste0( "lev_", v ) ) ) ]
  fit <- loess( as.formula( paste0( "lev_", v, " ~ growth_pct_yr * l_share" ) ), y, span = 0.6, degree = 1 )
  gr <- CJ( growth_pct_yr = seq( quantile( y$growth_pct_yr, .02 ), quantile( y$growth_pct_yr, .98 ), length.out = 60 ),
            l_share = seq( quantile( y$l_share, .02 ), quantile( y$l_share, .98 ), length.out = 60 ) )
  gr[ , fit := predict( fit, newdata = gr ) ]
  ## keep grid points with at least 6 metros within a standardized radius of 0.5 SD
  sg <- sd( y$growth_pct_yr ); ss <- sd( y$l_share )
  gr[ , support := sapply( seq_len( .N ), function( i ) sum( ( ( y$growth_pct_yr - growth_pct_yr[ i ] ) / sg )^2 + ( ( y$l_share - l_share[ i ] ) / ss )^2 < 0.25 ) ) ]
  gr[ support >= 6 ][ , outcome := v ] } ) )
surf[ , outcome := factor( OUTC[ outcome ], OUTC ) ]
g <- ggplot( surf, aes( growth_pct_yr, 100 * exp( l_share ) ) ) +
  geom_raster( aes( fill = pmax( pmin( fit, 0.6 ), -0.6 ) ), interpolate = TRUE ) +
  geom_contour( aes( z = fit ), breaks = seq( -0.6, 0.6, 0.2 ), colour = "grey30", linewidth = 0.25 ) +
  geom_point( data = x, aes( growth_pct_yr, 100 * share ), inherit.aes = FALSE, size = 0.4, colour = "grey40", alpha = 0.5 ) +
  scale_y_log10( breaks = c( 3, 5, 10, 20 ) ) + facet_wrap( ~ outcome ) +
  scale_fill_gradient2( low = C2, mid = "#f4f4f4", high = C1, midpoint = 0, limits = c( -0.6, 0.6 ), name = "Fitted SAMI" ) +
  labs( title = "Fitted density surface: growth history x nonprofit market share",
        subtitle = "Loess of mean SAMI 2013-2022 on 1970-2000 growth and log share (span 0.6); contours every 0.2 log points; shown only where data support it",
        x = "Population growth 1970-2000 (%/yr)", y = "Nonprofit share of private employment (%, log scale)" ) + theme_c
ggsave( file.path( CU, "fig_c10_surface.png" ), g, width = 11, height = 4.6, dpi = 160, bg = "white" )

## ---- 4. variance decomposition and the capacity fit ----------------------------------------
dec <- rbindlist( lapply( names( OUTC ), function( v ) { y <- paste0( "lev_", v ); z <- x[ is.finite( get( y ) ) ]
  z[ , `:=`( zg = ( growth_pct_yr - mean( growth_pct_yr ) ) / sd( growth_pct_yr ), zs = ( l_share - mean( l_share ) ) / sd( l_share ) ) ]
  R <- function( f ) summary( lm( as.formula( paste( y, f ) ), z ) )$r.squared
  full <- feols( as.formula( paste( y, "~ zg * zs" ) ), z, vcov = "hetero" ); ct <- coeftable( full )
  dch <- feols( as.formula( paste0( "d_", v, " ~ zg * zs + a_", v ) ), z, vcov = "hetero" ); cd <- coeftable( dch )
  data.table( outcome = v, n = nrow( z ), r2_growth = R( "~ zg" ), r2_share = R( "~ zs" ), r2_both = R( "~ zg + zs" ), r2_inter = R( "~ zg * zs" ),
              r2_region = R( "~ factor( region )" ), r2_both_region = R( "~ zg * zs + factor( region )" ),
              b_growth = ct[ "zg", 1 ], se_growth = ct[ "zg", 2 ], b_share = ct[ "zs", 1 ], se_share = ct[ "zs", 2 ],
              b_inter = ct[ "zg:zs", 1 ], se_inter = ct[ "zg:zs", 2 ], p_inter = ct[ "zg:zs", 4 ],
              chg_growth = cd[ "zg", 1 ], chg_growth_p = cd[ "zg", 4 ], chg_share = cd[ "zs", 1 ], chg_share_p = cd[ "zs", 4 ],
              chg_inter = cd[ "zg:zs", 1 ], chg_inter_p = cd[ "zg:zs", 4 ] ) } ) )
dec[ , `:=`( shapley_growth = ( r2_growth + ( r2_both - r2_share ) ) / 2, shapley_share = ( r2_share + ( r2_both - r2_growth ) ) / 2 ) ]
print( r3( dec ) ); fwrite( dec, file.path( CU, "c10_decomposition.csv" ) )
## capacity fit for filers and grassroots: residual = above / below what growth history and share predict
cap <- rbindlist( lapply( c( "n_filer_c3", "n_grass_c3pc" ), function( v ) { y <- paste0( "lev_", v )
  r <- lm( as.formula( paste( y, "~ growth_pct_yr * l_share" ) ), x )
  z <- x[ , .( cbsa, name, region, growth_pct_yr, share_pct = 100 * share, sami = get( y ) ) ][ , `:=`( expected = fitted( r ), outcome = v ) ]
  z[ , gap := sami - expected ] } ) )
fwrite( cap, file.path( CU, "c10_capacity_fit.csv" ) )
for ( v in c( "n_filer_c3", "n_grass_c3pc" ) ) { z <- cap[ outcome == v ][ order( gap ) ]
  cat( "\n==", OUTC[[ v ]], ": furthest below / above the growth x share expectation (log points)\n" )
  print( r3( rbind( z[ 1:10 ], z[ ( .N - 9 ):.N ] )[ , .( name, region, growth_pct_yr, share_pct, sami, expected, gap ) ] ) ) }
log_msg( "done" )
