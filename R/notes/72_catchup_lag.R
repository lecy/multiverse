## 72_catchup_lag.R
## How far does nonprofit-sector growth lag metro population growth, how long
## does catch-up take, and what predicts it? 387 metros, 2000-2024.
##   Part 1  horizon elasticities: d log N over h years on d log P over the same
##           h years (h = 1..24), against the cross-sectional scaling exponent b.
##           elasticity / b = share of size-expected growth realized by horizon h.
##   Part 2  local projections (impulse response of the SAMI to a growth episode):
##           s[t+h] - s[t-3] on growth t-3 -> t, holding prior growth (t-6 -> t-3),
##           later growth (t -> t+h) and the starting SAMI fixed. theta_h / theta_0
##           = share of the growth-induced deficit still open h years later.
##           Also new-registration (entry) rates on lagged growth: the mechanism.
##   Part 3  trajectories: SAMI paths by 2000-2010 growth quintile; fast growers.
##   Part 4  what predicts catch-up over 2010-12 -> 2022-24: growth (both
##           decades), the deficit, endowment levels in 2000, change over the
##           catch-up decade; moderators of recovery (interactions with 2000s
##           growth); Shapley R2 by block.
##   Part 5  declines: horizon elasticities and local projections split by the
##           sign of population change; relative income gains vs losses.
##   Part 6  mechanism: entries, implied exits and filing share (filers /
##           registered) on lagged growth, across and within metros.
##   Part 7  evidence figures for the catch-up predictors (needs part 4 output):
##           partial r by factor, added-variable plots, recovery slopes by
##           tercile of 2000 conditions, Shapley R2.
##   Part 8  BLS nonprofit market share (share of private employment) and the
##           composition-adjusted nonprofit wage ratio, 2013-2022: density
##           levels, catch-up 2013-15 -> 2022-24, convergence by share tercile
##           (ceiling test), health-care nonprofit share.
## Outcomes: filer counts (crosses the 2011 CORE -> SOI splice), reconstructed
## BMF stock (no splice), expenditures excl. hospitals/universities.
## Writes D:/MULTIVERSE/data/catchup/ (c1_* ... c8_*, fig_c*); write-up in notes/catchup-lag-results.md.
##   Rscript R/notes/72_catchup_lag.R [parts]      parts: 1-8 (default all; 7 reads part 4 output)

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( ggplot2 ) } )
setFixest_notes( FALSE )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
## log of an extensive covariate with a per-capita floor (as flog() in 40_multiverse.R, run v6 on;
## switched here 2026-11-16). log(x + 1) put zero-asset metros (Eagle Pass, TX) ~11 SD below the mean.
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
options( width = 220 )
## "grass" anywhere in the arguments switches the outcome set to filers / grassroots /
## all registered and writes to data/catchup_grass/ instead of data/catchup/
args <- commandArgs( TRUE ); GRASS <- "grass" %in% args; args <- setdiff( args, "grass" )
PARTS <- if ( length( args ) ) args else as.character( 1:8 )
CU <- file.path( BUILD, "data", if ( GRASS ) "catchup_grass" else "catchup" ); dir.create( CU, showWarnings = FALSE, recursive = TRUE )
theme_c <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"; C5 <- "#7c5cd6"

d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
m <- d[ cbsa_type == "metro" ]; rm( d )
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
m[ , name := paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ) ]
setorder( m, cbsa, year )
m[ , logP := log( population ) ]

## year-specific scaling lines and SAMIs
## grassroots: registered public charities that are not filers (postcard 990-N filers, 990-EZ
## filers below the $50k floor, registered churches and other non-filers, dormant registrations)
m[ , n_grass_c3pc := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]
## default: filers vs grassroots (+ expenditures); "grass" mode keeps the pooled registered
## stock as a splice-free robustness check (data/catchup_grass/)
OUTC <- if ( GRASS ) c( n_filer_c3 = "Filers", n_grass_c3pc = "Grassroots (registered, not filing)", n_bmf_c3pc = "Registered (all)" ) else
  c( n_filer_c3 = "Filers", n_grass_c3pc = "Grassroots (registered, not filing)", exp_c3_nohu = "Expenditures" )
bt <- list()
for ( v in unique( c( names( OUTC ), "n_bmf_c3pc" ) ) ) {      # registered SAMI always built: part 6 uses it as a control
  m[ , c( "a_", "b_" ) := { ok <- get( v ) > 0; cf <- coef( lm( log( get( v )[ ok ] ) ~ logP[ ok ] ) ); list( cf[ 1 ], cf[ 2 ] ) }, by = year ]
  m[ , ( paste0( "s_", v ) ) := fifelse( get( v ) > 0, log( get( v ) ) - a_ - b_ * logP, NA_real_ ) ]
  bt[[ v ]] <- unique( m[ , .( outcome = v, year, b = b_ ) ] )
  m[ , c( "a_", "b_" ) := NULL ]
}
bt <- rbindlist( bt )
bsum <- bt[ , .( b = mean( b ), b_2000 = b[ year == 2000 ], b_2024 = b[ year == 2024 ] ), by = outcome ]
print( r3( bsum ) )
resid_logP <- function( y, logP, year ) {
  out <- rep( NA_real_, length( y ) )
  for ( t in unique( year ) ) { i <- which( year == t & is.finite( y ) & is.finite( logP ) )
    if ( length( i ) > 3 ) out[ i ] <- resid( lm( y[ i ] ~ logP[ i ] ) ) }
  out }
m[ , `:=`( x_pf = resid_logP( flog( pf_assets, population ), logP, year ), x_gov = resid_logP( flog( gov_earn_sl, population ), logP, year ),
           x_wage = resid_logP( flog( qcew_priv_wages, population ), logP, year ), x_gdp = resid_logP( flog( gdp_real, population ), logP, year ),
           l_pcpi = log( pcpi ), l_mhi = log( med_hh_income ) ) ]
## entry: new 501(c)(3) public-charity registrations per 100 registered orgs (stock at t-1)
m[ , entry_rate := 100 * n_entry_c3pc / shift( n_bmf_c3pc ), by = cbsa ]
m[ , entry_pc := 1e5 * n_entry_c3pc / population ]
## implied exits from the reconstructed stock: stock[t-1] + entries[t] - stock[t] (year-specific
## source artifacts, e.g. the 2011 auto-revocations, are absorbed by year FE)
m[ , exit_rate := 100 * ( shift( n_bmf_c3pc ) + n_entry_c3pc - n_bmf_c3pc ) / shift( n_bmf_c3pc ), by = cbsa ]
m[ , file_share := log( n_filer_c3 / n_bmf_c3pc ) ]          # share of registered orgs that file
## relative income change: 3-year d log per capita income minus the year mean (nominal; year FE absorbs inflation)
m[ , inc3 := l_pcpi - shift( l_pcpi, 3 ), by = cbsa ][ , inc3r := inc3 - mean( inc3, na.rm = TRUE ), by = year ]

## % effect on density of a change of log size `shift` given a coefficient per log point
pct10 <- function( b, shift ) 100 * ( exp( b * shift ) - 1 )
K <- copy( m ); setkey( K, cbsa, year )
at <- function( col, cb, yr ) K[ .( cb, yr ), get( col ) ]
YMAX <- max( m$year )

## ===========================================================================
## Part 1: horizon elasticities
## ===========================================================================
if ( "1" %in% PARTS ) {
  log_msg( "Part 1: horizon elasticities" )
  H <- c( 1, 2, 3, 5, 8, 10, 15, 20, 24 )
  p1 <- rbindlist( lapply( names( OUTC ), function( v ) rbindlist( lapply( H, function( h ) {
    x <- m[ year + h <= YMAX & get( v ) > 0, .( cbsa, year, region, lN0 = log( get( v ) ), lP0 = logP, s0 = get( paste0( "s_", v ) ) ) ]
    x[ , `:=`( lN1 = log( at( v, cbsa, year + h ) ), lP1 = at( "logP", cbsa, year + h ) ) ]
    x <- x[ is.finite( lN1 ) ][ , `:=`( dN = lN1 - lN0, dP = lP1 - lP0 ) ]
    r1 <- feols( dN ~ dP | year, x, cluster = ~cbsa )
    r2 <- feols( dN ~ dP + s0 | year, x, cluster = ~cbsa )
    r3_ <- feols( dN ~ dP + s0 | year^region, x, cluster = ~cbsa )
    data.table( outcome = v, h = h, n = nobs( r1 ), start_years = uniqueN( x$year ), sd_dP = sd( x$dP ),
                elast = coef( r1 )[[ "dP" ]], se = se( r1 )[[ "dP" ]],
                elast_cond = coef( r2 )[[ "dP" ]], se_cond = se( r2 )[[ "dP" ]],
                elast_region = coef( r3_ )[[ "dP" ]], se_region = se( r3_ )[[ "dP" ]] )
  } ) ) ) )
  p1 <- merge( p1, bsum[ , .( outcome, b ) ], by = "outcome" )
  p1[ , `:=`( realized = elast / b, realized_cond = elast_cond / b, realized_region = elast_region / b ) ]
  setorder( p1, outcome, h ); print( r3( p1 ) )
  fwrite( p1, file.path( CU, "c1_horizon_elasticity.csv" ) )
  g <- ggplot( p1, aes( h, realized_cond, colour = OUTC[ outcome ] ) ) +
    geom_hline( yintercept = c( 0, 1 ), colour = "grey75" ) +
    geom_ribbon( aes( ymin = ( elast_cond - 1.96 * se_cond ) / b, ymax = ( elast_cond + 1.96 * se_cond ) / b, fill = OUTC[ outcome ] ),
                 alpha = 0.12, colour = NA ) +
    geom_line( linewidth = 0.8 ) + geom_point( size = 1.6 ) +
    scale_colour_manual( values = c( C1, C2, C3 ), name = NULL ) + scale_fill_manual( values = c( C1, C2, C3 ), guide = "none" ) +
    labs( title = "Share of size-expected nonprofit growth realized, by horizon",
          subtitle = "Elasticity of d log N on d log P over h years (start-year FE, start SAMI held fixed) / cross-sectional scaling exponent",
          x = "Horizon (years)", y = "Elasticity / scaling exponent" ) + theme_c
  ggsave( file.path( CU, "fig_c1_horizon.png" ), g, width = 8, height = 4.6, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part 2: local projections and entry
## ===========================================================================
if ( "2" %in% PARTS ) {
  log_msg( "Part 2: local projections" )
  LPH <- 0:15
  lp <- rbindlist( lapply( names( OUTC ), function( v ) {
    sv <- paste0( "s_", v )
    rbindlist( lapply( LPH, function( h ) {
      x <- m[ year >= 2006 & year + h <= YMAX, .( cbsa, year, region, state_fips, logP ) ]
      x[ , `:=`( s_m3 = at( sv, cbsa, year - 3 ), s_h = at( sv, cbsa, year + h ),
                 g_past = logP - at( "logP", cbsa, year - 3 ), g_pre = at( "logP", cbsa, year - 3 ) - at( "logP", cbsa, year - 6 ),
                 g_fut = at( "logP", cbsa, year + h ) - logP ) ]
      x <- x[ is.finite( s_m3 ) & is.finite( s_h ) ][ , dep := s_h - s_m3 ]
      fx <- if ( h == 0 ) "dep ~ g_past + g_pre + s_m3" else "dep ~ g_past + g_pre + g_fut + s_m3"
      ## no_fut: classic LP (later growth not held fixed, so the response includes growth persistence)
      ## metro_year: metro FE = metro-specific SAMI trend; theta from growth accelerations within a metro
      fits <- list( year = feols( as.formula( paste( fx, "| year" ) ), x, cluster = ~cbsa ),
                    region_year = feols( as.formula( paste( fx, "| year^region" ) ), x, cluster = ~cbsa ),
                    state_year = feols( as.formula( paste( fx, "| year^state_fips" ) ), x, cluster = ~cbsa ),
                    no_fut = feols( dep ~ g_past + g_pre + s_m3 | year, x, cluster = ~cbsa ),
                    metro_year = feols( as.formula( paste( fx, "| cbsa + year" ) ), x, cluster = ~cbsa ) )
      rbindlist( lapply( names( fits ), function( k ) { r <- fits[[ k ]]; ct <- coeftable( r )
        data.table( outcome = v, h = h, fe = k, n = nobs( r ), start_years = paste( range( x$year ), collapse = "-" ),
                    theta = ct[ "g_past", 1 ], se = ct[ "g_past", 2 ],
                    fut = if ( "g_fut" %in% rownames( ct ) ) ct[ "g_fut", 1 ] else NA_real_,
                    fut_se = if ( "g_fut" %in% rownames( ct ) ) ct[ "g_fut", 2 ] else NA_real_,
                    rho = ct[ "s_m3", 1 ] ) } ) )
    } ) )
  } ) )
  lp[ , theta0 := theta[ h == 0 ], by = .( outcome, fe ) ][ , remaining := theta / theta0 ]
  pk <- function( z ) if ( length( z ) ) z[ 1 ] else NA_real_
  half <- lp[ , .( theta0 = pk( theta0 ), theta5 = pk( theta[ h == 5 ] ), theta10 = pk( theta[ h == 10 ] ), theta15 = pk( theta[ h == 15 ] ),
                   remaining10 = pk( remaining[ h == 10 ] ), remaining15 = pk( remaining[ h == 15 ] ),
                   half_life = { i <- which( remaining <= 0.5 ); if ( length( i ) ) as.numeric( h[ min( i ) ] ) else NA_real_ } ), by = .( outcome, fe ) ]
  half <- merge( half, bsum[ , .( outcome, b ) ], by = "outcome" )
  ## theta0 = -b means no nonprofit response at all during the growth episode
  half[ , immediate_shortfall := -theta0 / b ]
  print( r3( lp[ fe == "year", .( outcome, h, n, theta, se, remaining, fut, rho ) ] ), nrows = 100 )
  print( r3( half ) )
  fwrite( lp, file.path( CU, "c2_local_projection.csv" ) ); fwrite( half, file.path( CU, "c2_half_life.csv" ) )
  FEL <- c( year = "Across metros (year FE)", metro_year = "Within metro (metro + year FE)" )
  ## plotted in % terms: density after a 10% population increase relative to the same metro
  ## without it (0 = nonprofits fully caught up; -5% = 5% fewer organizations than the new
  ## population size implies)
  pg <- lp[ fe %in% names( FEL ) & h <= 12 ][ , `:=`( eff = pct10( theta, log( 1.1 ) ), lo = pct10( theta - 1.96 * se, log( 1.1 ) ),
                                                    hi = pct10( theta + 1.96 * se, log( 1.1 ) ) ) ]
  g <- ggplot( pg, aes( h, eff, colour = fe, fill = fe ) ) +
    geom_hline( yintercept = 0, colour = "grey50" ) +
    geom_ribbon( aes( ymin = lo, ymax = hi ), alpha = 0.12, colour = NA ) +
    geom_line( linewidth = 0.8 ) + geom_point( size = 1.4 ) + facet_wrap( ~ OUTC[ outcome ], scales = "free_y" ) +
    scale_colour_manual( values = c( year = C2, metro_year = C1 ), labels = FEL, name = NULL ) +
    scale_fill_manual( values = c( year = C2, metro_year = C1 ), guide = "none" ) +
    labs( title = "What happens to nonprofit density after a metro grows 10% in three years",
          subtitle = paste( "Year 0 = end of the 3-year growth spurt. y = density vs. size-expected, relative to the same metro without the spurt.",
                            "0 = nonprofits have fully caught up; -5% = 5% fewer than the new population implies.", sep = "\n" ),
          x = "Years after the growth spurt", y = "Effect on density (%)" ) + theme_c
  ggsave( file.path( CU, "fig_c2_irf.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part 3 + 4 data: metro-level periods
## ===========================================================================
avg <- function( v, yrs ) m[ year %in% yrs, .( z = mean( get( v ), na.rm = TRUE ) ), by = cbsa ]$z
ch <- m[ year == 2000, .( cbsa, name, region, state_fips, pop0 = population ) ]
ch[ , pop1 := m[ year == YMAX ][ ch, on = "cbsa", population ] ]
Y0 <- 2000:2002; YM <- 2010:2012; Y1 <- 2022:2024
for ( v in names( OUTC ) ) {
  sv <- paste0( "s_", v )
  set( ch, j = paste0( "s0_", v ), value = avg( sv, Y0 ) )
  set( ch, j = paste0( "sm_", v ), value = avg( sv, YM ) )
  set( ch, j = paste0( "s1_", v ), value = avg( sv, Y1 ) )
}
ch[ , `:=`( gA = avg( "logP", YM ) - avg( "logP", Y0 ), gB = avg( "logP", Y1 ) - avg( "logP", YM ) ) ]
LV <- c( pct_ba = "% BA", l_pcpi = "Per capita income (log)", poverty_rate = "Poverty rate", pct_65plus = "% 65+",
         pct_nonwhite = "% nonwhite", pct_foreign_born = "% foreign-born", x_pf = "Foundation assets (SAMI)",
         x_gov = "S&L gov earnings (SAMI)", x_wage = "Private wages (SAMI)", turnout = "Turnout", pct_adherent = "% adherent",
         gini = "Gini", unemp_rate = "Unemployment" )
for ( v in names( LV ) ) {
  set( ch, j = paste0( "L0_", v ), value = avg( v, Y0 ) )
  set( ch, j = paste0( "Lm_", v ), value = avg( v, YM ) )
  set( ch, j = paste0( "dA_", v ), value = avg( v, YM ) - avg( v, Y0 ) )
  set( ch, j = paste0( "dB_", v ), value = avg( v, Y1 ) - avg( v, YM ) )
}
## pre-1970 endowment (SAMI of pre-1970 rulings in the 2000 BMF stock; group-exemption artifacts dropped)
en <- fread( file.path( BUILD, "data", "cov", "cov_endowment.csv" ), colClasses = list( character = "cbsa" ) )
ch <- merge( ch, en[ , .( cbsa, hist_pre1970, artifact ) ], by = "cbsa", all.x = TRUE )
ch[ , L0_hist := { ok <- is.finite( hist_pre1970 ) & hist_pre1970 > 0 & !( artifact %in% TRUE ); z <- rep( NA_real_, .N )
                   z[ ok ] <- resid( lm( log( hist_pre1970[ ok ] ) ~ log( pop0[ ok ] ) ) ); z } ]
LV <- c( LV, hist = "Pre-1970 endowment (SAMI)" )
fwrite( ch, file.path( CU, "c3_metro_periods.csv" ) )
log_msg( "metros: ", nrow( ch ), "; growth 2000s (log) median ", round( median( ch$gA ), 3 ), ", 2010s ", round( median( ch$gB ), 3 ),
         "; cor(gA, gB) = ", round( cor( ch$gA, ch$gB ), 3 ) )

## ===========================================================================
## Part 3: trajectories
## ===========================================================================
if ( "3" %in% PARTS ) {
  log_msg( "Part 3: trajectories" )
  ch[ , gq := cut( gA, quantile( gA, 0:5 / 5 ), include.lowest = TRUE, labels = paste0( "Q", 1:5 ) ) ]
  tr <- merge( m[ , c( "cbsa", "year", paste0( "s_", names( OUTC ) ) ), with = FALSE ], ch[ , .( cbsa, gq ) ], by = "cbsa" )
  tr <- melt( tr, id.vars = c( "cbsa", "year", "gq" ), variable.name = "outcome", value.name = "s" )[ , outcome := sub( "^s_", "", outcome ) ]
  trq <- tr[ , .( s = mean( s, na.rm = TRUE ), n = .N ), by = .( outcome, gq, year ) ]
  trq[ , s_rel := s - s[ year == 2000 ], by = .( outcome, gq ) ]
  gqr <- ch[ , .( metros = .N, growth_2000s_pct = 100 * ( exp( median( gA ) ) - 1 ), growth_2010s_pct = 100 * ( exp( median( gB ) ) - 1 ),
                  south_west = mean( region %in% c( "South", "West" ) ) ), keyby = gq ]
  print( r3( gqr ) ); print( r3( dcast( trq[ year %in% c( 2000, 2005, 2010, 2015, 2020, 2024 ) ], outcome + gq ~ year, value.var = "s" ) ) )
  fwrite( trq, file.path( CU, "c3_trajectories.csv" ) ); fwrite( gqr, file.path( CU, "c3_growth_quintiles.csv" ) )
  g <- ggplot( trq, aes( year, s, colour = gq ) ) + geom_hline( yintercept = 0, colour = "grey80" ) + geom_line( linewidth = 0.8 ) +
    facet_wrap( ~ factor( OUTC[ outcome ], OUTC ) ) +                        # shared y-axis across outcomes
    scale_colour_manual( values = c( "#b8c4d6", "#8ca3c4", "#5f83b5", C1, "#123f7a" ), name = "Population growth 2000-2010, quintile" ) +
    labs( title = "Mean SAMI by 2000s population growth quintile", x = NULL, y = "Mean SAMI (log deviation from size-expected)" ) + theme_c
  ggsave( file.path( CU, "fig_c3_trajectories.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )
  ## the same paths re-based so each quintile starts at 0 in 2000: change since 2000, not level
  ## one shared y-axis so the size of the moves is comparable across outcomes
  trq[ , olab := factor( OUTC[ outcome ], OUTC ) ]
  g <- ggplot( trq, aes( year, s_rel, colour = gq ) ) + geom_hline( yintercept = 0, colour = "grey60" ) + geom_line( linewidth = 0.8 ) +
    ggrepel::geom_text_repel( data = trq[ year == max( year ) ], aes( label = gq ), direction = "y", hjust = 0, nudge_x = 0.6,
                              xlim = c( max( trq$year ) + 0.4, NA ), size = 2.5, segment.size = 0.2, segment.colour = "grey70",
                              min.segment.length = 0, show.legend = FALSE, seed = 1 ) +
    facet_wrap( ~ olab ) +
    scale_x_continuous( expand = expansion( add = c( 0.5, 4 ) ) ) +
    scale_colour_manual( values = c( "#b8c4d6", "#8ca3c4", "#5f83b5", C1, "#123f7a" ), name = "Population growth 2000-2010, quintile" ) +
    labs( title = "Change in mean SAMI since 2000, by 2000s population growth quintile",
          subtitle = "Each quintile re-based to 0 in 2000; above 0 = denser relative to size than in 2000. Same y-axis in every panel (0.1 log points = about 10%).",
          x = NULL, y = "Change in mean SAMI since 2000 (log points)" ) + theme_c
  ggsave( file.path( CU, "fig_c3_trajectories_change.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )
  ## expenditures per capita (real 2022 $, excl. hospitals/universities) by the same quintiles:
  ## level (median metro) and % change since 2000 (mean of each metro's log change), all metros dashed
  pc <- merge( m[ , .( cbsa, year, pc = exp_c3_nohu / population ) ][ pc > 0 ], ch[ , .( cbsa, gq ) ], by = "cbsa" )
  pc[ , dl := log( pc ) - log( pc[ year == 2000 ] ), by = cbsa ]
  pq <- rbind( pc[ , .( median_pc = median( pc ), chg = 100 * ( exp( mean( dl, na.rm = TRUE ) ) - 1 ) ), by = .( gq, year ) ],
               pc[ , .( gq = "All metros", median_pc = median( pc ), chg = 100 * ( exp( mean( dl, na.rm = TRUE ) ) - 1 ) ), by = year ] )
  pq[ , gq := factor( gq, c( paste0( "Q", 1:5 ), "All metros" ) ) ]
  fwrite( pq, file.path( CU, "c3_exp_per_capita.csv" ) )
  print( dcast( pq[ year %in% c( 2000, 2010, 2024 ) ], gq ~ year, value.var = c( "median_pc", "chg" ), fun.aggregate = function( z ) round( z ) ) )
  QC <- c( "#b8c4d6", "#8ca3c4", "#5f83b5", C1, "#123f7a", "grey35" )
  ga <- ggplot( pq, aes( year, median_pc, colour = gq, linetype = gq == "All metros" ) ) + geom_line( linewidth = 0.8 ) +
    scale_y_continuous( labels = scales::dollar ) + scale_linetype_manual( values = c( "FALSE" = 1, "TRUE" = 2 ), guide = "none" ) +
    scale_colour_manual( values = QC, name = "Population growth 2000-2010, quintile", guide = guide_legend( nrow = 1 ) ) +
    labs( title = "A. Median spending per resident", x = NULL, y = "Real expenditures per capita (2022 $)" ) + theme_c
  gb <- ggplot( pq, aes( year, chg, colour = gq, linetype = gq == "All metros" ) ) + geom_hline( yintercept = 0, colour = "grey60" ) +
    geom_line( linewidth = 0.8 ) + scale_linetype_manual( values = c( "FALSE" = 1, "TRUE" = 2 ), guide = "none" ) +
    ggrepel::geom_text_repel( data = pq[ year == max( year ) ], aes( label = gq ), direction = "y", hjust = 0, nudge_x = 0.6,
                              xlim = c( max( pq$year ) + 0.4, NA ), size = 2.5, segment.size = 0.2, segment.colour = "grey70",
                              min.segment.length = 0, show.legend = FALSE, seed = 1 ) +
    scale_x_continuous( expand = expansion( add = c( 0.5, 4 ) ) ) +
    scale_colour_manual( values = QC, guide = "none" ) +
    labs( title = "B. Change since 2000 (mean of metro log changes)", x = NULL, y = "% change in real spending per capita since 2000" ) + theme_c
  gp <- patchwork::wrap_plots( ga, gb, ncol = 2, guides = "collect" ) +
    patchwork::plot_annotation( title = "Nonprofit spending per resident by 2000s population growth quintile",
      subtitle = "501(c)(3) filer expenditures excl. hospitals and universities, 2022 dollars. Source changes in 2011 (CORE to SOI) and 2020 (SOI to e-file).",
      theme = theme( legend.position = "top" ) ) & theme( legend.position = "bottom" )
  ggsave( file.path( CU, "fig_c3_exp_per_capita.png" ), gp, width = 10, height = 4.6, dpi = 160, bg = "white" )
  ## fastest growers among metros over 250k in 2024
  fg <- ch[ pop1 >= 250e3 ][ order( -( gA + gB ) ) ][ 1:25, .( name, region, pop2000_k = round( pop0 / 1e3 ), pop2024_k = round( pop1 / 1e3 ),
    growth_00s = round( 100 * ( exp( gA ) - 1 ) ), growth_10s = round( 100 * ( exp( gB ) - 1 ) ),
    pct_dev_2000 = round( 100 * ( exp( s0_n_filer_c3 ) - 1 ) ), pct_dev_2011 = round( 100 * ( exp( sm_n_filer_c3 ) - 1 ) ),
    pct_dev_2023 = round( 100 * ( exp( s1_n_filer_c3 ) - 1 ) ),
    grass_dev_2000 = round( 100 * ( exp( s0_n_grass_c3pc ) - 1 ) ), grass_dev_2023 = round( 100 * ( exp( s1_n_grass_c3pc ) - 1 ) ) ) ]
  print( fg ); fwrite( fg, file.path( CU, "c3_fast_growers.csv" ) )
}

## ===========================================================================
## Part 4: what predicts catch-up
## ===========================================================================
if ( "4" %in% PARTS ) {
  log_msg( "Part 4: catch-up predictors" )
  zs <- function( x ) ( x - mean( x, na.rm = TRUE ) ) / sd( x, na.rm = TRUE )
  res <- list(); shap <- list(); cu <- list(); cors <- list()
  for ( v in names( OUTC ) ) {
    x <- copy( ch )
    x[ , `:=`( dsA = get( paste0( "sm_", v ) ) - get( paste0( "s0_", v ) ), dsB = get( paste0( "s1_", v ) ) - get( paste0( "sm_", v ) ),
               s0 = get( paste0( "s0_", v ) ), sm = get( paste0( "sm_", v ) ) ) ]
    x <- x[ is.finite( dsA ) & is.finite( dsB ) ]
    ## 4a: lag building in the 2000s and recovery in the 2010s (growth in log points; coefficients per 10% growth)
    x[ , `:=`( gA10 = gA / log( 1.1 ), gB10 = gB / log( 1.1 ) ) ]
    a1 <- feols( dsA ~ gA10 + s0, x, vcov = "hetero" )
    b1 <- feols( dsB ~ gA10 + gB10, x, vcov = "hetero" )
    b2 <- feols( dsB ~ gA10 + gB10 + sm, x, vcov = "hetero" )
    b3 <- feols( dsB ~ gA10 + gB10 + sm | region, x, vcov = "hetero" )
    cu[[ v ]] <- rbindlist( lapply( list( "2000s: change on 2000s growth (+ start SAMI)" = a1, "2010s: change on growth, both decades" = b1,
                                          "2010s: + 2011 SAMI" = b2, "2010s: + 2011 SAMI + region" = b3 ), function( r ) {
      ct <- coeftable( r ); data.table( term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ], r2 = r2( r, "r2" ), n = nobs( r ) ) } ), idcol = "model" )[ , outcome := v ]
    ## 4b: one moderator at a time: main effect and interaction with 2000s growth
    for ( k in names( LV ) ) for ( blk in c( "L0", "Lm", "dA", "dB" ) ) {
      col <- paste0( blk, "_", k ); if ( !col %in% names( x ) || all( !is.finite( x[[ col ]] ) ) ) next
      if ( blk %in% c( "Lm", "dA" ) && k == "hist" ) next
      y <- x[ is.finite( get( col ) ) ][ , z := zs( get( col ) ) ][ , zgA := zs( gA ) ]
      r <- feols( dsB ~ zgA + zs( gB ) + zs( sm ) + z, y, vcov = "hetero" )
      ri <- feols( dsB ~ zgA * z + zs( gB ) + zs( sm ), y, vcov = "hetero" )
      ct <- coeftable( r ); ci <- coeftable( ri )
      res[[ length( res ) + 1 ]] <- data.table( outcome = v, var = k, label = LV[[ k ]], block = blk, n = nobs( r ),
        est = ct[ "z", 1 ], se = ct[ "z", 2 ], p = ct[ "z", 4 ], partial_r = ct[ "z", 3 ] / sqrt( ct[ "z", 3 ]^2 + degrees_freedom( r, "resid" ) ),
        int_est = ci[ "zgA:z", 1 ], int_se = ci[ "zgA:z", 2 ], int_p = ci[ "zgA:z", 4 ],
        cor_gA = cor( y$gA, y[[ col ]] ), cor_gB = cor( y$gB, y[[ col ]] ) )
    }
    ## 4c: full model and Shapley R2 by block
    L0v <- paste0( "L0_", c( "pct_ba", "l_pcpi", "poverty_rate", "pct_65plus", "pct_nonwhite", "x_pf", "x_gov", "turnout", "pct_adherent", "hist" ) )
    dBv <- paste0( "dB_", c( "pct_ba", "l_pcpi", "poverty_rate", "pct_65plus", "pct_nonwhite", "x_pf", "x_gov", "x_wage" ) )
    BL <- list( growth = c( "gA", "gB" ), deficit = "sm", endowment_2000 = L0v, change_2010s = dBv, region = "region" )
    y <- x[ complete.cases( x[ , c( unlist( BL ), "dsB" ), with = FALSE ] ) ]
    for ( cc in setdiff( unlist( BL ), "region" ) ) set( y, j = cc, value = zs( y[[ cc ]] ) )
    r2b <- function( bs ) if ( !length( bs ) ) 0 else summary( lm( as.formula( paste( "dsB ~", paste( unlist( BL[ bs ] ), collapse = " + " ) ) ), y ) )$r.squared
    nb <- names( BL ); memo <- list()
    R2 <- function( bs ) { key <- paste0( "k:", paste( sort( bs ), collapse = "|" ) ); if ( is.null( memo[[ key ]] ) ) memo[[ key ]] <<- r2b( bs ); memo[[ key ]] }
    sh <- sapply( nb, function( b ) { o <- setdiff( nb, b ); tot <- 0
      for ( s in 0:length( o ) ) { cmb <- if ( s == 0 ) list( character( 0 ) ) else combn( o, s, simplify = FALSE )
        w <- factorial( s ) * factorial( length( nb ) - s - 1 ) / factorial( length( nb ) )
        for ( S in cmb ) tot <- tot + w * ( R2( c( S, b ) ) - R2( S ) ) }
      tot } )
    shap[[ v ]] <- data.table( outcome = v, block = nb, shapley_r2 = sh, total_r2 = R2( nb ), n = nrow( y ) )
    full <- feols( as.formula( paste( "dsB ~", paste( setdiff( unlist( BL ), "region" ), collapse = " + " ), "| region" ) ), y, vcov = "hetero" )
    ct <- coeftable( full )
    res[[ length( res ) + 1 ]] <- data.table( outcome = v, var = rownames( ct ), label = "full model", block = "full", n = nobs( full ),
      est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ], partial_r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + degrees_freedom( full, "resid" ) ),
      int_est = NA_real_, int_se = NA_real_, int_p = NA_real_, cor_gA = NA_real_, cor_gB = NA_real_ )
  }
  cu <- rbindlist( cu ); res <- rbindlist( res ); shap <- rbindlist( shap )
  ## recovery share per outcome: rebound coefficient (2010s, model b1/b2) relative to the 2000s shortfall
  rec <- cu[ term == "gA10", .( est = est[ grepl( "^2000s", model ) ], rebound = est[ model == "2010s: change on growth, both decades" ],
                                rebound_cond = est[ model == "2010s: + 2011 SAMI" ], rebound_region = est[ model == "2010s: + 2011 SAMI + region" ] ), by = outcome ]
  rec[ , `:=`( recovered = -rebound / est, recovered_cond = -rebound_cond / est, recovered_region = -rebound_region / est ) ]
  print( r3( cu[ term != "(Intercept)" ] ), nrows = 100 ); print( r3( rec ) )
  print( r3( res[ block != "full" & outcome == "n_filer_c3" ][ order( block, p ) ] ), nrows = 200 )
  print( r3( res[ block == "full" ] ), nrows = 100 ); print( r3( shap ) )
  fwrite( cu, file.path( CU, "c4_catchup_growth.csv" ) ); fwrite( rec, file.path( CU, "c4_recovery_share.csv" ) )
  fwrite( res, file.path( CU, "c4_predictors.csv" ) ); fwrite( shap, file.path( CU, "c4_shapley.csv" ) )
}

## local-projection sample for outcome sv at horizon h (shared by parts 5)
lp_data <- function( sv, h ) {
  x <- m[ year >= 2006 & year + h <= YMAX, .( cbsa, year, region, logP, inc3r ) ]
  x[ , `:=`( s_m3 = at( sv, cbsa, year - 3 ), s_h = at( sv, cbsa, year + h ),
             g_past = logP - at( "logP", cbsa, year - 3 ), g_pre = at( "logP", cbsa, year - 3 ) - at( "logP", cbsa, year - 6 ),
             g_fut = at( "logP", cbsa, year + h ) - logP, inc_pre = at( "inc3r", cbsa, year - 3 ) ) ]
  x <- x[ is.finite( s_m3 ) & is.finite( s_h ) & is.finite( inc3r ) & is.finite( inc_pre ) ][ , dep := s_h - s_m3 ]
  x[ , `:=`( gp_pos = pmax( g_past, 0 ), gp_neg = pmin( g_past, 0 ), inc_pos = pmax( inc3r, 0 ), inc_neg = pmin( inc3r, 0 ) ) ]
  x }
FE2 <- c( year = "| year", metro_year = "| cbsa + year" )
half_of <- function( h, rem ) { i <- which( rem <= 0.5 ); if ( length( i ) ) as.numeric( h[ min( i ) ] ) else NA_real_ }

## ===========================================================================
## Part 5: does the sector shrink with a lag when population or income falls?
## ===========================================================================
if ( "5" %in% PARTS ) {
  log_msg( "Part 5: declines" )
  ## 5a horizon elasticities with separate slopes for growth and decline
  p5 <- rbindlist( lapply( names( OUTC ), function( v ) rbindlist( lapply( c( 1, 3, 5, 10, 15 ), function( h ) {
    x <- m[ year + h <= YMAX & get( v ) > 0, .( cbsa, year, lN0 = log( get( v ) ), lP0 = logP, s0 = get( paste0( "s_", v ) ) ) ]
    x[ , `:=`( lN1 = log( at( v, cbsa, year + h ) ), lP1 = at( "logP", cbsa, year + h ) ) ]
    x <- x[ is.finite( lN1 ) ][ , `:=`( dN = lN1 - lN0, dP = lP1 - lP0 ) ][ , `:=`( dPp = pmax( dP, 0 ), dPn = pmin( dP, 0 ) ) ]
    r <- feols( dN ~ dPp + dPn + s0 | year, x, cluster = ~cbsa ); ct <- coeftable( r )
    rd <- feols( dN ~ dP + dPn + s0 | year, x, cluster = ~cbsa )             # dPn = decline slope - growth slope
    data.table( outcome = v, h = h, n = nobs( r ), n_decline = sum( x$dP < 0 ), metros_decline = uniqueN( x[ dP < 0, cbsa ] ),
                elast_grow = ct[ "dPp", 1 ], se_grow = ct[ "dPp", 2 ], elast_decline = ct[ "dPn", 1 ], se_decline = ct[ "dPn", 2 ],
                p_diff = coeftable( rd )[ "dPn", 4 ] )
  } ) ) ) )
  p5 <- merge( p5, bsum[ , .( outcome, b ) ], by = "outcome" )
  p5[ , `:=`( realized_grow = elast_grow / b, realized_decline = elast_decline / b ) ]
  setorder( p5, outcome, h ); print( r3( p5 ) ); fwrite( p5, file.path( CU, "c5_horizon_by_sign.csv" ) )

  ## 5b local projections: growth vs absolute decline, and relative income gains vs losses
  lp5 <- rbindlist( lapply( names( OUTC ), function( v ) rbindlist( lapply( 0:12, function( h ) {
    x <- lp_data( paste0( "s_", v ), h )
    fut <- if ( h == 0 ) "" else "+ g_fut"
    rbindlist( lapply( names( FE2 ), function( k ) {
      r <- feols( as.formula( paste( "dep ~ gp_pos + gp_neg + g_pre", fut, "+ s_m3 + inc_pos + inc_neg + inc_pre", FE2[[ k ]] ) ), x, cluster = ~cbsa )
      ct <- coeftable( r )
      data.table( outcome = v, h = h, fe = k, n = nobs( r ), n_decline = sum( x$g_past < 0 ), n_inc_loss = sum( x$inc3r < 0 ),
                  term = c( "pop_growth", "pop_decline", "income_gain", "income_loss" ),
                  est = ct[ c( "gp_pos", "gp_neg", "inc_pos", "inc_neg" ), 1 ], se = ct[ c( "gp_pos", "gp_neg", "inc_pos", "inc_neg" ), 2 ] ) } ) )
  } ) ) ) )
  lp5[ , est0 := est[ h == 0 ], by = .( outcome, fe, term ) ][ , remaining := est / est0 ]
  hl5 <- lp5[ , .( est0 = est0[ 1 ], se0 = se[ h == 0 ], est6 = est[ h == 6 ], est10 = est[ h == 10 ], est12 = est[ h == 12 ],
                   remaining10 = remaining[ h == 10 ], half_life = half_of( h, remaining ) ), by = .( outcome, fe, term ) ]
  print( r3( hl5 ), nrows = 100 )
  fwrite( lp5, file.path( CU, "c5_local_projection_sign.csv" ) ); fwrite( hl5, file.path( CU, "c5_half_life_sign.csv" ) )
  ## plotted in % terms for a 10% change in each direction (gains: log 1.1; losses: log 0.9)
  TL <- c( pop_growth = "After 10% population growth", pop_decline = "After 10% population decline",
           income_gain = "After a 10% relative income gain", income_loss = "After a 10% relative income loss" )
  SH <- c( pop_growth = log( 1.1 ), pop_decline = log( 0.9 ), income_gain = log( 1.1 ), income_loss = log( 0.9 ) )
  pd <- lp5[ fe == "metro_year" ][ , `:=`( eff = pct10( est, SH[ term ] ), a = pct10( est - 1.96 * se, SH[ term ] ), b = pct10( est + 1.96 * se, SH[ term ] ) ) ]
  pd[ , `:=`( lo = pmin( a, b ), hi = pmax( a, b ) ) ]
  pl <- function( terms, ttl, sub ) ggplot( pd[ term %in% terms ], aes( h, eff, colour = term, fill = term ) ) +
    geom_hline( yintercept = 0, colour = "grey50" ) +
    geom_ribbon( aes( ymin = lo, ymax = hi ), alpha = 0.12, colour = NA ) +
    geom_line( linewidth = 0.8 ) + geom_point( size = 1.3 ) + facet_wrap( ~ OUTC[ outcome ], scales = "free_y" ) +
    scale_colour_manual( values = setNames( c( C1, C2 ), terms ), labels = TL[ terms ], name = NULL ) +
    scale_fill_manual( values = setNames( c( C1, C2 ), terms ), guide = "none" ) +
    labs( title = ttl, subtitle = sub, x = "Years after the 3-year episode ends", y = "Effect on density (%)" ) + theme_c
  ggsave( file.path( CU, "fig_c5_decline.png" ), pl( c( "pop_growth", "pop_decline" ), "Nonprofit density after a metro grows or shrinks 10% in three years",
          paste( "Within metro (metro + year FE). Below 0: fewer organizations than the new population implies (growth outpaced the sector).",
                 "Above 0: more than it implies (organizations stayed while people left). 0 = fully adjusted.", sep = "\n" ) ),
          width = 10, height = 4.4, dpi = 160, bg = "white" )
  ggsave( file.path( CU, "fig_c5_income.png" ), pl( c( "income_gain", "income_loss" ), "Nonprofit density after a 10% relative income gain or loss",
          paste( "Within metro (metro + year FE); income change over three years relative to the year's average across metros.",
                 "0 = no lasting effect.", sep = "\n" ) ),
          width = 10, height = 4.4, dpi = 160, bg = "white" )
  ## alternative layout: one panel per direction, outcomes as lines on a shared y-axis
  ## (expenditures dashed without a band: its interval is several times wider)
  OCOL <- setNames( c( C1, C3, "grey45" ), names( OUTC ) )
  ## 5c stable-population baseline: the same within-metro projection plus a dummy for metro-years
  ## whose 3-year population change is within +/-1% (median about 0). The linear terms put a
  ## no-change metro at 0 by construction; the dummy lets stable metro-years depart from that, so
  ## a lag (or excess) in metros whose population did not move would show up as a non-zero path.
  ## Growth and decline panels keep the original model (adding the dummy moves the count paths by
  ## at most 0.6 points and expenditures by at most 1.4).
  ## The same is done for income: metro-years whose relative 3-year income change (inc3r) is within +/-1%.
  STB <- list( pop = list( term = "pop_stable", var = "g_past", up = "gp_pos", down = "gp_neg", ref = c( "pop_growth", "pop_decline" ) ),
               income = list( term = "income_stable", var = "inc3r", up = "inc_pos", down = "inc_neg", ref = c( "income_gain", "income_loss" ) ) )
  st5 <- rbindlist( lapply( STB, function( k ) rbindlist( lapply( names( OUTC ), function( v ) rbindlist( lapply( 0:12, function( h ) {
    x <- lp_data( paste0( "s_", v ), h )
    x[ , stable := as.numeric( abs( get( k$var ) ) <= log( 1.01 ) ) ]
    fut <- if ( h == 0 ) "" else "+ g_fut"
    r <- feols( as.formula( paste( "dep ~ stable + gp_pos + gp_neg + g_pre", fut, "+ s_m3 + inc_pos + inc_neg + inc_pre | cbsa + year" ) ), x, cluster = ~cbsa )
    ct <- coeftable( r )
    data.table( outcome = v, h = h, fe = "metro_year", term = k$term, est = ct[ "stable", 1 ], se = ct[ "stable", 2 ],
                g_est = ct[ k$up, 1 ], d_est = ct[ k$down, 1 ],
                n_stable = sum( x$stable ), metros_stable = uniqueN( x[ stable == 1, cbsa ] ), median_g = median( x[ stable == 1, get( k$var ) ] ) ) } ) ) ) ) ) )
  st5[ , `:=`( eff = 100 * ( exp( est ) - 1 ), lo = 100 * ( exp( est - 1.96 * se ) - 1 ), hi = 100 * ( exp( est + 1.96 * se ) - 1 ) ) ]
  print( r3( st5[ h %in% c( 0, 2, 4, 6, 8, 10, 12 ), .( term, outcome, h, eff, lo, hi, n_stable, metros_stable, median_g_pct = 100 * median_g ) ] ), nrows = 100 )
  ## how much adding the dummy moves the up / down paths of the original model (percentage points)
  for ( k in STB ) print( r3( merge( st5[ term == k$term, .( outcome, h, g_est, d_est ) ],
      lp5[ fe == "metro_year" & term %in% k$ref, .( g0 = est[ term == k$ref[ 1 ] ], d0 = est[ term == k$ref[ 2 ] ] ), by = .( outcome, h ) ], by = c( "outcome", "h" ) )
    [ , .( stable = k$term, max_shift_up_pt = max( abs( pct10( g_est, log( 1.1 ) ) - pct10( g0, log( 1.1 ) ) ) ),
           max_shift_down_pt = max( abs( pct10( d_est, log( 0.9 ) ) - pct10( d0, log( 0.9 ) ) ) ) ), by = outcome ] ) )
  fwrite( st5, file.path( CU, "c5_stable_baseline.csv" ) )
  pd <- rbind( pd, st5[ , .( outcome, h, fe, term, est, se, eff, lo, hi ) ], fill = TRUE )
  by_direction <- function( terms, panels, ttl, sub, file, width = 10 ) {
    dd <- pd[ term %in% terms ]
    dd[ , panel := factor( panels[ term ], panels ) ][ , olab := factor( OUTC[ outcome ], OUTC ) ]
    g <- ggplot( dd, aes( h, eff, colour = olab ) ) +
      geom_hline( yintercept = 0, colour = "grey40" ) +
      geom_ribbon( data = dd[ outcome != "exp_c3_nohu" ], aes( ymin = lo, ymax = hi, fill = olab ), alpha = 0.12, colour = NA ) +
      geom_line( aes( linetype = outcome == "exp_c3_nohu" ), linewidth = 0.85 ) + geom_point( size = 1.3 ) +
      facet_wrap( ~ panel ) + scale_x_continuous( breaks = seq( 0, 12, 2 ) ) +
      scale_colour_manual( values = setNames( OCOL, OUTC ), name = NULL ) + scale_fill_manual( values = setNames( OCOL, OUTC ), guide = "none" ) +
      scale_linetype_manual( values = c( "FALSE" = 1, "TRUE" = 2 ), guide = "none" ) +
      labs( title = ttl, subtitle = sub, x = "Years after the 3-year episode ends", y = "Effect on density (%)" ) + theme_c
    ggsave( file.path( CU, file ), g, width = width, height = 4.6, dpi = 160, bg = "white" ) }
  BANDS <- "Bands: 95% CI (expenditures, dashed, omitted: interval is several times wider)."
  by_direction( c( "pop_growth", "pop_stable", "pop_decline" ),
    c( pop_growth = "A. After 10% population growth (3 years)", pop_stable = "B. Stable population (within ±1% in 3 years)",
       pop_decline = "C. After 10% population decline (3 years)" ),
    "Nonprofit density after a metro grows, holds steady, or shrinks over three years",
    paste( "Within metro (metro + year FE). Below 0: fewer organizations than the new population implies; above 0: more than it implies.",
           "Panel B: metro-years whose population barely moved (median change +0.2%), against the same model's no-change baseline.",
           paste( "0 = fully adjusted.", BANDS ), sep = "\n" ), "fig_c5_decline_by_direction.png", width = 12 )
  by_direction( c( "income_gain", "income_stable", "income_loss" ),
    c( income_gain = "A. After a 10% relative income gain (3 years)", income_stable = "B. Stable relative income (within ±1% in 3 years)",
       income_loss = "C. After a 10% relative income loss (3 years)" ),
    "Nonprofit density after a metro's income rises, holds steady, or falls relative to other metros",
    paste( "Within metro (metro + year FE); income change over three years relative to the year's average across metros.",
           "Panel B: metro-years whose relative income barely moved, against the same model's no-change baseline.",
           paste( "0 = no lasting effect.", BANDS ), sep = "\n" ), "fig_c5_income_by_direction.png", width = 12 )
}

## ===========================================================================
## Part 6: mechanism: entries, exits and filing share on lagged growth
## ===========================================================================
if ( "6" %in% PARTS ) {
  log_msg( "Part 6: entries, exits, filing share" )
  e <- m[ year >= 2011 & year <= 2021, .( cbsa, year, entry_rate, exit_rate, file_share ) ]
  for ( k in 0:4 ) e[ , ( paste0( "g", k ) ) := at( "logP", cbsa, year - 2 * k ) - at( "logP", cbsa, year - 2 * k - 2 ) ]
  e[ , s_bmf_m1 := at( "s_n_bmf_c3pc", cbsa, year - 1 ) ]
  gx <- paste0( "g", 0:4 )
  mech <- rbindlist( lapply( c( "entry_rate", "exit_rate", "file_share" ), function( y ) rbindlist( lapply( c( "year", "metro+year" ), function( k ) {
    f <- if ( k == "year" ) paste( y, "~", paste( gx, collapse = " + " ), "+ s_bmf_m1 | year" ) else paste( y, "~", paste( gx, collapse = " + " ), "| cbsa + year" )
    r <- feols( as.formula( f ), e, cluster = ~cbsa ); ct <- coeftable( r )
    data.table( dv = y, fe = k, n = nobs( r ), mean_dv = mean( e[[ y ]], na.rm = TRUE ), term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ] ) } ) ) ) )
  LAGL <- c( g0 = "t-2 to t", g1 = "t-4 to t-2", g2 = "t-6 to t-4", g3 = "t-8 to t-6", g4 = "t-10 to t-8" )
  mech[ , lag := LAGL[ term ] ]
  print( r3( mech ), nrows = 100 ); fwrite( mech, file.path( CU, "c6_mechanism.csv" ) )
  DVL <- c( entry_rate = "Entries per 100 registered orgs", exit_rate = "Exits per 100 registered orgs", file_share = "log(filers / registered)" )
  g <- ggplot( mech[ term %in% gx ], aes( factor( lag, LAGL ), est, colour = fe ) ) +
    geom_hline( yintercept = 0, colour = "grey70" ) +
    geom_pointrange( aes( ymin = est - 1.96 * se, ymax = est + 1.96 * se ), position = position_dodge( width = 0.45 ), size = 0.3 ) +
    facet_wrap( ~ DVL[ dv ], scales = "free_y" ) +
    scale_colour_manual( values = c( year = C2, "metro+year" = C1 ), labels = c( year = "Across metros (year FE)", "metro+year" = "Within metro (metro + year FE)" ), name = NULL ) +
    labs( title = "Entries, exits and filing share on lagged population growth, 2011-2021",
          subtitle = "Distributed lag in 2-year growth bins (log points); metro-clustered 95% CIs", x = "Population growth window", y = "Coefficient per log point of growth" ) +
    theme_c + theme( axis.text.x = element_text( angle = 30, hjust = 1 ) )
  ggsave( file.path( CU, "fig_c6_mechanism.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )
}

## metro-level change data for one outcome (parts 7-8)
period_data <- function( v ) {
  x <- copy( ch )
  x[ , `:=`( dsA = get( paste0( "sm_", v ) ) - get( paste0( "s0_", v ) ), dsB = get( paste0( "s1_", v ) ) - get( paste0( "sm_", v ) ),
             s0 = get( paste0( "s0_", v ) ), sm = get( paste0( "sm_", v ) ), gA10 = gA / log( 1.1 ), gB10 = gB / log( 1.1 ) ) ]
  x[ is.finite( dsA ) & is.finite( dsB ) ] }
avp <- function( x, y, xv, ctrl ) {            # added-variable (FWL) residuals of y and xv on ctrl
  ok <- complete.cases( x[ , c( y, xv, ctrl ), with = FALSE ] ); z <- x[ ok ]
  f <- function( dv ) resid( lm( as.formula( paste( dv, "~", paste( ctrl, collapse = " + " ) ) ), z ) )
  data.table( z[ , .( cbsa, name, region ) ], ry = f( y ), rx = f( xv ) ) }

## ===========================================================================
## Part 7: evidence figures for the catch-up predictors (uses c4_* from part 4)
## ===========================================================================
if ( "7" %in% PARTS ) {
  log_msg( "Part 7: predictor figures" )
  res <- fread( file.path( CU, "c4_predictors.csv" ) ); shap <- fread( file.path( CU, "c4_shapley.csv" ) )
  ## 7a moderators: partial r, 2000 levels and 2010s changes
  mo <- res[ block %in% c( "L0", "dB" ) ][ , blk := fifelse( block == "L0", "Level in 2000 (endowment)", "Change over 2010-12 to 2022-24" ) ]
  ord <- mo[ outcome == "n_filer_c3" ][ order( partial_r ), unique( label ) ]
  mo[ , label := factor( label, ord ) ][ , sig := p < 0.05 ]
  g <- ggplot( mo, aes( partial_r, label, colour = OUTC[ outcome ] ) ) + geom_vline( xintercept = 0, colour = "grey70" ) +
    geom_point( aes( shape = sig ), size = 2.2, position = position_dodge( width = 0.6 ) ) +
    scale_shape_manual( values = c( "TRUE" = 16, "FALSE" = 1 ), labels = c( "TRUE" = "p < .05", "FALSE" = "n.s." ), name = NULL ) +
    facet_wrap( ~ blk ) + scale_colour_manual( values = c( C1, C2, C3 ), name = NULL ) +
    labs( title = "What predicts the change in SAMI, 2010-12 to 2022-24",
          subtitle = "Partial r of each factor entered alone, controlling for population growth in both decades and the 2011 SAMI", x = "Partial correlation", y = NULL ) + theme_c
  ggsave( file.path( CU, "fig_c7_moderators.png" ), g, width = 10, height = 5.4, dpi = 160, bg = "white" )

  ## 7b covariation of the 2010s changes with population growth
  cv <- res[ block == "dB" & outcome == "n_filer_c3", .( label, cor_gA, cor_gB, partial_r_filers = partial_r ) ][ order( -cor_gB ) ]
  print( r3( cv ) ); fwrite( cv, file.path( CU, "c7_change_covariation.csv" ) )

  ## 7c added-variable plots: income growth (changes) and poverty (level), filers
  x <- period_data( "n_filer_c3" ); ctl <- c( "gA10", "gB10", "sm" )
  av <- rbind( avp( x, "dsB", "dB_l_pcpi", ctl )[ , panel := "A. Income growth, 2010s (d log per capita income)" ],
               avp( x, "dsB", "L0_poverty_rate", ctl )[ , panel := "B. Poverty rate, 2000" ],
               avp( x, "dsB", "L0_hist", ctl )[ , panel := "C. Pre-1970 endowment (SAMI)" ],
               avp( x, "dsB", "sm", c( "gA10", "gB10" ) )[ , panel := "D. Deficit: SAMI in 2010-12" ] )
  lab <- c( "Charleston, SC", "Las Vegas, NV", "Myrtle Beach, SC", "Cape Coral, FL", "Raleigh, NC", "Austin, TX", "Lakeland, FL", "McAllen, TX" )
  g <- ggplot( av, aes( rx, ry ) ) + geom_hline( yintercept = 0, colour = "grey80" ) +
    geom_point( aes( colour = region ), size = 1.2, alpha = 0.65 ) + geom_smooth( method = "lm", formula = y ~ x, se = TRUE, colour = "grey20", linewidth = 0.6 ) +
    ggrepel::geom_text_repel( data = av[ name %in% lab ], aes( label = name ), size = 2.3, colour = "grey25", seed = 1, max.overlaps = 20 ) +
    facet_wrap( ~ panel, scales = "free_x", ncol = 2 ) +
    scale_colour_manual( values = c( Northeast = C1, Midwest = C3, South = C2, West = C4 ), name = NULL ) +
    labs( title = "Filer-count SAMI change, 2010-12 to 2022-24: added-variable plots",
          subtitle = "Both axes residualized on population growth in the 2000s and 2010s and the 2011 SAMI (panel D: growth only)",
          x = "Factor (residualized)", y = "Change in SAMI (residualized)" ) + theme_c
  ggsave( file.path( CU, "fig_c7_added_variable.png" ), g, width = 10, height = 7.2, dpi = 160, bg = "white" )

  ## 7d does a factor speed recovery of 2000s fast growers? slope of 2010s change on 2000s growth by tercile
  MODS <- c( pct_ba = "% BA", poverty_rate = "Poverty rate", turnout = "Turnout", pct_nonwhite = "% nonwhite",
             gini = "Gini", unemp_rate = "Unemployment", x_pf = "Foundation assets (SAMI)", l_pcpi = "Per capita income (log)" )
  it <- rbindlist( lapply( names( OUTC )[ 1:2 ], function( v ) { x <- period_data( v )
    rbindlist( lapply( names( MODS ), function( k ) { col <- paste0( "L0_", k )
      x[ , terc := cut( get( col ), quantile( get( col ), 0:3 / 3, na.rm = TRUE ), include.lowest = TRUE, labels = c( "Low", "Middle", "High" ) ) ]
      rbindlist( lapply( c( "Low", "Middle", "High" ), function( tq ) {
        r <- feols( dsB ~ gA10 + gB10 + sm, x[ terc == tq ], vcov = "hetero" ); ct <- coeftable( r )
        data.table( outcome = v, moderator = MODS[[ k ]], tercile = tq, n = nobs( r ), est = ct[ "gA10", 1 ], se = ct[ "gA10", 2 ] ) } ) ) } ) ) } ) )
  it[ , tercile := factor( tercile, c( "Low", "Middle", "High" ) ) ]
  fwrite( it, file.path( CU, "c7_recovery_by_tercile.csv" ) )
  g <- ggplot( it, aes( est, factor( moderator, rev( MODS ) ), colour = tercile ) ) + geom_vline( xintercept = 0, colour = "grey70" ) +
    geom_pointrange( aes( xmin = est - 1.96 * se, xmax = est + 1.96 * se ), position = position_dodge( width = 0.6 ), size = 0.25 ) +
    facet_wrap( ~ OUTC[ outcome ], scales = "free_x" ) + scale_colour_manual( values = c( Low = "#9db7dc", Middle = C1, High = "#123f7a" ), name = "Tercile of the factor in 2000" ) +
    labs( title = "Recovery of the 2000s growth deficit, by starting conditions",
          subtitle = "Slope of the 2010s SAMI change on 2000s population growth (per 10%), holding 2010s growth and the 2011 SAMI fixed; > 0 = fast growers recover",
          x = "Recovery slope (change in SAMI per 10% growth in the 2000s)", y = NULL ) + theme_c
  ggsave( file.path( CU, "fig_c7_recovery_terciles.png" ), g, width = 10, height = 5, dpi = 160, bg = "white" )

  ## 7e Shapley R2 by block
  BLL <- c( growth = "Population growth", deficit = "2011 deficit", endowment_2000 = "Endowment levels, 2000", change_2010s = "Change in the 2010s", region = "Region" )
  g <- ggplot( shap, aes( shapley_r2, OUTC[ outcome ], fill = factor( block, rev( names( BLL ) ) ) ) ) + geom_col( width = 0.6 ) +
    scale_fill_manual( values = setNames( c( C1, C2, C3, C4, C5 ), names( BLL ) ), labels = BLL, name = NULL, breaks = names( BLL ) ) +
    labs( title = "Share of the 2010s SAMI change explained, by block (Shapley R2)", x = "R2", y = NULL ) + theme_c
  ggsave( file.path( CU, "fig_c7_shapley.png" ), g, width = 8, height = 3.2, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part 8: nonprofit market share and wage premium (BLS QCEW nonprofit data, 2013-2022)
## ===========================================================================
if ( "8" %in% PARTS ) {
  log_msg( "Part 8: BLS market share and wage premium" )
  b <- m[ year %in% 2013:2022 & !( bls_delin_flag %in% TRUE ) & is.finite( bls_pct_npo ) & bls_pct_npo > 0 ]
  b[ , `:=`( l_share = log( bls_pct_npo ), wr = fifelse( bls_wage_ratio_cov >= 0.5, bls_wage_ratio_adj, NA_real_ ) ) ]
  log_msg( "BLS metro-years: ", nrow( b ), "; metros: ", uniqueN( b$cbsa ), "; with adjusted wage ratio: ", b[ is.finite( wr ), uniqueN( cbsa ) ] )
  ## 8a levels: how much of the SAMI (and its percentile) does market share account for?
  lv <- rbindlist( lapply( names( OUTC ), function( v ) { sv <- paste0( "s_", v )
    y <- b[ is.finite( get( sv ) ) ]; y[ , pr := 100 * frank( get( sv ) ) / .N, by = year ][ , pr_share := 100 * frank( l_share ) / .N, by = year ]
    fits <- list( "share" = feols( as.formula( paste( sv, "~ l_share | year" ) ), y, cluster = ~cbsa ),
                  "wage ratio" = feols( as.formula( paste( sv, "~ wr | year" ) ), y, cluster = ~cbsa ),
                  "share + wage ratio" = feols( as.formula( paste( sv, "~ l_share + wr | year" ) ), y, cluster = ~cbsa ),
                  "share + wage ratio + region" = feols( as.formula( paste( sv, "~ l_share + wr | year^region" ) ), y, cluster = ~cbsa ),
                  "percentile on share percentile" = feols( pr ~ pr_share | year, y, cluster = ~cbsa ),
                  "health-care nonprofit share" = feols( as.formula( paste( sv, "~ bls_pct_npo_62 | year" ) ), y, cluster = ~cbsa ),
                  "share + health-care nonprofit share" = feols( as.formula( paste( sv, "~ l_share + bls_pct_npo_62 | year" ) ), y, cluster = ~cbsa ) )
    rbindlist( lapply( names( fits ), function( k ) { r <- fits[[ k ]]; ct <- coeftable( r )
      data.table( outcome = v, model = k, term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ],
                  within_r2 = r2( r, "wr2" ), n = nobs( r ) ) } ) ) } ) )
  print( r3( lv ), nrows = 100 ); fwrite( lv, file.path( CU, "c8_levels.csv" ) )
  ## 8b catch-up: change 2013-15 -> 2022-24 on market share and wage ratio at the start
  mb <- b[ year %in% 2013:2015, .( share0 = mean( l_share ), wr0 = mean( wr, na.rm = TRUE ), pct_npo0 = mean( bls_pct_npo ),
                                     h62_0 = mean( bls_pct_npo_62, na.rm = TRUE ) ), by = cbsa ]
  mb[ !is.finite( h62_0 ), h62_0 := NA_real_ ]
  mb <- merge( mb, b[ year %in% 2020:2022, .( share1 = mean( l_share ) ), by = cbsa ], by = "cbsa" )[ , d_share := share1 - share0 ]
  mb[ !is.finite( wr0 ), wr0 := NA_real_ ]
  YC <- 2013:2015
  cu8 <- list(); ceil <- list(); bins <- list()
  for ( v in names( OUTC ) ) {
    sv <- paste0( "s_", v ); x <- copy( ch )
    x[ , `:=`( sC = avg( sv, YC ), s1 = get( paste0( "s1_", v ) ), gC = avg( "logP", Y1 ) - avg( "logP", YC ), gP = avg( "logP", YC ) - avg( "logP", 2003:2005 ) ) ]
    x <- merge( x[ , .( cbsa, name, region, sC, s1, gC, gP, dsC = s1 - sC ) ], mb, by = "cbsa" )[ is.finite( dsC ) ]
    x[ , `:=`( z_share = ( share0 - mean( share0 ) ) / sd( share0 ), z_wr = ( wr0 - mean( wr0, na.rm = TRUE ) ) / sd( wr0, na.rm = TRUE ),
               gC10 = gC / log( 1.1 ), gP10 = gP / log( 1.1 ), z_h62 = ( h62_0 - mean( h62_0, na.rm = TRUE ) ) / sd( h62_0, na.rm = TRUE ) ) ]
    x[ , share_t := cut( share0, quantile( share0, 0:3 / 3 ), include.lowest = TRUE, labels = c( "Low share", "Middle", "High share" ) ) ]
    fits <- list( "base" = feols( dsC ~ sC + gC10 + gP10, x, vcov = "hetero" ),
                  "+ share" = feols( dsC ~ sC + gC10 + gP10 + z_share, x, vcov = "hetero" ),
                  "+ wage ratio" = feols( dsC ~ sC + gC10 + gP10 + z_wr, x, vcov = "hetero" ),
                  "+ share + wage ratio + region" = feols( dsC ~ sC + gC10 + gP10 + z_share + z_wr | region, x, vcov = "hetero" ),
                  "+ share x deficit" = feols( dsC ~ sC * z_share + gC10 + gP10, x, vcov = "hetero" ),
                  "+ share x prior growth" = feols( dsC ~ gP10 * z_share + sC + gC10, x, vcov = "hetero" ),
                  "+ change in share" = feols( dsC ~ sC + gC10 + gP10 + z_share + d_share, x, vcov = "hetero" ),
                  "+ health-care nonprofit share" = feols( dsC ~ sC + gC10 + gP10 + z_h62, x, vcov = "hetero" ),
                  "share x deficit, |change| < 0.5" = feols( dsC ~ sC * z_share + gC10 + gP10, x[ abs( dsC ) < 0.5 ], vcov = "hetero" ) )
    cu8[[ v ]] <- rbindlist( lapply( names( fits ), function( k ) { r <- fits[[ k ]]; ct <- coeftable( r )
      data.table( outcome = v, model = k, term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ], r2 = r2( r, "r2" ), n = nobs( r ) ) } ) )
    ## ceiling: convergence speed (slope on start SAMI) and mean change of below-expected metros, by share tercile
    ceil[[ v ]] <- rbindlist( lapply( levels( x$share_t ), function( tq ) { y <- x[ share_t == tq ]
      r <- feols( dsC ~ sC + gC10 + gP10, y, vcov = "hetero" ); rt <- feols( dsC ~ sC + gC10 + gP10, y[ abs( dsC ) < 0.5 ], vcov = "hetero" )
      data.table( outcome = v, share_tercile = tq, n = nrow( y ), median_share = median( y$pct_npo0 ), median_sami_start = median( y$sC ),
                  convergence = coef( r )[[ "sC" ]], conv_se = se( r )[[ "sC" ]], conv_trim = coef( rt )[[ "sC" ]], conv_trim_se = se( rt )[[ "sC" ]],
                  mean_change_below = mean( y[ sC < 0, dsC ] ), n_below = sum( y$sC < 0 ),
                  mean_change_above = mean( y[ sC >= 0, dsC ] ) ) } ) )
    bins[[ v ]] <- x[ , .( cbsa, name, region, outcome = v, sC, dsC, share0, pct_npo0, share_t, wr0 ) ]
  }
  cu8 <- rbindlist( cu8 ); ceil <- rbindlist( ceil ); bins <- rbindlist( bins )
  print( r3( cu8[ term != "(Intercept)" ] ), nrows = 200 ); print( r3( ceil ) )
  fwrite( cu8, file.path( CU, "c8_catchup.csv" ) ); fwrite( ceil, file.path( CU, "c8_ceiling.csv" ) ); fwrite( bins, file.path( CU, "c8_metro.csv" ) )
  ## figures: level relationship (2019 cross-section) and convergence by share tercile
  y19 <- merge( m[ year == 2019, c( "cbsa", "region", paste0( "s_", names( OUTC ) ) ), with = FALSE ],
                b[ year == 2019, .( cbsa, bls_pct_npo ) ], by = "cbsa" )
  y19 <- melt( y19, id.vars = c( "cbsa", "region", "bls_pct_npo" ), variable.name = "outcome", value.name = "s" )[ , outcome := sub( "^s_", "", outcome ) ]
  y19[ , pr := 100 * frank( s ) / .N, by = outcome ]
  g <- ggplot( y19, aes( 100 * bls_pct_npo, pr ) ) + geom_point( aes( colour = region ), size = 1.2, alpha = 0.65 ) +
    geom_smooth( method = "loess", formula = y ~ x, se = FALSE, colour = "grey20", linewidth = 0.6 ) +
    scale_x_log10( breaks = c( 2, 5, 10, 20, 40 ) ) + facet_wrap( ~ OUTC[ outcome ] ) +
    scale_colour_manual( values = c( Northeast = C1, Midwest = C3, South = C2, West = C4 ), name = NULL ) +
    labs( title = "Nonprofit share of private employment vs density percentile, 2019",
          x = "Nonprofit share of private employment (%, log scale; BLS)", y = "SAMI percentile" ) + theme_c
  ggsave( file.path( CU, "fig_c8_share_level.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )
  g <- ggplot( bins[ abs( dsC ) < 0.5 ], aes( sC, dsC, colour = share_t ) ) + geom_hline( yintercept = 0, colour = "grey80" ) +
    geom_point( size = 1, alpha = 0.45 ) + geom_smooth( method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.9 ) +
    facet_wrap( ~ OUTC[ outcome ], scales = "free" ) +
    scale_colour_manual( values = c( "Low share" = C2, Middle = "grey55", "High share" = C1 ), name = "Nonprofit employment share, 2013-15" ) +
    labs( title = "Convergence by nonprofit market share: SAMI change 2013-15 to 2022-24 vs starting SAMI",
          subtitle = "A flatter line for low-share metros means deficits there close more slowly (a ceiling); metros with |change| >= 0.5 omitted (1 BMF, 36 expenditure)",
          x = "SAMI, 2013-15", y = "Change in SAMI" ) + theme_c
  ggsave( file.path( CU, "fig_c8_ceiling.png" ), g, width = 10, height = 4.4, dpi = 160, bg = "white" )
}
log_msg( "done" )
