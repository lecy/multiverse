## 73_pre2000_growth.R
## Is the Sunbelt density gap an old growth lag? Uses 1970/1980/1990 census
## county counts to ask
##   (a) do 2000 SAMIs reflect 1970-2000 growth, decade by decade, and is the
##       pattern what the within-metro catch-up rates (72_catchup_lag.R, half-
##       life ~5 yrs filers, ~9 yrs registered) would leave behind?
##   (b) how much of the fastest-growers' 2000 gap does pre-2000 growth explain?
##   (c) do metros whose growth slowed after 2000 converge 2000 -> 2024, as an
##       old lag would predict?
## Sources (public, downloaded to D:/MULTIVERSE/raw/census-hist/):
##   e7079co.txt  1970s intercensal county estimates (1970 census column)
##   e8089co.txt  1980s intercensal county estimates (1980 census column)
##   99c8_00.txt  CO-99-8, 1990s county estimates (April 1, 1990 base, last column)
## 2000-2024 population and SAMIs come from the analysis panel. Connecticut
## metros are dropped (2023 CBSAs are built on planning regions, which do not
## nest in the old counties). Writes D:/MULTIVERSE/data/catchup/c9_*.
##   Rscript R/73_pre2000_growth.R

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( ggplot2 ) } )
setFixest_notes( FALSE )
## "grass" argument: filers / grassroots / all registered, reading and writing data/catchup_grass/
GRASS <- "grass" %in% commandArgs( TRUE )
CU <- file.path( BUILD, "data", if ( GRASS ) "catchup_grass" else "catchup" ); HIST <- file.path( BUILD, "raw", "census-hist" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
options( width = 220 )
theme_c <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"

## ---- parse the decennial counts ------------------------------------------------------
## 1970s/1980s tables: "FIPS name census est est ..." with long names wrapped onto
## the next line; the first block of each county holds the census column.
read_old <- function( f ) {
  L <- readLines( file.path( HIST, f ), warn = FALSE )
  i <- grep( "^[0-9]{5} ", L ); out <- list()
  for ( k in i ) {
    fips <- substr( L[ k ], 1, 5 ); if ( !is.null( out[[ fips ]] ) || grepl( "000$", fips ) ) next
    tk <- strsplit( trimws( substring( L[ k ], 7 ) ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ]
    if ( !length( nums ) ) { tk <- strsplit( trimws( L[ k + 1 ] ), " +" )[[ 1 ]]; nums <- tk[ grepl( "^[0-9]+$", tk ) ] }
    if ( length( nums ) ) out[[ fips ]] <- as.numeric( nums[ 1 ] )
  }
  data.table( fips = names( out ), pop = unlist( out ) ) }
p70 <- read_old( "e7079co.txt" ); p80 <- read_old( "e8089co.txt" )
## CO-99-8 block 1: "1  SSCCC  1999 ... 1990-base  name"
L <- readLines( file.path( HIST, "99c8_00.txt" ), warn = FALSE )
L <- L[ grepl( "^1 +[0-9]{5} ", L ) ]
p90 <- rbindlist( lapply( L, function( x ) { tk <- strsplit( trimws( x ), " +" )[[ 1 ]]
  nums <- tk[ grepl( "^[0-9,]+$", tk ) ][ -1 ]                      # drop the block id
  data.table( fips = tk[ 2 ], pop = as.numeric( gsub( ",", "", nums[ length( nums ) ] ) ) ) } ) )
log_msg( "counties parsed: 1970 ", nrow( p70 ), ", 1980 ", nrow( p80 ), ", 1990 ", nrow( p90 ) )
cat( "US totals check (counties summed): 1970", sum( p70$pop ), " 1980", sum( p80$pop ), " 1990", sum( p90$pop ), "\n" )

## old FIPS -> 2023 county-equivalent
RECODE <- c( "12025" = "12086",   # Dade -> Miami-Dade (1997)
             "51780" = "51083",   # South Boston city -> Halifax Co (1995)
             "51560" = "51005",   # Clifton Forge city -> Alleghany Co (2001)
             "51515" = "51019",   # Bedford city -> Bedford Co (2013)
             "51123" = "51800",   # Nansemond Co -> Suffolk city (1974)
             "51695" = "51800",   # Nansemond city -> Suffolk city
             "30113" = "30067" )  # Yellowstone NP (MT) -> Park Co
hist <- rbindlist( list( p70[ , year := 1970 ], p80[ , year := 1980 ], p90[ , year := 1990 ] ) )
hist[ fips %in% names( RECODE ), fips := RECODE[ fips ] ]
hist <- hist[ , .( pop = sum( pop ) ), by = .( fips, year ) ]

sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "state_fips" ) ) )
sp <- sp[ cbsa_type == "metro" ]
cp <- fread( file.path( BUILD, "data", "pop", "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )[ year == 2000 ]
sp <- merge( sp, cp[ , .( geoid, pop2000 = population ) ], by = "geoid", all.x = TRUE )
## coverage: share of each metro's 2000 population in counties found in each historical year
cov <- rbindlist( lapply( c( 1970, 1980, 1990 ), function( y ) {
  h <- hist[ year == y ]
  sp[ , .( year = y, coverage = sum( pop2000[ geoid %in% h$fips ], na.rm = TRUE ) / sum( pop2000, na.rm = TRUE ),
           pop = sum( h$pop[ match( geoid[ geoid %in% h$fips ], h$fips ) ] ), ct = any( substr( geoid, 1, 2 ) == "09" ) ),
      by = .( cbsa, cbsa_title ) ] } ) )
low <- unique( cov[ coverage < 0.98 | ct, .( cbsa_title, year, coverage = round( coverage, 3 ) ) ] )
cat( "metros with incomplete historical coverage (dropped below 0.98) or in CT:\n" ); print( low )
mp <- dcast( cov[ coverage >= 0.98 & !ct ], cbsa ~ year, value.var = "pop" )
setnames( mp, c( "1970", "1980", "1990" ), c( "p1970", "p1980", "p1990" ) )
mp <- mp[ complete.cases( mp ) ]

## ---- panel SAMIs ------------------------------------------------------------------------
d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )[ cbsa_type == "metro" ]
d[ , logP := log( population ) ]
d[ , n_grass_c3pc := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]     # registered public charities that are not filers
OUTC <- if ( GRASS ) c( n_filer_c3 = "Filers", n_grass_c3pc = "Grassroots (registered, not filing)", n_bmf_c3pc = "Registered (all)" ) else
  c( n_filer_c3 = "Filers", n_grass_c3pc = "Grassroots (registered, not filing)", exp_c3_nohu = "Expenditures" )
for ( v in names( OUTC ) ) {
  d[ , ( paste0( "s_", v ) ) := { ok <- get( v ) > 0; z <- rep( NA_real_, .N ); z[ ok ] <- resid( lm( log( get( v )[ ok ] ) ~ logP[ ok ] ) ); z }, by = year ]
}
REGION <- c( "09" = "Northeast", "23" = "Northeast", "25" = "Northeast", "33" = "Northeast", "44" = "Northeast",
  "50" = "Northeast", "34" = "Northeast", "36" = "Northeast", "42" = "Northeast",
  "17" = "Midwest", "18" = "Midwest", "26" = "Midwest", "39" = "Midwest", "55" = "Midwest", "19" = "Midwest",
  "20" = "Midwest", "27" = "Midwest", "29" = "Midwest", "31" = "Midwest", "38" = "Midwest", "46" = "Midwest",
  "10" = "South", "11" = "South", "12" = "South", "13" = "South", "24" = "South", "37" = "South", "45" = "South",
  "51" = "South", "54" = "South", "01" = "South", "21" = "South", "28" = "South", "47" = "South", "05" = "South",
  "22" = "South", "40" = "South", "48" = "South",
  "04" = "West", "08" = "West", "16" = "West", "30" = "West", "32" = "West", "35" = "West", "49" = "West",
  "56" = "West", "02" = "West", "06" = "West", "15" = "West", "41" = "West", "53" = "West" )
avg <- function( v, yrs ) d[ year %in% yrs, .( z = mean( get( v ), na.rm = TRUE ) ), keyby = cbsa ]
x <- d[ year == 2000, .( cbsa, name = paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ),
                         region = unname( REGION[ state_fips ] ), p2000 = population, pct_ba0 = pct_ba, l_pcpi0 = log( pcpi ),
                         pov0 = poverty_rate, old0 = pct_65plus, nonwhite0 = pct_nonwhite ) ]
x <- merge( x, d[ year == 2024, .( cbsa, p2024 = population ) ], by = "cbsa" )
for ( v in names( OUTC ) ) {
  x <- merge( x, setnames( avg( paste0( "s_", v ), 2000:2002 ), "z", paste0( "s0_", v ) ), by = "cbsa" )
  x <- merge( x, setnames( avg( paste0( "s_", v ), 2022:2024 ), "z", paste0( "s1_", v ) ), by = "cbsa" )
}
x <- merge( x, mp, by = "cbsa" )
x[ , `:=`( g70 = log( p1980 / p1970 ), g80 = log( p1990 / p1980 ), g90 = log( p2000 / p1990 ),
           g7000 = log( p2000 / p1970 ), g0024 = log( p2024 / p2000 ) ) ]
## annualized growth rates (log points per year) before and after 2000
x[ , `:=`( r_pre = g7000 / 30, r_post = g0024 / 24 ) ][ , slowdown := r_pre - r_post ]
log_msg( "metros with 1970-2024 population: ", nrow( x ), " of ", uniqueN( d$cbsa ) )
cat( "median growth 1970-2000:", round( 100 * ( exp( median( x$g7000 ) ) - 1 ) ), "%; 2000-2024:", round( 100 * ( exp( median( x$g0024 ) ) - 1 ) ),
     "%; cor(annual rate pre, post) =", round( cor( x$r_pre, x$r_post ), 3 ), "\n" )
x[ , gq := cut( g7000, quantile( g7000, 0:5 / 5 ), include.lowest = TRUE, labels = paste0( "Q", 1:5 ) ) ]
fwrite( x, file.path( CU, "c9_metro_1970_2024.csv" ) )

## ---- (a) decade growth and the 2000 SAMI ------------------------------------------------
## expected coefficient under the within-metro catch-up rates: an episode leaves an
## immediate gap theta0 that decays with half-life H; a decade's growth spread evenly
## over the decade has average age a (years before 2001)
irf <- fread( file.path( CU, "c2_half_life.csv" ) )[ fe == "metro_year" ]
## within-metro half-lives from 72_catchup_lag.R part 2 (expenditures have none: noisy; use 5)
HL <- setNames( pmax( irf$half_life, 1 ), irf$outcome ); HL[ is.na( HL ) ] <- 5
expected <- rbindlist( lapply( names( OUTC ), function( v ) { th <- irf[ outcome == v, theta0 ]
  data.table( outcome = v, term = c( "g90", "g80", "g70" ), age = c( 6, 16, 26 ),
              expected = th * 0.5^( c( 6, 16, 26 ) / HL[[ v ]] ) ) } ) )
dec <- rbindlist( lapply( names( OUTC ), function( v ) { y <- paste0( "s0_", v )
  f <- list( "decades" = paste( y, "~ g90 + g80 + g70" ),
             "decades + region" = paste( y, "~ g90 + g80 + g70 | region" ),
             "decades + region + 2000 traits" = paste( y, "~ g90 + g80 + g70 + pct_ba0 + l_pcpi0 + pov0 + old0 + nonwhite0 | region" ),
             "1970-2000 total" = paste( y, "~ g7000" ),
             "1970-2000 total + region + 2000 traits" = paste( y, "~ g7000 + pct_ba0 + l_pcpi0 + pov0 + old0 + nonwhite0 | region" ) )
  rbindlist( lapply( names( f ), function( k ) { r <- feols( as.formula( f[[ k ]] ), x, vcov = "hetero" ); ct <- coeftable( r )
    data.table( outcome = v, model = k, term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ], r2 = r2( r, "r2" ), n = nobs( r ) ) } ) ) } ) )
dec <- merge( dec, expected, by = c( "outcome", "term" ), all.x = TRUE )
print( r3( dec[ term != "(Intercept)" ][ order( outcome, model, term ) ] ), nrows = 200 )
fwrite( dec, file.path( CU, "c9_decade_growth_2000sami.csv" ) )

## ---- (b) how much of the fast growers' 2000 gap does pre-2000 growth explain? -----------
## Q5 - Q1 gap in the 2000 SAMI, and the part accounted for by (i) the cross-metro slope on
## 1970-2000 growth, raw and with region + 2000 traits, (ii) the lag deficit the within-metro
## catch-up rates predict from each decade's growth
lagdef <- function( v ) { e <- expected[ outcome == v ]; x$g90 * e[ term == "g90", expected ] + x$g80 * e[ term == "g80", expected ] + x$g70 * e[ term == "g70", expected ] }
lg <- rbindlist( lapply( names( OUTC ), function( v ) { z <- copy( x )[ , ld := lagdef( v ) ]
  z[ , .( metros = .N, growth_1970_2000_pct = 100 * ( exp( median( g7000 ) ) - 1 ), predicted_lag_deficit = mean( ld ),
          sami_2000 = mean( get( paste0( "s0_", v ) ), na.rm = TRUE ) ), keyby = gq ][ , outcome := v ] } ) )
print( r3( lg ) ); fwrite( lg, file.path( CU, "c9_predicted_lag_deficit.csv" ) )
gapq <- rbindlist( lapply( names( OUTC ), function( v ) {
  b_raw <- dec[ outcome == v & model == "1970-2000 total" & term == "g7000", est ]
  b_ctl <- dec[ outcome == v & model == "1970-2000 total + region + 2000 traits" & term == "g7000", est ]
  q <- lg[ outcome == v ]; dg <- x[ gq == "Q5", mean( g7000 ) ] - x[ gq == "Q1", mean( g7000 ) ]
  gap <- q[ gq == "Q5", sami_2000 ] - q[ gq == "Q1", sami_2000 ]
  data.table( outcome = v, gap_q5_q1 = gap, growth_diff_log = dg,
              share_cross_metro_slope = b_raw * dg / gap, share_slope_with_controls = b_ctl * dg / gap,
              share_lag_model = ( q[ gq == "Q5", predicted_lag_deficit ] - q[ gq == "Q1", predicted_lag_deficit ] ) / gap ) } ) )
print( r3( gapq ) ); fwrite( gapq, file.path( CU, "c9_gap_by_pre2000_growth.csv" ) )

## ---- (c) do metros whose growth slowed after 2000 converge? ------------------------------
## rates in % per year; d SAMI 2000-02 -> 2022-24
x[ , `:=`( r_pre_pct = 100 * r_pre, r_post_pct = 100 * r_post ) ]
cv <- rbindlist( lapply( names( OUTC ), function( v ) {
  z <- copy( x )[ , `:=`( ds = get( paste0( "s1_", v ) ) - get( paste0( "s0_", v ) ), s0 = get( paste0( "s0_", v ) ) ) ]
  f <- list( "pre and post growth" = "ds ~ r_pre_pct + r_post_pct",
             "+ 2000 SAMI" = "ds ~ r_pre_pct + r_post_pct + s0",
             "+ 2000 SAMI + region" = "ds ~ r_pre_pct + r_post_pct + s0 | region",
             "+ traits + region" = "ds ~ r_pre_pct + r_post_pct + s0 + pct_ba0 + l_pcpi0 + pov0 + old0 + nonwhite0 | region" )
  rbindlist( lapply( names( f ), function( k ) { r <- feols( as.formula( f[[ k ]] ), z, vcov = "hetero" ); ct <- coeftable( r )
    data.table( outcome = v, model = k, term = rownames( ct ), est = ct[ , 1 ], se = ct[ , 2 ], p = ct[ , 4 ], r2 = r2( r, "r2" ), n = nobs( r ) ) } ) ) } ) )
print( r3( cv[ term %in% c( "r_pre_pct", "r_post_pct", "s0" ) ] ), nrows = 100 )
fwrite( cv, file.path( CU, "c9_slowdown_convergence.csv" ) )
## group view: fast pre-2000 growers (top quintile) split by post-2000 growth
x[ , fast_pre := gq == "Q5" ][ , post_half := fifelse( r_post >= median( r_post[ fast_pre ] ), "kept growing fast", "slowed down" ), by = fast_pre ]
grp <- rbindlist( lapply( names( OUTC ), function( v ) x[ , .( metros = .N, growth_7000_pct = 100 * ( exp( median( g7000 ) ) - 1 ),
  growth_0024_pct = 100 * ( exp( median( g0024 ) ) - 1 ), sami_2000 = mean( get( paste0( "s0_", v ) ) ), sami_2024 = mean( get( paste0( "s1_", v ) ) ),
  change = mean( get( paste0( "s1_", v ) ) - get( paste0( "s0_", v ) ) ) ), keyby = .( fast_pre, post_half ) ][ , outcome := v ] ) )
print( r3( grp ) ); fwrite( grp, file.path( CU, "c9_fast_pre2000_groups.csv" ) )
cat( "\nfast pre-2000 growers that slowed most:\n" )
print( x[ fast_pre == TRUE ][ order( -slowdown ) ][ 1:15, .( name, region, g7000_pct = round( 100 * ( exp( g7000 ) - 1 ) ), g0024_pct = round( 100 * ( exp( g0024 ) - 1 ) ),
  dev2000 = round( 100 * ( exp( s0_n_filer_c3 ) - 1 ) ), dev2024 = round( 100 * ( exp( s1_n_filer_c3 ) - 1 ) ),
  grass2000 = round( 100 * ( exp( s0_n_grass_c3pc ) - 1 ) ), grass2024 = round( 100 * ( exp( s1_n_grass_c3pc ) - 1 ) ) ) ] )

## ---- figures --------------------------------------------------------------------------------
fd <- dec[ model %in% c( "decades", "decades + region + 2000 traits" ) & term %in% c( "g90", "g80", "g70" ) ]
fd[ , decade := factor( c( g90 = "1990s", g80 = "1980s", g70 = "1970s" )[ term ], c( "1970s", "1980s", "1990s" ) ) ]
g <- ggplot( fd, aes( decade, est, colour = model ) ) + geom_hline( yintercept = 0, colour = "grey70" ) +
  geom_pointrange( aes( ymin = est - 1.96 * se, ymax = est + 1.96 * se ), position = position_dodge( width = 0.5 ), size = 0.35 ) +
  geom_point( aes( y = expected ), shape = 4, size = 3, colour = "grey20", stroke = 1 ) +
  facet_wrap( ~ OUTC[ outcome ], scales = "free_y" ) +
  scale_colour_manual( values = c( C1, C2 ), labels = c( "Decade growth only", "+ region and 2000 traits" ), name = NULL ) +
  labs( title = "2000-02 SAMI on population growth in each decade before 2000",
        subtitle = "Per log point of decade growth; x = what the within-metro catch-up half-lives (72_catchup_lag.R part 2) would leave by 2001",
        x = NULL, y = "Coefficient" ) + theme_c
ggsave( file.path( CU, "fig_c9_decades.png" ), g, width = 10, height = 4.2, dpi = 160, bg = "white" )
v2 <- names( OUTC )[ 1:2 ]
xs <- x[ , .( name, region, gq, r_pre_pct, r_post_pct, d1 = get( paste0( "s1_", v2[ 1 ] ) ) - get( paste0( "s0_", v2[ 1 ] ) ),
              d2 = get( paste0( "s1_", v2[ 2 ] ) ) - get( paste0( "s0_", v2[ 2 ] ) ) ) ]
xs <- melt( xs, id.vars = c( "name", "region", "gq", "r_pre_pct", "r_post_pct" ), variable.name = "outcome", value.name = "dchange" )
xs[ , outcome := fifelse( outcome == "d1", OUTC[[ v2[ 1 ] ]], OUTC[[ v2[ 2 ] ]] ) ]
xs <- xs[ abs( dchange ) < 0.6 ]                        # drops 2 registered-stock artifacts from the display only
g <- ggplot( xs, aes( r_pre_pct, dchange ) ) + geom_hline( yintercept = 0, colour = "grey80" ) +
  geom_point( aes( colour = r_post_pct ), size = 1.3, alpha = 0.8 ) +
  geom_smooth( method = "lm", formula = y ~ x, colour = "grey20", linewidth = 0.6, se = FALSE ) +
  ggrepel::geom_text_repel( data = xs[ gq == "Q5" & r_pre_pct - r_post_pct > 2.6 ], aes( label = name ),
                            size = 2.2, colour = "grey25", seed = 1, max.overlaps = 25 ) +
  facet_wrap( ~ outcome, scales = "free_y" ) +
  scale_colour_gradient2( low = C2, mid = "grey80", high = C1, midpoint = median( x$r_post_pct ), name = "Growth 2000-24 (%/yr)" ) +
  labs( title = "SAMI change 2000-02 to 2022-24 vs population growth 1970-2000",
        subtitle = "An old growth lag would show fast pre-2000 growers that slowed after 2000 (grey/orange, right side) rising",
        x = "Population growth 1970-2000 (%/yr)", y = "Change in SAMI" ) + theme_c
ggsave( file.path( CU, "fig_c9_slowdown.png" ), g, width = 10, height = 4.6, dpi = 160, bg = "white" )
log_msg( "done" )
