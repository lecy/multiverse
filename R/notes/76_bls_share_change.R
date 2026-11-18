## 76_bls_share_change.R
## How much does the nonprofit share of private employment change within metros
## (BLS QCEW nonprofit research data, 2013-2022), and is a shift driven by
## establishment counts or by employees per establishment?
## Identity (nonprofit NP vs for-profit FP = all private minus NP):
##   d log(NP emp / FP emp) = [d log NP estab - d log FP estab]          (establishment growth gap)
##                          + [d log NP emp/estab - d log FP emp/estab]  (establishment size gap)
## Endpoints are 2-year averages, 2013-14 and 2021-22; the annual version uses
## year-to-year changes. Balanced panel: metros with all ten years and no
## delineation change. Also relates share change to the change in filer and
## grassroots SAMIs over the same window.
## Writes D:/MULTIVERSE/data/catchup/c11_*, fig_c11_*.
##   Rscript R/notes/76_bls_share_change.R

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ) } )
CU <- file.path( BUILD, "data", "catchup" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
options( width = 220 )
theme_c <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"
REGION <- c( "09" = "Northeast", "23" = "Northeast", "25" = "Northeast", "33" = "Northeast", "44" = "Northeast",
  "50" = "Northeast", "34" = "Northeast", "36" = "Northeast", "42" = "Northeast",
  "17" = "Midwest", "18" = "Midwest", "26" = "Midwest", "39" = "Midwest", "55" = "Midwest", "19" = "Midwest",
  "20" = "Midwest", "27" = "Midwest", "29" = "Midwest", "31" = "Midwest", "38" = "Midwest", "46" = "Midwest",
  "10" = "South", "11" = "South", "12" = "South", "13" = "South", "24" = "South", "37" = "South", "45" = "South",
  "51" = "South", "54" = "South", "01" = "South", "21" = "South", "28" = "South", "47" = "South", "05" = "South",
  "22" = "South", "40" = "South", "48" = "South",
  "04" = "West", "08" = "West", "16" = "West", "30" = "West", "32" = "West", "35" = "West", "49" = "West",
  "56" = "West", "02" = "West", "06" = "West", "15" = "West", "41" = "West", "53" = "West" )

d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )[ cbsa_type == "metro" ]
d[ , logP := log( population ) ][ , n_grass_c3pc := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]
for ( v in c( "n_filer_c3", "n_grass_c3pc" ) )
  d[ , ( paste0( "s_", v ) ) := { ok <- get( v ) > 0; z <- rep( NA_real_, .N ); z[ ok ] <- resid( lm( log( get( v )[ ok ] ) ~ logP[ ok ] ) ); z }, by = year ]
d[ , `:=`( region = unname( REGION[ state_fips ] ),
           name = paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ) ) ]
b <- d[ year %in% 2013:2022 & !( bls_delin_flag %in% TRUE ) & is.finite( bls_npo_emp ) & is.finite( bls_npo_estab ) &
        is.finite( bls_priv_emp ) & is.finite( bls_priv_estab ) & bls_npo_emp > 0 & bls_npo_estab > 0 ]
b <- b[ , if ( .N == 10 ) .SD, by = cbsa ]
b[ , `:=`( np_emp = bls_npo_emp, np_est = bls_npo_estab, fp_emp = bls_priv_emp - bls_npo_emp, fp_est = bls_priv_estab - bls_npo_estab ) ]
b[ , share := np_emp / bls_priv_emp ]
log_msg( "balanced BLS panel: ", uniqueN( b$cbsa ), " metros x 10 years" )

## ---- 1. how much does the share move? -----------------------------------------------------
w <- b[ , .( share_sd_within = sd( 100 * share ), share_mean = mean( 100 * share ),
             share_range = 100 * ( max( share ) - min( share ) ) ), by = cbsa ]
lev <- b[ , .( m = mean( 100 * share ) ), by = cbsa ]
mov <- data.table( between_sd = sd( lev$m ), median_within_sd = median( w$share_sd_within ),
                   median_range = median( w$share_range ),
                   cor_2013_2022 = b[ , .( s13 = share[ year == 2013 ], s22 = share[ year == 2022 ] ), by = cbsa ][ , cor( s13, s22 ) ] )
print( r3( mov ) )

## ---- 2. endpoint decomposition, 2013-14 -> 2021-22 ----------------------------------------
ep <- function( yrs ) b[ year %in% yrs, .( np_emp = mean( np_emp ), np_est = mean( np_est ), fp_emp = mean( fp_emp ), fp_est = mean( fp_est ),
                                          share = mean( share ), s_f = mean( s_n_filer_c3 ), s_g = mean( s_n_grass_c3pc ) ), by = cbsa ]
x <- merge( ep( 2013:2014 ), ep( 2021:2022 ), by = "cbsa", suffixes = c( "0", "1" ) )
x <- merge( x, unique( b[ , .( cbsa, name, region ) ] ), by = "cbsa" )
x[ , `:=`( d_pp = 100 * ( share1 - share0 ),
           g_np_est = log( np_est1 / np_est0 ), g_fp_est = log( fp_est1 / fp_est0 ),
           g_np_size = log( ( np_emp1 / np_est1 ) / ( np_emp0 / np_est0 ) ), g_fp_size = log( ( fp_emp1 / fp_est1 ) / ( fp_emp0 / fp_est0 ) ) ) ]
x[ , `:=`( c_est = g_np_est - g_fp_est, c_size = g_np_size - g_fp_size ) ][ , d_ratio := c_est + c_size ]
x[ , `:=`( d_sami_filer = s_f1 - s_f0, d_sami_grass = s_g1 - s_g0 ) ]
fwrite( x, file.path( CU, "c11_metro_share_change.csv" ) )
q <- function( z ) round( quantile( z, c( .05, .25, .5, .75, .95 ) ), 2 )
cat( "\nchange in share, percentage points (p5 p25 p50 p75 p95):", q( x$d_pp ), "\n" )
cat( "metros gaining > 1 pt:", sum( x$d_pp > 1 ), "; losing > 1 pt:", sum( x$d_pp < -1 ), "; of", nrow( x ), "\n" )
## variance decomposition across metros: cov(total, component) / var(total), shares sum to 1
vd <- data.table( component = c( "establishment growth gap (NP - FP)", "establishment size gap (NP - FP)" ),
                  share_of_variance = c( cov( x$d_ratio, x$c_est ), cov( x$d_ratio, x$c_size ) ) / var( x$d_ratio ),
                  mean = c( mean( x$c_est ), mean( x$c_size ) ), sd = c( sd( x$c_est ), sd( x$c_size ) ),
                  cor_with_total = c( cor( x$d_ratio, x$c_est ), cor( x$d_ratio, x$c_size ) ) )
## the four underlying growth rates
gr <- x[ , .( np_estab = mean( g_np_est ), np_size = mean( g_np_size ), fp_estab = mean( g_fp_est ), fp_size = mean( g_fp_size ),
              sd_np_estab = sd( g_np_est ), sd_np_size = sd( g_np_size ), sd_fp_estab = sd( g_fp_est ), sd_fp_size = sd( g_fp_size ) ) ]
## further split of the total into the four terms
vd4 <- data.table( term = c( "NP establishment growth", "NP size growth", "FP establishment growth (minus)", "FP size growth (minus)" ),
                   share_of_variance = c( cov( x$d_ratio, x$g_np_est ), cov( x$d_ratio, x$g_np_size ),
                                          cov( x$d_ratio, -x$g_fp_est ), cov( x$d_ratio, -x$g_fp_size ) ) / var( x$d_ratio ) )
print( r3( vd ) ); print( r3( gr ) ); print( r3( vd4 ) )
fwrite( vd, file.path( CU, "c11_decomposition.csv" ) ); fwrite( vd4, file.path( CU, "c11_decomposition_4.csv" ) ); fwrite( gr, file.path( CU, "c11_growth_rates.csv" ) )

## ---- 3. annual changes (year-to-year), pooled, net of the year mean ---------------------------
setorder( b, cbsa, year )
b[ , `:=`( a_est = c( NA, diff( log( np_est ) ) ) - c( NA, diff( log( fp_est ) ) ),
           a_size = c( NA, diff( log( np_emp / np_est ) ) ) - c( NA, diff( log( fp_emp / fp_est ) ) ) ), by = cbsa ]
a <- b[ is.finite( a_est ) ][ , `:=`( a_est = a_est - mean( a_est ), a_size = a_size - mean( a_size ) ), by = year ][ , a_tot := a_est + a_size ]
vda <- data.table( component = c( "establishment growth gap", "establishment size gap" ),
                   share_of_variance = c( cov( a$a_tot, a$a_est ), cov( a$a_tot, a$a_size ) ) / var( a$a_tot ),
                   cor_est_size = cor( a$a_est, a$a_size ) )
print( r3( vda ) ); fwrite( vda, file.path( CU, "c11_decomposition_annual.csv" ) )
cat( "cor(establishment gap, size gap) across metros, endpoints:", round( cor( x$c_est, x$c_size ), 3 ), "\n" )

## ---- 4. national (balanced metros summed) trends ------------------------------------------------
nat <- b[ , .( np_emp = sum( np_emp ), np_est = sum( np_est ), fp_emp = sum( fp_emp ), fp_est = sum( fp_est ) ), keyby = year ]
nat[ , `:=`( share = 100 * np_emp / ( np_emp + fp_emp ), np_size = np_emp / np_est, fp_size = fp_emp / fp_est ) ]
print( r3( nat ) ); fwrite( nat, file.path( CU, "c11_national.csv" ) )
ni <- melt( nat[ , .( year, `Nonprofit establishments` = np_est, `Nonprofit employees per establishment` = np_size,
                      `For-profit establishments` = fp_est, `For-profit employees per establishment` = fp_size ) ], id.vars = "year" )
ni[ , idx := 100 * value / value[ year == 2013 ], by = variable ]

## ---- 5. does share change line up with density change? -----------------------------------------
rel <- rbindlist( lapply( c( "d_sami_filer", "d_sami_grass" ), function( v ) data.table( outcome = v,
  cor_total = cor( x[[ v ]], x$d_ratio ), cor_est = cor( x[[ v ]], x$c_est ), cor_size = cor( x[[ v ]], x$c_size ),
  cor_np_est = cor( x[[ v ]], x$g_np_est ) ) ) )
print( r3( rel ) ); fwrite( rel, file.path( CU, "c11_share_vs_sami.csv" ) )
top <- rbind( x[ order( -d_pp ) ][ 1:10 ], x[ order( d_pp ) ][ 1:10 ] )[ , .( name, region, share0 = round( 100 * share0, 1 ), share1 = round( 100 * share1, 1 ),
  d_pp = round( d_pp, 1 ), est_gap = round( 100 * c_est ), size_gap = round( 100 * c_size ) ) ]
print( top ); fwrite( top, file.path( CU, "c11_top_movers.csv" ) )

## ---- figures -----------------------------------------------------------------------------------------
lab <- x[ order( -abs( d_pp ) ) ][ 1:14 ]
ga <- ggplot( x, aes( 100 * share0, 100 * share1 ) ) + geom_abline( slope = 1, intercept = 0, colour = "grey60" ) +
  geom_point( aes( colour = region ), size = 1.4, alpha = 0.75 ) +
  ggrepel::geom_text_repel( data = lab, aes( label = name ), size = 2.2, colour = "grey25", seed = 1, max.overlaps = 30 ) +
  scale_x_log10( breaks = c( 2, 5, 10, 20, 40 ) ) + scale_y_log10( breaks = c( 2, 5, 10, 20, 40 ) ) +
  scale_colour_manual( values = c( Northeast = C1, Midwest = C3, South = C2, West = C4 ), name = NULL ) +
  labs( title = "A. Nonprofit share of private employment",
        x = "Share 2013-14 (%, log scale)", y = "Share 2021-22 (%, log scale)" ) + theme_c
lim <- max( abs( c( x$c_est, x$c_size ) ) ) * 100
gb <- ggplot( x, aes( 100 * c_est, 100 * c_size ) ) +
  geom_abline( slope = -1, intercept = 0, colour = "grey55", linetype = 2 ) + geom_hline( yintercept = 0, colour = "grey80" ) + geom_vline( xintercept = 0, colour = "grey80" ) +
  geom_point( aes( fill = pmax( pmin( 100 * d_ratio, 40 ), -40 ) ), shape = 21, colour = "white", size = 1.9, stroke = 0.2 ) +
  scale_fill_gradient2( low = C2, mid = "#eeeeee", high = C1, midpoint = 0, limits = c( -40, 40 ), name = "Total change in\nNP/FP employment (%)" ) +
  coord_equal( xlim = c( -lim, lim ), ylim = c( -lim, lim ) ) +
  labs( title = "B. Where the change comes from",
        subtitle = "Dashed line: the two gaps offset (no change in share)",
        x = "Establishment growth gap, NP minus FP (log points x 100)", y = "Employees-per-establishment gap, NP minus FP" ) +
  theme_c + theme( legend.position = "right" )
ggsave( file.path( CU, "fig_c11_share_change.png" ), patchwork::wrap_plots( ga, gb, ncol = 2, widths = c( 1, 1.1 ) ),
        width = 11, height = 5.2, dpi = 160, bg = "white" )
## panel A by region: each region's metros in colour over all metros in grey; the 4 largest
## movers per region labelled; regional summary table
RG <- c( "Northeast", "Midwest", "South", "West" ); RCOL <- c( Northeast = C1, Midwest = C3, South = C2, West = C4 )
xr <- copy( x )[ , region := factor( region, RG ) ]
bg <- copy( x )[ , region := NULL ]
rl <- xr[ order( -abs( d_pp ) ), head( .SD, 4 ), by = region ]
rs <- xr[ , .( metros = .N, median_share_avg = round( median( 100 * ( share0 + share1 ) / 2 ), 1 ),
               p05_share_avg = round( quantile( 100 * ( share0 + share1 ) / 2, .05 ), 1 ), p95_share_avg = round( quantile( 100 * ( share0 + share1 ) / 2, .95 ), 1 ),
               median_share_2013 = round( median( 100 * share0 ), 1 ), median_share_2022 = round( median( 100 * share1 ), 1 ),
               median_change_pp = round( median( d_pp ), 2 ), gained_1pt = sum( d_pp > 1 ), lost_1pt = sum( d_pp < -1 ),
               size_share_of_variance = round( cov( d_ratio, c_size ) / var( d_ratio ), 2 ) ), keyby = region ]
print( rs ); fwrite( rs, file.path( CU, "c11_by_region.csv" ) )
## headline: median share (each metro averaged over both periods), large and in the region colour
rs_lab <- rs[ , .( region, big = sprintf( "%s  %.1f%%", region, median_share_avg ),
                   txt = sprintf( "median nonprofit share (avg. of both periods)\n%d metros; %.1f%% -> %.1f%%; %d gained >1 pt, %d lost >1 pt",
                                  metros, median_share_2013, median_share_2022, gained_1pt, lost_1pt ) ) ]
gr <- ggplot( xr, aes( 100 * share0, 100 * share1 ) ) + geom_abline( slope = 1, intercept = 0, colour = "grey60" ) +
  geom_point( data = bg, colour = "grey88", size = 1.1 ) +
  geom_point( aes( colour = region ), size = 1.6, alpha = 0.85 ) +
  ggrepel::geom_text_repel( data = rl, aes( label = sprintf( "%s (%+.1f)", name, d_pp ) ), size = 2.3, colour = "grey20", seed = 1, max.overlaps = 30,
                            min.segment.length = 0, segment.colour = "grey60", segment.size = 0.2 ) +
  geom_text( data = rs_lab, aes( x = 1.6, y = 52, label = big, colour = region ), hjust = 0, vjust = 1, size = 10, fontface = "bold", inherit.aes = FALSE ) +
  geom_text( data = rs_lab, aes( x = 1.6, y = 34, label = txt ), hjust = 0, vjust = 1, size = 2.5, colour = "grey35", lineheight = 1, inherit.aes = FALSE ) +
  ## middle 90% of metros (5th-95th percentile of the two-period average share) as a bar along the x-axis, median dot
  geom_segment( data = rs, aes( x = p05_share_avg, xend = p95_share_avg, y = 1.38, yend = 1.38, colour = region ),
                linewidth = 4, alpha = 0.45, lineend = "round", inherit.aes = FALSE ) +
  geom_point( data = rs, aes( x = median_share_avg, y = 1.38, colour = region ), size = 3.6, inherit.aes = FALSE ) +
  geom_text( data = rs, aes( x = p05_share_avg, y = 1.38, label = sprintf( "%.1f%%", p05_share_avg ), colour = region ),
             hjust = 1.35, size = 2.9, fontface = "bold", inherit.aes = FALSE ) +
  geom_text( data = rs, aes( x = p95_share_avg, y = 1.38, label = sprintf( "%.1f%%", p95_share_avg ), colour = region ),
             hjust = -0.35, size = 2.9, fontface = "bold", inherit.aes = FALSE ) +
  facet_wrap( ~ region, ncol = 2 ) +
  scale_x_log10( breaks = c( 2, 5, 10, 20, 40 ) ) + scale_y_log10( breaks = c( 2, 5, 10, 20, 40 ) ) +
  expand_limits( y = 1.3, x = 1.25 ) +
  scale_colour_manual( values = RCOL, guide = "none" ) +
  labs( title = "Nonprofit share of private employment, 2013-14 vs 2021-22, by region",
        subtitle = paste( "Grey: all other metros. Above the diagonal = share rose. Labels: the four largest changes in each region (percentage points).",
                          "Bar at the bottom: middle 90% of the region's metros (5th-95th percentile of the two-period average share); dot = median.", sep = "
" ),
        x = "Share 2013-14 (%, log scale)", y = "Share 2021-22 (%, log scale)" ) + theme_c +
  theme( strip.text = element_blank() )                     # region name is in the large coloured headline instead
ggsave( file.path( CU, "fig_c11_share_by_region.png" ), gr, width = 10, height = 8.4, dpi = 160, bg = "white" )
gn <- ggplot( ni, aes( year, idx, colour = variable, linetype = grepl( "per establishment", variable ) ) ) +
  geom_hline( yintercept = 100, colour = "grey75" ) + geom_line( linewidth = 0.9 ) + geom_point( size = 1.2 ) +
  scale_colour_manual( values = c( C1, C1, C2, C2 ), name = NULL ) + scale_linetype_manual( values = c( "FALSE" = 1, "TRUE" = 2 ), guide = "none" ) +
  guides( colour = guide_legend( nrow = 2, override.aes = list( linetype = c( 1, 2, 1, 2 ) ) ) ) +
  scale_x_continuous( breaks = 2013:2022 ) +
  labs( title = sprintf( "Establishments and establishment size, %d metros combined (2013 = 100)", uniqueN( b$cbsa ) ),
        subtitle = sprintf( "Nonprofit share of private employment: %.1f%% (2013) -> %.1f%% (2019) -> %.1f%% (2020) -> %.1f%% (2022)",
                            nat[ year == 2013, share ], nat[ year == 2019, share ], nat[ year == 2020, share ], nat[ year == 2022, share ] ),
        x = NULL, y = "Index, 2013 = 100" ) + theme_c
ggsave( file.path( CU, "fig_c11_national.png" ), gn, width = 9, height = 4.6, dpi = 160, bg = "white" )

## ---- 6. why the regions differ: regional medians of metro averages, 2013-2022 ---------------------
## all metros with BLS data (not only the balanced panel); endowment and growth from 72/73 outputs
pr <- d[ year %in% 2013:2022 & !( bls_delin_flag %in% TRUE ) & is.finite( bls_npo_emp ), .(
  np_jobs_per_1000 = 1000 * mean( bls_npo_emp / population ), fp_jobs_per_1000 = 1000 * mean( ( bls_priv_emp - bls_npo_emp ) / population ),
  np_share = 100 * mean( bls_pct_npo ), np_share_health = 100 * mean( bls_pct_npo_62, na.rm = TRUE ), np_share_educ = 100 * mean( bls_pct_npo_61, na.rm = TRUE ),
  np_share_socassist = 100 * mean( bls_pct_npo_624, na.rm = TRUE ), np_share_arts = 100 * mean( bls_pct_npo_71, na.rm = TRUE ),
  gov_emp_share = 100 * mean( gov_emp_share, na.rm = TRUE ), pct_adherent = 100 * mean( pct_adherent, na.rm = TRUE ),
  catholic_share_of_adherents = 100 * mean( pct_catholic, na.rm = TRUE ), pct_65plus = 100 * mean( pct_65plus, na.rm = TRUE ),
  pct_ba = 100 * mean( pct_ba, na.rm = TRUE ) ), by = .( cbsa, region ) ]
f3 <- file.path( CU, "c3_metro_periods.csv" ); f9 <- file.path( CU, "c9_metro_1970_2024.csv" )
if ( file.exists( f3 ) ) pr <- merge( pr, fread( f3, colClasses = list( character = "cbsa" ) )[ , .( cbsa, pre1970_endowment_sami = L0_hist,
  filer_sami_2000 = s0_n_filer_c3, foundation_asset_sami_2000 = L0_x_pf ) ], by = "cbsa", all.x = TRUE )
if ( file.exists( f9 ) ) pr <- merge( pr, fread( f9, colClasses = list( character = "cbsa" ) )[ , .( cbsa, growth_1970_2000_pct = 100 * ( exp( g7000 ) - 1 ) ) ],
  by = "cbsa", all.x = TRUE )
pr[ , region := factor( region, c( "Northeast", "Midwest", "South", "West" ) ) ]
num <- setdiff( names( pr ), c( "cbsa", "region" ) )
prof <- pr[ , c( list( metros = .N ), lapply( .SD, function( z ) round( median( z, na.rm = TRUE ), 2 ) ) ), keyby = region, .SDcols = num ]
print( prof ); fwrite( prof, file.path( CU, "c11_region_profile.csv" ) )
log_msg( "done" )
