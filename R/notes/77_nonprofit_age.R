## 77_nonprofit_age.R
## How old are nonprofits, by state and region; does an older sector go with a larger
## nonprofit market share; and how does age relate to size?
## Age = 2022 - IRS ruling year (BMF 2022 snapshot, 501(c)(3) public charities).
## Filers are matched by EIN to 2022 e-file returns (990/990-EZ with revenue or
## expenses >= the $50k-in-2010-dollars floor, as in 06_npo_assemble.R); the rest of
## the registered stock is "grassroots". Expenditure-weighted age = the age of the
## average dollar spent by filers, the closest analogue to employment share.
## Caveats: the ruling year is when the IRS recognized the exemption, not founding;
## it resets when a revoked organization is reinstated (many after the 2011
## auto-revocation), and group-exemption subordinates carry the group's date.
## Writes D:/MULTIVERSE/data/catchup/c12_*, fig_c12_*.
##   Rscript R/notes/77_nonprofit_age.R

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ) } )
CU <- file.path( BUILD, "data", "catchup" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
options( width = 220 )
theme_c <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"
RCOL <- c( Northeast = C1, Midwest = C3, South = C2, West = C4 )
REGION <- c( "09" = "Northeast", "23" = "Northeast", "25" = "Northeast", "33" = "Northeast", "44" = "Northeast",
  "50" = "Northeast", "34" = "Northeast", "36" = "Northeast", "42" = "Northeast",
  "17" = "Midwest", "18" = "Midwest", "26" = "Midwest", "39" = "Midwest", "55" = "Midwest", "19" = "Midwest",
  "20" = "Midwest", "27" = "Midwest", "29" = "Midwest", "31" = "Midwest", "38" = "Midwest", "46" = "Midwest",
  "10" = "South", "11" = "South", "12" = "South", "13" = "South", "24" = "South", "37" = "South", "45" = "South",
  "51" = "South", "54" = "South", "01" = "South", "21" = "South", "28" = "South", "47" = "South", "05" = "South",
  "22" = "South", "40" = "South", "48" = "South",
  "04" = "West", "08" = "West", "16" = "West", "30" = "West", "32" = "West", "35" = "West", "49" = "West",
  "56" = "West", "02" = "West", "06" = "West", "15" = "West", "41" = "West", "53" = "West" )
YR <- 2022L

## ---- organizations ----------------------------------------------------------------------------
floor22 <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )[ year == YR, floor_nom ]
b <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "state", "subsec", "pf", "ryear", "ntee3" ) )
b <- b[ year == YR & subsec == 3 & pf == FALSE & is.finite( ryear ) & ryear >= 1800 & ryear <= YR ]
b <- unique( b, by = "EIN" )[ , `:=`( age = YR - ryear, geoid = sprintf( "%05d", as.integer( geoid ) ) ) ]
e <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ) ) )[ year == YR & subsec_group == "c3" ]
e[ , `:=`( ein = as.integer( ein ), size = pmax( rev, exp, na.rm = TRUE ) ) ]
e <- e[ order( -size ) ][ !duplicated( ein ), .( ein, exp, size, ntee3_e = ntee3, ind ) ]
b <- merge( b, e, by.x = "EIN", by.y = "ein", all.x = TRUE )
b[ , filer := !is.na( size ) & size >= floor22 ]
b[ , hosp_univ := ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) | ntee3_e %in% c( NTEE_HOSP, NTEE_UNIV ) | ind %in% c( "HOS", "UNI" ) ]
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa", "state_fips" ) ) )
b <- merge( b, sp[ , .( geoid, cbsa, cbsa_type, state_name, state_fips ) ], by = "geoid", all.x = TRUE )
b[ is.na( state_fips ), state_fips := substr( geoid, 1, 2 ) ]
b[ , region := unname( REGION[ state_fips ] ) ]
b <- b[ !is.na( region ) ]
log_msg( "registered public charities ", YR, ": ", format( nrow( b ), big.mark = "," ), "; filers above floor: ", format( sum( b$filer ), big.mark = "," ) )

## ---- national and regional summaries ----------------------------------------------------------
wmean <- function( a, w ) sum( a * w, na.rm = TRUE ) / sum( w[ is.finite( a ) ], na.rm = TRUE )
summ <- function( z ) z[ , .( orgs = .N, filers = as.numeric( sum( filer ) ),
  median_age_all = as.numeric( median( age ) ), mean_age_all = mean( age ), pre1970_share = mean( ryear < 1970 ),
  mean_age_filers = mean( age[ filer ] ), mean_age_grassroots = mean( age[ !filer ] ),
  dollar_weighted_age = wmean( age[ filer & exp > 0 ], exp[ filer & exp > 0 ] ),
  dollar_weighted_age_nohu = wmean( age[ filer & exp > 0 & !hosp_univ ], exp[ filer & exp > 0 & !hosp_univ ] ) ) ]
nat <- summ( b ); print( r3( nat ) )
reg <- b[ , summ( .SD ), keyby = region ]; print( r3( reg ) ); fwrite( reg, file.path( CU, "c12_age_by_region.csv" ) )
st <- b[ , summ( .SD ), by = .( region, state_fips ) ]
st <- merge( st, unique( sp[ , .( state_fips, state_name ) ] ), by = "state_fips", all.x = TRUE )
fwrite( st, file.path( CU, "c12_age_by_state.csv" ) )

## ---- metro level, with BLS market share -------------------------------------------------------
mt <- b[ cbsa_type == "metro", summ( .SD ), by = .( cbsa, region ) ]
d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )[ cbsa_type == "metro" ]
sh <- d[ year %in% 2013:2022 & !( bls_delin_flag %in% TRUE ) & is.finite( bls_pct_npo ),
         .( share = mean( bls_pct_npo ), np_jobs_pc = 1000 * mean( bls_npo_emp / population ) ), by = cbsa ]
nm <- d[ year == YR, .( cbsa, population, name = paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ) ) ]
mt <- merge( merge( mt, sh, by = "cbsa" ), nm, by = "cbsa" )
fwrite( mt, file.path( CU, "c12_age_by_metro.csv" ) )
co <- rbindlist( lapply( c( "median_age_all", "mean_age_filers", "dollar_weighted_age", "dollar_weighted_age_nohu", "pre1970_share" ), function( v )
  data.table( measure = v, cor_log_share = cor( mt[[ v ]], log( mt$share ), use = "complete.obs" ),
              spearman = cor( mt[[ v ]], mt$share, method = "spearman", use = "complete.obs" ),
              cor_within_region = { z <- mt[ is.finite( get( v ) ) ]; cor( resid( lm( z[[ v ]] ~ z$region ) ), resid( lm( log( z$share ) ~ z$region ) ) ) } ) ) )
print( r3( co ) ); fwrite( co, file.path( CU, "c12_age_vs_share.csv" ) )
fit <- lm( log( share ) ~ dollar_weighted_age + region, mt ); print( summary( fit )$coefficients )

## ---- age vs size -------------------------------------------------------------------------------
f <- b[ filer == TRUE & exp > 0 ]
f[ , band := cut( age, c( -1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 200 ),
                  labels = c( "0-2", "3-5", "6-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81+" ) ) ]
sz <- f[ , .( orgs = .N, median_exp = median( exp ), p25 = quantile( exp, .25 ), p75 = quantile( exp, .75 ), mean_exp = mean( exp ),
              share_hosp_univ = mean( hosp_univ ) ), keyby = band ]
print( sz ); fwrite( sz, file.path( CU, "c12_age_vs_size.csv" ) )
cat( "Spearman age vs expenditures (filers):", round( cor( f$age, f$exp, method = "spearman" ), 3 ),
     "; elasticity log exp on log(age+1):", round( coef( lm( log( exp ) ~ log( age + 1 ), f ) )[ 2 ], 3 ),
     "; excluding hospitals/universities:", round( coef( lm( log( exp ) ~ log( age + 1 ), f[ hosp_univ == FALSE ] ) )[ 2 ], 3 ), "\n" )
sr <- f[ , .( median_exp = median( exp ) ), keyby = .( region, band ) ]; fwrite( sr, file.path( CU, "c12_age_vs_size_by_region.csv" ) )

## ---- figures -----------------------------------------------------------------------------------------
## A: states, mean age of filers and of all registered organizations
st[ , lab := factor( state_name, st[ order( mean_age_filers ), state_name ] ) ]
rg_txt <- reg[ , sprintf( "%s: filers %.0f yrs, all %.0f yrs", region, mean_age_filers, mean_age_all ) ]
g1 <- ggplot( st, aes( y = lab ) ) +
  geom_segment( aes( x = mean_age_all, xend = mean_age_filers, yend = lab ), colour = "grey80", linewidth = 0.6 ) +
  geom_point( aes( x = mean_age_all ), shape = 21, fill = "white", colour = "grey45", size = 1.9 ) +
  geom_point( aes( x = mean_age_filers, colour = region ), size = 2.3 ) +
  scale_colour_manual( values = RCOL, name = NULL ) +
  labs( title = "Average age of 501(c)(3) public charities by state, 2022",
        subtitle = paste0( "Filled dot: filers (990/990-EZ, >= $50k in 2010 $); open dot: all registered (incl. grassroots). Age = 2022 - IRS ruling year.\n",
                           paste( rg_txt, collapse = "   " ) ),
        x = "Mean age (years)", y = NULL ) + theme_c + theme( axis.text.y = element_text( size = 6.5 ) )
ggsave( file.path( CU, "fig_c12_age_by_state.png" ), g1, width = 9, height = 9.5, dpi = 160, bg = "white" )
## B: age vs market share (metros)
pm <- melt( mt[ , .( cbsa, region, name, share, population, `Mean age of filers` = mean_age_filers,
                     `Dollar-weighted age of filers` = dollar_weighted_age ) ],
            id.vars = c( "cbsa", "region", "name", "share", "population" ), variable.name = "measure", value.name = "age" )
cl <- co[ measure %in% c( "mean_age_filers", "dollar_weighted_age" ) ][ , measure := c( mean_age_filers = "Mean age of filers", dollar_weighted_age = "Dollar-weighted age of filers" )[ measure ] ]
cl[ , txt := sprintf( "r = %.2f with log share\n(within regions: %.2f)", cor_log_share, cor_within_region ) ]
lab <- pm[ name %in% c( "Boston, MA", "Pittsburgh, PA", "Rochester, MN", "Las Vegas, NV", "Phoenix, AZ", "Houston, TX", "Austin, TX",
                        "Cape Coral, FL", "Burlington, VT", "Johnstown, PA", "Provo, UT", "McAllen, TX", "Asheville, NC", "Ithaca, NY" ) ]
g2 <- ggplot( pm, aes( age, 100 * share ) ) +
  geom_point( aes( colour = region, size = population ), alpha = 0.7 ) +
  geom_smooth( method = "loess", formula = y ~ x, se = FALSE, colour = "grey20", linewidth = 0.7 ) +
  ggrepel::geom_text_repel( data = lab, aes( label = name ), size = 2.2, colour = "grey25", seed = 2, max.overlaps = 30 ) +
  geom_text( data = cl, aes( x = -Inf, y = Inf, label = txt ), hjust = -0.05, vjust = 1.2, size = 2.8, colour = "grey25", inherit.aes = FALSE ) +
  facet_wrap( ~ factor( measure, c( "Mean age of filers", "Dollar-weighted age of filers" ) ), scales = "free_x" ) +
  scale_y_log10( breaks = c( 2, 5, 10, 20, 40 ) ) + scale_size_area( max_size = 6, guide = "none" ) +
  scale_colour_manual( values = RCOL, name = NULL ) +
  labs( title = "Nonprofit age and nonprofit market share, metros",
        subtitle = "Age in 2022 (2022 - IRS ruling year). Dollar-weighted: each filer weighted by its expenditures (the age of the average nonprofit dollar).",
        x = "Age (years)", y = "Nonprofit share of private employment, 2013-2022 (%, log scale)" ) + theme_c
ggsave( file.path( CU, "fig_c12_age_vs_share.png" ), g2, width = 10.5, height = 5.2, dpi = 160, bg = "white" )
## B2: the same (mean age of filers) by region: region's metros in colour over all metros in grey,
## large coloured headline with the region's median filer age, within-region fit and correlation
RG <- c( "Northeast", "Midwest", "South", "West" )
mr <- copy( mt )[ is.finite( mean_age_filers ) ][ , region := factor( region, RG ) ]
bgm <- copy( mr )[ , region := NULL ]
rr <- mr[ , { f1 <- lm( log( share ) ~ mean_age_filers )
  .( metros = .N, median_age = median( mean_age_filers ), median_share = 100 * median( share ),
     r = cor( mean_age_filers, log( share ) ), pct_per_year = 100 * ( exp( coef( f1 )[[ 2 ]] ) - 1 ), p = summary( f1 )$coefficients[ 2, 4 ] ) }, keyby = region ]
## headline: mean age of all filers in the region's metros (organization-weighted)
rr <- merge( rr, b[ cbsa_type == "metro" & filer == TRUE, .( mean_age = mean( age ), filers = .N ), by = region ][ , region := factor( region, RG ) ], by = "region" )
print( r3( rr ) ); fwrite( rr, file.path( CU, "c12_age_vs_share_by_region.csv" ) )
rr[ , `:=`( big = sprintf( "%s  %.1f yrs", region, mean_age ),
            txt = sprintf( "mean age of filers in the region's metros\n%d metros; median share %.1f%%; r = %.2f; %+.1f%% share per year of age (%s)",
                           metros, median_share, r, pct_per_year, fifelse( p < .001, "p < .001", sprintf( "p = %.3f", p ) ) ) ) ]
lab2 <- mr[ order( -abs( resid( lm( log( share ) ~ mean_age_filers + region, mr ) ) ) ), head( .SD, 3 ), by = region ]
g4 <- ggplot( mr, aes( mean_age_filers, 100 * share ) ) +
  geom_point( data = bgm, colour = "grey88", size = 1.1 ) +
  geom_smooth( aes( colour = region ), method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.8, fill = "grey85" ) +
  geom_point( aes( colour = region, size = population ), alpha = 0.8 ) +
  ggrepel::geom_text_repel( data = lab2, aes( label = name ), size = 2.3, colour = "grey20", seed = 3, max.overlaps = 30,
                            min.segment.length = 0, segment.colour = "grey60", segment.size = 0.2 ) +
  geom_text( data = rr, aes( x = 12.5, y = 70, label = big, colour = region ), hjust = 0, vjust = 1, size = 9, fontface = "bold", inherit.aes = FALSE ) +
  geom_text( data = rr, aes( x = 12.5, y = 42, label = txt ), hjust = 0, vjust = 1, size = 2.5, colour = "grey35", inherit.aes = FALSE ) +
  facet_wrap( ~ region, ncol = 2 ) +
  scale_y_log10( breaks = c( 2, 5, 10, 20, 40 ) ) + scale_size_area( max_size = 6, guide = "none" ) +
  scale_colour_manual( values = RCOL, guide = "none" ) +
  labs( title = "Nonprofit age and nonprofit market share, metros, by region",
        subtitle = "Age = mean age of the metro's filers in 2022 (2022 - IRS ruling year). Grey: all other metros. Line: linear fit of log share on age within the region.",
        x = "Mean age of filers (years)", y = "Nonprofit share of private employment, 2013-2022 (%, log scale)" ) +
  theme_c + theme( strip.text = element_blank() )
ggsave( file.path( CU, "fig_c12_age_vs_share_by_region.png" ), g4, width = 10, height = 8.4, dpi = 160, bg = "white" )
## C: age vs size (filers) -- the simplified dumbbell used in the draft paper (58_outline_analyses.R, part O):
## median expenditures by age band, all filers vs excluding hospitals and universities
BANDS <- levels( f$band )
el_all <- coef( lm( log( exp ) ~ log( age + 1 ), f ) )[[ 2 ]]; el_nohu <- coef( lm( log( exp ) ~ log( age + 1 ), f[ hosp_univ == FALSE ] ) )[[ 2 ]]
s2 <- rbind( f[ , .( group = "All filers", orgs = .N, med = median( exp ), hu = mean( hosp_univ ) ), keyby = band ],
             f[ hosp_univ == FALSE, .( group = "Excluding hospitals and universities", orgs = .N, med = median( exp ), hu = 0 ), keyby = band ] )
fwrite( s2, file.path( CU, "c12_age_vs_size_dumbbell.csv" ) )
lab <- s2[ group == "All filers", .( band, yl = sprintf( "%s years\n%s filers · %s H&U", band, ifelse( orgs >= 1000, paste0( round( orgs / 1000 ), "k" ), orgs ),
                                                       ifelse( hu < 0.1, sprintf( "%.1f%%", 100 * hu ), sprintf( "%.0f%%", 100 * hu ) ) ) ) ]
money <- scales::label_dollar( accuracy = 1, scale_cut = scales::cut_short_scale() )
fm <- function( v ) ifelse( v >= 1e6, sprintf( "$%.1fM", v / 1e6 ), sprintf( "$%.0fK", v / 1e3 ) )
s2[ , band := factor( band, rev( BANDS ) ) ]
wd <- dcast( s2, band ~ group, value.var = "med" ); setnames( wd, c( "band", "all", "nohu" ) )
g3 <- ggplot( s2, aes( med, band ) ) +
  geom_segment( data = wd, aes( x = nohu, xend = all, y = band, yend = band ), colour = "grey70", linewidth = 2.4, lineend = "round" ) +
  geom_point( aes( colour = group ), size = 3.4 ) +
  geom_text( data = wd, aes( x = pmax( all, nohu ), y = band, label = fm( all ) ), hjust = -0.35, size = 3.1, colour = "#1f5fae" ) +
  geom_text( data = wd[ all / nohu > 1.15 ], aes( x = pmin( all, nohu ), y = band, label = fm( nohu ) ), hjust = 1.35, size = 3.1, colour = "#5f646c" ) +
  scale_x_log10( labels = money, breaks = c( 1e5, 3e5, 1e6, 3e6, 1e7 ), expand = expansion( mult = c( 0.08, 0.12 ) ) ) +
  scale_y_discrete( labels = setNames( lab$yl, lab$band ) ) +
  scale_colour_manual( values = c( "All filers" = "#2a78d6", "Excluding hospitals and universities" = "#8a8f98" ), name = NULL ) +
  labs( title = "Older nonprofits are larger",
        subtitle = sprintf( "Median expenditures by years since IRS ruling, with and without hospitals and universities; 501(c)(3) filers, %d", YR ),
        y = NULL, x = "Median expenditures (log scale)",
        caption = sprintf( "Age = %d minus IRS ruling year (recognition, not founding; resets on reinstatement).\nElasticity of expenditures with respect to age + 1: %s (%s excluding hospitals and universities).",
                           YR, sub( "^0", "", sprintf( "%.2f", el_all ) ), sub( "^0", "", sprintf( "%.2f", el_nohu ) ) ) ) +
  theme_minimal( base_size = 11 ) + theme( panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), plot.title.position = "plot",
                                           plot.subtitle = element_text( colour = "grey30" ), legend.position = "top", legend.justification = "left",
                                           axis.text.y = element_text( size = 8.5, lineheight = 0.95, hjust = 1 ),
                                           plot.caption = element_text( hjust = 0, size = 8.5, colour = "grey30" ), plot.caption.position = "plot" )
ggsave( file.path( CU, "fig_c12_age_vs_size.png" ), g3, width = 9, height = 7.2, dpi = 170, bg = "white" )
log_msg( "done" )
