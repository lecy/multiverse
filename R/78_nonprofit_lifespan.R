## 78_nonprofit_lifespan.R
## Lifespan of 501(c)(3) public charities ruled 1990-2021, filers vs non-filers, by region.
##   lifespan = last year observed - IRS ruling year
##     filers (ever filed a 990 / 990-EZ, any size, 2000-2024): last FILING year
##     non-filers (registered, never filed): last year in the BMF (2000-2022)
## Many organizations are still alive, so the plain average of completed lifespans is
## biased short. Main estimates are Kaplan-Meier survival curves with
##   - right censoring: still filing in 2022+ (filers) / still registered in 2021-22 (non-filers)
##   - left truncation (delayed entry): data start in 2000, so organizations ruled in the
##     1990s enter the risk set at their age in 2000 (those that died before 2000 are unseen)
## and the restricted mean lifespan to 25 years (RMST: expected years lived within the
## first 25) with 95% CIs. Also reported: median lifespan, survival at 5/10/20 years, and
## the mean lifespan among organizations that ended (with a t-based 95% CI).
## Ruling year = the earliest ruling year recorded for the EIN (reinstatements reset it),
## or the first year the EIN is seen if that is earlier.
## Writes D:/MULTIVERSE/data/catchup/c13_*, fig_c13_*.
##   Rscript R/78_nonprofit_lifespan.R

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ); library( survival ) } )
CU <- file.path( BUILD, "data", "catchup" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
options( width = 220 )
theme_c <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"; C4 <- "#eda100"
RG <- c( "Northeast", "Midwest", "South", "West" ); RCOL <- c( Northeast = C1, Midwest = C3, South = C2, West = C4 )
REGION <- c( "09" = "Northeast", "23" = "Northeast", "25" = "Northeast", "33" = "Northeast", "44" = "Northeast",
  "50" = "Northeast", "34" = "Northeast", "36" = "Northeast", "42" = "Northeast",
  "17" = "Midwest", "18" = "Midwest", "26" = "Midwest", "39" = "Midwest", "55" = "Midwest", "19" = "Midwest",
  "20" = "Midwest", "27" = "Midwest", "29" = "Midwest", "31" = "Midwest", "38" = "Midwest", "46" = "Midwest",
  "10" = "South", "11" = "South", "12" = "South", "13" = "South", "24" = "South", "37" = "South", "45" = "South",
  "51" = "South", "54" = "South", "01" = "South", "21" = "South", "28" = "South", "47" = "South", "05" = "South",
  "22" = "South", "40" = "South", "48" = "South",
  "04" = "West", "08" = "West", "16" = "West", "30" = "West", "32" = "West", "35" = "West", "49" = "West",
  "56" = "West", "02" = "West", "06" = "West", "15" = "West", "41" = "West", "53" = "West" )
TAU <- 25                                  # restriction for the mean (years)
FILE_END <- 2021L                          # filers: last filing <= 2021 = ended; 2022+ = censored (data to 2024)
BMF_END <- 2020L                           # non-filers: last BMF year <= 2020 = ended; 2021-22 = censored

## ---- registered public charities: ruling year, last BMF year, location -------------------------
b <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "subsec", "pf", "ryear" ) )
b <- b[ subsec == 3 & pf == FALSE ]
setorder( b, EIN, year )
reg <- b[ , .( ryear = { r <- ryear[ is.finite( ryear ) & ryear >= 1800 ]; if ( length( r ) ) min( r ) else NA_integer_ },
               first_bmf = min( year ), last_bmf = max( year ), geoid = geoid[ .N ] ), by = EIN ]
rm( b ); invisible( gc() )
log_msg( "registered public charities (EINs, BMF 2000-2022): ", format( nrow( reg ), big.mark = "," ) )

## ---- filings: first and last 990/990-EZ year per EIN (public charities) ------------------------
rd <- function( f ) as.data.table( open_dataset( file.path( OUT$npo, f ) ) |> dplyr::filter( subsec_group == "c3" ) |>
                                     dplyr::select( ein, year ) |> dplyr::collect() )
fl <- rbindlist( list( rd( "orgyear_core.parquet" ), rd( "orgyear_soi.parquet" ), rd( "orgyear_efile.parquet" ) ) )
fl <- fl[ , .( first_file = min( year ), last_file = max( year ), years_filed = uniqueN( year ) ), by = .( EIN = as.integer( ein ) ) ]
log_msg( "EINs with any c3 filing 2000-2024: ", format( nrow( fl ), big.mark = "," ) )

x <- merge( reg, fl, by = "EIN", all.x = TRUE )
x[ , filer := !is.na( last_file ) ]
x[ , ryear_eff := pmin( ryear, first_bmf, first_file, na.rm = TRUE ) ]          # reinstatement resets
x[ , region := unname( REGION[ substr( sprintf( "%05d", as.integer( geoid ) ), 1, 2 ) ] ) ]
x <- x[ !is.na( region ) & is.finite( ryear_eff ) & ryear_eff >= 1990 & ryear_eff <= 2021 ]
x[ , `:=`( last_obs = fifelse( filer, last_file, last_bmf ), ended = fifelse( filer, last_file <= FILE_END, last_bmf <= BMF_END ) ) ]
## time scale = age; +0.5 so an organization last seen in its ruling year has a positive lifespan
x[ , `:=`( entry = pmax( 0, 2000 - ryear_eff ), exit = last_obs - ryear_eff + 0.5 ) ]
x <- x[ exit > entry ]
x[ , `:=`( group = factor( fifelse( filer, "Filers", "Non-filers" ), c( "Filers", "Non-filers" ) ), region = factor( region, RG ) ) ]
log_msg( "organizations ruled 1990-2021: ", format( nrow( x ), big.mark = "," ), "; filers ", format( sum( x$filer ), big.mark = "," ),
         "; ended: filers ", round( 100 * mean( x[ filer == TRUE, ended ] ) ), "%, non-filers ", round( 100 * mean( x[ filer == FALSE, ended ] ) ), "%" )
fwrite( x[ , .N, keyby = .( group, region, ended ) ], file.path( CU, "c13_counts.csv" ) )

## ---- Kaplan-Meier with delayed entry, by group x region (and all regions) ---------------------
km_stats <- function( d, label_region ) {
  f <- survfit( Surv( entry, exit, ended ) ~ 1, data = d )
  tb <- summary( f, rmean = TAU )$table
  sv <- function( t ) { s <- summary( f, times = t, extend = TRUE ); c( s$surv, s$lower, s$upper ) }
  s5 <- sv( 5 ); s10 <- sv( 10 ); s20 <- sv( 20 )
  med_ci <- quantile( f, probs = .5, conf.int = TRUE )
  data.table( region = label_region, orgs = nrow( d ), ended = sum( d$ended ),
              rmst = tb[[ "rmean" ]], rmst_lo = tb[[ "rmean" ]] - 1.96 * tb[[ "se(rmean)" ]], rmst_hi = tb[[ "rmean" ]] + 1.96 * tb[[ "se(rmean)" ]],
              median = as.numeric( med_ci$quantile ), median_lo = as.numeric( med_ci$lower ), median_hi = as.numeric( med_ci$upper ),
              surv5 = s5[ 1 ], surv10 = s10[ 1 ], surv10_lo = s10[ 2 ], surv10_hi = s10[ 3 ], surv20 = s20[ 1 ], surv20_lo = s20[ 2 ], surv20_hi = s20[ 3 ],
              ## completed lifespans only (biased short; for reference)
              mean_completed = mean( d[ ended == TRUE, exit - 0.5 ] ),
              mean_completed_lo = mean( d[ ended == TRUE, exit - 0.5 ] ) - qt( .975, sum( d$ended ) - 1 ) * sd( d[ ended == TRUE, exit - 0.5 ] ) / sqrt( sum( d$ended ) ),
              mean_completed_hi = mean( d[ ended == TRUE, exit - 0.5 ] ) + qt( .975, sum( d$ended ) - 1 ) * sd( d[ ended == TRUE, exit - 0.5 ] ) / sqrt( sum( d$ended ) ) ) }
res <- rbindlist( lapply( levels( x$group ), function( g ) rbind(
  rbindlist( lapply( RG, function( r ) km_stats( x[ group == g & region == r ], r ) ) ),
  km_stats( x[ group == g ], "All regions" ) )[ , group := g ] ) )
setcolorder( res, c( "group", "region" ) )
print( r3( res ), nrows = 20 ); fwrite( res, file.path( CU, "c13_lifespan_by_region.csv" ) )
## differences between regions (log-rank, with delayed entry: Cox score test)
lr <- rbindlist( lapply( levels( x$group ), function( g ) { cx <- coxph( Surv( entry, exit, ended ) ~ region, data = x[ group == g ] )
  ct <- summary( cx )$coefficients
  data.table( group = g, term = rownames( ct ), hazard_ratio_vs_northeast = exp( ct[ , 1 ] ), lo = exp( ct[ , 1 ] - 1.96 * ct[ , 3 ] ),
              hi = exp( ct[ , 1 ] + 1.96 * ct[ , 3 ] ), p = ct[ , 5 ], score_test_p = summary( cx )$sctest[[ "pvalue" ]] ) } ) )
print( r3( lr ) ); fwrite( lr, file.path( CU, "c13_hazard_ratios.csv" ) )

## ---- curves for the figure -------------------------------------------------------------------------
cv <- rbindlist( lapply( levels( x$group ), function( g ) rbindlist( lapply( RG, function( r ) {
  f <- survfit( Surv( entry, exit, ended ) ~ 1, data = x[ group == g & region == r ] )
  s <- summary( f, times = seq( 0, 32, 0.5 ), extend = TRUE )
  data.table( group = g, region = r, age = s$time, surv = s$surv, lo = s$lower, hi = s$upper ) } ) ) ) )
cv[ , region := factor( region, RG ) ]
fwrite( cv, file.path( CU, "c13_survival_curves.csv" ) )
## one panel per region, filers vs grassroots (registered, never filed) on the same axes;
## the region name and the two medians in the panel headline
GCOL <- c( Filers = C1, `Non-filers` = C3 ); GLAB <- c( Filers = "Filers (last 990/990-EZ)", `Non-filers` = "Grassroots (registered, never filed; last BMF year)" )
hd <- res[ region != "All regions", .( region, group, median ) ]
hd <- dcast( hd, region ~ group, value.var = "median" )
hd[ , `:=`( region = factor( region, RG ), txt = sprintf( "median lifespan: filers ~%.0f yrs, grassroots ~%.0f yrs", Filers - 0.5, `Non-filers` - 0.5 ) ) ]
g1 <- ggplot( cv[ age <= 30 ], aes( age, surv, colour = group, fill = group ) ) +
  geom_hline( yintercept = 0.5, colour = "grey60", linetype = 2 ) +
  geom_ribbon( aes( ymin = lo, ymax = hi ), alpha = 0.2, colour = NA ) + geom_step( linewidth = 0.85 ) +
  geom_text( data = hd, aes( x = 30, y = 0.98, label = region ), hjust = 1, vjust = 1, size = 6, fontface = "bold", colour = RCOL[ as.character( hd$region ) ], inherit.aes = FALSE ) +
  geom_text( data = hd, aes( x = 30, y = 0.86, label = txt ), hjust = 1, vjust = 1, size = 2.7, colour = "grey30", inherit.aes = FALSE ) +
  facet_wrap( ~ region, ncol = 2 ) + scale_y_continuous( labels = scales::percent, limits = c( 0, 1 ) ) +
  scale_colour_manual( values = GCOL, labels = GLAB, name = NULL ) + scale_fill_manual( values = GCOL, guide = "none" ) +
  labs( title = "How long do nonprofits last? Survival of 501(c)(3) public charities ruled 1990-2021, by region",
        subtitle = "Kaplan-Meier with delayed entry (data from 2000) and right censoring; 95% bands. Dashed line = 50% (median lifespan).",
        x = "Years since IRS ruling", y = "Still active" ) + theme_c + theme( strip.text = element_blank() )
ggsave( file.path( CU, "fig_c13_survival.png" ), g1, width = 10, height = 7.6, dpi = 160, bg = "white" )
pr <- res[ region != "All regions" ][ , region := factor( region, rev( RG ) ) ]
pr2 <- rbind( pr[ , .( group, region, est = rmst, lo = rmst_lo, hi = rmst_hi, measure = sprintf( "A. Restricted mean lifespan (years lived within the first %d)", TAU ) ) ],
              pr[ , .( group, region, est = median, lo = median_lo, hi = median_hi, measure = "B. Median lifespan (years)" ) ],
              pr[ , .( group, region, est = mean_completed, lo = mean_completed_lo, hi = mean_completed_hi, measure = "C. Mean lifespan of organizations that ended (biased short)" ) ] )
g2 <- ggplot( pr2, aes( est, region, colour = region, shape = group ) ) +
  geom_pointrange( aes( xmin = lo, xmax = hi ), position = position_dodge( width = 0.55 ), size = 0.35 ) +
  facet_wrap( ~ measure, scales = "free_x", ncol = 3 ) +
  scale_colour_manual( values = RCOL, guide = "none" ) + scale_shape_manual( values = c( Filers = 16, `Non-filers` = 1 ), name = NULL ) +
  labs( title = "Nonprofit lifespan by region, with 95% confidence intervals",
        subtitle = "501(c)(3) public charities ruled 1990-2021. Filers: ruling to last 990/990-EZ; non-filers: ruling to last BMF registration.",
        x = "Years", y = NULL ) + theme_c
ggsave( file.path( CU, "fig_c13_lifespan_ci.png" ), g2, width = 11, height = 4.2, dpi = 160, bg = "white" )
log_msg( "done" )
