## 09b_revexp_analysis.R
## Auxiliary analysis / research note: measuring nonprofit sector size with
## revenue vs expenses, and the pass-through grant adjustment.
##
## Part 1  Volatility. Year-over-year % change, (X_t - X_{t-1}) / X_{t-1}, for
##         revenue and expenses of the same organization:
##         (a) consecutive filings in the org panel (same source era: CORE
##             2000-2010, SOI 2011-2019, e-file 2020-2024)
##         (b) current vs prior year on the SAME return (e-file 990 Part I
##             CY/PY columns, 2020-2024) -- no matching or restatement issues
##         Within-org dispersion (SD / MAD of % change, orgs with >= 5 changes),
##         share of years with |change| > 20%, co-occurrence, correlation.
## Part 2  Pass-through grants (2010-2024). Share of orgs paying grants to
##         other organizations; sector totals of revenue and expenses before
##         and after subtracting grants paid to organizations (main) or all
##         grants (upper bound); public charities, plus foundations.
## Part 3  Scaling. beta of log(total) on log(population) across metros, by
##         year, for revenue / expenses, gross / net; correlation of
##         pass-through share with size (direction of the bias); SAMI persistence.
## Sample: active 501(c)(3) public charities (max(rev, exp) >= $50k in 2010
##         dollars), 990 + 990-EZ; size class by prior-year expenses.
## Outputs: D:/MULTIVERSE/data/aux_revexp/*.csv and fig_*.png

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ) } )
AUX <- file.path( BUILD, "data", "aux_revexp" ); dir.create( AUX, showWarnings = FALSE )
rd  <- function( f ) as.data.table( read_parquet( file.path( OUT$npo, f ) ) )
z0  <- function( x ) fifelse( is.na( x ), 0, x )
wins <- function( x, p = 0.01 ) { q <- quantile( x, c( p, 1 - p ), na.rm = TRUE ); pmin( pmax( x, q[1] ), q[2] ) }
ink <- "#0b0b0b"; ink2 <- "#52514e"; blue <- "#2a78d6"; orange <- "#eb6834"
theme_set( theme_minimal( base_size = 10 ) + theme( panel.grid.minor = element_blank(),
           panel.grid.major = element_line( color = "#e6e5e1", linewidth = 0.3 ),
           legend.position = "top", legend.title = element_blank(),
           plot.title = element_text( face = "bold", size = 10.5 ) ) )

pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )
core <- rd( "orgyear_core.parquet" )[ subsec_group == "c3" & year %in% 2000:2010 ]
soi  <- rd( "orgyear_soi.parquet"  )[ subsec_group == "c3" & year %in% 2011:2019 ]
ef   <- rd( "orgyear_efile.parquet" )[ subsec_group == "c3" & year %in% 2020:2024 ]
keep <- c( "source", "form", "ein", "year", "rev", "exp", "ntee3" )
for ( d in list( core, soi, ef ) ) for ( v in setdiff( keep, names( d ) ) ) d[ , ( v ) := NA ]
org <- rbindlist( lapply( list( core, soi, ef ), function( d ) d[ , ..keep ] ) )
rm( core, soi, ef ); gc()
org <- unique( org[ order( ein, year, -z0( exp ) ) ], by = c( "ein", "year" ) )
org <- merge( org, pce[ , .( year, to2022, floor_nom ) ], by = "year" )
org[ , active := pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
org[ , hosp_univ := toupper( ntee3 ) %in% c( NTEE_HOSP, NTEE_UNIV ) ]
org[ , ez := grepl( "EZ", toupper( form ) ) ]
gr <- rd( "orgyear_grants.parquet" )
org <- merge( org, gr[ , .( source, ein, year, grants_org, grants_frgn, grants_indiv, grants_all, gr_known ) ],
              by = c( "source", "ein", "year" ), all.x = TRUE )
log_msg( "org-years: ", nrow( org ), "; active: ", sum( org$active, na.rm = TRUE ) )

sizecls <- function( x ) cut( x, c( -Inf, 1e5, 1e6, 1e7, Inf ), right = FALSE,
                              labels = c( "<$100k", "$100k-1M", "$1M-10M", "$10M+" ) )

## ---- Part 1a: consecutive filings -------------------------------------------------
setorder( org, ein, year )
org[ , `:=`( rev_l = shift( rev ), exp_l = shift( exp ), yr_l = shift( year ), src_l = shift( source ),
             act_l = shift( active ) ), by = ein ]
ch <- org[ yr_l == year - 1L & src_l == source & active & act_l & rev_l > 0 & exp_l > 0 &
             !is.na( rev ) & !is.na( exp ),
           .( ein, year, source, hosp_univ, ez, size = sizecls( exp_l * to2022 ),
              d_rev = ( rev - rev_l ) / rev_l, d_exp = ( exp - exp_l ) / exp_l ) ]
log_msg( "year-over-year pairs: ", nrow( ch ) )

summ_change <- function( x ) x[ , {
  big_r <- abs( d_rev ) > 0.2; big_e <- abs( d_exp ) > 0.2
  .( pairs = .N,
     med_abs_rev = median( abs( d_rev ) ), med_abs_exp = median( abs( d_exp ) ),
     p10_rev = quantile( d_rev, .1 ), p90_rev = quantile( d_rev, .9 ),
     p10_exp = quantile( d_exp, .1 ), p90_exp = quantile( d_exp, .9 ),
     iqr_rev = IQR( d_rev ), iqr_exp = IQR( d_exp ),
     share_rev_gt20 = mean( big_r ), share_exp_gt20 = mean( big_e ),
     share_rev_up20 = mean( d_rev > 0.2 ), share_rev_dn20 = mean( d_rev < -0.2 ),
     p_exp20_given_rev20 = mean( big_e[ big_r ] ),
     p_exp20_same_sign_given_rev20 = mean( ( big_e & sign( d_exp ) == sign( d_rev ) )[ big_r ] ),
     p_rev20_given_exp20 = mean( big_r[ big_e ] ),
     cor_pearson_wins = cor( wins( d_rev ), wins( d_exp ) ),
     cor_spearman = cor( d_rev, d_exp, method = "spearman" ) ) } ]
p1_all  <- summ_change( ch )[ , group := "all" ]
p1_size <- ch[ , summ_change( .SD ), by = size ][ order( size ) ]
p1_hu   <- ch[ , summ_change( .SD ), by = hosp_univ ]
p1_form <- ch[ , summ_change( .SD ), by = .( form = fifelse( ez, "990-EZ", "990" ) ) ]
p1_year <- ch[ , summ_change( .SD ), keyby = year ]
fwrite( rbindlist( list( p1_all, p1_size[ , group := paste( "size", size ) ][ , size := NULL ],
                         p1_hu[ , group := paste( "hosp_univ", hosp_univ ) ][ , hosp_univ := NULL ],
                         p1_form[ , group := paste( "form", form ) ][ , form := NULL ] ), fill = TRUE ),
        file.path( AUX, "p1_yoy_change_summary.csv" ) )
fwrite( p1_year, file.path( AUX, "p1_yoy_change_by_year.csv" ) )

## within-org dispersion
disp <- ch[ , .( n = .N, sd_rev = sd( d_rev ), sd_exp = sd( d_exp ),
                 mad_rev = mad( d_rev ), mad_exp = mad( d_exp ),
                 share_rev_gt20 = mean( abs( d_rev ) > .2 ), share_exp_gt20 = mean( abs( d_exp ) > .2 ),
                 cor_org = if ( .N >= 5 ) suppressWarnings( cor( d_rev, d_exp, method = "spearman" ) ) else NA_real_,
                 size = size[ .N ] ), by = ein ][ n >= 5 ]
p1_disp <- rbind(
  disp[ , .( group = "all", orgs = .N, median_sd_rev = median( sd_rev ), median_sd_exp = median( sd_exp ),
             median_mad_rev = median( mad_rev ), median_mad_exp = median( mad_exp ),
             median_ratio_sd = median( sd_rev / sd_exp, na.rm = TRUE ),
             share_orgs_rev_more_volatile = mean( sd_rev > sd_exp ),
             median_within_org_spearman = median( cor_org, na.rm = TRUE ) ) ],
  disp[ , .( orgs = .N, median_sd_rev = median( sd_rev ), median_sd_exp = median( sd_exp ),
             median_mad_rev = median( mad_rev ), median_mad_exp = median( mad_exp ),
             median_ratio_sd = median( sd_rev / sd_exp, na.rm = TRUE ),
             share_orgs_rev_more_volatile = mean( sd_rev > sd_exp ),
             median_within_org_spearman = median( cor_org, na.rm = TRUE ) ), keyby = .( group = paste( "size", size ) ) ] )
fwrite( p1_disp, file.path( AUX, "p1_within_org_dispersion.csv" ) )

## ---- Part 1b: CY vs PY on the same return (e-file 990) ----------------------------
cp <- gr[ source == "EFILE" & !grepl( "EZ", form ) & rev_py > 0 & exp_py > 0 & !is.na( rev_cy ) & !is.na( exp_cy ) ]
cp <- merge( cp, org[ source == "EFILE", .( ein, year, active ) ], by = c( "ein", "year" ) )[ active == TRUE ]
cp[ , `:=`( d_rev = ( rev_cy - rev_py ) / rev_py, d_exp = ( exp_cy - exp_py ) / exp_py, size = sizecls( exp_py ) ) ]
p1b <- rbind( summ_change( cp )[ , group := "all" ],
              cp[ , summ_change( .SD ), keyby = size ][ , group := paste( "size", size ) ][ , size := NULL ],
              cp[ , summ_change( .SD ), keyby = year ][ , group := paste( "year", year ) ][ , year := NULL ], fill = TRUE )
fwrite( p1b, file.path( AUX, "p1b_same_return_cy_py.csv" ) )

## figure: distribution of % change
fd <- melt( ch[ sample( .N, min( .N, 400000 ) ), .( Revenue = d_rev, Expenses = d_exp ) ],
            measure.vars = c( "Revenue", "Expenses" ) )
g <- ggplot( fd[ abs( value ) <= 1 ], aes( value, color = variable ) ) +
  geom_density( adjust = 1.2, linewidth = 0.7 ) +
  geom_vline( xintercept = c( -.2, .2 ), linetype = "dashed", color = ink2, linewidth = 0.3 ) +
  scale_color_manual( values = c( Revenue = orange, Expenses = blue ) ) +
  scale_x_continuous( labels = scales::percent ) +
  labs( x = "Change from prior year (truncated at +/-100%)", y = "Density",
        title = "Year-over-year change in revenue vs expenses, same organization" )
ggsave( file.path( AUX, "fig_yoy_change_density.png" ), g, width = 7, height = 4, dpi = 150 )
g <- ggplot( melt( p1_year[ , .( year, Revenue = share_rev_gt20, Expenses = share_exp_gt20 ) ], id.vars = "year" ),
             aes( year, value, color = variable ) ) + geom_line( linewidth = 0.8 ) + geom_point( size = 1.2 ) +
  scale_color_manual( values = c( Revenue = orange, Expenses = blue ) ) +
  scale_y_continuous( labels = scales::percent, limits = c( 0, NA ) ) +
  labs( x = NULL, y = "Share of organizations", title = "Share with a change of more than 20% from the prior year" )
ggsave( file.path( AUX, "fig_share_gt20_by_year.png" ), g, width = 7, height = 3.6, dpi = 150 )

## ---- Part 2: pass-through ------------------------------------------------------------
pt <- org[ active == TRUE & year >= 2012 ]   # grant fields incomplete in CORE 2010 / SOI 2011
pt[ , g_org := z0( grants_org ) ][ , g_all := z0( grants_all ) ]
pt[ , known := z0( gr_known ) == 1L ]
p2 <- pt[ , .( orgs = .N, orgs_grants_known = sum( known ),
               share_any_org_grants = mean( g_org[ known ] > 0 ),
               share_any_grants = mean( g_all[ known ] > 0 ),
               share_any_org_grants_990 = mean( g_org[ known & !ez ] > 0 ),
               rev_bn = sum( pmax( rev, 0 ), na.rm = TRUE ) / 1e9,
               exp_bn = sum( pmax( exp, 0 ), na.rm = TRUE ) / 1e9,
               grants_org_bn = sum( g_org ) / 1e9, grants_all_bn = sum( g_all ) / 1e9,
               top1pct_share_of_org_grants = { x <- sort( g_org, decreasing = TRUE ); sum( head( x, ceiling( .01 * .N ) ) ) / sum( x ) } ),
          keyby = .( year, source ) ]
p2[ , `:=`( pct_reduction_rev = grants_org_bn / rev_bn, pct_reduction_exp = grants_org_bn / exp_bn,
            pct_reduction_rev_allgrants = grants_all_bn / rev_bn, pct_reduction_exp_allgrants = grants_all_bn / exp_bn ) ]
fwrite( p2, file.path( AUX, "p2_pass_through_by_year.csv" ) )
## by subsector (NTEE major group)
pt[ , major := substr( toupper( ntee3 ), 1, 1 ) ]
p2s <- pt[ year >= 2015 & known, .( orgs = .N, share_any_org_grants = mean( g_org > 0 ),
                                    exp_bn = sum( pmax( exp, 0 ), na.rm = TRUE ) / 1e9, grants_org_bn = sum( g_org ) / 1e9 ),
           keyby = major ][ , pct_of_exp := grants_org_bn / exp_bn ][ order( -grants_org_bn ) ]
fwrite( p2s, file.path( AUX, "p2_pass_through_by_ntee_major.csv" ) )
p2z <- pt[ year >= 2015 & known, .( orgs = .N, share_any_org_grants = mean( g_org > 0 ),
                                    pct_of_exp = sum( g_org ) / sum( pmax( exp, 0 ), na.rm = TRUE ) ),
           keyby = .( size = sizecls( exp * to2022 ) ) ]
fwrite( p2z, file.path( AUX, "p2_pass_through_by_size.csv" ) )

## with foundations: foundation grants are pass-through too
pfo <- rbindlist( list(
  rd( "orgyear_core.parquet" )[ subsec_group == "pf" & year %in% 2010:2015, .( year, ein, rev, exp, grants_paid ) ],
  rd( "orgyear_soi_pf.parquet" )[ year %in% 2019:2020, .( year, ein, rev, exp, grants_paid ) ],
  rd( "orgyear_efile_pf.parquet" )[ year %in% 2021:2024, .( year, ein, rev, exp, grants_paid ) ] ), fill = TRUE )
pfo <- unique( pfo, by = c( "ein", "year" ) )[ , .( pf_rev_bn = sum( pmax( rev, 0 ), na.rm = TRUE ) / 1e9,
                                                     pf_exp_bn = sum( pmax( exp, 0 ), na.rm = TRUE ) / 1e9,
                                                     pf_grants_bn = sum( pmax( grants_paid, 0 ), na.rm = TRUE ) / 1e9 ), keyby = year ]
p2f <- merge( p2[ , .( rev_bn = sum( rev_bn ), exp_bn = sum( exp_bn ), grants_org_bn = sum( grants_org_bn ) ), keyby = year ],
              pfo, by = "year" )
p2f[ , `:=`( combined_rev_bn = rev_bn + pf_rev_bn, combined_exp_bn = exp_bn + pf_exp_bn,
             pct_reduction_rev_combined = ( grants_org_bn + pf_grants_bn ) / ( rev_bn + pf_rev_bn ),
             pct_reduction_exp_combined = ( grants_org_bn + pf_grants_bn ) / ( exp_bn + pf_exp_bn ) ) ]
fwrite( p2f, file.path( AUX, "p2_pass_through_with_foundations.csv" ) )

## ---- Part 3: scaling, gross vs net -------------------------------------------------
pan <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
pan <- pan[ cbsa_type == "metro" & population > 0 ]
MEAS <- intersect( c( "rev_c3", "rev_c3_net", "exp_c3", "exp_c3_net",
                      "rev_c3_nohu", "rev_c3_nohu_net", "exp_c3_nohu", "exp_c3_nohu_net" ), names( pan ) )
beta <- rbindlist( lapply( MEAS, function( v ) pan[ year >= 2012, {
  ok <- is.finite( get( v ) ) & get( v ) > 0
  if ( sum( ok ) < 30 ) NULL else {
    m <- summary( lm( log( get( v )[ ok ] ) ~ log( population[ ok ] ) ) )
    .( measure = v, beta = m$coefficients[ 2, 1 ], se = m$coefficients[ 2, 2 ], r2 = m$r.squared, metros = sum( ok ) ) } },
  by = year ] ) )
beta[ , `:=`( resource = sub( "_.*", "", measure ), net = grepl( "_net$", measure ),
              universe = fifelse( grepl( "nohu", measure ), "excl. hospitals & universities", "all 501(c)(3)" ) ) ]
fwrite( beta, file.path( AUX, "p3_scaling_gross_vs_net.csv" ) )
bw <- dcast( beta, year + resource + universe ~ net, value.var = c( "beta", "se" ) )
setnames( bw, c( "beta_FALSE", "beta_TRUE", "se_FALSE", "se_TRUE" ), c( "beta_gross", "beta_net", "se_gross", "se_net" ) )
bw[ , bias := beta_gross - beta_net ]
fwrite( bw, file.path( AUX, "p3_scaling_bias.csv" ) )
## pass-through share vs size (the bias mechanism)
mech <- pan[ year >= 2012 & exp_c3 > 0 & !is.na( exp_c3_net ), .( cbsa, year, logP = log( population ),
                                                               pt_share = 1 - exp_c3_net / exp_c3 ) ]
mech_fit <- mech[ , { m <- summary( lm( pt_share ~ logP ) ); .( slope = m$coefficients[ 2, 1 ], se = m$coefficients[ 2, 2 ],
                                                                 r2 = m$r.squared, mean_share = mean( pt_share ) ) }, keyby = year ]
fwrite( mech_fit, file.path( AUX, "p3_pass_through_share_vs_logP.csv" ) )
## persistence and year-to-year volatility of metro SAMIs (rev vs exp)
sami_v <- rbindlist( lapply( MEAS, function( v ) {
  x <- pan[ year >= 2012 & get( v ) > 0, .( cbsa, year, y = get( v ), P = population ) ]
  x[ , s := resid( lm( log( y ) ~ log( P ) ) ), by = year ]
  setorder( x, cbsa, year ); x[ , `:=`( s_l = shift( s ), yr_l = shift( year ) ), by = cbsa ]
  x <- x[ !is.na( s_l ) & yr_l == year - 1L ]
  ## mean of the year-specific cross-metro correlations, and SD of the change
  x[ , .( r = cor( s, s_l ), sdc = sd( s - s_l ) ), by = year ][ , .( measure = v, r_lag1 = mean( r ), sd_change = mean( sdc ) ) ] } ) )
fwrite( sami_v, file.path( AUX, "p3_sami_persistence.csv" ) )
## metro-level aggregate volatility (log change of totals)
agg_v <- rbindlist( lapply( MEAS, function( v ) {
  x <- pan[ year >= 2012, .( cbsa, year, y = get( v ) ) ][ y > 0 ]; setorder( x, cbsa, year )
  x[ , dl := log( y ) - shift( log( y ) ), by = cbsa ]
  x[ !is.na( dl ) & year != 2020 & year != 2012, .( measure = v, sd_dlog = sd( dl ), share_gt20 = mean( abs( exp( dl ) - 1 ) > .2 ) ) ] } ) )
fwrite( agg_v, file.path( AUX, "p3_metro_aggregate_volatility.csv" ) )

g <- ggplot( beta[ universe == "all 501(c)(3)" ], aes( year, beta, color = resource, linetype = net ) ) +
  geom_line( linewidth = 0.8 ) +
  geom_hline( yintercept = 1, color = ink2, linewidth = 0.3 ) +
  scale_color_manual( values = c( rev = orange, exp = blue ), labels = c( exp = "Expenses", rev = "Revenue" ) ) +
  scale_linetype_manual( values = c( `FALSE` = "solid", `TRUE` = "dashed" ), labels = c( `FALSE` = "gross", `TRUE` = "net of grants to orgs" ) ) +
  labs( x = NULL, y = expression( hat( beta ) ~ "(log total on log population)" ),
        title = "Scaling exponent of sector resources across metros, gross vs net of pass-through grants" )
ggsave( file.path( AUX, "fig_beta_gross_vs_net.png" ), g, width = 7.5, height = 4, dpi = 150 )

cat( "\n== Part 1: year-over-year change (consecutive filings) ==\n" ); print( p1_all, digits = 3 )
cat( "\n== by size ==\n" ); print( p1_size, digits = 3 )
cat( "\n== within-org dispersion ==\n" ); print( p1_disp, digits = 3 )
cat( "\n== Part 1b: same-return CY vs PY (e-file 990) ==\n" ); print( p1b[ group == "all" ], digits = 3 )
cat( "\n== Part 2: pass-through ==\n" ); print( p2, digits = 3 )
cat( "\n== with foundations ==\n" ); print( p2f, digits = 3 )
cat( "\n== Part 3: scaling bias (beta gross - beta net) ==\n" ); print( bw, digits = 3 )
cat( "\n== pass-through share vs log P ==\n" ); print( mech_fit, digits = 3 )
cat( "\n== metro aggregate volatility ==\n" ); print( agg_v, digits = 3 )
