## 60_metro_reports.R
## Sidecar descriptive reports for ~50 metros (design section 8).
## Sample: 4 size bands (2022 population: >2M, 500k-2M, 150k-500k, <150k) x
##   4 Census regions; in each cell the metros at the 10th, 50th and 90th
##   percentile of the 2000-2024 mean count SAMI (990 filers); plus named
##   cases (Minneapolis-St. Paul, Las Vegas). Seedless (deterministic rule).
## Each metro page: small multiples 2000-2024 -- scale, density (per capita vs
##   SAMI), covariates -- with the median of the metro's size band (dashed),
##   interpolated / carried-forward years as hollow points, splice years
##   (2011 SOI, 2020 e-file) as solid rules and break years (2014 Form
##   1023-EZ, 2018 TCJA) as dotted rules. Header: SAMI rank and actual vs
##   expected filers from the scaling fit in the latest year.
## Companion: one spaghetti chart per variable across the sample.
## Output: D:/MULTIVERSE/reports/metro_reports/{index.html, metros/*.html|png, variables/*.png}

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ) } )
REP <- file.path( BUILD, "reports", "metro_reports" )
for ( d in c( REP, file.path( REP, "metros" ), file.path( REP, "variables" ) ) ) dir.create( d, recursive = TRUE, showWarnings = FALSE )
ink <- "#0b0b0b"; ink2 <- "#52514e"; dots <- "#a3a29d"; blue <- "#2a78d6"; orange <- "#eb6834"
theme_memo <- theme_minimal( base_size = 8.5 ) +
  theme( panel.grid.minor = element_blank(), panel.grid.major = element_line( color = "#e6e5e1", linewidth = 0.3 ),
         axis.title = element_blank(), axis.text = element_text( color = ink2, size = 6.5 ),
         strip.text = element_text( color = ink, face = "bold", hjust = 0, size = 7.5 ),
         plot.title = element_text( color = ink, face = "bold", size = 12 ),
         plot.subtitle = element_text( color = ink2, size = 8.5 ), legend.position = "top", legend.title = element_blank() )

p <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
p <- p[ cbsa_type == "metro" & population > 0 ]
REGION <- c( setNames( rep( "Northeast", 9 ), c( "09", "23", "25", "33", "44", "50", "34", "36", "42" ) ),
             setNames( rep( "Midwest", 12 ), c( "17", "18", "26", "39", "55", "19", "20", "27", "29", "31", "38", "46" ) ),
             setNames( rep( "South", 17 ), c( "10", "11", "12", "13", "24", "37", "45", "51", "54", "01", "21", "28", "47", "05", "22", "40", "48" ) ),
             setNames( rep( "West", 13 ), c( "04", "08", "16", "30", "32", "35", "49", "56", "02", "06", "15", "41", "53" ) ) )
p[ , region := REGION[ state_fips ] ]

## ---- derived plotting series ---------------------------------------------------
sami <- function( y, P ) { ok <- is.finite( y ) & y > 0; r <- rep( NA_real_, length( y ) )
  if ( sum( ok ) >= 30 ) r[ ok ] <- resid( lm( log( y[ ok ] ) ~ log( P[ ok ] ) ) ); r }
p[ , `:=`( s_n = sami( n_filer_c3, population ), s_e = sami( exp_c3, population ) ), by = year ]
if ( "exp_c3_net" %in% names( p ) ) p[ , s_e_net := sami( exp_c3_net, population ), by = year ]
pc <- function( x, k = 1e4 ) x / p$population * k
p[ , `:=`( n_pc = pc( n_filer_c3 ), bmf_pc = pc( n_bmf_c3pc ), exp_pc = exp_c3 / population ) ]
if ( "socap_assn_n" %in% names( p ) ) p[ , assn_pc := pc( socap_assn_n ) ]
if ( "n_entry_c3pc" %in% names( p ) ) p[ , entry_pc := pc( n_entry_c3pc, 1e5 ) ]
if ( "patents_frac" %in% names( p ) ) p[ , pat_pc := pc( patents_frac ) ]
if ( "bls_npo_wages" %in% names( p ) ) p[ , bls_wages_bn := bls_npo_wages / 1e9 ]
p[ , `:=`( exp_bn = exp_c3 / 1e9, pop_k = population / 1e3 ) ]

## panel catalog: var, label, row, observed-flag column (hollow when FALSE)
PANELS <- data.table( var = c(
  "pop_k", "n_filer_c3", "n_bmf_c3pc", "exp_bn", "bls_npo_emp", "bls_wages_bn",
  "n_pc", "s_n", "bmf_pc", "exp_pc", "s_e", "s_e_net",
  "race_hhi", "pct_nonwhite", "contrib_per_return", "pf_assets", "turnout", "dem_margin",
  "gov_earn_share", "pcpi", "poverty_rate", "pct_ba", "pct_65plus", "unemp_rate",
  "rel_hhi", "pct_adherent", "assn_pc", "entry_pc", "bls_pct_npo", "gdp_per_worker", "pat_pc" ),
  label = c( "Population (000s)", "990 filers, 501(c)(3)", "Registered public charities (BMF)",
             "990 expenses ($bn, 2022$)", "BLS nonprofit employment", "BLS nonprofit payroll ($bn)",
             "Filers per 10k residents", "Filer SAMI (log residual)", "BMF charities per 10k",
             "Expenses per capita (2022$)", "Expense SAMI", "Expense SAMI, net of grants",
             "Racial HHI", "% non-white", "Itemized gifts per return ($)", "Foundation assets (2022$)",
             "Turnout (votes / adults)", "Dem - Rep margin", "State+local gov't earnings share",
             "Per capita income ($)", "Poverty rate", "% BA+", "% 65+", "Unemployment rate",
             "Religious HHI", "% religious adherents", "Associations per 10k", "New charities per 100k",
             "Nonprofit share of private jobs", "GDP per worker", "Patents per 10k" ),
  row = c( rep( "scale", 6 ), rep( "density", 6 ), rep( "covariates", 19 ) ) )
PANELS <- PANELS[ var %in% names( p ) ]
PANELS[ , obs := fifelse( paste0( sub( "_pc$", "", var ), "_obs" ) %in% names( p ), paste0( sub( "_pc$", "", var ), "_obs" ),
                  fifelse( paste0( var, "_obs" ) %in% names( p ), paste0( var, "_obs" ), NA_character_ ) ) ]
PANELS[ var == "assn_pc", obs := "socap_assn_n_obs" ]
PANELS[ !is.na( obs ) & !obs %in% names( p ), obs := NA_character_ ]

## ---- sample ------------------------------------------------------------------------
p22 <- p[ year == 2022, .( cbsa, cbsa_title, region, pop22 = population ) ]
p22[ , band := cut( pop22, c( 0, 1.5e5, 5e5, 2e6, Inf ), labels = c( "<150k", "150k-500k", "500k-2M", ">2M" ) ) ]
ms <- p[ , .( mean_s_n = mean( s_n, na.rm = TRUE ) ), by = cbsa ]
p22 <- merge( p22, ms, by = "cbsa" )
pick <- p22[ order( mean_s_n ), {
  k <- unique( pmax( 1L, ceiling( c( .1, .5, .9 ) * .N ) ) )
  .SD[ k ][ , pct := c( "p10", "p50", "p90" )[ seq_along( k ) ] ] }, by = .( band, region ) ]
NAMED <- c( "33460", "29820" )        # Minneapolis-St. Paul, Las Vegas
extra <- p22[ cbsa %in% setdiff( NAMED, pick$cbsa ) ][ , pct := "named" ]
samp <- rbind( pick, extra, fill = TRUE )
samp[ , rank_s_n := frank( -mean_s_n ) ]
p22[ , rank_all := frank( -mean_s_n ) ]
samp <- merge( samp, p22[ , .( cbsa, rank_all ) ], by = "cbsa" )
samp[ , rule := "size band x region x {p10,p50,p90} of mean 2000-2024 filer SAMI; plus named cases" ]
setorder( samp, band, region, mean_s_n )
fwrite( samp, file.path( BUILD, "data", "metro_report_sample.csv" ) )
log_msg( "sample: ", nrow( samp ), " metros" )

## band medians for the reference line (all metros in the band, not just the sample)
p <- merge( p, p22[ , .( cbsa, band ) ], by = "cbsa", all.x = TRUE )
long <- rbindlist( lapply( seq_len( nrow( PANELS ) ), function( i ) {
  v <- PANELS$var[ i ]; o <- PANELS$obs[ i ]
  data.table( cbsa = p$cbsa, year = p$year, band = p$band, var = v, value = p[[ v ]],
              observed = if ( is.na( o ) ) TRUE else as.logical( p[[ o ]] ) ) } ) )
long <- long[ is.finite( value ) ]
med <- long[ , .( med = median( value ) ), by = .( band, var, year ) ]
long[ , var := factor( var, levels = PANELS$var, labels = PANELS$label ) ]
med[ , var := factor( var, levels = PANELS$var, labels = PANELS$label ) ]

## scaling fit in the latest year with filers
ly <- max( p[ !is.na( n_filer_c3 ), year ] )
fit <- lm( log( n_filer_c3 ) ~ log( population ), data = p[ year == ly & n_filer_c3 > 0 ] )
p[ year == ly & n_filer_c3 > 0, expected := exp( predict( fit, newdata = .SD ) ) ]
bN <- unname( coef( fit )[2] )

rules <- data.table( x = c( 2011, 2020, 2014, 2018 ), type = c( "splice", "splice", "break", "break" ) )
plot_metro <- function( cb ) {
  s <- samp[ cbsa == cb ]; d <- long[ cbsa == cb ]; m <- med[ band == s$band ]
  hdr <- p[ cbsa == cb & year == ly ]
  sub <- sprintf( "%s band, %s | filer SAMI rank %d of %d metros (mean 2000-2024) | %d filers: %s vs %s expected from beta = %.2f (%+.0f%%)",
                  s$band, s$region, s$rank_all, nrow( p22 ), ly, format( hdr$n_filer_c3, big.mark = "," ),
                  format( round( hdr$expected ), big.mark = "," ), bN, 100 * ( hdr$n_filer_c3 / hdr$expected - 1 ) )
  ggplot( d, aes( year, value ) ) +
    geom_vline( data = rules[ type == "splice" ], aes( xintercept = x ), color = "#d6d5d0", linewidth = 0.4 ) +
    geom_vline( data = rules[ type == "break" ], aes( xintercept = x ), color = "#d6d5d0", linewidth = 0.4, linetype = "dotted" ) +
    geom_line( data = m[ var %in% d$var ], aes( year, med ), color = dots, linetype = "dashed", linewidth = 0.45 ) +
    geom_line( color = blue, linewidth = 0.6 ) +
    geom_point( aes( shape = observed ), color = blue, size = 0.9, fill = "white", show.legend = FALSE ) +
    scale_shape_manual( values = c( `TRUE` = 16, `FALSE` = 21 ) ) +
    facet_wrap( ~ var, scales = "free_y", ncol = 6 ) +
    scale_x_continuous( breaks = c( 2000, 2010, 2020 ) ) +
    scale_y_continuous( labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
    labs( title = s$cbsa_title, subtitle = sub,
          caption = "Blue: metro. Dashed grey: median of all metros in the same size band. Hollow points: interpolated or carried forward.\nSolid rules: source splices (2011 SOI, 2020 e-file). Dotted: Form 1023-EZ (2014), TCJA (2018)." ) +
    theme_memo + theme( plot.caption = element_text( color = ink2, size = 6.5, hjust = 0 ) )
}

page <- function( title, body ) sprintf( '<!doctype html><html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>%s</title><style>body{font-family:system-ui,sans-serif;margin:16px;color:#0b0b0b;background:#fff}img{max-width:100%%;height:auto}a{color:#2a78d6}table{border-collapse:collapse;font-size:13px}td,th{padding:3px 8px;border-bottom:1px solid #e6e5e1;text-align:left}</style></head><body>%s</body></html>', title, body )

for ( cb in samp$cbsa ) {
  g <- plot_metro( cb )
  ggsave( file.path( REP, "metros", paste0( cb, ".png" ) ), g, width = 13, height = 9, dpi = 110, bg = "white" )
  writeLines( page( samp[ cbsa == cb, cbsa_title ],
                    sprintf( '<p><a href="../index.html">&larr; all metros</a></p><img src="%s.png" alt="%s">', cb, samp[ cbsa == cb, cbsa_title ] ) ),
              file.path( REP, "metros", paste0( cb, ".html" ) ) )
}
log_msg( "metro pages written" )

## ---- companion: variable-centric spaghetti charts ---------------------------------
sl <- long[ cbsa %in% samp$cbsa ]
for ( v in levels( sl$var ) ) {
  d <- sl[ var == v ]; if ( !nrow( d ) ) next
  mm <- d[ , .( med = median( value ) ), by = year ]
  g <- ggplot( d, aes( year, value, group = cbsa ) ) +
    geom_vline( xintercept = c( 2011, 2020 ), color = "#d6d5d0", linewidth = 0.4 ) +
    geom_vline( xintercept = c( 2014, 2018 ), color = "#d6d5d0", linewidth = 0.4, linetype = "dotted" ) +
    geom_line( color = dots, linewidth = 0.3, alpha = 0.7 ) +
    geom_line( data = mm, aes( year, med ), inherit.aes = FALSE, color = orange, linewidth = 0.9 ) +
    scale_y_continuous( labels = scales::label_number( scale_cut = scales::cut_short_scale() ) ) +
    labs( title = v, subtitle = sprintf( "%d sample metros (grey) and their median (orange)", uniqueN( d$cbsa ) ) ) +
    theme_memo + theme( axis.text = element_text( size = 8 ) )
  ggsave( file.path( REP, "variables", paste0( make.names( v ), ".png" ) ), g, width = 7, height = 3.6, dpi = 120, bg = "white" )
}

## ---- index ---------------------------------------------------------------------
rows <- samp[ , sprintf( '<tr><td><a href="metros/%s.html">%s</a></td><td>%s</td><td>%s</td><td>%s</td><td>%d</td><td>%.3f</td></tr>',
                         cbsa, cbsa_title, band, region, pct, rank_all, mean_s_n ) ]
vars <- sprintf( '<h3>%s</h3><img src="variables/%s.png" alt="%s">', levels( sl$var ), make.names( levels( sl$var ) ), levels( sl$var ) )
writeLines( page( "Metro sidecar reports",
  paste0( "<h1>Metro sidecar reports</h1><p>", nrow( samp ), " metros: ", unique( samp$rule )[1],
          ". Data: D:/MULTIVERSE/data/analysis/cbsa_year.parquet (built ", format( Sys.Date() ), ").</p>",
          "<table><tr><th>Metro</th><th>Size band</th><th>Region</th><th>SAMI cell position</th><th>Filer SAMI rank</th><th>Mean filer SAMI</th></tr>",
          paste( rows, collapse = "" ), "</table><h2>Variables across the sample</h2>", paste( vars, collapse = "" ) ) ),
  file.path( REP, "index.html" ) )
log_msg( "wrote ", file.path( REP, "index.html" ) )
