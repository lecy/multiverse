## 74b_plot_scaling_aggregation.R
## Dot chart of the scaling exponent B by NCCS group across aggregations
## (county, metro CBSA, CSA), 2022, counts and expenses, OLS and Poisson.
## Reads D:/MULTIVERSE/data/scaling_agg/scaling_B_by_unit.csv (from script 74).
## Output: notes/scaling-aggregation-figures/fig_B_by_aggregation.png

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( ggplot2 ) )

B <- fread( file.path( BUILD, "data", "scaling_agg", "scaling_B_by_unit.csv" ) )[
  year == 2022 & unit %in% c( "county", "cbsa_metro", "csa" ) ]
B <- melt( B, id.vars = c( "unit", "grp", "grp_label", "measure" ),
           measure.vars = c( "B_ols", "B_pois" ), variable.name = "est", value.name = "B" )

UNITS <- c( county = "County", cbsa_metro = "Metro CBSA", csa = "CSA" )
COLS  <- c( County = "#2a78d6", `Metro CBSA` = "#eb6834", CSA = "#1baf7a" )
B[ , agg := factor( UNITS[ unit ], levels = UNITS ) ]
B[ , est := factor( est, c( "B_ols", "B_pois" ),
                    c( "OLS on log y (drops zeros)", "Poisson PML (keeps zeros)" ) ) ]
B[ , measure := factor( measure, c( "count", "expenses" ), c( "Organization counts", "Expenses (real 2022 $)" ) ) ]
B[ grp == "ALL", grp_label := "Full sector" ]

## fixed group order in every panel: full sector on top, then by metro Poisson count B
ord <- B[ unit == "cbsa_metro" & est == levels( est )[ 2 ] & measure == levels( measure )[ 1 ] & grp != "ALL" ][ order( B ), grp_label ]
B[ , grp_label := factor( grp_label, c( ord, "Full sector" ) ) ]

rng <- B[ , .( lo = min( B ), hi = max( B ) ), by = .( grp_label, est, measure ) ]

g <- ggplot( B, aes( B, grp_label ) ) +
  annotate( "rect", xmin = -Inf, xmax = Inf, ymin = length( levels( B$grp_label ) ) - 0.5,
            ymax = Inf, fill = "grey93" ) +
  geom_vline( xintercept = 1, colour = "grey55", linewidth = 0.4, linetype = "22" ) +
  geom_segment( data = rng, aes( x = lo, xend = hi, y = grp_label, yend = grp_label ),
                colour = "grey70", linewidth = 0.8, lineend = "round" ) +
  geom_point( aes( fill = agg, y = as.numeric( grp_label ) + c( 0.22, 0, -0.22 )[ as.integer( agg ) ] ),
              shape = 21, size = 2.4, colour = "white", stroke = 0.5 ) +
  facet_grid( est ~ measure, scales = "free_x" ) +
  scale_fill_manual( values = COLS, name = NULL ) +
  labs( title = "Scaling exponent B by aggregation, 501(c)(3) filers, 2022",
        subtitle = "log(Y) = a + B log(population); grey bar spans the three aggregations (dots offset top to bottom: county, metro, CSA); dashed line = linear scaling (B = 1)",
        x = "B", y = NULL,
        caption = "County OLS is pulled down by zero truncation (e.g. 58% of counties have no hospital filer). Source: IRS e-file, NCCS 12 groups; 3,144 counties, 387 metros, 180 CSAs." ) +
  theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
         panel.grid.major.x = element_line( colour = "grey90", linewidth = 0.3 ),
         strip.text = element_text( face = "bold", hjust = 0, size = 10 ),
         plot.title = element_text( face = "bold" ), plot.title.position = "plot",
         plot.caption = element_text( colour = "grey40", hjust = 0, size = 7.5 ),
         plot.caption.position = "plot",
         legend.position = "top", legend.justification = "left",
         panel.spacing.x = unit( 1.2, "lines" ), panel.spacing.y = unit( 1, "lines" ) )

FIG <- "notes/scaling-aggregation-figures"; dir.create( FIG, showWarnings = FALSE )
ggsave( file.path( FIG, "fig_B_by_aggregation.png" ), g, width = 9.5, height = 8, dpi = 170, bg = "white" )
message( "wrote ", file.path( FIG, "fig_B_by_aggregation.png" ) )
