## 102_card_figures.R
## Card figures of paper/04-design-decisions.qmd in the layout of the decision matrix (R/98):
## covariates as rows; for each outcome two columns,
##   "All options": the options combined -- median partial r (point), range of the option estimates
##                  (thick bar) and the envelope of their 95% intervals (thin line); blue when every
##                  option is significant with the same sign, light blue when some are, gray when none;
##                  the count of significant options under the point
##   "By option":   each option as a shape, stacked within the row and connected; blue when p < .05,
##                  gray otherwise
## D11 (subsectors) is a matrix: subsectors as rows, covariates as columns, counts and spending as shapes.
## Reads the single-model tables of R/paper/96_lean_card_evidence.R (D:/MULTIVERSE/data/outline/v11/single/);
## writes paper/figures/d*_card.png. Usage: Rscript R/paper/102_card_figures.R

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( ggplot2 ) )
SD <- file.path( BUILD, "data", "outline", "v11", "single" )
FIG <- "paper/figures"
LAB <- c( pct_ba = "% BA", pf_assets = "Foundation assets", pct_nonwhite = "% nonwhite", turnout = "Turnout",
          poverty_rate = "Poverty rate", gov_earn_sl = "S&L govt earnings", tr_medicaid = "Medicaid transfers",
          lag_g10 = "Growth kernel (10-yr)", hist_pre1970 = "Pre-1970 endowment", pct_65plus = "% 65+",
          gini_m = "Gini (metro)", rel_frac = "Religious fractionalization", age_sd = "Age SD",
          bls_share = "Nonprofit market share", age_f = "Mean filer age" )
YL <- c( n_filer_c3 = "Filer counts", exp_c3_nohu = "Spending excl. H&U" )
BLUE <- "#2a78d6"; BLUE_D <- "#1d5fae"; LBLUE <- "#93bde9"; GRAY <- "#c4c8cf"; GRAY_D <- "#9aa0a8"
SHAPES <- c( 21, 22, 24, 23, 25, 4 )
thm <- theme_minimal( base_size = 10.5 ) +
  theme( panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
         panel.grid.major.x = element_line( colour = "grey92" ), panel.spacing.y = unit( 0, "pt" ),
         panel.spacing.x = unit( 10, "pt" ), strip.placement = "outside",
         strip.text.y.left = element_text( angle = 0, hjust = 1, size = 9.5 ),
         strip.text.x = element_text( face = "bold", size = 9.5 ), axis.text.y = element_blank(),
         plot.title.position = "plot", plot.title = element_text( face = "bold" ),
         plot.subtitle = element_text( colour = "grey30", size = 9.5 ), legend.position = "bottom", legend.box = "vertical",
         legend.margin = margin( 0, 0, 0, 0 ), plot.caption = element_text( colour = "grey35", size = 8.5, hjust = 0 ) )

## pd: term, r, lo, hi, p, opt (factor, option order), out (factor or NA)
cardfig <- function( pd, file, title, subtitle, opt_name, caption = NULL, w = 11, h = NULL ) {
  pd <- copy( pd )[ term %in% names( LAB ) ]
  if ( !"out" %in% names( pd ) ) pd[ , out := "" ]
  pd[ , out := factor( out, unique( out ) ) ]
  lv <- intersect( names( LAB ), unique( pd$term ) )
  pd[ , label := factor( LAB[ term ], LAB[ lv ] ) ]
  K <- nlevels( pd$opt ); step <- if ( K > 1 ) 0.78 / ( K - 1 ) else 0
  pd[ , `:=`( oi = as.integer( opt ), sig = fifelse( p < .05, "p < .05", "n.s." ) ) ]
  pd[ , y := -( oi - ( K + 1 ) / 2 ) * step ]
  pd[ , kind := factor( "By option", c( "All options", "By option" ) ) ]
  pan <- function( o, k ) factor( paste0( o, fifelse( o == "", "", ": " ), tolower( k ) ),
                                  paste0( rep( levels( pd$out ), each = 2 ), fifelse( levels( pd$out ) == "", "", ": " )[ rep( seq_len( nlevels( pd$out ) ), each = 2 ) ], c( "all options", "by option" ) ) )
  pd[ , panel := pan( as.character( out ), as.character( kind ) ) ]
  cm <- pd[ , .( r = median( r ), rmin = min( r ), rmax = max( r ), lo = min( lo ), hi = max( hi ),
                 nsig = sum( p < .05 ), k = .N,
                 same = all( sign( r[ p < .05 ] ) == sign( median( r ) ) ) ), by = .( out, label ) ]
  cm[ , status := factor( fifelse( nsig == k & same, "All significant, same sign", fifelse( nsig == 0, "None significant", "Some significant" ) ),
                          c( "All significant, same sign", "Some significant", "None significant" ) ) ]
  cm[ , `:=`( kind = factor( "All options", levels( pd$kind ) ), txt = sprintf( "%d/%d%s", nsig, k, fifelse( nsig > 0 & !same, " ±", "" ) ) ) ]
  cm[ , panel := pan( as.character( out ), "All options" ) ]
  xl <- range( c( pd$lo, pd$hi, 0 ), na.rm = TRUE ); xl <- c( floor( xl[ 1 ] * 10 ) / 10, ceiling( xl[ 2 ] * 10 ) / 10 )
  p <- ggplot() +
    geom_hline( yintercept = -0.5, colour = "grey85", linewidth = 0.3 ) +
    geom_vline( xintercept = 0, colour = "grey55", linewidth = 0.4 ) +
    geom_linerange( data = cm, aes( y = 0, xmin = lo, xmax = hi, colour = status ), linewidth = 0.5 ) +
    geom_linerange( data = cm, aes( y = 0, xmin = rmin, xmax = rmax, colour = status ), linewidth = 2.6, alpha = 0.55 ) +
    geom_point( data = cm, aes( r, 0, colour = status ), size = 2.3 ) +
    geom_text( data = cm, aes( r, -0.3, label = txt ), size = 2.6, colour = "grey30" ) +
    geom_path( data = pd[ order( out, label, oi ) ], aes( r, y, group = interaction( out, label ) ), colour = "grey75", linewidth = 0.4 ) +
    geom_point( data = pd, aes( r, y, shape = opt, fill = sig ), colour = "grey25", stroke = 0.35, size = 2.1 ) +
    scale_colour_manual( values = c( "All significant, same sign" = BLUE, "Some significant" = LBLUE, "None significant" = GRAY_D ),
                         name = "All options:" ) +
    scale_fill_manual( values = c( "p < .05" = BLUE, "n.s." = GRAY ), name = "By option:",
                       guide = guide_legend( override.aes = list( shape = 21, size = 2.6 ), order = 2 ) ) +
    scale_shape_manual( values = SHAPES[ seq_len( K ) ], name = paste0( opt_name, ":" ),
                        guide = guide_legend( override.aes = list( fill = BLUE, size = 2.6 ), order = 1, nrow = if ( K > 3 ) 2 else 1 ) ) +
    scale_x_continuous( limits = xl, breaks = seq( -1, 1, .25 ), labels = function( b ) sub( "^(-?)0\\.", "\\1.", sprintf( "%.2f", b ) ) ) +
    scale_y_continuous( limits = c( -0.5, 0.5 ), expand = c( 0, 0 ) ) +
    facet_grid( label ~ panel, switch = "y" ) +
    labs( title = title, subtitle = subtitle, x = "Partial r", y = NULL, caption = caption ) + thm
  if ( is.null( h ) ) h <- 2.1 + 0.42 * length( lv ) * max( 1, K / 4 )^0.5
  ggsave( file.path( FIG, file ), p, width = w, height = h, dpi = 180, bg = "white" )
  log_msg( "figure ", file ) }

CAP <- "All options: point = median of the options, thick bar = range of their estimates, thin line = envelope of their 95% intervals; k/K = options significant at .05 (± = with opposite signs)."
rd <- function( f ) fread( file.path( SD, f ) )

## D1: one model, four outcome populations
d <- rd( "d1_outcome_models.csv" )[ set == "lean" ]
d[ , opt := factor( outcome, c( "Filers", "Grassroots", "Registered stock", "Spending excl. H&U" ) ) ]
cardfig( d, "d1_card.png", "D1. One model, four outcome populations",
         sprintf( "13-covariate log-log model with log P, %d metros, 2022", d$n[ 1 ] ), "Outcome", CAP, w = 7.5 )

## D2: six geographic units, filers and spending
d <- rd( "d2_units.csv" )[ set == "lean" & outcome %in% names( YL ) ]
d[ , `:=`( opt = factor( unit, c( "Metro CBSA", "CSA or metro (metro areas)", "Metro + micro CBSA", "Micropolitan only", "County (all)",
                                 "Counties within metros (metro FE)" ) ), out = factor( YL[ outcome ], YL ) ) ]
cardfig( d, "d2_card.png", "D2. The same model at six geographic units", "13-covariate log-log model with log P, 2022", "Unit", CAP, w = 11 )

## D4: dependent-variable form
d <- rd( "d4_forms.csv" )[ set == "lean" & form %in% c( "loglog", "logpcP", "logpc", "pc" ) ]
d[ , `:=`( opt = factor( c( loglog = "Log-log (= SAMI)", logpcP = "Log per capita + log P", logpc = "Log per capita, no log P",
                            pc = "Per capita (linear), no log P" )[ form ],
                         c( "Log-log (= SAMI)", "Log per capita + log P", "Log per capita, no log P", "Per capita (linear), no log P" ) ),
           out = factor( YL[ outcome ], YL ) ) ]
cardfig( d, "d4_card.png", "D4. Dependent-variable form", sprintf( "13-covariate model, %d metros, 2022", d$n[ 1 ] ), "DV form", CAP )

## D8: endowment, market share, growth kernel
AL8 <- c( lean = "Lean (with endowment)", lean_noendow = "Without endowment", lean_share = "Lean + market share",
          lean_noendow_share = "Without endowment + share", lean_nogrowth = "Without growth kernel" )
d <- rd( "d8_models.csv" )[ arm %in% names( AL8 ) ]
d[ , `:=`( opt = factor( AL8[ arm ], AL8 ), out = factor( YL[ outcome ], YL ) ) ]
cardfig( d, "d8_card.png", "D8. What the endowment, market share and growth kernel absorb",
         sprintf( "SAMI form, %d metros, 2022 (market share = mean 2013-2022)", d$n[ 1 ] ), "Model", CAP )

## D9: estimators
AL9 <- c( ols = "OLS", div_fe = "Division FE", state_fe = "State FE", state_re = "State RE" )
d <- rd( "d9_estimators.csv" )[ set == "lean" ]
d[ , `:=`( opt = factor( AL9[ estimator ], AL9 ), out = factor( YL[ outcome ], YL ) ) ]
cardfig( d, "d9_card.png", "D9. Estimator: across and within states", sprintf( "13-covariate log-log model, %d metros, 2022", d$n[ 1 ] ), "Estimator", CAP )

## D10: money measure
LB <- c( exp_c3 = "Expenses, all 501(c)(3)", exp_c3_nohu = "Expenses excl. H&U", exp_c3_nohu_net = "... and net of pass-through",
         rev_c3_nohu = "Revenue excl. H&U" )
d <- rd( "d10_models.csv" )[ set == "lean" & outcome %in% names( LB ) ]
d[ , opt := factor( LB[ outcome ], LB ) ]
cardfig( d, "d10_card.png", "D10. Money measure", sprintf( "13-covariate log-log model with log P, %d metros, 2022", d$n[ 1 ] ), "Measure", CAP, w = 7.5 )

## D12: sector age
AL12 <- c( lean = "Lean", lean_age = "Lean + mean filer age", lean_noendow = "Without endowment", lean_noendow_age = "Without endowment + age" )
d <- rd( "d12_levels.csv" )[ arm %in% names( AL12 ) & outcome %in% names( YL ) ]
d[ , `:=`( opt = factor( AL12[ arm ], AL12 ), out = factor( YL[ outcome ], YL ) ) ]
cardfig( d, "d12_card.png", "D12. Sector age", sprintf( "SAMI form, %d metros, 2022", d$n[ 1 ] ), "Model", CAP )

## D11: subsectors x covariates (Poisson PML, counts and spending)
d <- rd( "d11_subsector_models.csv" )[ set == "lean" & term %in% names( LAB ) ]
d[ , df := z^2 * ( 1 / r^2 - 1 ) ]
SUBL <- c( "All 501(c)(3) filers", "Arts", "Education", "Environment/animals", "Health", "Human services", "International",
           "Public/societal benefit", "Religion-related", "Mutual/membership", "Hospitals", "Universities" )
ag <- rd( "d11_sign_agreement.csv" )[ set == "lean" ]
ag <- dcast( ag[ term %in% names( LAB ) ], term ~ outcome, value.var = "same_sign" )[ , txt := sprintf( "%d / %d", count, spending ) ]
d[ , `:=`( row = factor( label, c( SUBL, "Same sign as all filers" ) ), col = factor( LAB[ term ], LAB[ intersect( names( LAB ), term ) ] ),
           y = fifelse( outcome == "count", 0.18, -0.18 ), sig = fifelse( p < .05, "p < .05", "n.s." ),
           opt = factor( fifelse( outcome == "count", "Counts", "Spending" ), c( "Counts", "Spending" ) ) ) ]
ag[ , `:=`( row = factor( "Same sign as all filers", levels( d$row ) ), col = factor( LAB[ term ], levels( d$col ) ) ) ]
xl <- range( c( d$r, 0 ) ); xl <- c( floor( xl[ 1 ] * 10 ) / 10, ceiling( xl[ 2 ] * 10 ) / 10 )
p <- ggplot( d ) +
  geom_rect( data = unique( d[ row == SUBL[ 1 ], .( row, col ) ] )[ , `:=`( xmin = -Inf, xmax = Inf, ymin = -.5, ymax = .5 ) ], aes( xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax ), fill = "#eef4fc", inherit.aes = FALSE ) +
  geom_hline( yintercept = -0.5, colour = "grey85", linewidth = 0.3 ) +
  geom_vline( xintercept = 0, colour = "grey55", linewidth = 0.4 ) +
  geom_path( aes( r, y, group = interaction( row, col ) ), colour = "grey75", linewidth = 0.4 ) +
  geom_point( aes( r, y, shape = opt, fill = sig ), colour = "grey25", stroke = 0.35, size = 2.1 ) +
  geom_label( data = ag, aes( x = mean( xl ), y = 0, label = txt ), size = 2.9, colour = "grey20", fill = "white", linewidth = 0 ) +
  scale_shape_manual( values = c( Counts = 21, Spending = 22 ), name = "Outcome:", guide = guide_legend( override.aes = list( fill = BLUE, size = 2.6 ) ) ) +
  scale_fill_manual( values = c( "p < .05" = BLUE, "n.s." = GRAY ), name = NULL, guide = guide_legend( override.aes = list( shape = 21, size = 2.6 ) ) ) +
  scale_x_continuous( limits = xl, breaks = c( -.25, 0, .25 ), labels = c( "-.25", "0", ".25" ) ) +
  scale_y_continuous( limits = c( -0.5, 0.5 ), expand = c( 0, 0 ) ) +
  facet_grid( row ~ col, switch = "y", labeller = labeller( col = label_wrap_gen( 17 ) ) ) +
  labs( title = "D11. Subsectors: the 13 covariates in each subsector's Poisson model",
        subtitle = sprintf( "Partial r, Poisson PML with log P, 2022 e-file filers (%d metros). Bottom row: subsectors (of 11) with the sign of all filers, counts / spending.", 385 ),
        x = "Partial r", y = NULL ) + thm +
  theme( strip.text.x = element_text( face = "bold", size = 8 ), axis.text.x = element_text( size = 7.5 ) )
ggsave( file.path( FIG, "d11_card.png" ), p, width = 15, height = 7.8, dpi = 180, bg = "white" )
log_msg( "figure d11_card.png" )

## D3: identification x population (R/notes/103_ident_by_population.R). Rows = covariates, columns = outcome population
## (filers, grassroots, expenditures); each cell: the median partial r under the three identifications (shapes:
## cross-section, pooled, within), and under them a red dumbbell from the cross-section to the within-metro
## estimate with the difference (within - cross-section) printed over the bar and each end's value beside it.
## Blue = more than half the specs significant with the median's sign.
ib <- fread( file.path( BUILD, "data", "outline", "v11", "mv", "ident_by_population_long.csv" ) )[ outcome %in% c( "n_filer_c3", "n_grass", "exp_c3_nohu" ) ]
ib <- ib[ term %in% names( LAB ) ]
POPL <- c( n_filer_c3 = "Filers", n_grass = "Grassroots", exp_c3_nohu = "Expenditures excl. H&U" )
IDL <- c( cs = "Cross-section", pooled = "Pooled, year FE", within = "Within metros" )
ib[ , `:=`( col = factor( POPL[ outcome ], POPL ), id = factor( IDL[ ident ], IDL ), sig = fifelse( sig_same > .5, "Mostly significant", "Mostly n.s." ),
            label = factor( LAB[ term ], LAB[ intersect( names( LAB ), term ) ] ) ) ]
ib[ , y := c( 0.28, 0.0, -0.28 )[ as.integer( id ) ] ]
fmt2 <- function( x ) { s <- sub( "^-", "−", sub( "^(-?)0\\.", "\\1.", sprintf( "%.2f", x ) ) ); s[ s == "−.00" ] <- ".00"; s }
db <- dcast( ib[ ident %in% c( "cs", "within" ) ], label + col ~ ident, value.var = c( "median_pr", "sig" ) )[ !is.na( median_pr_within ) ]
db[ , `:=`( lo = pmin( median_pr_cs, median_pr_within ), hi = pmax( median_pr_cs, median_pr_within ),
            txt = { d <- median_pr_within - median_pr_cs; s <- fmt2( d ); ifelse( d > 0, paste0( "+", s ), s ) } ) ]
db[ , `:=`( lo_txt = fmt2( lo ), hi_txt = fmt2( hi ) ) ]
## arrow from the cross-section toward the within estimate: blue when the within estimate is mostly
## significant, black otherwise; it stops short of the within triangle
db[ , `:=`( acol = fifelse( sig_within == "Mostly significant", "within sig", "within ns" ),
            xend = median_pr_within - sign( median_pr_within - median_pr_cs ) * pmin( .018, abs( median_pr_within - median_pr_cs ) / 2 ) ) ]
dbe <- db[ , .( label, col, x = median_pr_within, sig = sig_within, id = factor( IDL[[ "within" ]], IDL ) ) ]
## difference text: black when the within estimate is mostly significant, gray otherwise; end labels: the within
## r in blue beside the arrowhead, the cross-section r in gray beside the tail
db[ , `:=`( tcol = fifelse( sig_within == "Mostly significant", "diff sig", "diff ns" ),
            w_left = median_pr_within < median_pr_cs, w_txt = fmt2( median_pr_within ), c_txt = fmt2( median_pr_cs ) ) ]
db[ , `:=`( w_x = fifelse( w_left, median_pr_within - .03, median_pr_within + .03 ), w_h = fifelse( w_left, 1, 0 ),
            c_x = fifelse( w_left, median_pr_cs + .03, median_pr_cs - .03 ), c_h = fifelse( w_left, 0, 1 ) ) ]
## two columns per population: the three identifications (shapes) and, beside them, an arrow from the
## cross-section to the within-metro estimate with the two values at its ends. Fill blue = mostly significant.
KL <- c( "estimates", "cross-section to within" )
PAN <- as.vector( t( outer( levels( ib$col ), KL, paste, sep = ": " ) ) )
ib[ , panel := factor( paste( col, KL[ 1 ], sep = ": " ), PAN ) ]
db[ , panel := factor( paste( col, KL[ 2 ], sep = ": " ), PAN ) ]
dbe[ , panel := factor( paste( col, KL[ 2 ], sep = ": " ), PAN ) ]
ACOL <- c( "within sig" = BLUE, "within ns" = "grey60", "wlab sig" = BLUE, "wlab ns" = "grey45" )
db[ , wcol := fifelse( sig_within == "Mostly significant", "wlab sig", "wlab ns" ) ]
FILL <- c( "Mostly significant" = BLUE, "Mostly n.s." = GRAY )
xl <- range( c( ib$median_pr, 0 ) ); xl <- c( floor( xl[ 1 ] * 10 ) / 10 - .10, ceiling( xl[ 2 ] * 10 ) / 10 + .10 )   # room for end labels
p <- ggplot() +
  geom_hline( yintercept = -0.5, colour = "grey80", linewidth = 0.35 ) +
  geom_vline( xintercept = 0, colour = "grey55", linewidth = 0.4 ) +
  geom_path( data = ib[ order( label, col, id ) ], aes( median_pr, y, group = interaction( label, col ) ), colour = "grey78", linewidth = 0.35 ) +
  geom_point( data = ib, aes( median_pr, y, shape = id, fill = sig ), colour = "grey25", stroke = 0.35, size = 2.3 ) +
  geom_segment( data = db, aes( x = median_pr_cs, xend = xend, y = 0, yend = 0, colour = acol ), linewidth = 0.9,
                arrow = grid::arrow( length = unit( 4.5, "pt" ), type = "closed" ), show.legend = FALSE ) +
  geom_point( data = dbe, aes( x, 0, shape = id, fill = sig ), colour = "grey25", stroke = 0.35, size = 2.3 * 1.2, show.legend = FALSE ) +
  geom_text( data = db, aes( w_x, 0, label = w_txt, hjust = w_h, colour = wcol ), size = 2.3 * 1.5, fontface = "bold", show.legend = FALSE ) +
  geom_text( data = db, aes( c_x, 0, label = c_txt, hjust = c_h ), size = 2.3 * 1.5, colour = "grey45" ) +
  scale_colour_manual( values = ACOL, guide = "none" ) +
  scale_fill_manual( values = FILL, name = NULL, guide = guide_legend( override.aes = list( shape = 21, size = 2.6 ), order = 2 ) ) +
  scale_shape_manual( values = c( 24, 22, 21 ), name = "Identification:",
                      guide = guide_legend( override.aes = list( fill = BLUE, size = 2.6 ), order = 1 ) ) +
  scale_x_continuous( limits = xl, breaks = seq( -.3, .3, .1 ), labels = function( b ) sub( "^(-?)0\\.", "\\1.", sprintf( "%.1f", b ) ) ) +
  scale_y_continuous( limits = c( -0.5, 0.5 ), expand = c( 0, 0 ) ) +
  facet_grid( label ~ panel, switch = "y", labeller = labeller( panel = label_wrap_gen( 24 ) ) ) +
  labs( title = "D3. Identification changes the answer, differently for filers, grassroots and expenditures",
        subtitle = paste0( "Median partial r across default-model specifications of run v11 (df counted in metros): cross-section (2022, 2019), pooled 2015-2022 with year FE, within metros 2010-2022.\n",
                           "Beside each population's estimates, an arrow from the cross-section (gray value) to the within-metro estimate (circle); arrow and within value blue when the within estimate is mostly significant." ),
        caption = "Blue fill: more than half the specifications significant (p < .05) with the median's sign; gray: half or fewer. Grassroots = registered-stock specifications refit\nwith registered non-filers as the outcome. Expenditures exclude hospitals and universities. The endowment is fixed and drops out within metros (no arrow).",
        x = "Partial r", y = NULL ) + thm + theme( panel.spacing.y = unit( 3, "pt" ), strip.text.x = element_text( face = "bold", size = 9 ) )
ggsave( file.path( FIG, "d3_card.png" ), p, width = 15, height = 11, dpi = 180, bg = "white" )
log_msg( "figure d3_card.png" )
