## 98_decision_matrix.R
## Compact summary exhibit for the multiverse: one row per covariate, one column per design decision.
## Replaces the per-benchmark specification curves (S-shaped curve + option breakouts) of 58 part V.
##   first column  every specification's partial r on one line (a strip), the median, and the share of
##                 specifications that agree in sign and that are significant (p < .05)
##   decision cells the mean partial r of each option, on the same axis. Colored when the decision
##                 matters for that covariate (eta^2 >= .06, a "medium" effect); gray otherwise. With
##                 10^4-10^5 specifications every F test is significant, so eta^2, not p, is the screen.
##   bottom row    global score per decision, aggregated over covariates, from matched pairs (specs that
##                 differ only in this decision): mean |delta r| and the conclusion-change rate (share of
##                 pairs whose verdict differs: significantly positive / not significant / significantly
##                 negative), plus the number of covariates with eta^2 >= .06.
## Usage: Rscript R/multiverse/98_decision_matrix.R [run] (default principled_v11). Outputs to
##   D:/MULTIVERSE/data/outline/<v>/matrix/ and paper/figures/decision_matrix_<v>_{counts,spending}.png

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ) } )
args <- commandArgs( TRUE )
RUNN <- if ( length( args ) ) args[1] else "principled_v11"
VL   <- sub( "^principled_", "", RUNN )
RUN  <- file.path( OUT$spec, paste0( "run_", RUNN ) )
## MV_SPLIT=1: filers, grassroots and spending as three groups (grassroots from R/notes/104_fit_grassroots_v11.R)
## instead of counts pooling filers with the registered stock; outputs to matrix_split/ and figures
## decision_matrix_<v>_{filers,grassroots,spending}_vertical_p*.png
SPLIT <- Sys.getenv( "MV_SPLIT" ) == "1"
OD   <- file.path( BUILD, "data", "outline", VL, if ( SPLIT ) "matrix_split" else "matrix" ); dir.create( OD, recursive = TRUE, showWarnings = FALSE )
GTITLE <- c( counts = "counts (filers, registrations)", filers = "filer counts", grassroots = "grassroots counts (registered non-filers)",
             spending = "spending excl. hospitals & universities" )
FIG  <- "paper/figures"
ETA_MIN <- .06

LAB <- c( pct_ba = "% BA", pf_assets = "Foundation assets", pct_nonwhite = "% nonwhite", turnout = "Turnout",
          poverty_rate = "Poverty rate", gov_earn_sl = "S&L govt earnings", tr_medicaid = "Medicaid",
          lag_g10 = "Growth kernel", hist_pre1970 = "Pre-1970 endowment", gini_m = "Gini (metro)",
          rel_frac = "Religious fract.", age_sd = "Age SD", pct_65plus = "% 65+" )
## measurement swaps: the alternative term is read as the construct's default term
CONSTRUCT_OF <- c( unemp_rate = "poverty_rate", race_hhi = "pct_nonwhite", pct_foreign_born = "pct_nonwhite",
                   n_pf = "pf_assets", contrib_total = "pf_assets", contrib_per_return = "pf_assets",
                   contrib_share_agi = "pf_assets", socap_respn = "turnout", dem_margin = "turnout",
                   lag_g5 = "lag_g10", gov_emp_sl = "gov_earn_sl", gov_emp_share = "gov_earn_sl", cog_dge = "gov_earn_sl",
                   tr_medmaint = "tr_medicaid", cog_welfare = "tr_medicaid" )

log_msg( "read ", RUNN )
specs <- as.data.table( read_parquet( file.path( RUN, "specs.parquet" ) ) )[ status == "ok" ]
co <- as.data.table( read_parquet( file.path( RUN, "coefs.parquet" ) ) )
if ( SPLIT ) {
  GRD <- file.path( OUT$spec, paste0( "run_", RUNN, "_grass" ) )
  specs <- rbind( specs[ outcome != "n_bmf_c3pc" ], as.data.table( read_parquet( file.path( GRD, "specs.parquet" ) ) )[ status == "ok" ], fill = TRUE )
  co <- rbind( co, as.data.table( read_parquet( file.path( GRD, "coefs.parquet" ) ) ), fill = TRUE )
}
keep <- specs[ drop %in% c( "none", "history", "growth" ) & ( op == "default" | drop == "none" ) ]
co <- co[ spec_id %in% keep$spec_id ]
co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
co <- merge( co, keep[ , .( spec_id, ident, est ) ], by = "spec_id" )
co[ , n_units := n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) ]
co[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
co <- co[ term != "logP" & !( ident == "pooled" & est == "state_re" ) ]
co[ term %in% c( "rel_hhi", "race_hhi" ), pr := -pr ]                         # concentration -> diversity
co[ term == "rel_hhi", term := "rel_frac" ]
co[ , cls := fifelse( p < .05, sign( pr ), 0 ) ]
d <- merge( co[ , .( spec_id, term, pr, cls ) ], keep, by = "spec_id" )
rm( co ); gc()
d[ , construct_term := fifelse( term %in% names( CONSTRUCT_OF ), CONSTRUCT_OF[ term ], term ) ]
TERMS <- intersect( names( LAB ), unique( d[ op == "default" & drop == "none", term ] ) )
log_msg( "  terms: ", paste( TERMS, collapse = ", " ) )

FORKS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year",
            "outlier", "op", "drop" )
eta2 <- function( y, g ) { g <- as.factor( g ); if ( nlevels( g ) < 2 || length( y ) < 10 ) return( NA_real_ )
  m <- tapply( y, g, mean ); n <- tapply( y, g, length )
  sum( n * ( m - mean( y ) )^2, na.rm = TRUE ) / sum( ( y - mean( y ) )^2 ) }
## pairwise summaries within a matched group
pw_abs  <- function( r ) { k <- length( r ); if ( k < 2 ) return( NA_real_ ); sum( abs( outer( r, r, "-" ) ) ) / ( k * ( k - 1 ) ) }
pw_flip <- function( c ) { k <- length( c ); if ( k < 2 ) return( NA_real_ ); sum( outer( c, c, "!=" ) ) / ( k * ( k - 1 ) ) }

## ---- decisions --------------------------------------------------------------------------------
## each: data (default model unless noted), the fork column, the strata eta^2 is averaged over,
## the columns that vary together with the fork (dropped from the matching key), option labels
DEFAULT <- quote( op == "default" & drop == "none" )
GROUPS <- if ( SPLIT ) list( filers = quote( outcome == "n_filer_c3" ), grassroots = quote( outcome == "n_grass" ),
                             spending = quote( construct == "exp" & hu == "excl" ) ) else
          list( counts = quote( construct == "count" & hu == "incl" ), spending = quote( construct == "exp" & hu == "excl" ) )
decisions <- function( g ) {
  x0 <- d[ eval( DEFAULT ) & ident != "within" ]
  xg <- x0[ eval( GROUPS[[ g ]] ) ]
  L <- list(
    D1 = if ( g == "counts" ) list( x = xg, f = "outcome", by = "ident", lab = "Outcome",
                                    opt = c( n_filer_c3 = "filers", n_bmf_c3pc = "registered" ) )
         else if ( g != "spending" ) list( x = x0[ outcome %in% c( "n_filer_c3", "n_grass" ) ], f = "outcome", by = "ident", lab = "Population",
                                           opt = c( n_filer_c3 = "filers", n_grass = "grassroots" ) )
         else list( x = x0[ hu == "excl" & outcome %in% c( "n_filer_c3_nohu", "exp_c3_nohu" ) ], f = "construct",
                    by = "ident", lab = "Counts vs\nspending", vary = "outcome", opt = c( count = "counts", exp = "spending" ) ),
    D2 = list( x = xg, f = "unit", by = c( "ident", "outcome" ), lab = "Unit", opt = c( metro = "metro", cbsa = "+ micro" ) ),
    D3 = list( x = d[ eval( DEFAULT ) & eval( GROUPS[[ g ]] ) & est == "ols" & cs_year == 2022L ], f = "ident", by = "outcome", lab = "Identification",
               opt = c( cs = "cross-section", pooled = "pooled", within = "within" ) ),
    D4 = list( x = xg, f = "dv_form", by = c( "ident", "outcome" ), lab = "DV form",
               opt = c( pc = "per capita", logpc = "log pc", loglog = "log-log", sami = "SAMI", pois_off = "Poisson offset", pois_free = "Poisson free" ) ),
    D6 = list( x = xg, f = "pred_form", by = c( "ident", "outcome" ), lab = "Predictor form",
               opt = c( raw = "raw", pc = "per capita", log = "logged", sami = "SAMI form" ) ),
    RHS = list( x = xg, f = "rhs_logP", by = c( "ident", "outcome" ), lab = "log P on\nthe right", opt = c( "FALSE" = "no", "TRUE" = "yes" ) ),
    D7 = list( x = d[ drop == "none" & ident != "within" & eval( GROUPS[[ g ]] ) ], f = "op", by = c( "ident", "outcome" ),
               lab = "Measure\n(own swap)", opt = NULL, own = TRUE ),
    D8 = list( x = d[ op == "default" & drop %in% c( "none", "history" ) & ident != "within" & eval( GROUPS[[ g ]] ) ], f = "drop",
               by = c( "ident", "outcome" ), lab = "Endowment", opt = c( none = "in", history = "out" ) ),
    Growth = list( x = d[ op == "default" & drop %in% c( "none", "growth" ) & ident != "within" & eval( GROUPS[[ g ]] ) ], f = "drop",
                   by = c( "ident", "outcome" ), lab = "Growth\nkernel", opt = c( none = "in", growth = "out" ) ),
    D9 = list( x = xg, f = "est", by = c( "ident", "outcome" ), lab = "Estimator",
               opt = c( ols = "OLS", div_fe = "division FE", state_fe = "state FE", state_re = "state RE" ) ),
    D10 = { z <- x0[ construct == ifelse( g == "spending", "exp", "count" ) &
                     outcome %in% c( "n_filer_c3", "n_filer_c3_nohu", "exp_c3", "exp_c3_nohu" ) ]
            list( x = z, f = "hu", by = "ident", lab = "Hospitals &\nuniversities", vary = "outcome", opt = c( incl = "in", excl = "out" ) ) },
    Year = list( x = xg[ ident == "cs" ], f = "cs_year", by = "outcome", lab = "Year", opt = c( "2022" = "2022", "2019" = "2019" ) ),
    Winsor = list( x = xg, f = "outlier", by = c( "ident", "outcome" ), lab = "Winsorizing", opt = c( none = "none", winsor1 = "1%" ) ) )
  if ( g == "spending" ) L$Money <- list( x = xg, f = "outcome", by = "ident", lab = "Money\nmeasure",
                                          opt = c( exp_c3_nohu = "expenses", exp_c3_nohu_net = "net", rev_c3_nohu = "revenue", rev_c3_nohu_net = "rev. net" ) )
  if ( g == "grassroots" ) L$D10 <- NULL                         # no grassroots series without hospitals & universities
  if ( !any( d$drop == "growth" ) ) L$Growth <- NULL
  if ( !any( d$drop == "history" ) ) L$D8 <- NULL
  L
}

cell_stats <- function( D, term_v ) {
  x <- copy( D$x )
  if ( isTRUE( D$own ) ) {
    ## measurement: default vs swaps of the term's own block, read on the construct's term
    x <- x[ construct_term == term_v ]
    sw <- unique( x[ op != "default" & term != term_v, op ] )
    x <- x[ op == "default" | op %in% sw ]
    x <- x[ op == "default" & term == term_v | op %in% sw ]
  } else x <- x[ term == term_v ]
  if ( !nrow( x ) || uniqueN( x[[ D$f ]] ) < 2 ) return( NULL )
  x[ , opt := as.character( get( D$f ) ) ]
  e <- x[ , .( e = eta2( pr, opt ) ), by = c( D$by ) ][ , mean( e, na.rm = TRUE ) ]
  key <- setdiff( FORKS, c( D$f, D$vary, if ( isTRUE( D$own ) ) "op" ) )
  if ( !is.null( D$vary ) && D$f == "hu" ) { x[ , base_y := sub( "_nohu$", "", outcome ) ]; key <- c( key, "base_y" ) }
  m <- x[ , .( n_opt = .N, dabs = pw_abs( pr ), flip = pw_flip( cls ) ), by = key ][ n_opt >= 2 ]
  means <- x[ , .( mean_pr = mean( pr ), n = .N ), by = opt ]
  list( eta2 = e, dabs = mean( m$dabs ), flip = mean( m$flip ), pairs = nrow( m ), means = means )
}

out_cells <- list(); out_means <- list(); out_all <- list(); out_glob <- list()
for ( g in names( GROUPS ) ) {
  DL <- decisions( g )
  ## all-specifications strip (every covariate set and option, all identifications)
  a <- d[ eval( GROUPS[[ g ]] ) & construct_term %in% TERMS & term %in% TERMS ]
  out_all[[ g ]] <- a[ , .( group = g, n = .N, median = median( pr ), agree = mean( sign( pr ) == sign( median( pr ) ) ),
                            sig = mean( cls != 0 ), pos_sig = mean( cls > 0 ), neg_sig = mean( cls < 0 ) ), by = term ]
  for ( dn in names( DL ) ) for ( tv in TERMS ) {
    s <- tryCatch( cell_stats( DL[[ dn ]], tv ), error = function( e ) { message( dn, " ", tv, ": ", conditionMessage( e ) ); NULL } )
    if ( is.null( s ) ) next
    out_cells[[ length( out_cells ) + 1 ]] <- data.table( group = g, decision = dn, label = DL[[ dn ]]$lab, term = tv,
                                                          eta2 = s$eta2, dabs = s$dabs, flip = s$flip, pairs = s$pairs )
    mm <- s$means; op <- DL[[ dn ]]$opt
    mm[ , opt_lab := if ( is.null( op ) ) ifelse( opt == "default", "default", "swap" ) else unname( op[ opt ] ) ]
    mm[ , oi := if ( is.null( op ) ) ifelse( opt == "default", 1L, 2L ) else match( opt, names( op ) ) ]
    if ( is.null( op ) ) mm <- mm[ , .( mean_pr = weighted.mean( mean_pr, n ), n = sum( n ) ), by = .( opt_lab, oi ) ]
    out_means[[ length( out_means ) + 1 ]] <- mm[ , .( group = g, decision = dn, term = tv, opt_lab, oi, mean_pr, n ) ]
  }
  log_msg( "  ", g, " done" )
}
cells <- rbindlist( out_cells ); means <- rbindlist( out_means, fill = TRUE ); allr <- rbindlist( out_all )
glob <- cells[ , .( mean_dabs = mean( dabs, na.rm = TRUE ), flip_rate = mean( flip, na.rm = TRUE ),
                    n_meaningful = sum( eta2 >= ETA_MIN, na.rm = TRUE ), n_terms = .N,
                    pooled_eta2 = NA_real_ ), by = .( group, decision, label ) ]
fwrite( cells, file.path( OD, "cells.csv" ) ); fwrite( means, file.path( OD, "option_means.csv" ) )
fwrite( allr, file.path( OD, "all_specs.csv" ) ); fwrite( glob, file.path( OD, "global_scores.csv" ) )
print( glob[ order( group, -mean_dabs ) ][ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )

## ---- figure -----------------------------------------------------------------------------------------
PAL <- c( "#2a78d6", "#eb6834", "#1baf7a", "#8e5cd9", "#c9a227", "#d6457a" )
XL <- c( -.45, .6 )
fig <- function( g ) {
  gl <- glob[ group == g ][ order( -mean_dabs ) ]
  dec_lv <- c( "All specifications", gl$label )
  term_lv <- rev( allr[ group == g ][ order( -abs( median ) ) ]$term )
  ## strip: a subsample of specifications per term
  set.seed( 1 )
  st <- d[ eval( GROUPS[[ g ]] ) & term %in% TERMS ][ , .SD[ sample( .N, min( .N, 600 ) ) ], by = term ]
  st[ , `:=`( dec = "All specifications", yy = runif( .N, -.3, .12 ) ) ]
  st[ , scol := fcase( cls > 0, "#2a78d6", cls < 0, "#eb6834", default = "grey60" ) ]
  ar <- allr[ group == g ][ , dec := "All specifications" ]
  mfmt <- function( x ) sub( "^-", "−", sub( "^(-?)0\\.", "\\1.", sprintf( "%.2f", x ) ) )
  ar[ , txt := sprintf( "median %s · %d%% agree\n%d%% sig (+%d / −%d)", mfmt( median ), round( 100 * agree ), round( 100 * sig ),
                        round( 100 * pos_sig ), round( 100 * neg_sig ) ) ]
  ce <- cells[ group == g ][ , dec := label ]
  ce[ , hot := eta2 >= ETA_MIN ]
  mn <- merge( means[ group == g ], ce[ , .( decision, term, dec, hot, eta2 ) ], by = c( "decision", "term" ) )
  mn[ , col := ifelse( hot, PAL[ pmin( oi, length( PAL ) ) ], "grey60" ) ]
  rng <- mn[ , .( lo = min( mean_pr ), hi = max( mean_pr ) ), by = .( dec, term, hot ) ]
  ## option key above each column
  key <- unique( mn[ , .( dec, opt_lab, oi ) ] )[ order( dec, oi ) ]
  key[ , `:=`( term = "  Options", col = PAL[ pmin( oi, length( PAL ) ) ] ) ]
  key[ , x := XL[1] + .02 ][ , yy := .56 - ( oi - 1 ) * .21 ]
  ## global row
  gl[ , `:=`( dec = label, term = "Global score" ) ]
  gl[ , txt := sprintf( "|Δr| %.3f\n%d%% verdicts change\n%d of %d meaningful", mean_dabs, round( 100 * flip_rate ), n_meaningful, n_terms ) ]
  lv_term <- c( "Global score", term_lv, "  Options" )
  fix <- function( z ) { z[ , dec := factor( dec, dec_lv ) ]; z[ , term := factor( term, lv_term, c( "Global score", LAB[ term_lv ], "  Options" ) ) ]; z }
  st <- fix( st ); ar <- fix( ar ); ce <- fix( ce ); mn <- fix( mn ); rng <- fix( rng ); key <- fix( key ); gl <- fix( gl )
  ggplot() +
    geom_vline( data = CJ( dec = factor( dec_lv, dec_lv ) ), aes( xintercept = 0 ), colour = "grey80", linewidth = .3 ) +
    geom_point( data = st, aes( pr, yy ), colour = st$scol, size = .25, alpha = .45 ) +
    geom_point( data = ar, aes( median, -.09 ), colour = "black", size = 1.8 ) +
    geom_text( data = ar, aes( XL[1], .64, label = txt ), hjust = 0, vjust = 1, size = 2, lineheight = .9 ) +
    geom_segment( data = rng, aes( x = lo, xend = hi, y = 0, yend = 0, colour = hot ), linewidth = .5 ) +
    scale_colour_manual( values = c( "TRUE" = "grey35", "FALSE" = "grey80" ), guide = "none" ) +
    geom_point( data = mn, aes( mean_pr, 0 ), fill = mn$col, colour = "white", shape = 21, size = 2.4, stroke = .3 ) +
    geom_text( data = ce, aes( XL[2], .58, label = sprintf( "η² %s", sub( "^0\\.", ".", sprintf( "%.2f", eta2 ) ) ),
                               fontface = ifelse( hot, "bold", "plain" ) ), hjust = 1, vjust = 1, size = 2.2, colour = ifelse( ce$hot, "black", "grey55" ) ) +
    geom_text( data = key, aes( x, yy, label = opt_lab ), colour = key$col, hjust = 0, size = 2, fontface = "bold" ) +
    geom_text( data = gl, aes( XL[1] + .02, 0, label = txt ), hjust = 0, size = 2.2, lineheight = .9 ) +
    facet_grid( term ~ dec, switch = "y" ) +
    scale_x_continuous( limits = XL, breaks = c( -.3, 0, .3 ), labels = c( "−.3", "0", ".3" ) ) +
    scale_y_continuous( limits = c( -.65, .65 ) ) +
    labs( x = "Partial r", y = NULL,
          title = sprintf( "What each design decision does to each covariate, %s (run %s)", GTITLE[[ g ]], VL ),
          subtitle = sprintf( paste0( "First column: every specification's partial r (blue: significantly positive; orange: significantly negative; gray: n.s.), median (black). Decision columns: mean partial r of each option in the default model;\n",
                                      "colored when the decision explains η² ≥ %.2f of the covariate's spread, gray otherwise. Columns ordered by mean |Δr| in matched pairs." ), ETA_MIN ) ) +
    theme_minimal( base_size = 8 ) +
    theme( panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           strip.text.y.left = element_text( angle = 0, hjust = 1, size = 7.5 ), strip.text.x = element_text( size = 7, face = "bold" ),
           strip.placement = "outside", panel.spacing.x = unit( 4, "pt" ), panel.spacing.y = unit( 2, "pt" ),
           panel.border = element_rect( colour = "grey90", fill = NA, linewidth = .3 ),
           axis.text.x = element_text( size = 6, colour = "grey50" ), plot.title.position = "plot",
           plot.title = element_text( face = "bold", size = 10 ), plot.subtitle = element_text( size = 7.5, colour = "grey30" ) )
}
## ---- vertical version: decisions as rows, covariates as columns, partial r on the y axis ---------------
## one accent color for a decision that matters (eta^2 >= ETA_MIN), gray otherwise; options told apart by
## shape, with the shape key in each row label
SHP <- c( 21, 22, 24, 23, 25, 4 )                                   # circle, square, triangle, diamond, down-triangle, cross
SHP_TXT <- c( "●", "■", "▲", "◆", "▼", "✕" )
ACCENT <- "#2a78d6"
figv <- function( g, terms_page, page = 1L, n_pages = 1L ) {
  gl <- glob[ group == g ][ order( -mean_dabs ) ]
  mn <- merge( means[ group == g & term %in% terms_page ], cells[ group == g, .( decision, term, eta2 ) ], by = c( "decision", "term" ) )
  mn[ , hot := eta2 >= ETA_MIN ]
  mn[ , k := max( oi ), by = decision ]
  mn[ , y := -( oi - ( k + 1 ) / 2 ) * .9 ]                          # option 1 at the top
  ## label column: decision name (bold, blue) over its shape key (plain), wrapped to ~34 characters a line
  wrap_key <- function( parts, width = 34 ) {
    lines <- character( 0 ); cur <- ""
    for ( p in parts ) { cand <- if ( nchar( cur ) ) paste0( cur, "   ", p ) else p
      if ( nchar( cand ) > width && nchar( cur ) ) { lines <- c( lines, cur ); cur <- p } else cur <- cand }
    paste( c( lines, cur ), collapse = "\n" ) }
  keys <- unique( means[ group == g, .( decision, opt_lab, oi ) ] )[ order( decision, oi ) ][
    , .( key = wrap_key( sprintf( "%s %s", SHP_TXT[ pmin( oi, 6 ) ], opt_lab ) ) ), by = decision ]
  gl <- merge( gl, keys, by = "decision" )[ order( -mean_dabs ) ]
  gl[ , row := decision ]
  row_lv <- c( "ALL", gl$decision )
  mn <- merge( mn, gl[ , .( decision, row ) ], by = "decision" )
  ce <- merge( cells[ group == g & term %in% terms_page ], gl[ , .( decision, row ) ], by = "decision" )[ , hot := eta2 >= ETA_MIN ]
  set.seed( 1 )
  st <- d[ eval( GROUPS[[ g ]] ) & term %in% terms_page ][ , .SD[ sample( .N, min( .N, 500 ) ) ], by = term ]
  st[ , `:=`( row = "ALL", y = runif( .N, -2.2, 1.6 ),
              scol = fcase( cls > 0, "#2a78d6", cls < 0, "#eb6834", default = "grey65" ) ) ]
  mfmt <- function( x ) sub( "^-", "−", sub( "^(-?)0\\.", "\\1.", sprintf( "%.2f", x ) ) )
  ar <- allr[ group == g & term %in% terms_page ][ , `:=`( row = "ALL",
                         txt = sprintf( "(%s) (%d%%) (%d%%)", mfmt( median ), round( 100 * agree ), round( 100 * sig ) ) ) ]
  XLv <- c( -.5, .62 ); YT <- 3.25
  ## three kinds of columns, each on its own stretch of the x axis (free x scales, widths by range):
  ## labels at -20, covariates at XLv, Impact at XLv + 10 (so the partial-r ticks show only under covariates)
  LBX <- c( -21.9, -20 ); IOF <- 10
  LBC <- "LABELS"; IMP <- "IMPACT"
  lb <- rbind( data.table( row = "ALL", name = "All specifications", keytxt = "(median) (% agree in sign)\n(% significant)" ),
               gl[ , .( row, name = gsub( "\n", " ", label ), keytxt = key ) ] )[ , term := LBC ]
  ## Impact column: computed over all covariates, so identical on every page
  im <- copy( gl )[ , term := IMP ]
  im[ , txt := sprintf( "|Δr| %s\n%d%% verdicts change\n%d of %d covariates", sub( "^0\\.", ".", sprintf( "%.3f", mean_dabs ) ),
                        round( 100 * flip_rate ), n_meaningful, n_terms ) ]
  bmax <- max( gl$mean_dabs )
  im[ , bx := IOF + XLv[1] + .02 + ( mean_dabs / bmax ) * .55 * diff( XLv ) ]
  imh <- data.table( row = "ALL", term = IMP,
                     txt = "mean |Δr| in matched pairs\n% of pairs whose verdict\nchanges (+ / n.s. / −)\ncovariates with η² ≥ .06" )
  term_lv <- c( LBC, terms_page, IMP ); term_lb <- c( " ", LAB[ terms_page ], "Impact\n(all covariates)" )
  fix <- function( z ) { z[ , row := factor( row, row_lv ) ]; z[ , term := factor( term, term_lv, term_lb ) ]; z }
  ## panel extents (geom_blank) and frames for the data columns
  ext <- rbind( CJ( row = row_lv, term = LBC, x = LBX ), CJ( row = row_lv, term = terms_page, x = XLv ),
                CJ( row = row_lv, term = IMP, x = IOF + XLv ) )
  frm <- rbind( CJ( row = row_lv, term = terms_page )[ , `:=`( x0 = XLv[1], x1 = XLv[2] ) ],
                CJ( row = row_lv, term = IMP )[ , `:=`( x0 = IOF + XLv[1], x1 = IOF + XLv[2] ) ] )
  zl <- CJ( row = row_lv, term = terms_page )
  mn <- fix( mn ); ce <- fix( ce ); st <- fix( st ); ar <- fix( ar ); im <- fix( im ); imh <- fix( imh )
  lb <- fix( lb ); ext <- fix( ext ); frm <- fix( frm ); zl <- fix( zl )
  mo <- mn[ order( row, term, oi ) ]
  ggplot() +
    geom_blank( data = ext, aes( x, 0 ) ) +
    geom_rect( data = frm, aes( xmin = x0, xmax = x1, ymin = -2.9, ymax = YT ), fill = NA, colour = "grey90", linewidth = .3 ) +
    geom_vline( data = zl, aes( xintercept = 0 ), colour = "grey80", linewidth = .3 ) +
    geom_text( data = lb, aes( LBX[2] - .05, YT - .55, label = name ), hjust = 1, vjust = 1, size = 2.5, fontface = "bold", colour = ACCENT ) +
    geom_text( data = lb, aes( LBX[2] - .05, YT - 1.75, label = keytxt ), hjust = 1, vjust = 1, size = 2.15, lineheight = 1.05, colour = "grey15" ) +
    geom_point( data = st, aes( pr, y ), colour = st$scol, size = .25, alpha = .45 ) +
    geom_point( data = ar, aes( median, -.3 ), colour = "black", size = 1.6 ) +
    geom_text( data = ar, aes( XLv[1] + .02, YT, label = txt ), hjust = 0, vjust = 1, size = 1.9 ) +
    geom_path( data = mo, aes( mean_pr, y, group = interaction( row, term ) ),
               colour = ifelse( mo$hot, ACCENT, "grey80" ), linewidth = .35 ) +
    geom_point( data = mn, aes( mean_pr, y, shape = factor( pmin( oi, 6 ) ) ), fill = ifelse( mn$hot, ACCENT, "grey75" ),
                colour = ifelse( mn$hot, ifelse( pmin( mn$oi, 6 ) == 6, ACCENT, "white" ), ifelse( pmin( mn$oi, 6 ) == 6, "grey60", "white" ) ),
                size = 2, stroke = .35 ) +
    scale_shape_manual( values = setNames( SHP, 1:6 ), guide = "none" ) +
    geom_text( data = ce, aes( XLv[2], YT, label = sub( "^0\\.", ".", sprintf( "%.2f", eta2 ) ), fontface = ifelse( hot, "bold", "plain" ) ),
               hjust = 1, vjust = 1, size = 1.9, colour = ifelse( ce$hot, "black", "grey60" ) ) +
    geom_rect( data = im, aes( xmin = IOF + XLv[1] + .02, xmax = bx, ymin = 1.55, ymax = 2.55 ), fill = ACCENT, alpha = .8 ) +
    geom_text( data = im, aes( IOF + XLv[1] + .02, 1.05, label = txt ), hjust = 0, vjust = 1, size = 2, lineheight = .9 ) +
    geom_text( data = imh, aes( IOF + XLv[1] + .02, 2.3, label = txt ), hjust = 0, vjust = 1, size = 1.9, lineheight = .9, colour = "grey30" ) +
    facet_grid( row ~ term, scales = "free_x", space = "free_x" ) +
    scale_x_continuous( breaks = c( -.3, 0, .3 ), labels = c( "−.3", "0", ".3" ), expand = c( 0, 0 ) ) +
    scale_y_continuous( limits = c( -2.9, YT ), expand = c( 0, 0 ) ) +
    labs( x = "Partial r", y = NULL,
          title = sprintf( "What each design decision does to each covariate: %s (run %s)%s",
                           GTITLE[[ g ]], VL,
                           if ( n_pages > 1 ) sprintf( ", part %d of %d", page, n_pages ) else "" ),
          subtitle = sprintf( paste0( "Top row: every specification's partial r (blue: significantly positive; orange: significantly negative; gray: n.s.); black dot = median;\n",
                                      "text = (median) (%% agreeing in sign) (%% significant). Other rows: mean partial r of each option, top to bottom in the order of the key on the left;\n",
                                      "blue when the decision explains η² ≥ %.2f of the covariate's spread (number at top right), gray otherwise. Rows ordered by impact." ), ETA_MIN ) ) +
    theme_minimal( base_size = 8 ) +
    theme( panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
           axis.text.x = element_text( size = 5.5, colour = "grey55" ),
           strip.text.y = element_blank(), strip.text.x = element_text( size = 7, face = "bold" ),
           panel.spacing.x = unit( 3, "pt" ), panel.spacing.y = unit( 3, "pt" ), plot.title.position = "plot",
           plot.title = element_text( face = "bold", size = 10 ), plot.subtitle = element_text( size = 7, colour = "grey30" ) )
}

for ( g in names( GROUPS ) ) {
  p <- fig( g )
  nr <- length( TERMS ) + 2; nc <- nrow( glob[ group == g ] ) + 1
  ggsave( file.path( FIG, sprintf( "decision_matrix_%s_%s.png", VL, g ) ), p, width = 1.15 * nc + 1.3, height = .52 * nr + 1.2,
          dpi = 200, bg = "white", limitsize = FALSE )
  ## vertical version: covariates ordered by |median|, split into pages of at most 7 (6 + 7 for 13)
  tl <- allr[ group == g ][ order( -abs( median ) ) ]$term
  np <- ceiling( length( tl ) / 7 )
  pages <- split( tl, rep( seq_len( np ), times = diff( c( 0, round( seq_len( np ) * length( tl ) / np ) ) ) ) )
  nrv <- nrow( glob[ group == g ] ) + 1
  for ( i in seq_along( pages ) ) {
    pv <- figv( g, pages[[ i ]], i, length( pages ) )
    ggsave( file.path( FIG, sprintf( "decision_matrix_%s_%s_vertical%s.png", VL, g, if ( length( pages ) > 1 ) paste0( "_p", i ) else "" ) ), pv,
            width = .95 * ( length( pages[[ i ]] ) + 1 ) + 2.6, height = .78 * nrv + 1.1,
            dpi = 200, bg = "white", limitsize = FALSE, device = ragg::agg_png )
  }
}

log_msg( "done: ", OD )
