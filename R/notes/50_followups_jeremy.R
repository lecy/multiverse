## 50_followups_jeremy.R
## Follow-up analyses after the first principled-universe run (questions from
## the write-up for Jeremy, Nov 12 2026). Reads the assembled panel and the
## e-file org-year table; writes only to D:/MULTIVERSE/data/followup/.
##   Part 1  state vs metro/CSA fixed effects: multi-state metros, FE sensitivity
##   Part 2  SAMI persistence (lag correlations, rank mobility, between share)
##   Part 3  subsector (NCCS 12 groups) SAMIs: scaling exponents, correlations
##   Part 4  economy SAMIs (GDP, income, wages, establishments) vs nonprofit SAMIs
##   Part 5  pass-through grants: scaling exponents gross vs net
##   Part 6  metro-demeaned covariates vs SAMI, faceted scatterplots
## Usage: Rscript R/notes/50_followups_jeremy.R [part numbers, default all]

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ); library( ggplot2 ) } )
setFixest_notes( FALSE )

FU <- file.path( BUILD, "data", "followup" ); dir.create( FU, showWarnings = FALSE )
PARTS <- if ( length( commandArgs( TRUE ) ) ) as.integer( commandArgs( TRUE ) ) else 1:6

d <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
m <- d[ cbsa_type == "metro" ]
## log of an extensive covariate with a per-capita floor (as flog() in 40_multiverse.R, run v6 on;
## switched here 2026-11-16). log(x + 1) put zero-asset metros (Eagle Pass, TX) ~11 SD below the mean.
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]

## year-by-year SAMI: residual of log y on log P within each year
sami_vec <- function( y, P, plus1 = FALSE ) {
  ly <- if ( plus1 ) log( pmax( y, 0 ) + 1 ) else suppressWarnings( log( y ) )
  ok <- is.finite( ly ) & !is.na( P ) & P > 0
  out <- rep( NA_real_, length( y ) )
  if ( sum( ok ) > 10 ) out[ ok ] <- resid( lm( ly[ ok ] ~ log( P[ ok ] ) ) )
  out
}
add_sami <- function( dt, vars, plus1 = FALSE ) {
  for ( v in vars ) dt[ , ( paste0( "s_", v ) ) := sami_vec( get( v ), population, plus1 ), by = year ]
  dt[]
}

theme_fu <- theme_minimal( base_size = 10 ) +
  theme( panel.grid.minor = element_blank(), strip.text = element_text( face = "bold", hjust = 0 ),
         plot.title.position = "plot", legend.position = "top" )
C1 <- "#2a78d6"; C2 <- "#eb6834"; C3 <- "#1baf7a"

## ===========================================================================
## Part 1: state FE vs metro / CSA FE
## ===========================================================================
if ( 1 %in% PARTS ) {
  log_msg( "Part 1: fixed effects" )
  sp <- fread( file.path( OUT$geo, "county_spine.csv" ),
               colClasses = list( character = c( "geoid", "cbsa", "csa", "state_fips" ) ) )
  cp <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )[ year == 2022 ]
  sp <- merge( sp, cp[ , .( geoid, population ) ], by = "geoid" )
  ms <- merge( sp[ cbsa_type == "metro" ], m[ year == 2022, .( cbsa, state_assigned = state_fips ) ], by = "cbsa" )
  per <- ms[ , .( pop = sum( population ), n_states = uniqueN( state_fips ),
                  pop_outside = sum( population[ state_fips != state_assigned ] ), csa = csa[ 1 ] ), by = cbsa ]
  csa_n <- per[ !is.na( csa ) & csa != "", .( metros_in_csa = .N ), by = csa ]
  per <- merge( per, csa_n, by = "csa", all.x = TRUE )
  fe_summary <- data.table(
    metros = nrow( per ), multi_state = per[ n_states > 1, .N ],
    pop_share_in_multistate = per[ n_states > 1, sum( pop ) ] / per[ , sum( pop ) ],
    pop_share_outside_assigned_state = per[ , sum( pop_outside ) ] / per[ , sum( pop ) ],
    max_outside_share = per[ , max( pop_outside / pop ) ],
    metros_in_any_csa = per[ !is.na( metros_in_csa ), .N ],
    metros_sharing_csa = per[ metros_in_csa >= 2, .N ],
    csas_with_2plus_metros = csa_n[ metros_in_csa >= 2, .N ] )
  print( r3( fe_summary ) )
  fwrite( fe_summary, file.path( FU, "p1_fe_geography.csv" ) )
  fwrite( per[ n_states > 1 ][ order( -pop ) ][ , share_outside := pop_outside / pop ],
          file.path( FU, "p1_multistate_metros.csv" ) )

  DIV <- c( "09" = 1, "23" = 1, "25" = 1, "33" = 1, "44" = 1, "50" = 1, "34" = 2, "36" = 2, "42" = 2,
            "17" = 3, "18" = 3, "26" = 3, "39" = 3, "55" = 3, "19" = 4, "20" = 4, "27" = 4, "29" = 4,
            "31" = 4, "38" = 4, "46" = 4, "10" = 5, "11" = 5, "12" = 5, "13" = 5, "24" = 5, "37" = 5,
            "45" = 5, "51" = 5, "54" = 5, "01" = 6, "21" = 6, "28" = 6, "47" = 6, "05" = 7, "22" = 7,
            "40" = 7, "48" = 7, "04" = 8, "08" = 8, "16" = 8, "30" = 8, "32" = 8, "35" = 8, "49" = 8,
            "56" = 8, "02" = 9, "06" = 9, "15" = 9, "41" = 9, "53" = 9 )
  x <- merge( m[ year == 2022 ], per[ , .( cbsa, n_states, csa, metros_in_csa ) ], by = "cbsa" )
  x[ , division := DIV[ state_fips ] ]
  x[ , csa_g := fifelse( !is.na( metros_in_csa ) & metros_in_csa >= 2, csa, paste0( "solo_", cbsa ) ) ]
  x[ , `:=`( l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ), logP = log( population ) ) ]
  rhs <- "race_hhi + l_pf + turnout + l_gov + poverty_rate + pct_ba + logP"
  variants <- list( "No FE" = list( fe = NULL, single = FALSE ),
                    "Census division FE" = list( fe = "division", single = FALSE ),
                    "State FE" = list( fe = "state_fips", single = FALSE ),
                    "State FE, single-state metros" = list( fe = "state_fips", single = TRUE ),
                    "CSA FE" = list( fe = "csa_g", single = FALSE ) )
  fe_res <- rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y ) rbindlist( lapply( names( variants ), function( vn ) {
    v <- variants[[ vn ]]
    dd <- x[ get( y ) > 0 & ( !v$single | n_states == 1 ) ]
    f <- as.formula( paste( "log(", y, ") ~", rhs, if ( !is.null( v$fe ) ) paste( "|", v$fe ) ) )
    r <- feols( f, dd, vcov = "hetero" )
    ct <- coeftable( r ); dfr <- degrees_freedom( r, "resid" )
    eff <- if ( is.null( v$fe ) ) nobs( r ) else dd[ , .N, by = c( v$fe ) ][ N >= 2, sum( N ) ]
    data.table( outcome = y, variant = vn, term = rownames( ct ), estimate = ct[ , 1 ], se = ct[ , 2 ],
                t = ct[ , 3 ], p = ct[ , 4 ], partial_r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + dfr ),
                n = nobs( r ), n_identifying = eff,
                groups = if ( is.null( v$fe ) ) NA_integer_ else uniqueN( dd[[ v$fe ]] ) )
  } ) ) ) )
  fwrite( fe_res, file.path( FU, "p1_fe_sensitivity.csv" ) )
  print( r3( dcast( fe_res, outcome + term ~ variant, value.var = "partial_r" ) ) )
  print( unique( fe_res[ , .( outcome, variant, n, n_identifying, groups ) ] ) )
}

## ===========================================================================
## Part 2: SAMI persistence
## ===========================================================================
## grassroots = registered public charities that are not filers (see 72_catchup_lag.R)
m[ , n_grass_c3pc := pmax( n_bmf_c3pc - n_filer_c3, 0 ) ]
PERS_VARS <- c( n_filer_c3 = "Filers (501c3)", n_grass_c3pc = "Grassroots (not filing)",
                exp_c3 = "Expenditures", exp_c3_nohu = "Expenditures excl. hosp/univ",
                exp_c3_nohu_net = "Exp. excl. h/u, net of pass-through",
                gdp_real = "Real GDP (benchmark)", personal_income = "Personal income (benchmark)" )
if ( 2 %in% PARTS ) {
  log_msg( "Part 2: SAMI persistence" )
  p <- add_sami( copy( m[ , c( "cbsa", "year", "population", names( PERS_VARS ) ), with = FALSE ] ), names( PERS_VARS ) )
  one <- function( v ) {
    s <- p[ , .( cbsa, year, s = get( paste0( "s_", v ) ) ) ][ is.finite( s ) ]
    w <- dcast( s, cbsa ~ year, value.var = "s" ); yrs <- as.integer( names( w )[ -1 ] )
    M <- as.matrix( w[ , -1 ] ); colnames( M ) <- yrs
    C  <- cor( M, use = "pairwise.complete.obs" )
    Sp <- cor( M, use = "pairwise.complete.obs", method = "spearman" )
    lagc <- sapply( 1:20, function( k ) { i <- which( outer( yrs, yrs, "-" ) == -k, arr.ind = TRUE )
      if ( !nrow( i ) ) NA_real_ else mean( C[ i ] ) } )
    pr  <- apply( M, 2, function( z ) rank( z, na.last = "keep" ) / sum( !is.na( z ) ) * 100 )
    dpr <- abs( pr[ , -1 ] - pr[ , -ncol( pr ) ] )
    q   <- apply( M, 2, function( z ) ceiling( rank( z, na.last = "keep" ) / sum( !is.na( z ) ) * 5 ) )
    tot <- s[ , var( s ) ]; btw <- s[ , .( mu = mean( s ) ), by = cbsa ][ , var( mu ) ]
    sy <- function( a, b ) if ( all( c( a, b ) %in% colnames( Sp ) ) ) Sp[ a, b ] else NA_real_
    list(
      summary = data.table( var = v, label = PERS_VARS[[ v ]], first = min( yrs ), last = max( yrs ),
        r_lag1 = lagc[ 1 ], r_lag5 = lagc[ 5 ], r_lag10 = lagc[ 10 ], r_lag20 = lagc[ 20 ],
        spearman_first_last = Sp[ 1, ncol( Sp ) ], spearman_2012_2022 = sy( "2012", "2022" ),
        between_share = btw / tot,
        median_abs_pctile_change = median( dpr, na.rm = TRUE ),
        p90_abs_pctile_change = unname( quantile( dpr, .9, na.rm = TRUE ) ),
        same_quintile_yoy = mean( q[ , -1 ] == q[ , -ncol( q ) ], na.rm = TRUE ),
        same_quintile_first_last = mean( q[ , 1 ] == q[ , ncol( q ) ], na.rm = TRUE ),
        sd_sami = sqrt( tot ) ),
      decay = data.table( var = v, label = PERS_VARS[[ v ]], lag = 1:20, r = lagc ),
      lag1  = data.table( var = v, label = PERS_VARS[[ v ]], year = yrs[ -1 ],
                          r = sapply( 2:length( yrs ), function( j ) C[ j - 1, j ] ) ),
      ranks = data.table( cbsa = w$cbsa, var = v, pr ) )
  }
  res <- lapply( names( PERS_VARS ), one )
  pers  <- rbindlist( lapply( res, `[[`, "summary" ) )
  decay <- rbindlist( lapply( res, `[[`, "decay" ) )
  lag1  <- rbindlist( lapply( res, `[[`, "lag1" ) )
  fwrite( pers, file.path( FU, "p2_persistence_summary.csv" ) )
  fwrite( decay, file.path( FU, "p2_persistence_decay.csv" ) )
  fwrite( lag1, file.path( FU, "p2_persistence_lag1_by_year.csv" ) )
  print( r3( pers[ , -"label" ] ) )

  lv <- unname( PERS_VARS )
  cols <- setNames( c( C1, "#5598e7", C2, C3, "#86d4b4", "#8a8f98", "#c3c8d0" ), lv )
  g1 <- ggplot( decay[ is.finite( r ) ], aes( lag, r, colour = factor( label, lv ) ) ) +
    geom_line( linewidth = 0.8 ) + geom_point( size = 1.2 ) +
    scale_colour_manual( values = cols, name = NULL ) + scale_y_continuous( limits = c( NA, 1 ) ) +
    labs( title = "A. SAMI correlation k years apart", x = "Lag (years)", y = "Pearson r, averaged over start years" ) +
    theme_fu + guides( colour = guide_legend( nrow = 3 ) )
  ## expenditure series drop the splice years (2010->2011 CORE to SOI, 2019->2020 SOI to
  ## e-file), whose dips reflect the change of source; their lines join 2010 to 2012 and
  ## 2019 to 2021, and a grey dot marks each omitted year on the joining line
  SPLICE <- c( 2011, 2020 )
  EXPV <- c( "exp_c3", "exp_c3_nohu", "exp_c3_nohu_net" )
  lag1p <- lag1[ !( var %in% EXPV & year %in% SPLICE ) ]
  gap <- lag1[ var %in% EXPV & year %in% SPLICE, .( var, label, year ) ]
  gap[ , r := mapply( function( v, y ) mean( lag1[ var == v & year %in% c( y - 1, y + 1 ), r ] ), var, year ) ]
  gap <- gap[ is.finite( r ) ]
  g2 <- ggplot( lag1p, aes( year, r, colour = factor( label, lv ) ) ) +
    geom_vline( xintercept = SPLICE, linetype = 3, colour = "grey70" ) +
    geom_line( linewidth = 0.8 ) + geom_point( size = 1.2 ) +
    geom_point( data = gap, colour = "grey55", fill = "white", shape = 21, size = 1.8, stroke = 0.8 ) +
    scale_colour_manual( values = cols, guide = "none" ) +
    annotate( "text", x = SPLICE, y = -Inf, vjust = -0.6, size = 2.6, colour = "grey45",
              label = c( "CORE to SOI", "SOI to e-file" ) ) +
    labs( title = "B. Year-to-year correlation, by year",
          subtitle = "Grey dots: expenditure values omitted in the source-change years (lines joined across)",
          x = NULL, y = "r( SAMI t-1 , SAMI t )" ) + theme_fu + theme( plot.subtitle = element_text( size = 8, colour = "grey40" ) )
  ggsave( file.path( FU, "fig_p2_persistence.png" ), patchwork::wrap_plots( g1, g2, ncol = 2 ),
          width = 10, height = 4.4, dpi = 160, bg = "white" )

  ## rank trajectories for the 2000 extremes and the largest metros (filers)
  rk <- res[[ 1 ]]$ranks; rkl <- melt( rk, id.vars = c( "cbsa", "var" ), variable.name = "year", value.name = "pctile" )
  rkl[ , year := as.integer( as.character( year ) ) ]
  rkl <- merge( rkl, m[ year == 2022, .( cbsa, cbsa_title, population ) ], by = "cbsa" )
  big <- rkl[ year == 2022 ][ order( -population ) ][ 1:12, cbsa ]
  g3 <- ggplot( rkl, aes( year, pctile, group = cbsa ) ) +
    geom_line( colour = "grey85", linewidth = 0.25 ) +
    geom_line( data = rkl[ cbsa %in% big ], colour = C1, linewidth = 0.6 ) +
    geom_text( data = rkl[ cbsa %in% big & year == max( year ) ], aes( label = sub( "[-,].*", "", cbsa_title ) ),
               hjust = -0.05, size = 2.6, colour = "grey20" ) +
    scale_x_continuous( expand = expansion( add = c( 0.5, 4 ) ) ) +
    labs( title = "Percentile rank of the filer-count SAMI, 387 metros (12 largest highlighted)", x = NULL, y = "Percentile" ) + theme_fu
  ggsave( file.path( FU, "fig_p2_rank_paths.png" ), g3, width = 10, height = 4.6, dpi = 160, bg = "white" )

  ## the same paths split three ways: steady metros, climbers, decliners. Change is the
  ## percentile at 2022-24 minus 2000-02 (3-year averages, so one noisy year does not decide it)
  rkl[ , short := paste0( sub( "[-/].*", "", sub( ",.*", "", cbsa_title ) ), ", ", sub( "-.*", "", sub( ".*, ", "", cbsa_title ) ) ) ]
  mv <- rkl[ , .( p0 = mean( pctile[ year %in% 2000:2002 ] ), p1 = mean( pctile[ year %in% 2022:2024 ] ),
                  sd_path = sd( pctile ), pop = population[ 1 ] ), by = .( cbsa, short ) ][ , dp := p1 - p0 ]
  NSEL <- 12
  up <- mv[ order( -dp ) ][ 1:NSEL ]; dn <- mv[ order( dp ) ][ 1:NSEL ]
  ## steady: |change| < 3 points and the least wandering path, one per band of the starting percentile
  ## so the panel shows stability at every level of density
  st <- mv[ abs( dp ) < 3 ][ , band := cut( p0, seq( 0, 100, length.out = NSEL + 1 ), include.lowest = TRUE ) ][
    order( sd_path ) ][ , .SD[ 1 ], by = band ]
  pick <- rbind( up[ , grp := "Climbers" ], dn[ , grp := "Decliners" ], st[ , .( cbsa, short, p0, p1, sd_path, pop, dp, grp = "Steady" ) ] )
  fwrite( pick, file.path( FU, "p2_rank_paths_selected.csv" ) )
  GRP <- c( Steady = "#3d4450", Climbers = C1, Decliners = C2 )
  TTL <- c( Steady = sprintf( "A. Steady: %d metros with the least change and the least wandering, one per decile band of the 2000 rank", nrow( st ) ),
            Climbers = sprintf( "B. Climbers: the %d largest gains in percentile rank, 2000-02 to 2022-24", NSEL ),
            Decliners = sprintf( "C. Decliners: the %d largest losses in percentile rank, 2000-02 to 2022-24", NSEL ) )
  one_panel <- function( gname ) {
    sel <- pick[ grp == gname ]; hl <- rkl[ cbsa %in% sel$cbsa ]
    lab <- merge( hl[ year == max( year ) ], sel[ , .( cbsa, dp ) ], by = "cbsa" )
    lab[ , txt := if ( gname == "Steady" ) short else sprintf( "%s (%+.0f)", short, dp ) ]
    ggplot( rkl, aes( year, pctile, group = cbsa ) ) +
      geom_line( colour = "grey90", linewidth = 0.2 ) +
      geom_line( data = hl, colour = GRP[[ gname ]], linewidth = 0.55, alpha = 0.9 ) +
      ggrepel::geom_text_repel( data = lab, aes( label = txt ), direction = "y", hjust = 0, nudge_x = 0.6, size = 2.4,
                                colour = "grey20", segment.colour = "grey70", segment.size = 0.2, min.segment.length = 0,
                                xlim = c( max( rkl$year ) + 0.5, NA ), seed = 1, max.overlaps = 50 ) +
      scale_x_continuous( breaks = c( 2000, 2005, 2010, 2015, 2020, 2024 ), expand = expansion( add = c( 0.5, 7 ) ) ) +
      scale_y_continuous( limits = c( 0, 100 ), breaks = c( 0, 25, 50, 75, 100 ) ) +
      coord_cartesian( clip = "off" ) +
      labs( title = TTL[[ gname ]], x = NULL, y = "Percentile" ) + theme_fu + theme( plot.title = element_text( size = 10 ) ) }
  g4 <- patchwork::wrap_plots( lapply( names( GRP ), one_panel ), ncol = 1 ) +
    patchwork::plot_annotation( title = "Percentile rank of the filer-count SAMI, 387 metros",
                                subtitle = "Grey: all metros. Labels give the change in percentile points, 2000-02 to 2022-24." )
  ggsave( file.path( FU, "fig_p2_rank_paths_split.png" ), g4, width = 10, height = 11, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part 3: subsector SAMIs (NCCS 12 groups), e-file 2021-2023
## ===========================================================================
GRP <- c( ART = "Arts", EDU = "Education", UNI = "Universities", ENV = "Environment/animals",
          HEL = "Health", HOS = "Hospitals", HMS = "Human services", IFA = "International",
          PSB = "Public/societal benefit", REL = "Religion-related", MMB = "Mutual/membership", UNU = "Unknown" )
if ( any( c( 3, 4 ) %in% PARTS ) ) {
  log_msg( "Part 3: subsectors" )
  pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )
  sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )
  ef <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ),
          col_select = c( "ein", "year", "subsec_group", "rev", "exp", "ntee3", "ind", "geoid" ) ) )[
          year %in% 2021:2023 & subsec_group == "c3" & !is.na( geoid ) ]
  ef <- merge( ef, pce[ , .( year, to2022, floor_nom ) ], by = "year" )
  ef <- ef[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
  ## fill unknown industry (e-file NTEE "UND"/"INV"/"Z99"/blank -> ind UNU or NA) from the EIN's most
  ## recent valid BMF NTEE code. Invalid BMF codes are skipped (2026-11-17, same rule as 06_npo_assemble.R
  ## and 23_cov_compete.R); before, a latest BMF code of "UND" or "INV" was mapped to PSB or HMS.
  bad_ntee <- function( x ) is.na( x ) | x == "" | toupper( substr( x, 1, 3 ) ) %in% c( "UND", "INV", "Z99" )
  bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "ntee3" ),
                colClasses = list( character = c( "EIN", "ntee3" ) ) )[ !bad_ntee( ntee3 ) ]
  bmf <- unique( bmf[ order( EIN, -year ) ], by = "EIN" )[ , .( ein = EIN, ntee_bmf = toupper( substr( ntee3, 1, 3 ) ) ) ]
  ef <- merge( ef, bmf, by = "ein", all.x = TRUE )
  map_ntee <- function( n ) { a <- substr( n, 1, 1 )
    fcase( n %in% NTEE_UNIV, "UNI", n %in% NTEE_HOSP, "HOS", a == "A", "ART", a == "B", "EDU",
           a %in% c( "C", "D" ), "ENV", a %in% c( "E", "F", "G", "H" ), "HEL", a %in% LETTERS[ 9:16 ], "HMS",
           a == "Q", "IFA", a %in% LETTERS[ 18:23 ], "PSB", a == "X", "REL", a == "Y", "MMB", default = "UNU" ) }
  ef[ , grp := fifelse( is.na( ind ) | ind == "UNU", map_ntee( ntee_bmf ), ind ) ]
  ef[ !grp %in% names( GRP ), grp := "UNU" ]
  cov_note <- data.table( share_unknown_efile = ef[ , mean( is.na( ind ) | ind == "UNU" ) ],
                          share_unknown_after_bmf = ef[ , mean( grp == "UNU" ) ] )
  print( cov_note ); fwrite( cov_note, file.path( FU, "p3_ntee_coverage.csv" ) )
  ef <- merge( ef, sp[ , .( geoid, cbsa, cbsa_type ) ], by = "geoid" )[ cbsa_type == "metro" ]
  sub <- ef[ , .( n = .N, exp = sum( pmax( exp, 0 ) * to2022, na.rm = TRUE ) ), by = .( cbsa, year, grp ) ]
  sub <- CJ( cbsa = unique( m$cbsa ), year = 2021:2023, grp = names( GRP ) )[ sub, on = .( cbsa, year, grp ), `:=`( n = i.n, exp = i.exp ) ]
  sub[ is.na( n ), `:=`( n = 0L, exp = 0 ) ]
  sub <- merge( sub, m[ , .( cbsa, year, population ) ], by = c( "cbsa", "year" ) )
  fwrite( sub, file.path( FU, "p3_subsector_metro_year.csv" ) )

  ## scaling exponents by subsector (2022): OLS on metros with n > 0, and Poisson (keeps zeros)
  s22 <- sub[ year == 2022 ]
  beta_sub <- rbindlist( lapply( names( GRP ), function( g ) {
    z <- s22[ grp == g ]
    o <- coeftable( feols( log( n ) ~ log( population ), z[ n > 0 ], vcov = "hetero" ) )[ 2, ]
    pz <- coeftable( fepois( n ~ log( population ), z, vcov = "hetero" ) )[ 2, ]
    oe <- if ( z[ exp > 0, .N ] > 30 ) coeftable( feols( log( exp ) ~ log( population ), z[ exp > 0 ], vcov = "hetero" ) )[ 2, ] else c( NA, NA )
    data.table( grp = g, label = GRP[[ g ]], orgs = z[ , sum( n ) ], metros_with_zero = z[ n == 0, .N ],
                beta_count_ols = o[ 1 ], se_count_ols = o[ 2 ], beta_count_pois = pz[ 1 ], se_count_pois = pz[ 2 ],
                beta_exp_ols = oe[ 1 ], se_exp_ols = oe[ 2 ] ) } ) )
  tot <- m[ year == 2022 ]
  beta_sub <- rbind( beta_sub, data.table( grp = "ALL", label = "All 501(c)(3) filers", orgs = sum( tot$n_filer_c3 ), metros_with_zero = 0L,
    beta_count_ols = coef( lm( log( n_filer_c3 ) ~ log( population ), tot ) )[ 2 ], se_count_ols = NA,
    beta_count_pois = coef( fepois( n_filer_c3 ~ log( population ), tot ) )[[ "log(population)" ]], se_count_pois = NA,
    beta_exp_ols = coef( lm( log( exp_c3 ) ~ log( population ), tot ) )[ 2 ], se_exp_ols = NA ) )
  fwrite( beta_sub, file.path( FU, "p3_subsector_beta.csv" ) )
  print( r3( beta_sub ) )

  ## subsector count SAMIs: year-specific on log(n + 1), averaged 2021-2023
  sub[ , s := sami_vec( n, population, plus1 = TRUE ), by = .( year, grp ) ]
  ssub <- sub[ , .( s = mean( s ) ), by = .( cbsa, grp ) ]
  W <- dcast( ssub, cbsa ~ grp, value.var = "s" )
  tot3 <- add_sami( copy( m[ year %in% 2021:2023, .( cbsa, year, population, n_filer_c3 ) ] ), "n_filer_c3" )[ , .( ALL = mean( s_n_filer_c3 ) ), by = cbsa ]
  W <- merge( W, tot3, by = "cbsa" )
  keepg <- c( "ALL", setdiff( names( GRP ), c( "UNU" ) ) )
  Csub <- cor( W[ , ..keepg ], use = "pairwise.complete.obs" )
  fwrite( data.table( grp = rownames( Csub ), Csub ), file.path( FU, "p3_subsector_sami_cor.csv" ) )
  fwrite( W, file.path( FU, "p3_subsector_sami_wide.csv" ) )
  lab <- c( ALL = "All filers", GRP )
  ord <- keepg[ hclust( as.dist( 1 - Csub ) )$order ]
  cl <- as.data.table( as.table( Csub ) ); setnames( cl, c( "a", "b", "r" ) )
  cl[ , `:=`( a = factor( lab[ a ], lab[ ord ] ), b = factor( lab[ b ], rev( lab[ ord ] ) ) ) ]
  g <- ggplot( cl, aes( a, b, fill = r ) ) + geom_tile( colour = "white", linewidth = 0.6 ) +
    geom_text( aes( label = sub( "^(-?)0", "\\1", sprintf( "%.2f", r ) ) ), size = 2.7,
               colour = ifelse( abs( cl$r ) > 0.55, "white", "grey15" ) ) +
    scale_fill_gradient2( low = "#e34948", mid = "#f0efec", high = "#2a78d6", limits = c( -1, 1 ), name = "r" ) +
    labs( title = "Correlation of subsector count SAMIs across 387 metros (2021-2023 average)", x = NULL, y = NULL ) +
    theme_fu + theme( axis.text.x = element_text( angle = 40, hjust = 1 ), panel.grid = element_blank() )
  ggsave( file.path( FU, "fig_p3_subsector_cor.png" ), g, width = 8, height = 7, dpi = 160, bg = "white" )

  bs <- beta_sub[ grp != "UNU" ]; bs[ , label := factor( label, bs[ order( beta_count_pois ), label ] ) ]
  bl <- rbind( bs[ , .( label, what = "Count, Poisson", b = beta_count_pois, se = se_count_pois ) ],
               bs[ , .( label, what = "Count, log OLS (n > 0)", b = beta_count_ols, se = se_count_ols ) ],
               bs[ , .( label, what = "Expenditure, log OLS", b = beta_exp_ols, se = se_exp_ols ) ] )
  g <- ggplot( bl, aes( b, label, colour = what ) ) + geom_vline( xintercept = 1, linetype = 2, colour = "grey50" ) +
    geom_errorbarh( aes( xmin = b - 1.96 * se, xmax = b + 1.96 * se ), height = 0, na.rm = TRUE,
                    position = position_dodge( width = 0.6 ) ) +
    geom_point( size = 2, position = position_dodge( width = 0.6 ) ) +
    scale_colour_manual( values = c( C1, "#86b6ef", C2 ), name = NULL ) +
    labs( title = "Scaling exponent by subsector, 387 metros, 2022", x = expression( hat( beta ) ~ "(log Y on log population)" ), y = NULL ) + theme_fu
  ggsave( file.path( FU, "fig_p3_subsector_beta.png" ), g, width = 8, height = 5.4, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part 4: economy SAMIs vs nonprofit SAMIs
## ===========================================================================
ECON <- c( gdp_real = "Real GDP", personal_income = "Personal income", wages_total = "Total wages",
           qcew_priv_estab = "Private establishments", qcew_priv_emp = "Private employment" )
NPO  <- c( n_filer_c3 = "Filers", n_bmf_c3pc = "BMF registrations", exp_c3 = "Expenditures",
           exp_c3_nohu = "Exp. excl. h/u", exp_c3_nohu_net = "Exp. excl. h/u, net", pf_assets = "Foundation assets",
           n_entry_c3pc = "New registrations" )
## patents: fractional counts by inventor location (21_patents.R). Grant lag
## truncates recent filing years, so patents use 2000-2019; SAMIs on log(1 + x)
## because university/nonprofit patents are zero in about a third of metros.
PAT <- c( patents_frac = "Patents, all", patents_corp = "Patents, corporate", patents_univnp = "Patents, univ./nonprofit" )
if ( 4 %in% PARTS ) {
  log_msg( "Part 4: economy SAMIs" )
  econ_block <- function( evars, yrs, plus1, label ) {
    vv <- intersect( c( names( evars ), names( NPO ) ), names( m ) )
    e <- copy( m[ year %in% yrs, c( "cbsa", "year", "population", vv ), with = FALSE ] )
    e <- add_sami( e, intersect( names( NPO ), vv ) )
    e <- add_sami( e, intersect( names( evars ), vv ), plus1 = plus1 )
    sv <- paste0( "s_", vv )
    betw <- e[ , lapply( .SD, mean, na.rm = TRUE ), by = cbsa, .SDcols = sv ]
    ## within: metro-demeaned annual SAMIs, and year-to-year changes
    wi <- copy( e )[ , ( sv ) := lapply( .SD, function( z ) z - mean( z, na.rm = TRUE ) ), by = cbsa, .SDcols = sv ]
    fd <- copy( e )[ order( cbsa, year ) ][ , ( sv ) := lapply( .SD, function( z ) z - shift( z ) ), by = cbsa, .SDcols = sv ]
    cr <- function( dt, how ) rbindlist( lapply( intersect( names( evars ), vv ), function( a ) rbindlist( lapply( intersect( names( NPO ), vv ), function( b )
      data.table( econ = evars[[ a ]], npo = NPO[[ b ]], how = how, window = label,
                  r = cor( dt[[ paste0( "s_", a ) ]], dt[[ paste0( "s_", b ) ]], use = "pairwise.complete.obs" ) ) ) ) ) )
    rbind( cr( betw, "Between metros (period average)" ), cr( wi, "Within metro (demeaned)" ),
           cr( fd, "Within metro (year-to-year change)" ) )
  }
  econ_cor <- rbind( econ_block( ECON, 2012:2022, FALSE, "2012-2022" ), econ_block( PAT, 2000:2019, TRUE, "2000-2019" ) )
  ## subsectors vs economy, between metros: subsector SAMIs are 2021-2023
  ## averages; economy SAMIs over the same years, patents over 2015-2019
  W <- fread( file.path( FU, "p3_subsector_sami_wide.csv" ), colClasses = list( character = "cbsa" ) )
  e3 <- add_sami( copy( m[ year %in% 2021:2023, c( "cbsa", "year", "population", names( ECON ) ), with = FALSE ] ), names( ECON ) )
  p3 <- add_sami( copy( m[ year %in% 2015:2019, c( "cbsa", "year", "population", names( PAT ) ), with = FALSE ] ), names( PAT ), plus1 = TRUE )
  b3 <- Reduce( function( a, b ) merge( a, b, by = "cbsa" ), list(
    e3[ , lapply( .SD, mean, na.rm = TRUE ), by = cbsa, .SDcols = paste0( "s_", names( ECON ) ) ],
    p3[ , lapply( .SD, mean, na.rm = TRUE ), by = cbsa, .SDcols = paste0( "s_", names( PAT ) ) ], W ) )
  EP <- c( ECON, PAT )
  sub_econ <- rbindlist( lapply( names( EP ), function( a ) rbindlist( lapply( c( "ALL", setdiff( names( GRP ), "UNU" ) ), function( g )
    data.table( econ = EP[[ a ]], grp = g, label = c( ALL = "All filers", GRP )[[ g ]],
                r = cor( b3[[ paste0( "s_", a ) ]], b3[[ g ]], use = "pairwise.complete.obs" ) ) ) ) ) )
  fwrite( econ_cor, file.path( FU, "p4_econ_npo_sami_cor.csv" ) )
  fwrite( sub_econ, file.path( FU, "p4_econ_subsector_sami_cor.csv" ) )
  print( r3( dcast( econ_cor, npo + econ ~ how, value.var = "r" ) ) )
  print( r3( dcast( sub_econ, label ~ econ, value.var = "r" ) ) )

  ec <- copy( econ_cor ); ec[ , npo := factor( npo, rev( unname( NPO ) ) ) ][ , econ := factor( paste0( econ, " (", window, ")" ),
    c( paste0( unname( ECON ), " (2012-2022)" ), paste0( unname( PAT ), " (2000-2019)" ) ) ) ]
  g <- ggplot( ec, aes( r, npo, colour = how ) ) + geom_vline( xintercept = 0, colour = "grey60" ) +
    geom_point( size = 2.2 ) + facet_wrap( ~ econ, nrow = 2 ) +
    scale_colour_manual( values = c( C1, C2, C3 ), name = NULL ) +
    scale_x_continuous( limits = c( -0.3, 0.8 ), breaks = c( 0, 0.4, 0.8 ) ) +
    labs( title = "Correlation of nonprofit SAMIs with economic and patent SAMIs, 387 metros", x = "Pearson r", y = NULL ) + theme_fu
  ggsave( file.path( FU, "fig_p4_econ_cor.png" ), g, width = 11, height = 7, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part 5: pass-through grants and the scaling exponent
## ===========================================================================
if ( 5 %in% PARTS ) {
  log_msg( "Part 5: pass-through" )
  MEAS <- c( "exp_c3", "exp_c3_net", "exp_c3_nohu", "exp_c3_nohu_net", "rev_c3", "rev_c3_net", "rev_c3_nohu", "rev_c3_nohu_net" )
  bt <- rbindlist( lapply( c( "metro", "cbsa" ), function( u ) rbindlist( lapply( 2012:2024, function( yy ) rbindlist( lapply( MEAS, function( v ) {
    z <- d[ year == yy & ( u == "cbsa" | cbsa_type == "metro" ) & get( v ) > 0 ]
    ct <- coeftable( feols( as.formula( paste( "log(", v, ") ~ log( population )" ) ), z, vcov = "hetero" ) )[ 2, ]
    data.table( unit = u, year = yy, measure = sub( "_net$", "", v ), net = grepl( "_net$", v ), beta = ct[ 1 ], se = ct[ 2 ] ) } ) ) ) ) ) )
  bw <- dcast( bt, unit + year + measure ~ net, value.var = c( "beta", "se" ) )
  setnames( bw, c( "beta_FALSE", "beta_TRUE", "se_FALSE", "se_TRUE" ), c( "beta_gross", "beta_net", "se_gross", "se_net" ) )
  bw[ , diff := beta_net - beta_gross ]
  ## conditional: baseline 2022 cross-section with default covariates
  x <- m[ year == 2022 ][ , `:=`( l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ) ) ]
  cond <- rbindlist( lapply( MEAS, function( v ) {
    ct <- coeftable( feols( as.formula( paste( "log(", v, ") ~ log( population ) + race_hhi + l_pf + turnout + l_gov + poverty_rate + pct_ba" ) ),
                            x[ get( v ) > 0 ], vcov = "hetero" ) )
    data.table( measure = sub( "_net$", "", v ), net = grepl( "_net$", v ), term = rownames( ct ), est = ct[ , 1 ], t = ct[ , 3 ] ) } ) )
  ## pass-through share by metro size
  sh <- m[ year %in% 2012:2024 & exp_c3 > 0, .( share = 1 - exp_c3_net / exp_c3, share_nohu = 1 - exp_c3_nohu_net / exp_c3_nohu,
                                                 logP = log( population ) ), by = .( cbsa, year ) ]
  sh_sum <- sh[ , .( mean_share = mean( share ), slope_logP = coef( lm( share ~ logP ) )[ 2 ],
                     mean_share_nohu = mean( share_nohu ), slope_logP_nohu = coef( lm( share_nohu ~ logP ) )[ 2 ] ), by = year ]
  fwrite( bw, file.path( FU, "p5_beta_gross_net.csv" ) ); fwrite( cond, file.path( FU, "p5_conditional_gross_net.csv" ) )
  fwrite( sh_sum, file.path( FU, "p5_passthrough_share_by_size.csv" ) )
  print( r3( bw[ year %in% c( 2012, 2016, 2019, 2022, 2024 ) ] ) )
  print( r3( dcast( cond[ term != "(Intercept)" ], measure + term ~ net, value.var = "est" ) ) )
  print( r3( sh_sum ) )
  g <- ggplot( bt[ unit == "metro" ], aes( year, beta, colour = net ) ) +
    geom_ribbon( aes( ymin = beta - 1.96 * se, ymax = beta + 1.96 * se, fill = net ), alpha = 0.12, colour = NA ) +
    geom_line( linewidth = 0.8 ) + facet_wrap( ~ measure, nrow = 1 ) +
    scale_colour_manual( values = c( `FALSE` = C1, `TRUE` = C2 ), labels = c( "Gross", "Net of grants to other organizations" ), name = NULL ) +
    scale_fill_manual( values = c( `FALSE` = C1, `TRUE` = C2 ), guide = "none" ) +
    labs( title = "Scaling exponent, gross vs net of pass-through grants (387 metros)", x = NULL, y = expression( hat( beta ) ) ) + theme_fu
  ggsave( file.path( FU, "fig_p5_passthrough_beta.png" ), g, width = 10, height = 3.8, dpi = 160, bg = "white" )
}

## ===========================================================================
## Part 6: metro-demeaned covariates vs SAMI (faceted)
## ===========================================================================
COV6 <- data.table( var = c( "race_hhi", "pct_nonwhite", "pct_foreign_born", "gini", "age_sd",
  "pf_assets", "n_pf", "contrib_total", "contrib_per_return", "contrib_share_agi",
  "turnout", "dem_margin", "gov_earn_sl", "gov_emp_sl", "gov_emp_share",
  "poverty_rate", "pct_ba", "pcpi", "unemp_rate", "pct_65plus" ),
  label = c( "Racial HHI", "% non-white", "% foreign-born", "Income Gini", "Age dispersion",
  "Foundation assets (SAMI)", "No. foundations (SAMI)", "Contributions total (SAMI)", "Contrib. per return", "Contributions / AGI",
  "Turnout", "Dem. margin", "S&L govt earnings (SAMI)", "S&L govt employment (SAMI)", "Govt employment share",
  "Poverty rate", "% BA+", "Per capita income", "Unemployment", "% 65+" ),
  extensive = c( rep( FALSE, 5 ), TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, rep( FALSE, 6 ) ) )
if ( 6 %in% PARTS ) {
  log_msg( "Part 6: demeaned scatterplots" )
  pz <- copy( m[ year %in% 2010:2022 ] )
  pz <- add_sami( pz, c( "n_filer_c3", "exp_c3_nohu" ) )
  for ( i in which( COV6$extensive ) ) { v <- COV6$var[ i ]
    pz[ , ( paste0( "x_", v ) ) := sami_vec( get( v ), population, plus1 = TRUE ), by = year ] }
  for ( i in which( !COV6$extensive ) ) { v <- COV6$var[ i ]; pz[ , ( paste0( "x_", v ) ) := get( v ) ] }
  ## share of metro-years observed directly (not interpolated), where the panel records it
  obs_share <- sapply( COV6$var, function( v ) { o <- paste0( v, "_obs" ); if ( o %in% names( pz ) ) mean( as.logical( pz[[ o ]] ), na.rm = TRUE ) else NA_real_ } )
  xv <- paste0( "x_", COV6$var )
  facet_plot <- function( yv, ylab, twoway, file ) {
    cols <- c( yv, xv )
    dm <- if ( twoway ) as.data.table( demean( as.matrix( pz[ , ..cols ] ), pz[ , .( cbsa, year ) ], na.rm = FALSE ) )
          else pz[ , lapply( .SD, function( z ) z - mean( z, na.rm = TRUE ) ), by = cbsa, .SDcols = cols ][ , -"cbsa" ]
    long <- rbindlist( lapply( seq_len( nrow( COV6 ) ), function( i ) data.table( var = COV6$var[ i ], x = dm[[ xv[ i ] ]], y = dm[[ yv ]] ) ) )
    long <- long[ is.finite( x ) & is.finite( y ) ]
    st <- long[ , { r <- cor( x, y ); b <- coef( lm( y ~ x ) )[ 2 ]
                    .( r = r, slope = b, n = .N, lo = quantile( x, .005 ), hi = quantile( x, .995 ) ) }, by = var ]
    st <- merge( st, COV6[ , .( var, label ) ], by = "var" )
    st[ , obs := obs_share[ var ] ]
    st[ , strip := paste0( label, "\nr = ", sub( "^(-?)0", "\\1", sprintf( "%.2f", r ) ),
                           ifelse( is.na( obs ) | obs > 0.95, "", paste0( " · ", round( 100 * obs ), "% obs." ) ) ) ]
    long <- merge( long, st[ , .( var, strip, lo, hi ) ], by = "var" )[ x >= lo & x <= hi ]
    long[ , strip := factor( strip, st[ match( COV6$var, var ), strip ] ) ]
    ylim <- quantile( long$y, c( .005, .995 ) )
    g <- ggplot( long, aes( x, y ) ) +
      geom_hline( yintercept = 0, colour = "grey75", linewidth = 0.3 ) + geom_vline( xintercept = 0, colour = "grey75", linewidth = 0.3 ) +
      geom_point( alpha = 0.18, size = 0.5, colour = "grey30", stroke = 0 ) +
      geom_smooth( method = "lm", formula = y ~ x, se = FALSE, colour = C2, linewidth = 0.7 ) +
      facet_wrap( ~ strip, scales = "free_x", ncol = 5 ) + coord_cartesian( ylim = ylim ) +
      labs( title = paste0( ylab, " vs covariates, both demeaned ", if ( twoway ) "by metro and year" else "by metro",
                            " (387 metros, 2010-2022)" ),
            subtitle = "Extensive covariates enter as year-specific SAMIs of log(1 + x); intensive covariates as levels. Axes trimmed to the 0.5-99.5th percentiles; r uses all points.",
            x = "Covariate, deviation from metro mean", y = "SAMI, deviation from metro mean" ) +
      theme_fu + theme( strip.text = element_text( size = 8 ), axis.text = element_text( size = 7 ),
                        plot.subtitle = element_text( size = 8.5, colour = "grey35" ) )
    ggsave( file.path( FU, file ), g, width = 11, height = 10, dpi = 150, bg = "white" )
    st[ , .( var, label, r, slope, n, obs ) ][ , `:=`( y = yv, twoway = twoway ) ]
  }
  st6 <- rbind( facet_plot( "s_n_filer_c3", "Filer-count SAMI", FALSE, "fig_p6_demeaned_counts.png" ),
                facet_plot( "s_n_filer_c3", "Filer-count SAMI", TRUE, "fig_p6_demeaned_counts_twoway.png" ),
                facet_plot( "s_exp_c3_nohu", "Expenditure SAMI (excl. hosp/univ)", FALSE, "fig_p6_demeaned_exp.png" ) )
  fwrite( st6, file.path( FU, "p6_demeaned_correlations.csv" ) )
  print( r3( dcast( st6, label ~ y + twoway, value.var = "r" ) ) )
}
log_msg( "done" )
