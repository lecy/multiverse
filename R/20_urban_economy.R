## 20_urban_economy.R
## Bettencourt-style urban-economy module (design section 5).
##   Part A  QCEW county private-sector totals 2000-2024 (establishments,
##           employment, wages), total private and private excluding NAICS
##           813 (membership organizations, mostly nonprofits) -> cov_qcew.csv
##   Part B  scaling benchmarks: beta of log Y on log P across metros, by
##           year, for the canonical urban quantities next to the nonprofit
##           outcomes (Role 1) -> diagnostics/scaling_benchmarks.csv
##   Part C  SAMI-on-SAMI: time-averaged SAMIs correlated and regressed
##           (Role 2), and SAMI persistence (validation 7.6)
##           -> diagnostics/sami_on_sami_*.csv, sami_persistence.csv
## Parts B and C read the assembled CBSA panel; run 30_assemble first (the
## script re-reads it each time). Patents come from 21_patents.R when the
## PatentsView files are available.
##
## QCEW raw files: D:/MULTIVERSE/raw/cov/qcew/qcew_<year>_<10|813>.csv
## (2000-2013 from the annual by-industry zips, 2014+ from the open-data API).

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
source( "R/13_16_cov_helpers.R" )
suppressPackageStartupMessages( library( arrow ) )

qdir <- file.path( OUT$raw_cov, "qcew" )
PART <- if ( length( commandArgs( TRUE ) ) ) commandArgs( TRUE )[1] else "all"

## ---- Part A: QCEW -------------------------------------------------------------

if ( PART %in% c( "all", "qcew" ) ) {
  rd <- function( y, ind ) {
    f <- file.path( qdir, sprintf( "qcew_%d_%s.csv", y, ind ) )
    if ( !file.exists( f ) || file.size( f ) < 1000 ) { warning( "missing ", f ); return( NULL ) }
    d <- fread( f, select = c( "area_fips", "own_code", "industry_code", "agglvl_code", "year",
                               "disclosure_code", "annual_avg_estabs", "annual_avg_emplvl", "total_annual_wages" ),
                colClasses = list( character = c( "area_fips", "disclosure_code", "industry_code" ) ) )
    d <- d[ own_code == 5 & grepl( "^[0-9]{5}$", area_fips ) & !grepl( "000$|999$", area_fips ) &
              agglvl_code %in% c( 71L, 75L ) ]
    sup <- d$disclosure_code %in% "N"
    for ( v in c( "annual_avg_estabs", "annual_avg_emplvl", "total_annual_wages" ) )
      set( d, i = which( sup ), j = v, value = NA )
    d[ , .( geoid = area_fips, year = as.integer( year ), estab = as.numeric( annual_avg_estabs ),
            emp = as.numeric( annual_avg_emplvl ), wages = as.numeric( total_annual_wages ) ) ]
  }
  q <- rbindlist( lapply( YEARS, function( y ) {
    a <- rd( y, "10" ); b <- rd( y, "813" )
    if ( is.null( a ) ) return( NULL )
    setnames( a, c( "estab", "emp", "wages" ), c( "qcew_priv_estab", "qcew_priv_emp", "qcew_priv_wages" ) )
    if ( !is.null( b ) ) {
      setnames( b, c( "estab", "emp", "wages" ), c( "qcew_813_estab", "qcew_813_emp", "qcew_813_wages" ) )
      a <- merge( a, b, by = c( "geoid", "year" ), all.x = TRUE )
    }
    a } ), fill = TRUE )
  q[ , qcew_813_suppressed := as.integer( is.na( qcew_813_emp ) ) ]
  for ( k in c( "estab", "emp", "wages" ) )
    set( q, j = paste0( "qcew_privx813_", k ),
         value = q[[ paste0( "qcew_priv_", k ) ]] - fcoalesce( q[[ paste0( "qcew_813_", k ) ]], 0 ) )
  cnt <- grep( "^qcew_(priv|813|privx813)_", names( q ), value = TRUE )
  cnt <- setdiff( cnt, "qcew_813_suppressed" )
  q <- harmonize_counties( q, counts = c( cnt, "qcew_813_suppressed" ) )
  validate_cov( q, "QCEW private" )
  print( q[ , .( counties = .N, priv_emp = sum( qcew_priv_emp, na.rm = TRUE ),
                 priv_wages_bn = sum( qcew_priv_wages, na.rm = TRUE ) / 1e9,
                 emp_813 = sum( qcew_813_emp, na.rm = TRUE ), sup_813 = sum( qcew_813_suppressed ) ), keyby = year ] )
  fwrite( q, file.path( OUT$cov, "cov_qcew.csv" ) )
  fwrite( data.table(
    var = c( "qcew_priv_estab", "qcew_priv_emp", "qcew_priv_wages", "qcew_813_*", "qcew_privx813_*", "qcew_813_suppressed" ),
    desc = c( "private establishments (annual average)", "private employment (annual average)",
              "private total annual wages ($)", "NAICS 813 membership organizations, private (NA = suppressed)",
              "private minus NAICS 813 (suppressed 813 treated as 0)", "1 = county's 813 cell suppressed" ),
    type = c( "B", "B", "B", "B", "B", "flag" ), years = "2000-2024",
    source = "BLS QCEW county annual averages, ownership 5 (private)" ),
    file.path( OUT$cov, "cov_qcew_codebook.csv" ) )
  log_msg( "wrote cov_qcew.csv: ", nrow( q ), " rows" )
}

## ---- Parts B and C: benchmarks and SAMI-on-SAMI --------------------------------

if ( PART %in% c( "all", "bench" ) ) {
  p <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
  p <- p[ cbsa_type == "metro" & population > 0 ]
  if ( "gdp_real" %in% names( p ) && "emp_total" %in% names( p ) ) p[ , gdp_per_worker := gdp_real / emp_total ]
  BENCH <- data.table( var = c(
      "gdp_real", "personal_income", "wages_total", "earn_total", "emp_total",
      "qcew_priv_wages", "qcew_priv_emp", "qcew_priv_estab", "qcew_privx813_wages",
      "patents_frac", "patents_corp", "patents_univnp",
      "n_filer_c3", "n_filer_c3_nohu", "n_bmf_c3pc", "exp_c3", "exp_c3_nohu", "n_pf", "pf_assets",
      "bls_npo_emp", "bls_npo_wages", "bls_npo_estab", "n_entry_c3pc",
      "rel_cng", "rel_adh", "socap_assn_n", "contrib_total", "gov_earn_sl" ),
    group = c( rep( "urban economy", 9 ), rep( "innovation", 3 ),
               rep( "nonprofit (990/BMF)", 7 ), rep( "nonprofit (BLS)", 3 ), "nonprofit entry",
               rep( "civic", 3 ), "philanthropy", "government" ) )
  BENCH <- BENCH[ var %in% names( p ) ]
  fit <- function( y, P ) {
    ok <- is.finite( y ) & y > 0 & is.finite( P )
    if ( sum( ok ) < 30 ) return( NULL )
    m <- lm( log( y[ ok ] ) ~ log( P[ ok ] ) ); s <- summary( m )
    data.table( beta = coef( m )[2], se = s$coefficients[ 2, 2 ], r2 = s$r.squared, metros = sum( ok ) )
  }
  bench <- rbindlist( lapply( seq_len( nrow( BENCH ) ), function( i ) {
    v <- BENCH$var[ i ]
    rbindlist( lapply( sort( unique( p$year ) ), function( y ) {
      r <- fit( p[ year == y ][[ v ]], p[ year == y ]$population )
      if ( is.null( r ) ) NULL else cbind( data.table( var = v, group = BENCH$group[ i ], year = y ), r ) } ) ) } ) )
  fwrite( bench, file.path( OUT$diag, "scaling_benchmarks.csv" ) )
  ## headline: 2022 (or latest year with data) and the 2000-2024 mean
  head_tab <- bench[ , .( beta_2022 = beta[ year == 2022 ][1], se_2022 = se[ year == 2022 ][1],
                          beta_mean = mean( beta ), beta_min = min( beta ), beta_max = max( beta ),
                          years = paste( range( year ), collapse = "-" ) ), by = .( group, var ) ][ order( -beta_2022 ) ]
  ## Bettencourt et al. (2007) reference exponents
  ref <- c( patents_frac = 1.27, gdp_real = 1.15, wages_total = 1.12, qcew_priv_wages = 1.12,
            emp_total = 1.01, qcew_priv_emp = 1.01, personal_income = 1.12 )
  head_tab[ , bettencourt_2007 := ref[ var ] ]
  fwrite( head_tab, file.path( OUT$diag, "scaling_benchmarks_summary.csv" ) )
  cat( "\n== Scaling exponents across metros ==\n" ); print( head_tab, digits = 3 )

  ## SAMIs by year, then time averages per metro
  svars <- BENCH$var
  for ( v in svars ) p[ , paste0( "S_", v ) := {
    y <- get( v ); ok <- is.finite( y ) & y > 0; r <- rep( NA_real_, .N )
    if ( sum( ok ) >= 30 ) r[ ok ] <- resid( lm( log( y[ ok ] ) ~ log( population[ ok ] ) ) ); r }, by = year ]
  sc <- paste0( "S_", svars )
  avg <- p[ , lapply( .SD, function( x ) if ( sum( !is.na( x ) ) >= 3 ) mean( x, na.rm = TRUE ) else NA_real_ ),
            by = cbsa, .SDcols = sc ]
  cm <- cor( avg[ , ..sc ], use = "pairwise.complete.obs" )
  dimnames( cm ) <- list( svars, svars )
  cmt <- as.data.table( cm, keep.rownames = "var" )
  fwrite( cmt, file.path( OUT$diag, "sami_on_sami_correlations.csv" ) )
  econ <- intersect( c( "gdp_real", "wages_total", "qcew_priv_wages", "personal_income", "patents_frac" ), svars )
  np   <- intersect( c( "n_filer_c3", "n_filer_c3_nohu", "n_bmf_c3pc", "exp_c3", "exp_c3_nohu", "bls_npo_emp", "n_entry_c3pc" ), svars )
  cat( "\n== Correlation of time-averaged SAMIs: nonprofit (rows) x urban economy (cols) ==\n" )
  print( round( cm[ np, econ, drop = FALSE ], 2 ) )
  ## between regressions: nonprofit SAMI on economy SAMIs (one at a time)
  btw <- rbindlist( lapply( np, function( y ) rbindlist( lapply( econ, function( x ) {
    d <- avg[ is.finite( get( paste0( "S_", y ) ) ) & is.finite( get( paste0( "S_", x ) ) ) ]
    if ( nrow( d ) < 30 ) return( NULL )
    m <- summary( lm( as.formula( paste0( "S_", y, " ~ S_", x ) ), data = d ) )
    data.table( outcome = y, predictor = x, slope = m$coefficients[ 2, 1 ], se = m$coefficients[ 2, 2 ],
                r2 = m$r.squared, metros = nrow( d ) ) } ) ) ) )
  fwrite( btw, file.path( OUT$diag, "sami_on_sami_between.csv" ) )
  ## persistence: correlation of a metro's SAMI in year t with t-1 and t-5
  pers <- rbindlist( lapply( svars, function( v ) {
    w <- dcast( p[ , .( cbsa, year, s = get( paste0( "S_", v ) ) ) ], cbsa ~ year, value.var = "s" )
    yrs <- as.integer( setdiff( names( w ), "cbsa" ) )
    lag_cor <- function( k ) { r <- sapply( yrs[ ( yrs - k ) %in% yrs ], function( y )
      tryCatch( suppressWarnings( cor( w[[ as.character( y ) ]], w[[ as.character( y - k ) ]], use = "complete.obs" ) ),
                error = function( e ) NA_real_ ) )
      if ( length( r ) ) mean( r, na.rm = TRUE ) else NA_real_ }
    data.table( var = v, r_lag1 = lag_cor( 1 ), r_lag5 = lag_cor( 5 ), r_lag10 = lag_cor( 10 ) ) } ) )
  fwrite( pers, file.path( OUT$diag, "sami_persistence.csv" ) )
  cat( "\n== SAMI persistence (mean year-to-year correlation) ==\n" ); print( pers, digits = 3 )
}
