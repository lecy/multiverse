test_that( "grids have the sizes of runs v10 and v11", {
  expect_equal( nrow( mv_grid( "principled", model = "lean" )[ is.na( dup_of ) ] ), 432432L )
  expect_equal( nrow( mv_grid( "principled", model = "default" )[ is.na( dup_of ) ] ), 605352L )
  cfg <- mv_spec_config( "default" )
  expect_true( "history" %in% mv_addon_blocks( cfg ) )
  expect_length( mv_addon_blocks( mv_spec_config( "lean" ) ), 0 )
} )

## a synthetic CBSA-year panel with every column the lean model reads
fake_panel <- function( n = 120, years = 2010:2022 ) {
  set.seed( 3 )
  cfg <- mv_spec_config( "lean" )
  vars <- unique( c( unlist( lapply( cfg$blocks, function( b ) c( b$default, unlist( b$alt ) ) ) ), "lag_g5", "lag_g10", "gini_m", "hist_pre1970" ) )
  d <- data.table::CJ( cbsa = sprintf( "%05d", seq_len( n ) ), year = years )
  d[ , population := rep( exp( rnorm( n, 12, 1 ) ), each = length( years ) ) * exp( rnorm( .N, 0, .02 ) ) ]
  d[ , state_fips := rep( sample( names( DIVISION ), n, TRUE ), each = length( years ) ) ]
  d[ , cbsa_type := rep( sample( c( "metro", "micro" ), n, TRUE, c( .8, .2 ) ), each = length( years ) ) ]
  for ( v in vars ) data.table::set( d, j = v, value = abs( rnorm( nrow( d ), 10, 3 ) ) * if ( v %in% cfg$covs[ type == "B", var ] ) d$population else 1 )
  for ( o in unlist( cfg$outcomes ) ) data.table::set( d, j = o, value = d$population * exp( rnorm( nrow( d ), -6, .3 ) ) )
  mv_load_panel( d )
}

test_that( "mv_run_spec fits each estimator", {
  cfg <- mv_spec_config( "lean" ); panel <- fake_panel()
  g <- mv_grid( "principled", cfg = cfg )[ is.na( dup_of ) & outcome == "n_filer_c3" & op == "default" & drop == "none" ]
  for ( e in c( "ols", "state_fe", "div_fe", "state_re" ) ) {
    s <- g[ ident == "cs" & est == e & dv_form == "loglog" & pred_form == "log" & outlier == "none" & unit == "cbsa" & cs_year == 2022L ][ 1 ]
    r <- mv_run_spec( s, panel, cfg )
    expect_s3_class( r, "data.table" )
    expect_setequal( r$term, c( mv_covars( s, cfg ), "logP" ) )
    expect_true( all( is.finite( r$estimate ) ) )
  }
  s <- g[ ident == "within" & dv_form == "sami" & pred_form == "sami" ][ 1 ]
  expect_false( "hist_pre1970" %in% mv_run_spec( s, panel, cfg )$term )
} )

test_that( "mv_run_universe runs, resumes and combines", {
  cfg <- mv_spec_config( "lean" ); panel <- fake_panel( 60 )
  g <- mv_grid( "principled", cfg = cfg )[ is.na( dup_of ) & outcome == "n_filer_c3" & ident == "cs" & cs_year == 2022L &
                                            op == "default" & drop == "none" & est == "ols" ]
  d <- file.path( tempdir(), "mv_run_test" ); unlink( d, recursive = TRUE )
  sp <- suppressMessages( mv_run_universe( g, panel, d, cfg, batch = 20, workers = 1, verbose = FALSE ) )
  expect_equal( nrow( sp ), nrow( g ) )
  expect_true( all( sp$status == "ok" ) )
  co <- arrow::read_parquet( file.path( d, "coefs.parquet" ) )
  expect_setequal( unique( co$spec_id ), g$spec_id )
  t0 <- file.mtime( file.path( d, "batches", "coefs_0001.parquet" ) )
  suppressMessages( mv_run_universe( g, panel, d, cfg, batch = 20, workers = 1, verbose = FALSE ) )
  expect_equal( file.mtime( file.path( d, "batches", "coefs_0001.parquet" ) ), t0 )
} )

test_that( "partial r and eta squared", {
  expect_equal( mv_partial_r( 2, 103, 2, "cs" ), 2 / sqrt( 4 + 100 ) )
  expect_equal( mv_partial_r( 2, 800, 2, "pooled" ), 2 / sqrt( 4 + 97 ) )
  expect_equal( mv_partial_r( 2, 13, 5, "within" ), 2 / sqrt( 4 + 1 ) )
  y <- c( 1, 2, 3, 11, 12, 13 ); g <- rep( c( "a", "b" ), each = 3 )
  expect_equal( mv_eta2( y, g ), 150 / 154 )
  expect_true( is.na( mv_eta2( y, g, min_n = 10 ) ) )
  expect_true( is.na( mv_eta2( y, rep( "a", 6 ) ) ) )
} )
