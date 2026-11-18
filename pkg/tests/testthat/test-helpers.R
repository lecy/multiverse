test_that( "bad_ntee flags blank, missing and the invalid codes, case-insensitively", {
  expect_equal( bad_ntee( c( "B20", "UND", "", NA, "z99", "Z991", " inv ", "E22" ) ),
                c( FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE ) )
} )

test_that( "ntee_best_from keeps each EIN's latest valid code", {
  d <- data.table::data.table( EIN = c( "1", "1", "1", "22-0000002" ), year = c( 2010, 2015, 2018, 2012 ),
                               ntee3 = c( "b20", "E22", "UND", "P20" ) )
  b <- ntee_best_from( d )
  expect_equal( b$EIN, c( "000000001", "220000002" ) )
  expect_equal( b$ntee_best, c( "E22", "P20" ) )
} )

test_that( "resolve_ntee fills, keeps and blanks by reference", {
  best <- data.table::data.table( EIN = "000000001", ntee_best = "E22" )
  dt <- data.table::data.table( ein = c( "1", "000000002", "3" ), ntee3 = c( "und", "inv", "b201" ) )
  st <- resolve_ntee( dt, best = best )
  expect_equal( st, c( "filled", "unresolved", "valid" ) )
  expect_equal( dt$ntee3, c( "E22", NA, "B20" ) )
} )

test_that( "pad and recode_county", {
  expect_equal( pad( c( 1, 42, NA, 123456 ), 5 ), c( "00001", "00042", NA, "123456" ) )
  expect_equal( recode_county( c( "51515", "02261", "04013", NA ) ), c( "51019", "02063", "04013", NA ) )
} )

test_that( "transforms", {
  expect_equal( winsor( c( 1:99, 1000 ) )[ 100 ], unname( stats::quantile( c( 1:99, 1000 ), .99 ) ) )
  expect_equal( slog( c( -1, 0, exp( 1 ) - 1 ) ), c( 0, 0, 1 ) )
  ## floor: the zero is raised to the 1st percentile of positive per-capita values
  x <- c( 0, 10, 20 ); pop <- c( 100, 100, 100 )
  f <- stats::quantile( c( .1, .2 ), .01, names = FALSE )
  expect_equal( flog( x, pop ), log( pmax( x / pop, f ) * pop ) )
  ## resid_logP: residuals are mean zero and orthogonal to logP within each year
  set.seed( 1 ); lp <- rnorm( 40 ); yr <- rep( 1:2, 20 ); y <- 1 + 0.9 * lp + rnorm( 40, sd = .1 )
  r <- resid_logP( y, lp, yr )
  for ( t in 1:2 ) {
    expect_equal( mean( r[ yr == t ] ), 0, tolerance = 1e-12 )
    expect_equal( sum( r[ yr == t ] * lp[ yr == t ] ), 0, tolerance = 1e-10 )
  }
  expect_true( all( is.na( resid_logP( 1:3, 1:3, c( 1, 1, 1 ) ) ) ) )
} )

test_that( "mv_fetch keeps a good file and rejects a changed one", {
  d <- tempfile(); dir.create( d ); f <- file.path( d, "x.txt" ); writeLines( "abc", f )
  h <- file_sha256( f )
  expect_equal( mv_fetch( "https://example.invalid/x.txt", d, sha256 = h ), f )    # no download
  src <- tempfile( fileext = ".txt" ); writeLines( "changed", src )
  expect_error( mv_fetch( paste0( "file:///", normalizePath( src, winslash = "/" ) ), d, file = "y.txt", sha256 = h ),
                "does not match" )
  expect_false( file.exists( file.path( d, "y.txt" ) ) )
} )

test_that( "mv_read_frame rebuilds a frame from slices and checks it", {
  skip_if_not_installed( "arrow" )
  d <- tempfile(); dir.create( file.path( d, "cbsa_year" ), recursive = TRUE )
  full <- data.table::data.table( cbsa = rep( c( "10100", "10140" ), each = 2 ), year = rep( 2000:2001, 2 ), x = c( 1.5, 2, 3, 4 ) )
  man <- list()
  for ( y in 2000:2001 ) {
    f <- file.path( "cbsa_year", paste0( "cbsa_year_", y, ".parquet" ) )
    arrow::write_parquet( full[ year == y ], file.path( d, f ) )
    man[[ length( man ) + 1 ]] <- data.table::data.table( file = f, frame = "cbsa_year", year = y, rows = 2L, cols = 3L,
                                                         bytes = file.size( file.path( d, f ) ), sha256 = file_sha256( file.path( d, f ) ) )
  }
  data.table::fwrite( data.table::rbindlist( man ), file.path( d, "MANIFEST.csv" ) )
  data.table::fwrite( data.table::data.table( frame = "cbsa_year", rows = 4L, cols = 3L, years = "2000-2001", sort_keys = "cbsa,year",
                                              schema_sha256 = frame_signature( full ), source = "", source_sha256 = "" ),
                      file.path( d, "FRAMES.csv" ) )
  got <- mv_read_frame( "cbsa_year", dir = d )
  expect_true( isTRUE( all.equal( got, full, check.attributes = FALSE ) ) )
  ## a slice that no longer matches its recorded hash is refused (the hash is changed
  ## rather than the file: arrow may keep the file open on Windows)
  m <- data.table::fread( file.path( d, "MANIFEST.csv" ), colClasses = list( character = "sha256" ) )
  m[ 2, sha256 := strrep( "0", 64 ) ]
  data.table::fwrite( m, file.path( d, "MANIFEST.csv" ) )
  expect_error( mv_read_frame( "cbsa_year", dir = d ), "do not match" )
} )
