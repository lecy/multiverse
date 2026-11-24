## fixtures: a two-year frame in data/ layout, a run, and a freeze, each with its manifest rows
fx <- function() {
  root <- file.path( tempdir(), paste0( "getfx_", sample.int( 1e6, 1 ) ) ); dir.create( root )
  d <- data.table::data.table( cbsa = rep( c( "10180", "10420" ), 2 ), year = rep( 2020:2021, each = 2 ), x = c( 1.5, 2, 3, 4 ) )
  dd <- file.path( root, "data" ); dir.create( file.path( dd, "cbsa_year" ), recursive = TRUE )
  man <- data.table::rbindlist( lapply( 2020:2021, function( y ) {
    f <- sprintf( "cbsa_year/cbsa_year_%d.parquet", y ); arrow::write_parquet( d[ year == y ], file.path( dd, f ) )
    data.table::data.table( frame = "cbsa_year", year = y, file = f, rows = 2L, cols = 3L, bytes = file.size( file.path( dd, f ) ),
                            sha256 = file_sha256( file.path( dd, f ) ) ) } ) )
  data.table::fwrite( man, file.path( dd, "MANIFEST.csv" ) )
  data.table::fwrite( data.table::data.table( frame = "cbsa_year", rows = 4L, cols = 3L, years = "2020-2021", sort_keys = "cbsa,year",
                                              schema_sha256 = frame_signature( d ), source_sha256 = "x" ), file.path( dd, "FRAMES.csv" ) )
  rows <- function( cls, rel, item, file, path, location = file )
    data.table::data.table( class = cls, release = rel, item = item, file = file, bytes = file.size( path ), sha256 = file_sha256( path ), location = location )
  m <- rbind( man[ , rows( "frame", "t1", frame, file, file.path( dd, file ) ), by = seq_len( nrow( man ) ) ][ , seq_len := NULL ],
              rows( "frame", "t1", "frames", "MANIFEST.csv", file.path( dd, "MANIFEST.csv" ) ),
              rows( "frame", "t1", "frames", "FRAMES.csv", file.path( dd, "FRAMES.csv" ) ) )
  ## a run, as release assets
  ra <- file.path( root, "assets" ); dir.create( ra )
  arrow::write_parquet( data.table::data.table( spec_id = 1:2, status = "ok" ), file.path( ra, "run_r1_specs.parquet" ) )
  arrow::write_parquet( data.table::data.table( spec_id = 1:2, term = "x", estimate = c( .1, .2 ) ), file.path( ra, "run_r1_coefs.parquet" ) )
  m <- rbind( m, rows( "run", "t1", "r1", "specs.parquet", file.path( ra, "run_r1_specs.parquet" ), "run_r1_specs.parquet" ),
                 rows( "run", "t1", "r1", "coefs.parquet", file.path( ra, "run_r1_coefs.parquet" ), "run_r1_coefs.parquet" ) )
  ## a freeze: folder/<root>/<path>
  fz <- file.path( root, "freeze" ); dir.create( file.path( fz, "src1", "build", "raw" ), recursive = TRUE ); dir.create( file.path( fz, "src1", "hub", "A" ), recursive = TRUE )
  writeLines( "a,b\n1,2", file.path( fz, "src1", "build", "raw", "one.csv" ) ); writeLines( "hello", file.path( fz, "src1", "hub", "A", "two.txt" ) )
  m <- rbind( m, rows( "input", "f1", "src1", "build/raw/one.csv", file.path( fz, "src1/build/raw/one.csv" ), "src1/build/raw/one.csv" ),
                 rows( "input", "f1", "src1", "hub/A/two.txt", file.path( fz, "src1/hub/A/two.txt" ), "src1/hub/A/two.txt" ) )
  list( root = root, d = d, dd = dd, ra = ra, fz = fz, m = m )
}
furl <- function( p ) paste0( "file:///", normalizePath( p, winslash = "/" ) )

test_that( "mv_get_frame gets, checks and rebuilds a frame", {
  x <- fx(); cache <- file.path( x$root, "cache" )
  d <- mv_get_frame( "cbsa_year", release = "t1", base = x$dd, cache = cache, manifest = x$m )
  expect_equal( d, x$d[ order( cbsa, year ) ], ignore_attr = TRUE )
  d2 <- mv_get_frame( "cbsa_year", release = "t1", base = furl( x$dd ), cache = file.path( x$root, "cache2" ), manifest = x$m )
  expect_equal( d2, d )
  bad <- data.table::copy( x$m ); bad$sha256[ bad$file == "cbsa_year/cbsa_year_2021.parquet" ] <- strrep( "0", 64 )
  expect_error( mv_get_frame( "cbsa_year", release = "t1", base = x$dd, cache = file.path( x$root, "cache3" ), manifest = bad ), "does not match" )
  expect_error( mv_get_frame( "cbsa_year", release = "nope", base = x$dd, cache = cache, manifest = x$m ), "no frame files for release nope" )
} )

test_that( "mv_get_run gets a run from release assets", {
  x <- fx()
  r <- mv_get_run( "r1", release = "t1", base = furl( x$ra ), cache = file.path( x$root, "cache" ), manifest = x$m )
  expect_named( r, c( "specs", "coefs" ) )
  expect_equal( r$coefs$estimate, c( .1, .2 ) )
  expect_error( mv_get_run( "r1", release = "t1", base = furl( file.path( x$root, "missing" ) ), cache = file.path( x$root, "c2" ), manifest = x$m ),
                "attached to the release" )
} )

test_that( "mv_get_inputs restores a freeze into the roots, then keeps it", {
  x <- fx()
  roots <- c( build = file.path( x$root, "B" ), hub = file.path( x$root, "H" ), bls = file.path( x$root, "L" ), dropbox = file.path( x$root, "D" ) )
  r1 <- suppressMessages( mv_get_inputs( roots = roots, release = "f1", source = furl( x$fz ), manifest = x$m ) )
  expect_equal( r1$restored, 2L )
  expect_equal( readLines( file.path( roots[[ "hub" ]], "A", "two.txt" ) ), "hello" )
  r2 <- suppressMessages( mv_get_inputs( roots = roots, release = "f1", source = furl( x$fz ), manifest = x$m ) )
  expect_equal( r2$kept, 2L )
  expect_error( mv_get_inputs( "nope", roots = roots, release = "f1", source = furl( x$fz ), manifest = x$m ), "not in the manifest" )
} )

test_that( "the package manifest and the specification dictionary", {
  m <- mv_manifest()
  expect_true( all( c( "frame", "run", "input" ) %in% m$class ) )
  expect_true( all( grepl( "^[0-9a-f]{64}$", m$sha256 ) ) )
  expect_false( anyDuplicated( m[ , .( class, release, item, file ) ] ) > 0 )
  sd <- mv_spec_dictionary()
  g <- mv_grid( "principled", model = "lean" )
  for ( v in c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "unit", "est", "outlier", "drop", "op" ) )
    expect_true( all( unique( as.character( g[[ v ]] ) ) %in% sd[ column == v, code ] ), info = v )
} )

test_that( "mv_fetch_source: live downloads compared with the frozen hash", {
  x <- fx()
  roots <- c( build = file.path( x$root, "B" ), hub = file.path( x$root, "H" ), bls = file.path( x$root, "L" ), dropbox = file.path( x$root, "D" ) )
  reg <- data.table::data.table( source = "src1", file = c( "build/raw/one.csv", "hub/A/two.txt" ), method = c( "http", "frozen" ),
                                 url = c( paste0( furl( x$fz ), "/src1/build/raw/one.csv" ), NA ), note = c( NA, "by hand" ) )
  r <- suppressMessages( mv_fetch_source( "src1", roots, mode = "live", registry = reg, manifest = x$m ) )
  expect_equal( r$status, c( "same", "frozen only" ) )
  expect_true( file.exists( file.path( roots[[ "build" ]], "raw", "one.csv" ) ) )
  writeLines( "a,b\n1,3", file.path( x$fz, "src1", "build", "raw", "one.csv" ) )          # the source changes
  expect_error( suppressMessages( mv_fetch_source( "src1", roots, mode = "live", registry = reg, manifest = x$m ) ), "has changed at its source" )
  r <- suppressMessages( mv_fetch_source( "src1", roots, mode = "live", on_change = "report", registry = reg, manifest = x$m ) )
  expect_equal( r$status[1], "changed" )
  expect_equal( readLines( file.path( roots[[ "build" ]], "raw", "one.csv" ) ), c( "a,b", "1,2" ) )   # frozen copy kept
  writeLines( "<html><head><title>Request Rejected</title></head></html>", file.path( x$fz, "src1", "build", "raw", "one.csv" ) )
  r <- suppressMessages( mv_fetch_source( "src1", roots, mode = "live", registry = reg, manifest = x$m ) )   # no stop: a refusal is not a change
  expect_equal( r$status[1], "rejected" )
  expect_true( all( mv_sources()$file %in% mv_manifest()$file ) )
} )
