## env/compare_builds.R
## Phase 5 check: compare a rebuilt build folder with a reference copy, file by file.
##   Rscript env/compare_builds.R <new_build> <ref_build> [subdir=data] [tol=1e-9] [files=all|new]
## Every file under <ref>/<subdir> and <new>/<subdir> is matched by relative path.
## files=new compares only the files the new build wrote (the reference folder may also
## hold outputs of research-note scripts or backups that the build does not write).
##   1. sha256: identical files are done.
##   2. Tables that differ (.csv, .csv.gz, .parquet) are read and compared cell by cell,
##      after sorting by the key columns found (ein, geoid, cbsa, year, ...) when they
##      are unique, otherwise by all columns. Numbers count as equal within tol
##      (relative).
## Writes <new>/check/diff_files.csv (one row per file) and <new>/check/diff_cells.csv
## (one row per differing column). Exits 1 if any file is missing or differs.

suppressPackageStartupMessages( { library( data.table ); library( arrow ) } )
a <- commandArgs( TRUE )
NEW <- a[1]; REF <- a[2]
SUB <- if ( length( a ) >= 3 ) a[3] else "data"
TOL <- if ( length( a ) >= 4 ) as.numeric( a[4] ) else 1e-9
KEYS <- c( "ein", "geoid", "fips", "county_fips", "cbsa", "state", "year", "tax_year", "fiscal_year",
           "source", "form", "ntee", "subsector", "group", "variable" )

sha <- function( f ) digest::digest( f, "sha256", file = TRUE )
rel <- function( root ) { d <- file.path( root, SUB ); sub( paste0( "^", d, "/" ), "", list.files( d, recursive = TRUE, full.names = TRUE ) ) }
is_tab <- function( f ) grepl( "\\.(csv|csv\\.gz|parquet)$", f )
rd <- function( f ) if ( grepl( "\\.parquet$", f ) ) as.data.table( read_parquet( f ) ) else
  fread( f, colClasses = "character", showProgress = FALSE )   # csv: compare as text, then numbers

ONLY_NEW <- length( a ) >= 5 && a[5] == "new"
files <- sort( if ( ONLY_NEW ) rel( NEW ) else union( rel( NEW ), rel( REF ) ) )
cat( length( files ), "files under", SUB, "\n" )
out_f <- vector( "list", length( files ) ); out_c <- list()

for ( k in seq_along( files ) ) {
  f <- files[ k ]; fn <- file.path( NEW, SUB, f ); fr <- file.path( REF, SUB, f )
  row <- data.table( path = f, status = "", bytes_new = NA_real_, bytes_ref = NA_real_,
                     rows_new = NA_integer_, rows_ref = NA_integer_, cols_new = NA_integer_, cols_ref = NA_integer_,
                     diff_cols = NA_integer_, note = "" )
  if ( !file.exists( fn ) ) { row$status <- "missing_new"; out_f[[ k ]] <- row; next }
  if ( !file.exists( fr ) ) { row$status <- "missing_ref"; out_f[[ k ]] <- row; next }
  row$bytes_new <- file.size( fn ); row$bytes_ref <- file.size( fr )
  if ( sha( fn ) == sha( fr ) ) { row$status <- "identical"; out_f[[ k ]] <- row; next }
  if ( !is_tab( f ) ) { row$status <- "differs"; row$note <- "not a table"; out_f[[ k ]] <- row; next }

  res <- tryCatch( {
    n <- rd( fn ); r <- rd( fr )
    row$rows_new <- nrow( n ); row$rows_ref <- nrow( r ); row$cols_new <- ncol( n ); row$cols_ref <- ncol( r )
    notes <- character( 0 )
    on <- setdiff( names( n ), names( r ) ); orf <- setdiff( names( r ), names( n ) )
    if ( length( on ) ) notes <- c( notes, paste( "only new:", paste( head( on, 8 ), collapse = " " ) ) )
    if ( length( orf ) ) notes <- c( notes, paste( "only ref:", paste( head( orf, 8 ), collapse = " " ) ) )
    cols <- intersect( names( r ), names( n ) )
    if ( nrow( n ) != nrow( r ) ) {
      row$status <- "differs"; row$note <- paste( c( notes, "row counts differ" ), collapse = "; " ); row
    } else {
      key <- intersect( KEYS, cols )
      if ( !length( key ) || uniqueN( r, by = key ) < nrow( r ) ) key <- cols
      setorderv( n, key, na.last = TRUE ); setorderv( r, key, na.last = TRUE )
      nd <- 0L
      for ( v in cols ) {
        x <- n[[ v ]]; y <- r[[ v ]]
        if ( identical( x, y ) ) next
        if ( is.character( x ) && is.character( y ) ) {      # csv text: equal as numbers?
          xn <- suppressWarnings( as.numeric( x ) ); yn <- suppressWarnings( as.numeric( y ) )
          if ( sum( is.na( xn ) ) == sum( is.na( x ) | x == "" ) && sum( is.na( yn ) ) == sum( is.na( y ) | y == "" ) ) { x <- xn; y <- yn }
        }
        na <- sum( is.na( x ) != is.na( y ) )
        if ( is.numeric( x ) && is.numeric( y ) ) {
          ok <- !is.na( x ) & !is.na( y )
          d <- abs( x[ ok ] - y[ ok ] ); rr <- d / pmax( abs( y[ ok ] ), 1e-12 )
          nv <- sum( rr > TOL ); ma <- if ( length( d ) ) max( d ) else 0; mr <- if ( length( rr ) ) max( rr ) else 0
        } else {
          ok <- !is.na( x ) & !is.na( y ); nv <- sum( as.character( x[ ok ] ) != as.character( y[ ok ] ) ); ma <- NA_real_; mr <- NA_real_
        }
        if ( na + nv > 0 ) {
          nd <- nd + 1L
          out_c[[ length( out_c ) + 1L ]] <- data.table( path = f, column = v, na_mismatch = na, value_diffs = nv,
                                                         max_abs = ma, max_rel = mr )
        }
      }
      row$diff_cols <- nd
      row$status <- if ( nd == 0 && !length( on ) && !length( orf ) ) "cells_equal" else "differs"
      row$note <- paste( notes, collapse = "; " ); row
    }
  }, error = function( e ) { row$status <- "error"; row$note <- conditionMessage( e ); row } )
  out_f[[ k ]] <- res
  if ( res$status != "identical" ) cat( sprintf( "  %-11s %s %s\n", res$status, f, res$note ) )
}

df <- rbindlist( out_f )
dc <- if ( length( out_c ) ) rbindlist( out_c ) else
  data.table( path = character( 0 ), column = character( 0 ), na_mismatch = integer( 0 ),
              value_diffs = integer( 0 ), max_abs = numeric( 0 ), max_rel = numeric( 0 ) )
od <- file.path( NEW, "check" ); dir.create( od, showWarnings = FALSE )
fwrite( df, file.path( od, "diff_files.csv" ) ); fwrite( dc, file.path( od, "diff_cells.csv" ) )
cat( "\n" ); print( df[ , .N, keyby = status ] )
cat( "wrote", file.path( od, "diff_files.csv" ), "and diff_cells.csv\n" )
quit( status = as.integer( any( !df$status %in% c( "identical", "cells_equal" ) ) ) )
