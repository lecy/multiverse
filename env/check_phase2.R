## check_phase2.R
## Phase 2 check (reproducibility plan): the rebuilt database must equal the
## reference build plus the four CBSA covariates that load_panel() used to attach.
##   1. the side covariate files rebuilt by 25, 26 and 27 equal the reference copies
##   2. the rebuilt cbsa_year.parquet equals the reference cbsa_year.parquet plus
##      hist_pre1970, lag_g5, lag_g10 and gini_m, merged from the reference side
##      files the way the old load_panel() merged them
##   3. load_panel() on the rebuilt file returns the same table as the old
##      load_panel() on the reference file (column order aside)
##   Rscript env/check_phase2.R [ref_dir = <MV_BUILD>/ref]
## The reference folder holds analysis/{cbsa,county}_year.parquet, cov/<side files>
## and need_socap/acs_need_cbsa.csv from the build being reproduced.
## Exit status 1 if anything differs.

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( library( arrow ) )
a <- commandArgs( TRUE )
REF <- if ( length( a ) ) a[1] else file.path( BUILD, "ref" )
bad <- 0L

same_table <- function( x, y, key, label ) {
  x <- as.data.table( x ); y <- as.data.table( y )
  if ( !setequal( names( x ), names( y ) ) ) {
    cat( label, ": columns differ; new-only", setdiff( names( x ), names( y ) ),
         "; ref-only", setdiff( names( y ), names( x ) ), "\n" ); return( FALSE ) }
  if ( nrow( x ) != nrow( y ) ) { cat( label, ": rows", nrow( x ), "vs", nrow( y ), "\n" ); return( FALSE ) }
  setcolorder( x, names( y ) ); setkeyv( x, key ); setkeyv( y, key )
  diff <- names( y )[ !vapply( names( y ), function( v ) identical( x[[ v ]], y[[ v ]] ), logical( 1 ) ) ]
  if ( length( diff ) ) {
    for ( v in diff ) {
      xx <- x[[ v ]]; yy <- y[[ v ]]
      d <- if ( is.numeric( xx ) && is.numeric( yy ) ) suppressWarnings( max( abs( xx - yy ), na.rm = TRUE ) ) else NA
      cat( sprintf( "%s: %s differs (classes %s / %s; NA mismatches %d; max abs diff %s)\n", label, v,
                    class( xx )[1], class( yy )[1], sum( is.na( xx ) != is.na( yy ) ), format( d ) ) )
    }
    return( FALSE ) }
  cat( sprintf( "%s: IDENTICAL (%d rows x %d cols)\n", label, nrow( x ), ncol( x ) ) ); TRUE
}

## ---- 1. side files -------------------------------------------------------------------
rd <- function( f ) fread( f, colClasses = list( character = "cbsa" ) )
side <- list( c( "cov/cov_endowment.csv", "cbsa" ), c( "cov/cov_lag_kernel.csv", "cbsa,year" ),
              c( "cov/cov_lag_kernel_coverage.csv", "cbsa,year" ), c( "cov/cov_gini_metro.csv", "cbsa,year" ),
              c( "need_socap/acs_need_cbsa.csv", "cbsa,year" ) )
for ( s in side ) {
  ok <- same_table( rd( file.path( BUILD, "data", s[1] ) ), rd( file.path( REF, s[1] ) ),
                    strsplit( s[2], "," )[[ 1 ]], s[1] )
  bad <- bad + !ok
}

## ---- 2. cbsa_year = reference + four merged columns ------------------------------------
new <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
ref <- as.data.table( read_parquet( file.path( REF, "analysis", "cbsa_year.parquet" ) ) )
added <- setdiff( names( new ), names( ref ) )
cat( "columns added to cbsa_year.parquet:", added, "\n" )
if ( !setequal( added, c( "hist_pre1970", "lag_g5", "lag_g10", "gini_m" ) ) || length( setdiff( names( ref ), names( new ) ) ) ) bad <- bad + 1L
## the old load_panel() merges, applied to the reference file and reference side files
old_attach <- function( d ) {
  d <- copy( d )
  e <- fread( file.path( REF, "cov", "cov_endowment.csv" ), select = c( "cbsa", "hist_pre1970" ), colClasses = list( character = "cbsa" ) )
  d[ e, on = "cbsa", hist_pre1970 := i.hist_pre1970 ]
  k <- fread( file.path( REF, "cov", "cov_lag_kernel.csv" ), select = c( "cbsa", "year", "lag_g5", "lag_g10" ), colClasses = list( character = "cbsa" ) )
  d[ k, on = c( "cbsa", "year" ), `:=`( lag_g5 = i.lag_g5, lag_g10 = i.lag_g10 ) ]
  gm <- fread( file.path( REF, "cov", "cov_gini_metro.csv" ), colClasses = list( character = "cbsa" ) )
  d[ gm, on = c( "cbsa", "year" ), gini_m := i.gini_m ]
  d
}
bad <- bad + !same_table( new, old_attach( ref ), c( "cbsa", "year" ), "cbsa_year.parquet vs reference + side columns" )

## ---- 3. load_panel() --------------------------------------------------------------------
lp_new <- load_panel( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) )
lp_old <- copy( ref )[ , logP := log( population ) ][ , division := unname( DIVISION[ state_fips ] ) ]
lp_old <- old_attach( lp_old )
bad <- bad + !same_table( lp_new, lp_old, c( "cbsa", "year" ), "load_panel() new vs old" )

cat( if ( bad ) sprintf( "\nPHASE 2 CHECK: %d difference(s)\n", bad ) else "\nPHASE 2 CHECK: PASSED\n" )
quit( status = as.integer( bad > 0 ) )
