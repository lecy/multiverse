## check_v11_slice.R
## Reproducibility check for run v11 (MV_MODEL=lean, principled universe):
##   1. rebuild the spec grid and compare it with the stored specs_grid.parquet
##   2. re-fit a random slice of N specs (seeded) the way 44_run_universe.R does
##   3. compare status and coefficients with the stored run, spec by spec
##   Rscript env/check_v11_slice.R <ref_run_dir> [N=1000] [workers]
## Writes <MV_BUILD>/check/v11_slice_{coefs_new,diff}.csv.

Sys.setenv( MV_MODEL = "lean" )
source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )

a <- commandArgs( TRUE )
REF <- a[1]
N <- if ( length( a ) >= 2 ) as.integer( a[2] ) else 1000L
WORKERS <- if ( length( a ) >= 3 ) as.integer( a[3] ) else max( 1L, detectCores() - 2L )
od <- file.path( BUILD, "check" ); dir.create( od, showWarnings = FALSE )

## ---- 1. grid ---------------------------------------------------------------------
grid <- build_grid( "principled" )[ is.na( dup_of ) ]
grid[ , batch := ( seq_len( .N ) - 1L ) %/% 2000L + 1L ]
gref <- as.data.table( read_parquet( file.path( REF, "specs_grid.parquet" ) ) )
cat( "grid rows new", nrow( grid ), "ref", nrow( gref ), "; identical:",
     isTRUE( all.equal( grid, gref, check.attributes = FALSE ) ), "\n" )

## ---- 2. re-fit a seeded slice ----------------------------------------------------
set.seed( 20261117 )
ids <- sort( sample( grid$spec_id, N ) )
g <- grid[ spec_id %in% ids ]
panel <- load_panel()
fit_one <- function( i ) {
  r <- tryCatch( run_spec( g[ i ], panel ), error = function( e ) e )
  st <- if ( inherits( r, "error" ) ) paste( "error:", conditionMessage( r ) )
        else if ( is.null( r ) || !nrow( r ) ) "no fit" else "ok"
  list( status = data.table( spec_id = g$spec_id[ i ], status = st ),
        coefs = if ( st == "ok" ) r else NULL )
}
t0 <- proc.time()[[ "elapsed" ]]
cl <- makePSOCKcluster( WORKERS )
invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1", MV_MODEL = "lean" )
  setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
  source( "R/00_setup.R" ); source( "R/40_multiverse.R" )
  data.table::setDTthreads( 1L ); NULL } ) )
clusterExport( cl, c( "panel", "g" ) )
res <- parLapplyLB( cl, seq_len( nrow( g ) ), fit_one )
stopCluster( cl )
cat( "fitted", nrow( g ), "specs in", round( ( proc.time()[[ "elapsed" ]] - t0 ) / 60, 1 ), "min\n" )
st_new <- rbindlist( lapply( res, `[[`, "status" ) )
co_new <- rbindlist( lapply( res, `[[`, "coefs" ), fill = TRUE )
fwrite( co_new, file.path( od, "v11_slice_coefs_new.csv" ) )

## ---- 3. compare with the stored run ----------------------------------------------
sref <- as.data.table( read_parquet( file.path( REF, "specs.parquet" ), col_select = c( "spec_id", "status" ) ) )
st <- merge( st_new, sref[ spec_id %in% ids ], by = "spec_id", suffixes = c( "_new", "_ref" ) )
cat( "status: same", sum( st$status_new == st$status_ref ), "of", nrow( st ), "\n" )

co_ref <- as.data.table( open_dataset( file.path( REF, "coefs.parquet" ) ) |>
                           dplyr::filter( spec_id %in% ids ) |> dplyr::collect() )
cat( "coef rows new", nrow( co_new ), "ref", nrow( co_ref ), "\n" )
if ( !identical( sort( names( co_new ) ), sort( names( co_ref ) ) ) )
  cat( "columns differ: new-only", setdiff( names( co_new ), names( co_ref ) ),
       "; ref-only", setdiff( names( co_ref ), names( co_new ) ), "\n" )
key <- intersect( c( "spec_id", "term" ), names( co_ref ) )
m <- merge( co_new, co_ref, by = key, suffixes = c( ".new", ".ref" ), all = TRUE )
num <- intersect( names( co_ref )[ vapply( co_ref, is.numeric, logical( 1 ) ) ], names( co_new ) )
num <- setdiff( num, key )
out <- rbindlist( lapply( num, function( v ) {
  x <- m[[ paste0( v, ".new" ) ]]; y <- m[[ paste0( v, ".ref" ) ]]
  d <- abs( x - y ); rel <- d / pmax( abs( y ), 1e-12 )
  data.table( column = v, rows = length( x ), na_mismatch = sum( is.na( x ) != is.na( y ) ),
              exact = sum( x == y, na.rm = TRUE ), max_abs = suppressWarnings( max( d, na.rm = TRUE ) ),
              max_rel = suppressWarnings( max( rel, na.rm = TRUE ) ) )
} ) )
fwrite( out, file.path( od, "v11_slice_diff.csv" ) )
print( out )
