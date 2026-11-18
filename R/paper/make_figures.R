## R/paper/make_figures.R   (Phase 6 of REPRODUCIBILITY-PLAN.md)
## Writes every figure the paper cites from a named source, listed in paper/figures.csv:
##   figure  file name in paper/figures/ (blank: a prerequisite step that writes no figure)
##   order   run order of the producing scripts
##   script, args, env   how the producer is run (env as NAME=value, space separated)
##   source  the producer's output: data/... under MV_BUILD, otherwise repo-relative.
##           When it is paper/figures/<figure>, the producer writes the figure directly.
##
##   Rscript R/paper/make_figures.R            copy each source into paper/figures/ and report
##   Rscript R/paper/make_figures.R --run      run the producers first, in order
##   Rscript R/paper/make_figures.R --run --from 50   start the producers at order 50
##   Rscript R/paper/make_figures.R --run --from 50 --to 80   run only orders 50-80
##
## Producers run from the repository root with the current MV_* settings; each log goes to
## MV_BUILD/logs/figures_<script>.log. A failed producer stops the run.
## Writes MV_BUILD/check/figures_manifest.csv: figure, source, status, sha256 before and after.
## Exits 1 if any cited figure has no source file.

source( "R/lib/config.R" )
suppressPackageStartupMessages( library( data.table ) )
args <- commandArgs( TRUE )
RUN  <- "--run" %in% args
FROM <- { i <- match( "--from", args ); if ( is.na( i ) ) -Inf else as.numeric( args[ i + 1 ] ) }
TO   <- { i <- match( "--to", args ); if ( is.na( i ) ) Inf else as.numeric( args[ i + 1 ] ) }
FIG  <- file.path( "paper", "figures" )

m <- fread( file.path( "paper", "figures.csv" ), colClasses = "character", na.strings = NULL )
m[ , order := as.numeric( order ) ]
setorder( m, order, figure )
sha <- function( f ) if ( file.exists( f ) ) digest::digest( f, "sha256", file = TRUE ) else NA_character_
src_path <- function( s ) if ( startsWith( s, "data/" ) ) file.path( BUILD, s ) else s

## ---- 1. producers -------------------------------------------------------------------
before <- setNames( vapply( file.path( FIG, m[ nzchar( figure ), figure ] ), sha, "" ), m[ nzchar( figure ), figure ] )
if ( RUN ) {
  steps <- unique( m[ order >= FROM & order <= TO, .( order, script, args, env ) ] )
  dir.create( file.path( BUILD, "logs" ), showWarnings = FALSE )
  for ( k in seq_len( nrow( steps ) ) ) {
    s <- steps[ k ]
    ev <- if ( nzchar( s$env ) ) strsplit( s$env, " " )[[ 1 ]] else character( 0 )
    a  <- if ( nzchar( s$args ) ) strsplit( s$args, " " )[[ 1 ]] else character( 0 )
    lg <- file.path( BUILD, "logs", paste0( "figures_", sub( "\\.R$", "", basename( s$script ) ),
                                            if ( length( a ) ) paste0( "_", paste( a, collapse = "" ) ),
                                            if ( length( ev ) ) paste0( "_", gsub( "[^A-Za-z0-9]", "", paste( ev, collapse = "" ) ) ), ".log" ) )
    cat( format( Sys.time(), "%H:%M:%S" ), s$order, s$script, s$args, s$env, "\n" )
    t0 <- Sys.time()
    ## set env in this process for the child: system2( env = ) does not work on Windows
    kv <- strsplit( ev, "=", fixed = TRUE ); nm <- vapply( kv, `[`, "", 1 )
    old <- Sys.getenv( nm, unset = NA, names = TRUE )
    if ( length( nm ) ) do.call( Sys.setenv, setNames( as.list( vapply( kv, function( x ) paste( x[ -1 ], collapse = "=" ), "" ) ), nm ) )
    st <- system2( file.path( R.home( "bin" ), "Rscript" ), c( s$script, a ), stdout = lg, stderr = lg )
    for ( v in nm ) if ( is.na( old[ v ] ) ) Sys.unsetenv( v ) else do.call( Sys.setenv, setNames( list( old[[ v ]] ), v ) )
    cat( sprintf( "         done in %.1f min\n", as.numeric( difftime( Sys.time(), t0, units = "mins" ) ) ) )
    if ( st != 0 ) stop( "producer failed (exit ", st, "): ", s$script, "; see ", lg )
  }
}

## ---- 2. copy sources into paper/figures/ ----------------------------------------------
f <- m[ nzchar( figure ) ]
out <- f[ , {
  dest <- file.path( FIG, figure ); src <- src_path( source )
  direct <- normalizePath( src, mustWork = FALSE ) == normalizePath( dest, mustWork = FALSE )
  if ( !file.exists( src ) ) st <- "missing source"
  else { if ( !direct ) file.copy( src, dest, overwrite = TRUE ); st <- "ok" }
  .( source = source, status = st, sha_before = before[ figure ], sha_after = sha( dest ) )
}, by = figure ]
out[ status == "ok", status := fifelse( sha_before %in% sha_after & !is.na( sha_before ), "unchanged", "updated" ) ]

dir.create( file.path( BUILD, "check" ), showWarnings = FALSE )
fwrite( out, file.path( BUILD, "check", "figures_manifest.csv" ) )
print( out[ , .N, keyby = status ] )
if ( any( out$status == "missing source" ) ) print( out[ status == "missing source", .( figure, source ) ] )
cited <- unique( gsub( ".*figures/", "", unlist( lapply( list.files( "paper", "\\.qmd$", full.names = TRUE ), function( q )
  regmatches( readLines( q, warn = FALSE ), gregexpr( "figures/[A-Za-z0-9_.-]+\\.png", readLines( q, warn = FALSE ) ) ) ) ) ) )
miss <- setdiff( cited, f$figure )
if ( length( miss ) ) cat( "cited in paper/*.qmd but not in paper/figures.csv:", miss, "\n" )
cat( "wrote", file.path( BUILD, "check", "figures_manifest.csv" ), "\n" )
quit( status = as.integer( any( out$status == "missing source" ) || length( miss ) > 0 ) )
