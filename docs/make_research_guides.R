## Build datagoodr Data Governance Files (DGFs) for the two research frames.
##
##   Rscript docs/make_research_guides.R [frame] [workers]
##
## frame: cbsa_year, county_year or both (default). Reads the year slices in
## data/<frame>/, takes labels and definitions from data/variable_dictionary.csv
## (gap-fill variants _obs/_lin/_locf get their base variable's text plus a note
## on the variant), and writes docs/research-guide/<frame>/DGF.{csv,xlsx}.
## Type checks and rendering are in docs/render_research_guides.R.

suppressMessages( { library( arrow ); library( data.table ); library( datagoodr ) } )

args    <- commandArgs( trailingOnly = TRUE )
frames  <- if ( length( args ) >= 1 && args[1] != "both" ) args[1] else c( "cbsa_year", "county_year" )
workers <- if ( length( args ) >= 2 ) as.integer( args[2] ) else 8L

dd <- fread( "data/variable_dictionary.csv" )

variant_note <- c(
  obs  = "Observed flag: TRUE where the base value was observed that year, FALSE where it was filled.",
  lin  = "Linear interpolation between observed years (no extrapolation past the first or last).",
  locf = "Last observation carried forward from the most recent observed year."
)
variant_label <- c( obs = "observed flag", lin = "linear fill", locf = "carried forward" )

## Short label: the definition up to its first bracket, semicolon or " = ".
short_label <- function( d ) {
  s <- trimws( sub( "( \\[| \\(|;| = | -- ).*$", "", d ) )
  ifelse( nchar( s ) > 70, paste0( substr( s, 1, 67 ), "..." ), s )
}

describe <- function( vars ) {
  base <- sub( "_(obs|lin|locf)$", "", vars )
  suf  <- ifelse( vars %in% dd$variable, "", sub( "^.*_(obs|lin|locf)$", "\\1", vars ) )
  key  <- ifelse( vars %in% dd$variable, vars, base )
  row  <- dd[ match( key, variable ) ]
  if ( anyNA( row$variable ) ) stop( "not in dictionary: ", paste( vars[ is.na( row$variable ) ], collapse = ", " ) )
  desc <- sub( " \\[interpolated; also _obs.*\\]$", "", row$description )
  lab  <- short_label( desc )
  desc <- ifelse( row$gapfill & suf == "",
                  paste0( desc, ". Gap-filled: interpolated between observed years; see also ", vars, "_obs, _lin and _locf." ),
                  desc )
  is_v <- suf != ""
  lab[ is_v ]  <- paste0( lab[ is_v ], " (", variant_label[ suf[ is_v ] ], ")" )
  desc[ is_v ] <- paste0( desc[ is_v ], ". ", variant_note[ suf[ is_v ] ] )
  data.table( variable = vars, group = row$group, step = row$step, label = lab, description = desc )
}

read_frame <- function( frame ) {
  f <- sort( list.files( file.path( "data", frame ), pattern = "\\.parquet$", full.names = TRUE ) )
  x <- rbindlist( lapply( f, read_parquet ) )
  setorderv( x, c( if ( frame == "cbsa_year" ) "cbsa" else "geoid", "year" ) )
  as.data.frame( x )
}

for ( frame in frames ) {
  out <- file.path( "docs", "research-guide", frame )
  dir.create( out, recursive = TRUE, showWarnings = FALSE )
  x <- read_frame( frame )
  m <- describe( names( x ) )
  fwrite( m, file.path( out, "labels.csv" ) )
  cat( frame, ":", nrow( x ), "rows x", ncol( x ), "columns\n" )
  t0 <- Sys.time()
  dgf <- create_dgf( x,
                     dd_vlabel = m$label, dd_vdesc = m$description,
                     dir = out, file = "DGF", open = FALSE,
                     workers = workers, preflight = "warn" )
  pf <- attr( dgf, "preflight" )
  if ( !is.null( pf ) ) fwrite( as.data.table( pf ), file.path( out, "preflight.csv" ) )
  saveRDS( dgf, file.path( out, "DGF.rds" ) )
  cat( frame, "DGF built in", format( round( Sys.time() - t0, 1 ) ), "\n" )
}
