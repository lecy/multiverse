## Check the data types datagoodr elected for each research-frame variable, and
## correct the ones that are wrong before the research guide is rendered.
##
##   Rscript docs/check_research_guide_types.R [frame]
##
## The expected type of each column comes from its storage type in the parquet
## slices, its values, and its role in data/variable_dictionary.csv:
##   identifier   geographic codes (geoid, cbsa, state_fips): kept as text so
##                leading zeros survive
##   temporal     year, acs_mid_year (grain: year)
##   text         cbsa_title (a name, one per CBSA)
##   categorical  other character columns (cbsa_type, src_990, src_pf)
##   boolean      logical columns (gap-fill _obs flags and nonprofit flags) and
##                0/1 integer flags ("1 = ..." in the dictionary)
##   number       every other numeric column (counts, dollars, shares, rates),
##                including "1 = ..." county flags that aggregation summed into
##                counts of flagged counties (their definitions say so)
## Definitions of columns identical to an earlier column get "Identical to ..."
## (listed in duplicates.csv).
## Writes docs/research-guide/<frame>/type_review.csv (one row per variable:
## datagoodr's guess, the expected type, the reason, and whether it was
## corrected) and replaces DGF.{rds,xlsx} with the corrected DGF.

suppressMessages( { library( arrow ); library( data.table ); library( datagoodr ) } )

args   <- commandArgs( trailingOnly = TRUE )
frames <- if ( length( args ) >= 1 && args[1] != "both" ) args[1] else c( "cbsa_year", "county_year" )

ids      <- c( "geoid", "cbsa", "state_fips" )
years    <- c( "year", "acs_mid_year" )
names_tx <- c( "cbsa_title" )

expected_type <- function( x, labels ) {
  rbindlist( lapply( names( x ), function( v ) {
    z    <- x[[ v ]]
    desc <- labels$description[ labels$variable == v ]
    u    <- unique( z[ !is.na( z ) ] )
    is01 <- is.numeric( z ) && length( u ) > 0 && all( u %in% c( 0, 1 ) )
    code <- grepl( "^1 = ", desc )
    t <- if ( v %in% ids ) c( "identifier", "geographic code; keep as text (leading zeros)" ) else
         if ( v %in% years ) c( "temporal", "calendar year" ) else
         if ( v %in% names_tx ) c( "text", "place name" ) else
         if ( is.character( z ) ) c( "categorical", sprintf( "character code, %d values", length( u ) ) ) else
         if ( is.logical( z ) ) c( "boolean", "logical flag" ) else
         if ( code && ( is01 || length( u ) <= 1 ) ) c( "boolean", "0/1 flag ('1 = ...' in dictionary)" ) else
         if ( code ) c( "number", "county 0/1 flags summed: a count of flagged counties" ) else
         c( "number", "numeric measure" )
    data.table( var_name = v, storage = class( z )[1], n_distinct = length( u ),
                expected = t[1], reason = t[2] )
  } ) )
}

read_frame <- function( frame ) {
  f <- sort( list.files( file.path( "data", frame ), pattern = "\\.parquet$", full.names = TRUE ) )
  x <- rbindlist( lapply( f, read_parquet ) )
  setorderv( x, c( if ( frame == "cbsa_year" ) "cbsa" else "geoid", "year" ) )
  as.data.frame( x )
}

for ( frame in frames ) {
  out    <- file.path( "docs", "research-guide", frame )
  ## DGF_guess.rds keeps datagoodr's untouched guesses, so reruns compare
  ## against them rather than against an already-corrected DGF.
  if ( !file.exists( file.path( out, "DGF_guess.rds" ) ) )
    file.copy( file.path( out, "DGF.rds" ), file.path( out, "DGF_guess.rds" ) )
  dgf    <- readRDS( file.path( out, "DGF_guess.rds" ) )
  labels <- fread( file.path( out, "labels.csv" ), colClasses = "character" )
  x      <- read_frame( frame )

  ex  <- expected_type( x, labels )

  ## Definitions: say when a "1 = ..." flag is a sum over counties, and when a
  ## column is identical to an earlier one (preflight's duplicate_column).
  desc <- setNames( dgf$dd_vdesc, dgf$var_name )
  summed <- ex[ grepl( "summed", reason ), var_name ]
  unit   <- if ( frame == "cbsa_year" ) "member counties" else "merged county-equivalents"
  desc[ summed ] <- paste0( "Count of ", unit, " flagged (county flags summed; fractional where a county was split by population). County flag: ", desc[ summed ] )
  first_same <- vapply( seq_along( x ), function( j ) {
    for ( i in seq_len( j - 1 ) ) if ( identical( x[[ i ]], x[[ j ]] ) ) return( names( x )[ i ] )
    NA_character_ }, "" )
  dup <- which( !is.na( first_same ) )
  desc[ names( x )[ dup ] ] <- paste0( desc[ names( x )[ dup ] ], " Identical to `", first_same[ dup ], "` in this frame." )
  dgf$dd_vdesc <- unname( desc[ dgf$var_name ] )
  cat( frame, ":", length( summed ), "summed flags,", length( dup ), "duplicate columns noted\n" )
  fwrite( data.table( var_name = names( x )[ dup ], identical_to = first_same[ dup ] ),
          file.path( out, "duplicates.csv" ) )
  rev <- merge( data.table( var_name = dgf$var_name, guessed = dgf$desired_data_type,
                            guessed_class = dgf$desired_data_class ),
                ex, by = "var_name", sort = FALSE )
  ## datagoodr writes "numeric" for numbers in some places; treat as number.
  rev[ , guessed_norm := fifelse( guessed %in% c( "numeric", "number" ), "number", guessed ) ]
  rev[ , corrected := guessed_norm != expected ]

  fix <- rev[ corrected == TRUE ]
  cat( "\n==", frame, ":", nrow( rev ), "variables;", nrow( fix ), "type elections corrected\n" )
  if ( nrow( fix ) ) print( fix[ , .N, by = .( guessed, expected ) ] )

  if ( nrow( fix ) ) {
    tm <- fix[ expected == "temporal", var_name ]
    dgf <- retype_dgf( dgf, x,
                       types = setNames( fix$expected, fix$var_name ),
                       units = if ( length( tm ) ) setNames( rep( "year", length( tm ) ), tm ) else NULL )
  }
  ## Year columns that were already temporal still need the year grain.
  yr <- intersect( years, dgf$var_name )
  dgf <- retype_dgf( dgf, x, units = setNames( rep( "year", length( yr ) ), yr ) )

  ## Confirm: every variable now carries its expected type.
  now <- data.table( var_name = dgf$var_name, final = dgf$desired_data_type )
  rev <- merge( rev, now, by = "var_name", sort = FALSE )
  rev[ , final_norm := fifelse( final %in% c( "numeric", "number" ), "number", final ) ]
  bad <- rev[ final_norm != expected ]
  if ( nrow( bad ) ) { print( bad ); stop( frame, ": ", nrow( bad ), " variables still mistyped" ) }
  cat( frame, ": all", nrow( rev ), "variables carry their expected type\n" )
  print( rev[ , .N, by = final_norm ] )

  v <- inspect_dgf( dgf, verbose = FALSE )
  cat( frame, ": inspect_dgf valid =", v$valid, "\n" )
  if ( !isTRUE( v$valid ) ) print( v$problems )

  fwrite( rev[ , .( var_name, storage, n_distinct, guessed, expected, final = final_norm, corrected, reason ) ],
          file.path( out, "type_review.csv" ) )
  saveRDS( dgf, file.path( out, "DGF.rds" ) )
  save_to_excel( dgf, file.path( out, "DGF.xlsx" ), open = FALSE )
  fwrite( dgf, file.path( out, "DGF.csv" ) )
}
