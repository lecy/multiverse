## 13_16_cov_helpers.R
## Local helpers for the SAIPE / LAUS / IRS SOI / election covariate scripts
## (13-16). Sourced after 00_setup.R and geo_helpers.R.

## Census API key, read at runtime from the project's CENSUS-DATA.R (never
## printed or written anywhere)
census_key <- function() {
  f <- file.path( DROPBOX, "04 - PAPERS", "01 - In Progress", "38 - resource environments",
                  "CENSUS-DATA.R" )
  l <- suppressWarnings( readLines( f ) )
  sub( '^KEY <- "([^"]+)".*$', "\\1", grep( '^KEY <- "', l, value = TRUE )[1] )
}

## browser-like download (BLS and IRS reject bare scripted requests)
UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
fetch_ua <- function( url, dir, file = basename( url ), overwrite = FALSE ) {
  dir.create( dir, recursive = TRUE, showWarnings = FALSE )
  dest <- file.path( dir, file )
  if ( file.exists( dest ) && file.size( dest ) > 0 && !overwrite ) return( dest )
  h <- curl::new_handle( useragent = UA, followlocation = TRUE )
  curl::handle_setheaders( h, "Accept" = "*/*", "Accept-Language" = "en-US,en;q=0.9" )
  r <- tryCatch( curl::curl_download( url, dest, handle = h, quiet = TRUE ),
                 error = function( e ) { unlink( dest ); NA_character_ } )
  if ( is.na( r ) ) return( NA_character_ )
  dest
}

## Harmonize county-level covariates onto 2023 county-equivalents.
##  counts : additive columns (summed / split)
##  rates  : non-additive columns (weighted mean by `w`, copied on splits)
##  w      : weight column for rates (e.g., a universe count); NULL = equal
harmonize_counties <- function( d, counts = character(), rates = character(), w = NULL ) {
  d <- as.data.table( copy( d ) )
  d[ , geoid := pad( geoid, 5 ) ]

  ## 1. Alaska splits onto successors (counts by 2020 population share,
  ##    rates copied); only fills years where the successors are absent
  SPLITS <- list( "02261" = c( "02063", "02066" ),    # Valdez-Cordova (2019)
                  "02232" = c( "02105", "02230" ),    # Skagway-Hoonah-Angoon (2007)
                  "02280" = c( "02195", "02275" ),    # Wrangell-Petersburg (2008)
                  "02201" = c( "02198" ) )            # Prince of Wales-Outer Ketchikan (2008)
  if ( any( d$geoid %in% names( SPLITS ) ) ) {
    pop <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ),
                  colClasses = list( character = "geoid" ) )
    for ( from in intersect( names( SPLITS ), d$geoid ) ) {
      to <- SPLITS[[ from ]]
      sh <- pop[ year == 2020 & geoid %in% to, .( geoid, share = population / sum( population ) ) ]
      old <- d[ geoid == from ]
      new <- rbindlist( lapply( seq_len( nrow( sh ) ), function( i ) {
        x <- copy( old )[ , geoid := sh$geoid[i] ]
        for ( v in counts ) set( x, j = v, value = x[[ v ]] * sh$share[i] )
        x } ) )
      new <- new[ !paste( geoid, year ) %in% d[ , paste( geoid, year ) ] ]
      d <- rbind( d[ geoid != from ], new )
    }
  }

  ## 2. renames / merges (Bedford city, Shannon, Wade Hampton; plus older
  ##    codes seen in 2000-era files: Dade -> Miami-Dade, Clifton Forge city
  ##    -> Alleghany County, South Boston city -> Halifax County)
  OLD <- c( "12025" = "12086", "51560" = "51005", "51780" = "51083" )
  d[ geoid %in% names( OLD ), geoid := OLD[ geoid ] ]
  d[ , geoid := recode_county( geoid ) ]
  dups <- d[ , .N, by = .( geoid, year ) ][ N > 1, .( geoid, year ) ]
  if ( nrow( dups ) ) {
    sub <- d[ dups, on = .( geoid, year ) ]
    sub[ , .w := if ( is.null( w ) ) 1 else as.numeric( get( w ) ) ]
    out <- unique( sub[ , .( geoid, year ) ] )
    if ( length( counts ) )
      out <- merge( out, sub[ , lapply( .SD, sum, na.rm = TRUE ), by = .( geoid, year ),
                              .SDcols = counts ], by = c( "geoid", "year" ) )
    if ( length( rates ) )
      out <- merge( out, sub[ , { ww <- .w; lapply( .SD, function( x )
                                    sum( x * ww, na.rm = TRUE ) / sum( ww[ !is.na( x ) ] ) ) },
                              by = .( geoid, year ), .SDcols = rates ], by = c( "geoid", "year" ) )
    d <- rbind( d[ !dups, on = .( geoid, year ) ], out, fill = TRUE )
  }

  ## 3. Connecticut legacy counties -> planning regions
  if ( any( d$geoid %in% .ct$legacy_county ) ) {
    keep <- d[ , c( "geoid", "year", counts, rates ), with = FALSE ]
    a <- if ( length( counts ) ) ct_legacy_to_pr( keep[ , c( "geoid", "year", counts ), with = FALSE ],
                                                  counts, "total" ) else NULL
    b <- if ( length( rates ) )  ct_legacy_to_pr( keep[ , c( "geoid", "year", rates ), with = FALSE ],
                                                  rates, "rate" ) else NULL
    d <- if ( is.null( a ) ) b else if ( is.null( b ) ) a else merge( a, b, by = c( "geoid", "year" ) )
  }
  d[ , c( "geoid", "year", counts, rates ), with = FALSE ]
}

spine_geoids <- function() fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )$geoid

## validation summary printed by each script
validate_cov <- function( d, label ) {
  sp <- spine_geoids()
  cat( "\n==", label, "==\n" )
  print( d[ , .( counties = uniqueN( geoid ), in_spine = sum( geoid %in% sp ),
                 off_spine = sum( !geoid %in% sp ) ), keyby = year ] )
  dup <- d[ , .N, by = .( geoid, year ) ][ N > 1 ]
  cat( "duplicate geoid-years:", nrow( dup ), "\n" )
  miss <- setdiff( sp, d$geoid )
  cat( "spine counties never observed:", length( miss ), if ( length( miss ) ) paste( head( miss, 20 ), collapse = " " ), "\n" )
}
