## 09_npo_grants.R
## Grants paid by public charities, org x year, for the pass-through
## adjustment (research note on measuring sector size; multiverse outcome
## fork exp/rev x gross/net).
##   CORE 2010     harmonized PZ: F9_09_EXP_GRANT_US_ORG_TOT (Part IX line 1 only)
##   SOI 2011-2023 IRS 990 extracts: Part IX lines 1-3. The SOI 990-EZ extract
##                 carries no grants-paid line: EZ grants are unknown (gr_known = 0)
##   e-file 2020-2024 Part IX lines 1-3 (990); Part I line 10 (990-EZ, total only)
## The 1990-2007 Form 990 has no Part IX split, and CORE 2000-2009 has no
## grants-to-organizations field, so the adjustment could start in 2010; in
## practice CORE 2010 and SOI 2011 grant fields are incomplete (7% of SOI 2011
## filers report org grants vs 18-20% from 2012), so analyses start in 2012.
##
## Pass-through definition (main): grants to domestic organizations and
## governments (Part IX line 1) -- money that reappears as another
## organization's revenue. Variants: + foreign grants (line 3), and all
## grants incl. individuals (lines 1-3; an upper bound -- grants to
## individuals leave the sector and are not double counted).
## For 990-EZ (line 10 only) the main measure uses line 10 as an upper bound.
##
## Output: D:/MULTIVERSE/data/npo/orgyear_grants.parquet
##   source, ein, year, form, grants_org, grants_frgn, grants_indiv, grants_all,
##   gr_known, rev_py, exp_py (e-file 990 only: prior-year columns on the same return)

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
num <- function( x ) suppressWarnings( as.numeric( x ) )
z0  <- function( x ) fifelse( is.na( x ), 0, x )

## ---- CORE 2010 ---------------------------------------------------------------
f_core <- file.path( RAW$core_hrmn, "core_pz_2010_grants.parquet" )
if ( !file.exists( f_core ) ) {
  tmp <- tmpfile( ".csv" )
  utils::download.file( "https://nccsdata.s3.us-east-1.amazonaws.com/harmonized/core/501c3-pz/CORE-2010-501C3-CHARITIES-PZ-HRMN.csv",
                        tmp, mode = "wb", quiet = TRUE )
  hdr <- names( fread( tmp, nrows = 0 ) )
  keep <- intersect( c( "EIN2", "TAX_YEAR", "F9_09_EXP_GRANT_US_ORG_TOT", "F9_09_EXP_GRANT_US_INDIV_TOT",
                        "F9_09_EXP_GRANT_FRGN_TOT" ), hdr )
  x <- fread( tmp, select = keep, colClasses = "character" )
  unlink( tmp )
  write_parquet( x, f_core )
}
x <- as.data.table( read_parquet( f_core ) )
core <- x[ , .( source = "CORE", ein = pad( gsub( "[^0-9]", "", EIN2 ), 9 ), year = 2010L, form = NA_character_,
                grants_org = num( F9_09_EXP_GRANT_US_ORG_TOT ),
                grants_frgn = if ( "F9_09_EXP_GRANT_FRGN_TOT" %in% names( x ) ) num( F9_09_EXP_GRANT_FRGN_TOT ) else NA_real_,
                grants_indiv = if ( "F9_09_EXP_GRANT_US_INDIV_TOT" %in% names( x ) ) num( F9_09_EXP_GRANT_US_INDIV_TOT ) else NA_real_ ) ]
## CORE PZ mixes 990 and EZ; EZ rows have no Part IX (NA) -> unknown
core[ , gr_known := as.integer( !is.na( grants_org ) ) ]
core <- unique( core[ order( ein, -z0( grants_org ) ) ], by = "ein" )
log_msg( "CORE 2010: ", nrow( core ), " EINs; with Part IX line 1: ", sum( core$gr_known ) )

## ---- SOI 2011-2023 (990 extract; EZ unknown) -----------------------------------
sdir <- file.path( DATA_HUB, "SOI", "01_data", "04_final" )
soi <- rbindlist( lapply( 2011:2023, function( y ) {
  f <- file.path( sdir, sprintf( "pc_taxyear_%d.csv", y ) )
  x <- fread( f, select = c( "EIN2", "TAXYEAR", "F9_09_EXP_GRANT_US_ORG_TOT", "F9_09_EXP_GRANT_US_INDIV_TOT",
                             "F9_09_EXP_GRANT_FRGN_TOT", "F9_00_TAX_PERIOD_END_DATE" ),
              colClasses = "character", showProgress = FALSE )
  x[ , .( source = "SOI", ein = pad( gsub( "[^0-9]", "", EIN2 ), 9 ), year = as.integer( TAXYEAR ), form = "990",
          taxpd = F9_00_TAX_PERIOD_END_DATE,
          grants_org = z0( num( F9_09_EXP_GRANT_US_ORG_TOT ) ), grants_indiv = z0( num( F9_09_EXP_GRANT_US_INDIV_TOT ) ),
          grants_frgn = z0( num( F9_09_EXP_GRANT_FRGN_TOT ) ), gr_known = 1L ) ]
} ) )
## one row per EIN-year (latest tax period, then larger grants)
setorder( soi, ein, year, -taxpd, -grants_org )
soi <- unique( soi, by = c( "ein", "year" ) )[ , taxpd := NULL ]
log_msg( "SOI 990 grant rows: ", nrow( soi ) )

## ---- e-file 2020-2024 -------------------------------------------------------
ef <- rbindlist( lapply( 2020:2024, function( y ) {
  f <- file.path( RAW$panel990, sprintf( "efile_grants_%d.parquet", y ) )
  if ( !file.exists( f ) ) { warning( "missing ", f ); return( NULL ) }
  d <- as.data.table( read_parquet( f ) )
  g <- function( v ) { k <- intersect( c( v, sub( "^F9_[0-9]{2}_", "", v ) ), names( d ) ); if ( length( k ) ) num( d[[ k[1] ]] ) else rep( NA_real_, nrow( d ) ) }
  gc_ <- function( v ) { k <- intersect( c( v, sub( "^F9_[0-9]{2}_", "", v ) ), names( d ) ); if ( length( k ) ) as.character( d[[ k[1] ]] ) else rep( NA_character_, nrow( d ) ) }
  ein <- if ( "EIN2" %in% names( d ) ) d$EIN2 else if ( "ORG_EIN" %in% names( d ) ) d$ORG_EIN else d$F9_00_ORG_EIN
  rt  <- gc_( "F9_00_RETURN_TYPE" )
  tp  <- gc_( "F9_00_TAX_PERIOD_END_DATE" )
  ty  <- if ( "TAX_YEAR" %in% names( d ) ) as.integer( d$TAX_YEAR ) else rep( y, nrow( d ) )
  out <- data.table( source = "EFILE", ein = pad( gsub( "[^0-9]", "", ein ), 9 ), year = ty, form = rt,
                     g1 = g( "F9_09_EXP_GRANT_US_ORG_TOT" ), g2 = g( "F9_09_EXP_GRANT_US_INDIV_TOT" ),
                     g3 = g( "F9_09_EXP_GRANT_FRGN_TOT" ), g13 = g( "F9_01_EXP_GRANT_SIMILAR_CY" ),
                     rev_py = g( "F9_01_REV_TOT_PY" ), exp_py = g( "F9_01_EXP_TOT_PY" ),
                     rev_cy = g( "F9_01_REV_TOT_CY" ), exp_cy = g( "F9_01_EXP_TOT_CY" ) )
  out[ , is_ez := grepl( "EZ", form ) ]
  out[ , `:=`( grants_org   = fifelse( is_ez, z0( g13 ), z0( g1 ) ),
               grants_indiv = fifelse( is_ez, NA_real_, z0( g2 ) ),
               grants_frgn  = fifelse( is_ez, NA_real_, z0( g3 ) ),
               gr_known     = 1L ) ]
  out[ , c( "g1", "g2", "g3", "g13", "is_ez" ) := NULL ]
  out
} ) )
if ( nrow( ef ) ) {
  ef <- unique( ef[ order( ein, year, -z0( exp_cy ) ) ], by = c( "ein", "year" ) )
  log_msg( "e-file grant rows: ", nrow( ef ) )
}

gr <- rbindlist( list( core, soi, ef ), fill = TRUE )
gr[ , grants_all := z0( grants_org ) + z0( grants_frgn ) + z0( grants_indiv ) ]
print( gr[ , .( rows = .N, known = sum( gr_known ), any_org = mean( grants_org > 0, na.rm = TRUE ),
                org_bn = sum( grants_org, na.rm = TRUE ) / 1e9, all_bn = sum( grants_all ) / 1e9 ),
           keyby = .( source, year ) ] )
write_parquet( gr, file.path( OUT$npo, "orgyear_grants.parquet" ) )
log_msg( "wrote orgyear_grants.parquet: ", nrow( gr ), " rows" )
