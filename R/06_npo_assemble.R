## 06_npo_assemble.R
## Combine CORE (2000-2019), SOI (2011-2023), SOI 990-PF extracts, and e-file
## (2021-2024) org-year tables into county x year nonprofit outcomes; splice;
## write overlap diagnostics (plan sections 2.2-2.5).
##
## Location rules
##   EIN-only sources (CORE, SOI 990-PF): BMF snapshot county for the same
##     EIN-year; else that EIN's nearest BMF year; else its SOI / e-file
##     current geocode
##   SOI, e-file: current geocode carried on the record
## Universe rules
##   active filer = max(revenue, expenses) >= $50k in 2010 dollars (PCE)
##   filters: all 501(c) | 501(c)(3) | 501(c)(3) excl. hospitals & universities
##            | human services (NTEE I-P); foundations tabulated separately
## NTEE: invalid codes (e-file "UND", "Z99", blank) replaced by the EIN's most recent
##   valid BMF code (2026-11-17; drives the hospital/university and human-services flags)
## Data-error guard: a single record > $200B in revenue, expenses, or assets
##   has that field set to NA (logged)
## Splice
##   990/EZ : CORE 2000-2010 | SOI 2011-2019 | e-file 2020-2024  (see SPLICE below)
##   990-PF : CORE 2000-2015 | interpolated 2016-2018 (no complete source) |
##            SOI extracts 2019-2020 | e-file 2021-2024
##
## Outputs (D:/MULTIVERSE/data/npo/)
##   npo_county_year_by_source.csv   source-pure series, long by source
##   npo_county_year.csv             spliced series + BMF counts, one row per county-year
##   npo_cbsa_year.csv               same, aggregated to CBSA 2023
##   ../diagnostics/splice_overlap.csv, location / guard diagnostics

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )

rd <- function( f ) as.data.table( read_parquet( file.path( OUT$npo, f ) ) )

## ---- price index (PCE, annual average), floor in nominal dollars ---------

f_pce <- fetch( "https://fred.stlouisfed.org/graph/fredgraph.csv?id=PCEPI", RAW$census_popest,
                file = "fred_pcepi.csv" )
pce <- fread( f_pce )
setnames( pce, c( "date", "pce" ) )
pce <- pce[ , .( pce = mean( pce ) ), by = .( year = year( as.IDate( date ) ) ) ][ year %in% 1999:2025 ]
pce[ , to2022 := pce[ year == 2022 ] / pce ]                       # nominal -> 2022 $
pce[ , floor_nom := FLOOR_REAL_2010 * pce / pce[ year == 2010 ] ]
fwrite( pce, file.path( OUT$diag, "pce_deflator.csv" ) )

## ---- org-year tables ---------------------------------------------------

core  <- rd( "orgyear_core.parquet" )[ year %in% 2000:2019 ]
soipf <- rd( "orgyear_soi_pf.parquet" )[ year %in% 2011:2020 ]
soi   <- rd( "orgyear_soi.parquet" )[ year %in% 2011:2023 ]
ef    <- rd( "orgyear_efile.parquet" )[ year %in% 2020:2024 ]
efpf  <- rd( "orgyear_efile_pf.parquet" )[ year %in% 2020:2024 ]

## locate EIN-only records through the BMF snapshot table
loc <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ),
              colClasses = list( character = c( "EIN", "geoid", "ntee3", "subsec", "zip5" ) ) )
loc <- loc[ !is.na( geoid ), .( ein = EIN, year_bmf = year, geoid_bmf = geoid, ntee3_bmf = ntee3 ) ]
setkey( loc, ein, year_bmf )
cur <- unique( rbind( soi[ !is.na( geoid ), .( ein, geoid, ntee3, year ) ],
                      ef[  !is.na( geoid ), .( ein, geoid, ntee3, year ) ] )[ order( ein, -year ) ],
               by = "ein" )[ , .( ein, geoid_cur = geoid, ntee3_cur = ntee3 ) ]

locate_ein <- function( d ) {
  d <- copy( d )[ , rowid := .I ]
  j <- loc[ d[ , .( ein, year_bmf = year, rowid ) ], on = .( ein, year_bmf ), roll = "nearest" ]
  d[ j, on = "rowid", `:=`( geoid_bmf = i.geoid_bmf, ntee3_bmf = i.ntee3_bmf, year_bmf = i.year_bmf ) ]
  ## roll returns the query year in year_bmf; recover the matched year
  jj <- loc[ d[ , .( ein, y = year, rowid ) ], on = .( ein, year_bmf = y ), roll = "nearest",
             .( rowid, matched = x.year_bmf ) ]
  d[ jj, on = "rowid", year_bmf := i.matched ]
  d <- merge( d, cur, by = "ein", all.x = TRUE )
  if ( !"ntee3" %in% names( d ) ) d[ , ntee3 := NA_character_ ]
  d[ , geoid := fifelse( !is.na( geoid_bmf ), geoid_bmf, geoid_cur ) ]
  d[ , ntee3 := fcoalesce( fifelse( ntee3_bmf == "", NA_character_, ntee3_bmf ),
                           fifelse( ntee3 == "", NA_character_, toupper( substr( ntee3, 1, 3 ) ) ),
                           ntee3_cur ) ]
  d[ , loc_rule := fcase( !is.na( geoid_bmf ) & year_bmf == year, "bmf_same_year",
                          !is.na( geoid_bmf ), "bmf_nearest_year",
                          !is.na( geoid_cur ), "current_geocode", default = "unlocated" ) ]
  d
}
core  <- locate_ein( core )
soipf <- locate_ein( soipf )
fwrite( rbind( core[ , .N, by = .( source, subsec_group, year, loc_rule ) ],
               soipf[ , .N, by = .( source, subsec_group, year, loc_rule ) ] )[ order( source, year ) ],
        file.path( OUT$diag, "ein_location_rules.csv" ) )

## ---- stack, guard, flag ------------------------------------------------

keep <- c( "source", "subsec_group", "ein", "year", "rev", "exp", "con", "assets",
           "grants_paid", "ntee3", "ind", "geoid" )
stack <- list( core, soipf, soi, ef, efpf )
for ( d in stack ) for ( v in setdiff( keep, names( d ) ) ) d[ , ( v ) := NA ]
org <- rbindlist( lapply( stack, function( d ) d[ , ..keep ] ) )
org[ , ntee3 := toupper( ntee3 ) ]

## invalid NTEE codes -> most recent valid BMF code for the EIN (2026-11-17). E-file
## records carry "UND" for ~16% of active c3 filers 2020-24; before this fix ~590
## hospitals / universities a year stayed in the nohu series (~6.5% of exp_c3_nohu)
## and human-services counts were 12-13% low. Same rule as 23_cov_compete.R.
bad_ntee <- function( x ) is.na( x ) | x == "" | substr( x, 1, 3 ) %in% c( "UND", "INV", "Z99" )
ntee_bmf <- unique( loc[ !bad_ntee( toupper( ntee3_bmf ) ) ][ order( ein, -year_bmf ) ],
                    by = "ein" )[ , .( ein, ntee_bmf = toupper( substr( ntee3_bmf, 1, 3 ) ) ) ]
org <- merge( org, ntee_bmf, by = "ein", all.x = TRUE, sort = FALSE )
fwrite( org[ , .( n = .N, invalid = sum( bad_ntee( ntee3 ) ),
                  backfilled = sum( bad_ntee( ntee3 ) & !is.na( ntee_bmf ) ) ), keyby = .( source, year ) ],
        file.path( OUT$diag, "ntee_backfill.csv" ) )
log_msg( "invalid NTEE codes: ", sum( bad_ntee( org$ntee3 ) ), "; backfilled from BMF: ",
         sum( bad_ntee( org$ntee3 ) & !is.na( org$ntee_bmf ) ) )
org[ bad_ntee( ntee3 ) & !is.na( ntee_bmf ), ntee3 := ntee_bmf ]
org[ , ntee_bmf := NULL ]

BIG <- 2e11
guard <- org[ pmax( abs( rev ), abs( exp ), abs( assets ), na.rm = TRUE ) > BIG,
              .( source, subsec_group, ein, year, rev, exp, assets ) ]
fwrite( guard, file.path( OUT$diag, "guard_over_200bn.csv" ) )
for ( v in c( "rev", "exp", "con", "assets", "grants_paid" ) )
  set( org, i = which( abs( org[[ v ]] ) > BIG ), j = v, value = NA_real_ )
log_msg( "records with a field > $200B set to NA: ", nrow( guard ) )

## grants paid to other organizations (pass-through; 09_npo_grants.R).
## Net measures exist from 2012; filers with no grants data (SOI-era 990-EZ)
## count as zero pass-through.
fg <- file.path( OUT$npo, "orgyear_grants.parquet" )
NET_START <- 2012L   # CORE 2010 and SOI 2011 grant fields are incomplete (09_npo_grants.R log)
if ( file.exists( fg ) ) {
  gr <- rd( "orgyear_grants.parquet" )[ , .( source, ein, year, g_org = grants_org ) ]
  org <- merge( org, gr, by = c( "source", "ein", "year" ), all.x = TRUE )
} else org[ , g_org := NA_real_ ]
org[ , g_org := fifelse( is.na( g_org ) | g_org < 0, 0, g_org ) ]
org[ abs( g_org ) > BIG, g_org := 0 ]
org <- merge( org, pce[ , .( year, to2022, floor_nom ) ], by = "year" )
org[ , hosp_univ := ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) | ind %in% c( "HOS", "UNI" ) ]
org[ , human_svc := substr( ntee3, 1, 1 ) %in% LETTERS[ 9:16 ] ]       # I-P
org[ , size := pmax( rev, exp, na.rm = TRUE ) ]
org[ , active := !is.na( size ) & size >= floor_nom ]
pos <- function( x ) pmax( x, 0, na.rm = TRUE )

fwrite( org[ , .( n = .N, unlocated = mean( is.na( geoid ) ) ), by = .( source, subsec_group, year ) ],
        file.path( OUT$diag, "unlocated_by_source.csv" ) )

## ---- tabulate by source -------------------------------------------------

pc <- org[ subsec_group != "pf" & !is.na( geoid ), .(
  n_filer_all        = sum( active ),
  n_filer_c3         = sum( active & subsec_group == "c3" ),
  n_filer_c3_nohu    = sum( active & subsec_group == "c3" & !hosp_univ ),
  n_filer_c3_hs      = sum( active & subsec_group == "c3" & human_svc ),
  n_filer_c3_nofloor = sum( subsec_group == "c3" ),
  exp_all            = sum( ( pos( exp ) * to2022 )[ active ] ),
  exp_c3             = sum( ( pos( exp ) * to2022 )[ active & subsec_group == "c3" ] ),
  exp_c3_nohu        = sum( ( pos( exp ) * to2022 )[ active & subsec_group == "c3" & !hosp_univ ] ),
  exp_c3_hs          = sum( ( pos( exp ) * to2022 )[ active & subsec_group == "c3" & human_svc ] ),
  rev_c3             = sum( ( pos( rev ) * to2022 )[ active & subsec_group == "c3" ] ),
  rev_c3_nohu        = sum( ( pos( rev ) * to2022 )[ active & subsec_group == "c3" & !hosp_univ ] ),
  ## net of pass-through grants to organizations (Part IX line 1; EZ line 10)
  grants_org_c3      = sum( ( g_org * to2022 )[ active & subsec_group == "c3" ] ),
  exp_c3_net         = sum( ( pos( exp - g_org ) * to2022 )[ active & subsec_group == "c3" ] ),
  exp_c3_nohu_net    = sum( ( pos( exp - g_org ) * to2022 )[ active & subsec_group == "c3" & !hosp_univ ] ),
  rev_c3_net         = sum( ( pos( rev - g_org ) * to2022 )[ active & subsec_group == "c3" ] ),
  rev_c3_nohu_net    = sum( ( pos( rev - g_org ) * to2022 )[ active & subsec_group == "c3" & !hosp_univ ] ),
  con_c3             = sum( ( pos( con ) * to2022 )[ active & subsec_group == "c3" ] ),
  assets_c3          = sum( ( pos( assets ) * to2022 )[ active & subsec_group == "c3" ] ) ),
  by = .( source, geoid, year ) ]

pf <- org[ subsec_group == "pf" & !is.na( geoid ), .(
  n_pf      = .N,
  pf_assets = sum( pos( assets ) * to2022 ),
  pf_grants = sum( pos( grants_paid ) * to2022 ) ),
  by = .( source, geoid, year ) ]

by_src <- merge( pc, pf, by = c( "source", "geoid", "year" ), all = TRUE )
fwrite( by_src, file.path( OUT$npo, "npo_county_year_by_source.csv" ) )
print( by_src[ , .( counties = .N, n_filer_c3 = sum( n_filer_c3, na.rm = TRUE ),
                    exp_c3_bn = round( sum( exp_c3, na.rm = TRUE ) / 1e9 ),
                    n_pf = sum( n_pf, na.rm = TRUE ),
                    pf_assets_bn = round( sum( pf_assets, na.rm = TRUE ) / 1e9 ) ),
               keyby = .( source, year ) ] )

## ---- splice diagnostics (overlap years, CBSA level) ---------------------

spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
cb <- merge( by_src, spine[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
vars <- c( "n_filer_c3", "exp_c3", "exp_c3_nohu", "con_c3", "n_pf", "pf_assets" )
cbsa_src <- cb[ , lapply( .SD, sum, na.rm = TRUE ), by = .( source, cbsa, year ), .SDcols = vars ]

splice_pair <- function( a, b, yrs, vv = vars ) {
  w <- dcast( cbsa_src[ source %in% c( a, b ) & year %in% yrs ],
              cbsa + year ~ source, value.var = vv )
  rbindlist( lapply( vv, function( v ) {
    xa <- paste0( v, "_", a ); xb <- paste0( v, "_", b )
    if ( !all( c( xa, xb ) %in% names( w ) ) ) return( NULL )
    w[ , {
      x <- get( xa ); y <- get( xb ); ok <- x > 0 & y > 0 & !is.na( x ) & !is.na( y )
      .( var = v, pair = paste( a, "->", b ),
         national_ratio = sum( y, na.rm = TRUE ) / sum( x, na.rm = TRUE ),
         median_ratio = median( ( y / x )[ ok ] ),
         p10_ratio = quantile( ( y / x )[ ok ], .1 ), p90_ratio = quantile( ( y / x )[ ok ], .9 ),
         log_corr = cor( log( x[ ok ] ), log( y[ ok ] ) ), cbsas = sum( ok ) ) }, by = year ]
  } ) )
}
diag <- rbind( splice_pair( "CORE", "SOI", 2011:2019, vars[ 1:4 ] ),
               splice_pair( "SOI", "EFILE", 2020:2023, vars[ 1:4 ] ),
               splice_pair( "CORE", "SOI_PF", 2011:2015, vars[ 5:6 ] ),
               splice_pair( "SOI_PF", "EFILE", 2020, vars[ 5:6 ] ) )
fwrite( diag, file.path( OUT$diag, "splice_overlap.csv" ) )
print( diag[ var %in% c( "n_filer_c3", "exp_c3", "n_pf" ) ] )

## ---- spliced series -----------------------------------------------------

## SOI's 2020 EZ extract is short (~154k rows vs ~220k other years), so TY2020
## comes from e-file (near-complete once the e-file mandate applies)
SPLICE <- data.table( year = YEARS )[ , source := fcase( year <= 2010, "CORE",
                                                         year <= 2019, "SOI",
                                                         default = "EFILE" ) ]
SPLICE_PF <- data.table( year = YEARS )[ , source := fcase( year <= 2015, "CORE",
                                                            year %in% 2019:2020, "SOI_PF",
                                                            year >= 2021, "EFILE",
                                                            default = NA_character_ ) ]
pcs <- merge( pc, SPLICE, by = c( "year", "source" ) )
pfs <- merge( pf, SPLICE_PF, by = c( "year", "source" ) )
setnames( pcs, "source", "src_990" ); setnames( pfs, "source", "src_pf" )

## BMF registered organizations. n_bmf_* = reconstructed stock 2000-2024
## (02b); the raw snapshot counts (legacy 2000-2022 + monthly 2023-2024) are
## kept as n_bmf_*_snap.
bmf <- fread( file.path( OUT$npo, "bmf_county_year.csv" ), colClasses = list( character = "geoid" ) )
fm <- file.path( OUT$npo, "bmf_county_year_monthly.csv" )
if ( file.exists( fm ) ) bmf <- rbind( bmf[ !year %in% 2023:2024 ],
                                       fread( fm, colClasses = list( character = "geoid" ) ), fill = TRUE )
if ( !"bmf_src_monthly" %in% names( bmf ) ) bmf[ , bmf_src_monthly := 0L ]
bmf[ is.na( bmf_src_monthly ), bmf_src_monthly := 0L ]
fr <- file.path( OUT$npo, "bmf_county_year_recon.csv" )
if ( file.exists( fr ) ) {
  sv <- grep( "^n_bmf", names( bmf ), value = TRUE )
  setnames( bmf, sv, paste0( sv, "_snap" ) )
  rec <- fread( fr, colClasses = list( character = "geoid" ) )
  bmf <- merge( rec, bmf, by = c( "geoid", "year" ), all = TRUE )
}
fe <- file.path( OUT$npo, "npo_entry_county_year.csv" )
entry <- if ( file.exists( fe ) ) fread( fe, colClasses = list( character = "geoid" ) ) else NULL
pop <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )

cy <- CJ( geoid = spine$geoid, year = YEARS )
cy <- merge( cy, pop[ , .( geoid, year, population ) ], by = c( "geoid", "year" ), all.x = TRUE )
cy <- merge( cy, SPLICE[ , .( year, src_990_y = source ) ], by = "year" )
cy <- merge( cy, SPLICE_PF[ , .( year, src_pf_y = source ) ], by = "year" )
cy <- merge( cy, pcs[ , !"src_990" ], by = c( "geoid", "year" ), all.x = TRUE )
cy <- merge( cy, pfs[ , !"src_pf" ], by = c( "geoid", "year" ), all.x = TRUE )
setnames( cy, c( "src_990_y", "src_pf_y" ), c( "src_990", "src_pf" ) )

## counties with no filers in a covered year are true zeros
v990 <- names( pc )[ !names( pc ) %in% c( "source", "geoid", "year" ) ]
vpf  <- c( "n_pf", "pf_assets", "pf_grants" )
for ( v in v990 ) set( cy, i = which( is.na( cy[[ v ]] ) ), j = v, value = 0 )
## grants-to-organizations incomplete before 2012
for ( v in grep( "_net$|^grants_org", v990, value = TRUE ) )
  set( cy, i = which( cy$year < NET_START ), j = v, value = NA_real_ )
for ( v in vpf )  set( cy, i = which( is.na( cy[[ v ]] ) & !is.na( cy$src_pf ) ), j = v, value = 0 )

## foundations 2016-2018: linear interpolation between 2015 and 2019, by county
cy[ , pf_interp := year %in% 2016:2018 ]
setorder( cy, geoid, year )
for ( v in vpf ) cy[ , ( v ) := {
  x <- get( v ); g <- which( year %in% 2016:2018 )
  a <- x[ year == 2015 ]; b <- x[ year == 2019 ]
  if ( length( a ) && length( b ) && !is.na( a ) && !is.na( b ) )
    x[ g ] <- a + ( b - a ) * ( year[ g ] - 2015 ) / 4
  x }, by = geoid ]
cy[ pf_interp == TRUE, src_pf := "INTERP" ]

cy <- merge( cy, bmf, by = c( "geoid", "year" ), all.x = TRUE )
## BMF: a county absent from a covered year has zero registered orgs
bmf_years <- unique( bmf$year )
for ( v in grep( "^n_bmf", names( cy ), value = TRUE ) )
  set( cy, i = which( is.na( cy[[ v ]] ) & cy$year %in% bmf_years ), j = v, value = 0 )
if ( !is.null( entry ) ) {
  cy <- merge( cy, entry, by = c( "geoid", "year" ), all.x = TRUE )
  ent_years <- unique( entry$year )
  for ( v in grep( "^n_entry", names( cy ), value = TRUE ) )
    set( cy, i = which( is.na( cy[[ v ]] ) & cy$year %in% ent_years ), j = v, value = 0 )
  cy[ , entry_src_monthly := as.integer( year >= 2022 ) ]
}
cy[ , post_splice_soi := year >= 2011 ][ , post_splice_efile := year >= 2020 ]
fwrite( cy, file.path( OUT$npo, "npo_county_year.csv" ) )

## ---- CBSA aggregation ---------------------------------------------------

flags <- c( "src_990", "src_pf", "bmf_months_off", "post_splice_soi", "post_splice_efile", "pf_interp",
            "bmf_src_monthly", "entry_src_monthly" )
sumv <- setdiff( names( cy ), c( "geoid", "year", flags ) )
cby <- merge( cy, spine[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa, cbsa_title, cbsa_type ) ], by = "geoid" )
cby <- cby[ , c( lapply( .SD, sum ),
                 .( src_990 = first( src_990 ), src_pf = first( src_pf ),
                    pf_interp = first( pf_interp ), n_counties = .N ) ),
            by = .( cbsa, cbsa_title, cbsa_type, year ), .SDcols = sumv ]
fwrite( cby, file.path( OUT$npo, "npo_cbsa_year.csv" ) )

print( cby[ cbsa_type == "metro", .( metros = .N, pop_m = round( sum( population ) / 1e6 ),
                                     n_filer_c3 = sum( n_filer_c3 ), exp_c3_bn = round( sum( exp_c3 ) / 1e9 ),
                                     n_pf = round( sum( n_pf ) ), n_bmf_c3pc = sum( n_bmf_c3pc ),
                                     src = first( src_990 ), src_pf = first( src_pf ) ), keyby = year ] )
