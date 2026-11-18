## 24_npo_govgrants.R
## Government grants received (Form 990 Part VIII line 1e) for active 501(c)(3)
## filers, 2011-2024, with paper filers imputed. See notes/government-funding-measures.md.
##
## Why: the IRS SOI 990 extracts (our 2011-2019 source) have no line-1e field.
## E-file XML has it, but only for e-filed returns until the 2020 mandate
## (2015: 79% of active 990s, 94% of 990 expenses; 2019: 89% and 97%).
## The 990-EZ has no line 1e in any year (its line 1 pools all contributions).
##
## Method
##   frame    : active c3 filers as in 06_npo_assemble.R (SOI 2011-2019, e-file 2020-2024;
##              max(rev, exp) >= $50k in 2010 dollars; record geocode; invalid NTEE
##              backfilled from the latest valid BMF code)
##   observed : full 990s matched to the e-file revenue table (P08) on EIN x tax-period
##              month. A blank line 1e on an e-filed 990 is a zero.
##   imputed  : government grants = share x total contributions (line 1h, which line 1e
##              is part of; SOI carries it for paper filers). The share comes from
##              A. the same EIN's nearest e-filed year (any distance), else
##              B. the cell ratio sum(gg) / sum(con) of observed filers in the same
##                 year x NTEE major group x size band, by state if >= 30 filers,
##                 else census region, else national.
##              Paper 990s and all 990-EZs are imputed this way; EZs are kept as their
##              own component.
##   validate : 20% of observed active filers with size < $1M (the paper-filer range)
##              are held out, imputed with their own year removed, and compared at the
##              org, county and metro level.
##
## Line 1e is revenue of the same filers counted in the outcomes, so it is mechanically
## tied to density (notes/government-funding-measures.md, "Cautions"). Use it for
## descriptive government-dependence shares or as an outcome, not as a predictor.
## Medicaid / Medicare payments are program-service revenue, not line 1e.
##
## Inputs : raw/panel990/<year>/F9-P08-T00-REVENUE-<year>.parquet (panel990::download_tables,
##          log in raw/panel990/logs/), data/npo/orgyear_{soi,efile}.parquet,
##          data/npo/ein_location_bmf.csv.gz, data/diagnostics/pce_deflator.csv,
##          data/geo/county_spine.csv
## Outputs: D:/MULTIVERSE/data/govgrants/
##          orgyear_govgrants.parquet    one row per active c3 filer-year
##          govgrants_county_year.csv    county x year totals (2022 $)
##          govgrants_cbsa_year.csv      CBSA x year totals (2022 $)
##          govgrants_national.csv       national totals by year and component
##          validation_*.csv             holdout checks
## Usage  : Rscript R/build/B_npo/24_npo_govgrants.R

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )

YRS  <- 2011:2024
OUTG <- file.path( BUILD, "data", "govgrants" )
dir.create( OUTG, recursive = TRUE, showWarnings = FALSE )
set.seed( 20261117 )

num  <- function( x ) suppressWarnings( as.numeric( x ) )
ym   <- function( x ) substr( gsub( "[^0-9]", "", as.character( x ) ), 1, 6 )
ein9 <- function( x ) pad( gsub( "[^0-9]", "", x ), 9 )
rd   <- function( f ) as.data.table( read_parquet( file.path( OUT$npo, f ) ) )

## ---- 1. e-file line 1e, full 990s ------------------------------------------------

P08_COLS <- c( "EIN2", "TAX_YEAR", "RETURN_TYPE", "TAX_PERIOD_END_DATE", "RETURN_GROUP_X",
               "RETURN_PARTIAL_X", "RETURN_AMENDED_X", "RETURN_TIME_STAMP",
               "F9_08_REV_CONTR_GOVT_GRANT", "F9_08_REV_CONTR_TOT", "F9_08_REV_TOT_TOT" )
p08 <- rbindlist( lapply( YRS, function( y ) {
  f <- file.path( RAW$panel990, y, sprintf( "F9-P08-T00-REVENUE-%d.parquet", y ) )
  if ( !file.exists( f ) ) stop( "missing ", f, ": run panel990::download_tables( 'P08', ", y, " )" )
  have <- intersect( P08_COLS, open_dataset( f )$schema$names )
  x <- as.data.table( read_parquet( f, col_select = have ) )
  for ( v in setdiff( P08_COLS, have ) ) x[ , ( v ) := NA_character_ ]
  x[ toupper( RETURN_TYPE ) == "990" ]
} ) )
p08[ , `:=`( ein = ein9( EIN2 ), taxpd = ym( TAX_PERIOD_END_DATE ),
             gg8 = fcoalesce( num( F9_08_REV_CONTR_GOVT_GRANT ), 0 ),
             con8 = fcoalesce( num( F9_08_REV_CONTR_TOT ), 0 ),
             rev8 = num( F9_08_REV_TOT_TOT ) ) ]
## one return per EIN x period: non-group, non-partial, amended, latest timestamp (panel990 rule)
flag <- function( x ) !is.na( x ) & toupper( x ) %in% c( "X", "1", "TRUE", "Y" )
setorder( p08[ , `:=`( o_grp = flag( RETURN_GROUP_X ), o_part = flag( RETURN_PARTIAL_X ),
                       o_amd = !flag( RETURN_AMENDED_X ) ) ],
          ein, taxpd, o_grp, o_part, o_amd, -RETURN_TIME_STAMP )
n0 <- nrow( p08 )
p08 <- unique( p08, by = c( "ein", "taxpd" ) )[ , .( ein, taxpd, gg8, con8, rev8 ) ]
log_msg( sprintf( "e-file P08 990 returns: %s -> %s EIN-periods", format( n0, big.mark = "," ),
                  format( nrow( p08 ), big.mark = "," ) ) )
## line 1e is part of line 1h; a few returns report 1e > 1h
log_msg( "returns with line 1e > line 1h: ", p08[ gg8 > con8 + 1, .N ] )

## ---- 2. active c3 filer frame (as in 06) -----------------------------------------

soi <- rd( "orgyear_soi.parquet" )[ year %in% 2011:2019 & subsec_group == "c3" ]
efl <- rd( "orgyear_efile.parquet" )[ year %in% 2020:2024 & subsec_group == "c3" ]
org <- rbind( soi[ , .( source, form, ein, year, taxpd = ym( taxpd_end ), rev, exp, con, ntee3, ind, geoid ) ],
              efl[ , .( source, form, ein, year, taxpd = ym( taxpd_end ), rev, exp, con, ntee3, ind, geoid ) ] )
org[ , full990 := form %in% c( "PC", "990" ) ]
org[ , ez := form %in% c( "EZ", "990EZ" ) ]
org <- org[ full990 | ez ]

pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )[ , .( year, to2022, floor_nom ) ]
org <- merge( org, pce, by = "year" )
BIG <- 2e11
for ( v in c( "rev", "exp", "con" ) ) set( org, i = which( abs( org[[ v ]] ) > BIG ), j = v, value = NA_real_ )
org[ , size := pmax( rev, exp, na.rm = TRUE ) ]
org <- org[ !is.na( size ) & size >= floor_nom ]

## invalid NTEE -> latest valid BMF code for the EIN (same rule as 06 and 23)
bad_ntee <- function( x ) is.na( x ) | x == "" | substr( x, 1, 3 ) %in% c( "UND", "INV", "Z99" )
loc <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "ntee3" ),
              colClasses = list( character = c( "EIN", "ntee3" ) ) )
loc[ , ntee3 := toupper( substr( ntee3, 1, 3 ) ) ]
ntee_bmf <- unique( loc[ !bad_ntee( ntee3 ) ][ order( EIN, -year ) ], by = "EIN" )[ , .( ein = pad( EIN, 9 ), ntee_bmf = ntee3 ) ]
rm( loc ); invisible( gc() )
org[ , ntee3 := toupper( substr( ntee3, 1, 3 ) ) ]
org <- merge( org, ntee_bmf, by = "ein", all.x = TRUE, sort = FALSE )
org[ bad_ntee( ntee3 ) & !is.na( ntee_bmf ), ntee3 := ntee_bmf ][ , ntee_bmf := NULL ]
org[ , hosp_univ := ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) | ind %in% c( "HOS", "UNI" ) ]
org[ , major := fifelse( bad_ntee( ntee3 ), "?", substr( ntee3, 1, 1 ) ) ]
org[ hosp_univ == TRUE & substr( ntee3, 1, 1 ) == "E", major := "Ehosp" ]
org[ hosp_univ == TRUE & substr( ntee3, 1, 1 ) == "B", major := "Buniv" ]
org[ , con := pmax( fcoalesce( con, 0 ), 0 ) ]

## size bands in 2022 dollars
org[ , band := cut( size * to2022, c( -Inf, 1e5, 2.5e5, 5e5, 1e6, 5e6, 2.5e7, Inf ),
                    labels = c( "<100k", "100-250k", "250-500k", "500k-1M", "1-5M", "5-25M", "25M+" ) ) ]

## state and region from the county code
spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
org[ , st := substr( geoid, 1, 2 ) ]
REGION <- list( NE = c( "09", "23", "25", "33", "44", "50", "34", "36", "42" ),
                MW = c( "17", "18", "26", "39", "55", "19", "20", "27", "29", "31", "38", "46" ),
                S  = c( "10", "11", "12", "13", "24", "37", "45", "51", "54", "01", "21", "28", "47",
                        "05", "22", "40", "48" ),
                W  = c( "04", "08", "16", "30", "32", "35", "49", "56", "02", "06", "15", "41", "53" ) )
org[ , region := "other" ]
for ( r in names( REGION ) ) org[ st %in% REGION[[ r ]], region := r ]

## ---- 3. match e-filed 990s ------------------------------------------------------

org <- merge( org, p08, by = c( "ein", "taxpd" ), all.x = TRUE, sort = FALSE )
org[ , status := fcase( full990 & !is.na( gg8 ), "observed",
                        full990, "paper990",
                        default = "ez" ) ]
## shares use the e-filed return's own line 1h; cap line 1e at 1h
org[ status == "observed", gg_obs := pmin( gg8, pmax( con8, gg8 ) ) ]
org[ status == "observed", s_obs := fifelse( con8 > 0, pmin( gg8 / con8, 1 ), NA_real_ ) ]
cov_tab <- org[ , .( active_c3 = .N, full990 = sum( full990 ), observed = sum( status == "observed" ),
                     paper990 = sum( status == "paper990" ), ez = sum( status == "ez" ),
                     share_990_observed = round( sum( status == "observed" ) / sum( full990 ), 4 ),
                     share_990_rev_observed = round( sum( pmax( rev, 0 )[ status == "observed" ], na.rm = TRUE ) /
                                                       sum( pmax( rev, 0 )[ full990 ], na.rm = TRUE ), 4 ) ),
                keyby = year ]
fwrite( cov_tab, file.path( OUTG, "coverage_by_year.csv" ) )
print( cov_tab )

## ---- 4. imputation ----------------------------------------------------------------

## COVID relief (PPP forgiveness, Provider Relief Fund, ESSER) is often reported on
## line 1e: flag tax periods ending April 2020 - June 2022 (TY2019 fiscal-year filers
## through TY2021)
org[ , covid := taxpd >= "202004" & taxpd <= "202206" ]
OWN_MAX <- 4L

## cell ratios: sum(gg) / sum(con) among observed filers with con > 0
cell_ratio <- function( obs, keys ) {
  obs[ con8 > 0, .( r = sum( gg_obs ) / sum( con8 ), n = .N ), by = keys ]
}
impute <- function( d, obs ) {
  d <- copy( d )
  ## A. own nearest observed year (excluding the row's own year). A donor across the
  ##    COVID-relief boundary counts 5 years farther; donors farther than OWN_MAX are
  ##    not used (validation: ratio 1.02 at 1-2 years, 1.1 at 3-4, 1.36 at 5+).
  own <- obs[ con8 > 0 & !is.na( s_obs ), .( ein, year_own = year, s_own = s_obs, cv_own = covid ) ]
  setkey( own, ein, year_own )
  d[ , rid := .I ]
  ##    Large filers (size >= $25M in 2022 dollars) are idiosyncratic: their own history
  ##    is used at any distance.
  m <- own[ d[ , .( ein, y = year, cv = covid, big = size * to2022 >= 25e6, rid ) ], on = .( ein ),
            allow.cartesian = TRUE, nomatch = NULL ]
  m <- m[ year_own != y ][ , dist := abs( year_own - y ) + 5L * ( cv_own != cv ) ]
  m <- unique( m[ dist <= OWN_MAX | big ][ order( rid, dist, -year_own ) ], by = "rid" )
  d[ m, on = "rid", `:=`( s_own = i.s_own, own_dist = i.dist ) ]
  ## B. cell ratio, state -> region -> national
  b_st  <- cell_ratio( obs, c( "year", "major", "band", "st" ) )[ n >= 30 ]
  b_rg  <- cell_ratio( obs, c( "year", "major", "band", "region" ) )[ n >= 30 ]
  b_nat <- cell_ratio( obs, c( "year", "major", "band" ) )
  b_yr  <- cell_ratio( obs, "year" )
  d[ b_st,  on = .( year, major, band, st ),     r_st  := i.r ]
  d[ b_rg,  on = .( year, major, band, region ), r_rg  := i.r ]
  d[ b_nat, on = .( year, major, band ),         r_nat := i.r ]
  d[ b_yr,  on = .( year ),                      r_yr  := i.r ]
  d[ , s_cell := fcoalesce( r_st, r_rg, r_nat, r_yr ) ]
  d[ , cell_level := fcase( !is.na( r_st ), "state", !is.na( r_rg ), "region", !is.na( r_nat ), "national",
                            default = "year" ) ]
  d[ , method := fifelse( !is.na( s_own ), "own_nearest", paste0( "cell_", cell_level ) ) ]
  d[ is.na( s_own ), own_dist := NA_integer_ ]
  d[ , gg_imp := con * fifelse( !is.na( s_own ), s_own, s_cell ) ]
  d[ con <= 0, `:=`( gg_imp = 0, method = "no_contributions" ) ]
  d[ , c( "rid", "r_st", "r_rg", "r_nat", "r_yr" ) := NULL ]
  d
}

obs <- org[ status == "observed" ]
imp <- impute( org[ status != "observed" ], obs )
org <- rbind( org[ status == "observed" ][ , `:=`( gg = gg_obs, method = "observed" ) ],
              imp[ , gg := gg_imp ], fill = TRUE )
fwrite( org[ , .( n = .N, gg_bn = round( sum( gg ) / 1e9, 2 ) ), keyby = .( year, status, method ) ],
        file.path( OUTG, "imputation_methods.csv" ) )

## ---- 5. validation (holdout of observed filers in the paper-filer size range) -----

val_years <- 2011:2019
pool <- org[ status == "observed" & year %in% val_years & size * to2022 < 1e6 ]
hold <- pool[ sample( .N, round( .2 * .N ) ) ]
hold_key <- hold[ , paste( ein, year ) ]
obs_v <- org[ status == "observed" & !( paste( ein, year ) %chin% hold_key ) ]
vh <- impute( hold[ , .SD, .SDcols = setdiff( names( hold ), c( "gg_imp", "s_own", "own_dist", "s_cell",
                                                                "cell_level", "method" ) ) ], obs_v )
vh[ , truth := gg_obs ]
v_org <- vh[ , .( n = .N, pos_true = mean( truth > 0 ), pos_pred = mean( gg_imp > 0 ),
                  dollars_true_m = sum( truth ) / 1e6, dollars_imp_m = sum( gg_imp ) / 1e6,
                  ratio = sum( gg_imp ) / sum( truth ),
                  mae_share_of_con = mean( abs( gg_imp - truth )[ con > 0 ] / con[ con > 0 ] ),
                  r_org = cor( gg_imp, truth ) ), keyby = .( method_group = sub( "_.*", "", method ) ) ]
v_own <- vh[ method == "own_nearest", .( n = .N, ratio = sum( gg_imp ) / sum( truth ), r_org = cor( gg_imp, truth ) ),
             keyby = .( own_dist = pmin( own_dist, 5 ) ) ]
geo_v <- function( d, unit ) {
  if ( unit == "cbsa" ) d <- merge( d, spine[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa, cbsa_type ) ], by = "geoid" )[ cbsa_type == "metro" ]
  k <- if ( unit == "cbsa" ) "cbsa" else "geoid"
  a <- d[ , .( truth = sum( truth ), imp = sum( gg_imp ) ), by = c( k, "year" ) ]
  a[ , .( unit = unit, units = .N, r = cor( imp, truth ), r_log = cor( log1p( imp ), log1p( truth ) ),
          ratio = sum( imp ) / sum( truth ) ), keyby = year ]
}
v_geo <- rbind( geo_v( vh, "county" ), geo_v( vh, "cbsa" ) )
fwrite( v_org, file.path( OUTG, "validation_org.csv" ) )
fwrite( v_own, file.path( OUTG, "validation_own_distance.csv" ) )
fwrite( v_geo, file.path( OUTG, "validation_geo.csv" ) )
log_msg( "validation, org level:" ); print( v_org )
log_msg( "validation, own-year distance:" ); print( v_own )
log_msg( "validation, metro and county totals:" ); print( v_geo )

## ---- 6. outputs -------------------------------------------------------------------

keep <- c( "ein", "year", "source", "form", "status", "method", "own_dist", "geoid", "ntee3", "major",
           "hosp_univ", "band", "covid", "rev", "exp", "con", "con8", "gg_obs", "gg", "to2022" )
write_parquet( org[ , ..keep ], file.path( OUTG, "orgyear_govgrants.parquet" ) )

agg <- function( d, by ) d[ , .(
  gg_c3            = sum( gg * to2022 ),
  gg_c3_obs        = sum( ( gg * to2022 )[ status == "observed" ] ),
  gg_c3_imp990     = sum( ( gg * to2022 )[ status == "paper990" ] ),
  gg_c3_impez      = sum( ( gg * to2022 )[ status == "ez" ] ),
  gg_c3_nohu       = sum( ( gg * to2022 )[ !hosp_univ ] ),
  gg_c3_nohu_obs   = sum( ( gg * to2022 )[ !hosp_univ & status == "observed" ] ),
  rev_c3           = sum( pmax( rev, 0, na.rm = TRUE ) * to2022 ),
  rev_c3_nohu      = sum( ( pmax( rev, 0, na.rm = TRUE ) * to2022 )[ !hosp_univ ] ),
  con_c3           = sum( con * to2022 ),
  covid_dollars    = sum( ( gg * to2022 )[ covid ] ) / sum( gg * to2022 ),
  n_c3             = .N,
  n_c3_observed    = sum( status == "observed" ),
  n_gg_pos_obs     = sum( status == "observed" & gg > 0 ) ), by = by ]

cty <- agg( org[ !is.na( geoid ) ], c( "geoid", "year" ) )
cty[ , gg_imputed_share := fifelse( gg_c3 > 0, 1 - gg_c3_obs / gg_c3, NA_real_ ) ]
fwrite( cty, file.path( OUTG, "govgrants_county_year.csv" ) )

cb <- merge( cty, spine[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa, cbsa_title, cbsa_type ) ], by = "geoid" )
sv <- setdiff( names( cty ), c( "geoid", "year", "gg_imputed_share" ) )
cb <- cb[ , lapply( .SD, sum ), by = .( cbsa, cbsa_title, cbsa_type, year ), .SDcols = sv ]
cb[ , gg_imputed_share := fifelse( gg_c3 > 0, 1 - gg_c3_obs / gg_c3, NA_real_ ) ]
cb[ , `:=`( gg_share_rev = gg_c3 / rev_c3, gg_share_rev_nohu = gg_c3_nohu / rev_c3_nohu ) ]
fwrite( cb, file.path( OUTG, "govgrants_cbsa_year.csv" ) )

## national totals over located filers (the county panel's universe); unlocated filers,
## mostly foreign-address organizations (EIN prefix 98), are reported separately
nat <- agg( org[ !is.na( geoid ) ], "year" )
nat <- merge( nat, org[ is.na( geoid ), .( gg_c3_unlocated = sum( gg * to2022 ), n_unlocated = .N ), by = year ],
              by = "year", all.x = TRUE )
nat[ , `:=`( gg_share_rev = gg_c3 / rev_c3, gg_share_rev_nohu = gg_c3_nohu / rev_c3_nohu,
             gg_imputed_share = 1 - gg_c3_obs / gg_c3 ) ]
setorder( nat, year )
fwrite( nat, file.path( OUTG, "govgrants_national.csv" ) )
log_msg( "national totals (2022 $ bn):" )
print( nat[ , .( year, gg_bn = round( gg_c3 / 1e9, 1 ), obs_bn = round( gg_c3_obs / 1e9, 1 ),
                 imp990_bn = round( gg_c3_imp990 / 1e9, 1 ), impez_bn = round( gg_c3_impez / 1e9, 1 ),
                 share_rev = round( gg_share_rev, 4 ), share_rev_nohu = round( gg_share_rev_nohu, 4 ),
                 imputed = round( gg_imputed_share, 3 ), covid = round( covid_dollars, 2 ),
                 unlocated_bn = round( gg_c3_unlocated / 1e9, 1 ) ) ] )
log_msg( "metro coverage: ", cb[ cbsa_type == "metro", .( metros = uniqueN( cbsa ),
                                                          max_imputed_share = round( max( gg_imputed_share, na.rm = TRUE ), 3 ) ) ] )
