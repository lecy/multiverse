## 74_scaling_aggregation.R
## How much does the scaling exponent B in log(Y) = a + B log(P) depend on the
## geographic aggregate? Units: counties, metro CBSAs, all CBSAs (metro+micro),
## CSAs, and "metro areas" (CSA where one exists, else the metro CBSA).
## Outcomes: 501(c)(3) filer counts and real (2022 $) expenses above the
## $50k-real floor, full sector and the NCCS 12 groups, e-file 2021-2023.
## Estimators: OLS on log(y) for y > 0, and Poisson PML (keeps zeros).
## Side question: county model with metro (CBSA) fixed effects -- within-metro
## slope and how much variance is left after the metro FE.
## Output: D:/MULTIVERSE/data/scaling_agg/
## Usage: Rscript R/74_scaling_aggregation.R

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( fixest ) } )
setFixest_notes( FALSE ); setFixest_nthreads( 4 )

SA <- file.path( BUILD, "data", "scaling_agg" ); dir.create( SA, showWarnings = FALSE )
YRS <- 2021:2023

GRP <- c( ALL = "Full sector (all 501(c)(3))",
          ART = "Arts", EDU = "Education", UNI = "Universities", ENV = "Environment/animals",
          HEL = "Health", HOS = "Hospitals", HMS = "Human services", IFA = "International",
          PSB = "Public/societal benefit", REL = "Religion-related", MMB = "Mutual/membership",
          UNU = "Unknown" )

## ---- org-year table (same construction as 50_followups part 3) ---------------
log_msg( "reading e-file org-years" )
pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )
sp <- fread( file.path( OUT$geo, "county_spine.csv" ),
             colClasses = list( character = c( "geoid", "cbsa", "csa", "state_fips" ) ) )
sp[ is.na( cbsa ), cbsa := "" ]; sp[ is.na( csa ), csa := "" ]
ef <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ),
        col_select = c( "ein", "year", "subsec_group", "rev", "exp", "ind", "geoid" ) ) )[
        year %in% YRS & subsec_group == "c3" & !is.na( geoid ) ]
ef <- merge( ef, pce[ , .( year, to2022, floor_nom ) ], by = "year" )
ef <- ef[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "ntee3" ),
              colClasses = list( character = c( "EIN", "ntee3" ) ) )[ !is.na( ntee3 ) & ntee3 != "" ]
bmf <- unique( bmf[ order( EIN, -year ) ], by = "EIN" )[ , .( ein = EIN, ntee_bmf = toupper( substr( ntee3, 1, 3 ) ) ) ]
ef <- merge( ef, bmf, by = "ein", all.x = TRUE )
map_ntee <- function( n ) { a <- substr( n, 1, 1 )
  fcase( n %in% NTEE_UNIV, "UNI", n %in% NTEE_HOSP, "HOS", a == "A", "ART", a == "B", "EDU",
         a %in% c( "C", "D" ), "ENV", a %in% c( "E", "F", "G", "H" ), "HEL", a %in% LETTERS[ 9:16 ], "HMS",
         a == "Q", "IFA", a %in% LETTERS[ 18:23 ], "PSB", a == "X", "REL", a == "Y", "MMB", default = "UNU" ) }
ef[ , grp := fifelse( is.na( ind ) | ind == "UNU", map_ntee( ntee_bmf ), ind ) ]
ef[ !grp %in% names( GRP ), grp := "UNU" ]
ef <- ef[ geoid %in% sp$geoid ]
ef[ , xr := pmax( exp, 0, na.rm = TRUE ) * to2022 ]

## county x year x group, zeros filled, full sector added
cy <- ef[ , .( n = .N, exp = sum( xr ) ), by = .( geoid, year, grp ) ]
cy <- rbind( cy, ef[ , .( grp = "ALL", n = .N, exp = sum( xr ) ), by = .( geoid, year ) ] )
cy <- CJ( geoid = sp$geoid, year = YRS, grp = names( GRP ) )[ cy, on = .( geoid, year, grp ), `:=`( n = i.n, exp = i.exp ) ]
cy[ is.na( n ), `:=`( n = 0L, exp = 0 ) ]
pop <- fread( file.path( OUT$pop, "county_pop_2000_2025.csv" ), colClasses = list( character = "geoid" ) )[ year %in% YRS ]
cy <- merge( cy, pop[ , .( geoid, year, population ) ], by = c( "geoid", "year" ) )
cy <- merge( cy, sp[ , .( geoid, cbsa, cbsa_type, csa, state_fips ) ], by = "geoid" )
log_msg( "orgs ", ef[ , .N ], "; counties ", cy[ , uniqueN( geoid ) ] )

## ---- aggregates --------------------------------------------------------------
agg <- function( dt, unit ) dt[ , .( n = sum( n ), exp = sum( exp ), population = sum( population ) ),
                                by = c( unit, "year", "grp" ) ][ , unit_id := get( unit ) ][]
## metro area = CSA if the metro belongs to one, else the metro CBSA
cy[ , metro_area := fifelse( cbsa_type == "metro", fifelse( csa != "", paste0( "C", csa ), paste0( "M", cbsa ) ), "" ) ]
UNITS <- list(
  county        = cy[ , unit_id := geoid ][],
  county_metro  = cy[ cbsa_type == "metro" ][ , unit_id := geoid ][],
  cbsa_metro    = agg( cy[ cbsa_type == "metro" ], "cbsa" ),
  cbsa_all      = agg( cy[ cbsa_type %in% c( "metro", "micro" ) ], "cbsa" ),
  csa           = agg( cy[ csa != "" ], "csa" ),
  metro_area    = agg( cy[ metro_area != "" ], "metro_area" ) )
UNIT_LAB <- c( county = "Counties (all)", county_metro = "Counties in metros", cbsa_metro = "Metro CBSAs",
               cbsa_all = "CBSAs (metro + micro)", csa = "CSAs", metro_area = "Metro areas (CSA else metro)" )

## ---- scaling exponents -------------------------------------------------------
fit_one <- function( z ) {
  out <- list()
  for ( v in c( "n", "exp" ) ) {
    y <- z[[ v ]]; pos <- y > 0
    o <- if ( sum( pos ) > 20 ) {
      zz <- data.table( ly = log( y[ pos ] ), lp = log( z$population[ pos ] ) )
      f <- feols( ly ~ lp, zz, vcov = "hetero" )
      c( coeftable( f )[ 2, 1:2 ], r2 = r2( f, "r2" ), rsd = sqrt( mean( resid( f )^2 ) ) )
    } else c( NA, NA, NA, NA )
    p <- if ( sum( pos ) > 20 ) {
      zp <- data.table( y = y, lp = log( z$population ) )
      f <- tryCatch( fepois( y ~ lp, zp, vcov = "hetero" ), error = function( e ) NULL )
      if ( is.null( f ) ) c( NA, NA ) else coeftable( f )[ 2, 1:2 ]
    } else c( NA, NA )
    out[[ v ]] <- data.table( measure = c( n = "count", exp = "expenses" )[[ v ]], units = nrow( z ),
      zeros = sum( !pos ), B_ols = o[ 1 ], se_ols = o[ 2 ], r2_ols = o[ 3 ], resid_sd = o[ 4 ],
      B_pois = p[ 1 ], se_pois = p[ 2 ] )
  }
  rbindlist( out )
}
log_msg( "fitting" )
B <- rbindlist( lapply( names( UNITS ), function( u ) {
  U <- UNITS[[ u ]]
  rbindlist( lapply( YRS, function( yy ) rbindlist( lapply( names( GRP ), function( g )
    fit_one( U[ year == yy & grp == g ] )[ , `:=`( unit = u, year = yy, grp = g ) ] ) ) ) ) } ) )
B[ , `:=`( unit_label = UNIT_LAB[ unit ], grp_label = GRP[ grp ] ) ]
setcolorder( B, c( "unit", "unit_label", "year", "grp", "grp_label", "measure" ) )
fwrite( B, file.path( SA, "scaling_B_by_unit.csv" ) )

## 2022 wide table and spread across aggregations (excluding the county_metro subset)
main <- c( "county", "cbsa_metro", "cbsa_all", "csa", "metro_area" )
W <- dcast( B[ year == 2022 ], grp + grp_label + measure ~ unit, value.var = "B_ols" )
WP <- dcast( B[ year == 2022 ], grp + measure ~ unit, value.var = "B_pois" )
spread <- B[ year == 2022 & unit %in% main, .(
  ols_min = min( B_ols, na.rm = TRUE ), ols_max = max( B_ols, na.rm = TRUE ),
  ols_range = diff( range( B_ols, na.rm = TRUE ) ),
  pois_min = min( B_pois, na.rm = TRUE ), pois_max = max( B_pois, na.rm = TRUE ),
  pois_range = diff( range( B_pois, na.rm = TRUE ) ) ), by = .( grp, measure ) ]
## year stability: range of B over 2021-2023 within unit
yr_stab <- B[ , .( yr_range_ols = diff( range( B_ols, na.rm = TRUE ) ),
                   yr_range_pois = diff( range( B_pois, na.rm = TRUE ) ) ), by = .( unit, grp, measure ) ]
fwrite( W, file.path( SA, "B_ols_2022_wide.csv" ) ); fwrite( WP, file.path( SA, "B_pois_2022_wide.csv" ) )
fwrite( spread, file.path( SA, "B_spread_2022.csv" ) ); fwrite( yr_stab, file.path( SA, "B_year_stability.csv" ) )

## ---- side question: counties with metro fixed effects ------------------------
## Sample: counties in metro CBSAs. Compare
##   (a) pooled  log y ~ log P
##   (b) FE      log y ~ log P | cbsa       (within-metro slope)
##   (c) FE only log y ~ 1 | cbsa
##   (d) metro-level model on the metro aggregates (residual variance per metro)
## Variance left = residual variance; also decompose the pooled residual
## (county SAMI) into between-metro and within-metro parts.
log_msg( "metro FE models" )
CM <- UNITS$county_metro
multi <- CM[ year == 2022 & grp == "ALL", .N, by = cbsa ][ N >= 2, cbsa ]
FE <- rbindlist( lapply( YRS, function( yy ) rbindlist( lapply( names( GRP ), function( g ) {
  rbindlist( lapply( c( "n", "exp" ), function( v ) {
    z <- CM[ year == yy & grp == g ]; z <- z[ z[[ v ]] > 0 ]; z[ , ly := log( get( v ) ) ]; z[ , lp := log( population ) ]
    if ( nrow( z ) < 50 ) return( NULL )
    a  <- feols( ly ~ lp, z )
    b  <- feols( ly ~ lp | cbsa, z, cluster = ~cbsa )
    c0 <- feols( ly ~ 1 | cbsa, z )
    z[ , e_pool := resid( a ) ]
    z[ , e_bar := mean( e_pool ), by = cbsa ]
    ## counties in multi-county metros only, for the within slope's effective sample
    zm <- z[ cbsa %in% multi ]
    bm <- feols( ly ~ lp | cbsa, zm, cluster = ~cbsa )
    Mz <- UNITS$cbsa_metro[ year == yy & grp == g ]; Mz <- Mz[ Mz[[ v ]] > 0 ]
    Mz[ , ly := log( get( v ) ) ]; Mz[ , lp := log( population ) ]
    mm <- feols( ly ~ lp, Mz )
    data.table( year = yy, grp = g, measure = c( n = "count", exp = "expenses" )[[ v ]],
      counties = nrow( z ), metros = uniqueN( z$cbsa ), counties_multi = nrow( zm ),
      var_logy = var( z$ly ),
      B_pooled = coef( a )[[ "lp" ]], r2_pooled = r2( a, "r2" ), resvar_pooled = mean( resid( a )^2 ),
      B_within = coef( b )[[ "lp" ]], se_within = se( b )[[ "lp" ]],
      r2_fe = r2( b, "r2" ), wr2_fe = r2( b, "wr2" ), resvar_fe = mean( resid( b )^2 ),
      r2_fe_only = r2( c0, "r2" ),
      B_within_multi = coef( bm )[[ "lp" ]], wr2_fe_multi = r2( bm, "wr2" ),
      ## pooled county SAMI variance split: between-metro (metro mean) vs within
      sami_between_share = var( z$e_bar ) / var( z$e_pool ),
      B_metro = coef( mm )[[ 2 ]], resvar_metro = mean( resid( mm )^2 ) )
  } ) ) } ) ) ) )
fwrite( FE, file.path( SA, "county_metro_fe.csv" ) )

## does the metro FE soak up only metro SAMI? central vs outlying county check
cen <- CM[ year == 2022 & grp == "ALL" & n > 0 & cbsa %in% multi ]
cen <- merge( cen, sp[ , .( geoid, central ) ], by = "geoid" )
fc <- feols( log( n ) ~ log( population ) + central | cbsa, cen, cluster = ~cbsa )
fe2 <- feols( log( exp ) ~ log( population ) + central | cbsa, cen[ exp > 0 ], cluster = ~cbsa )
cen_tab <- rbind( data.table( measure = "count", term = names( coef( fc ) ), est = coef( fc ), se = se( fc ), wr2 = r2( fc, "wr2" ) ),
                  data.table( measure = "expenses", term = names( coef( fe2 ) ), est = coef( fe2 ), se = se( fe2 ), wr2 = r2( fe2, "wr2" ) ) )
fwrite( cen_tab, file.path( SA, "county_metro_fe_central.csv" ) )

log_msg( "done" )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
print( r3( W ) ); print( r3( WP ) ); print( r3( spread ) )
print( r3( FE[ year == 2022 ] ) ); print( r3( cen_tab ) )
