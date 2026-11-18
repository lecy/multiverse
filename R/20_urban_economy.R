## 20_urban_economy.R
## QCEW county private-sector totals 2000-2024 for the Bettencourt-style
## urban-economy module (design section 5): establishments, employment, wages,
## total private and private excluding NAICS 813 (membership organizations,
## mostly nonprofits) -> cov_qcew.csv. The scaling benchmarks and SAMI-on-SAMI
## parts read the assembled panel and moved to 20b_benchmarks.R (run after 30).
##
## QCEW raw files: D:/MULTIVERSE/raw/cov/qcew/qcew_<year>_<10|813>.csv
## (2000-2013 from the annual by-industry zips, 2014+ from the open-data API).

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
source( "R/13_16_cov_helpers.R" )

qdir <- file.path( OUT$raw_cov, "qcew" )

## ---- QCEW -----------------------------------------------------------------------

{
  rd <- function( y, ind ) {
    f <- file.path( qdir, sprintf( "qcew_%d_%s.csv", y, ind ) )
    if ( !file.exists( f ) || file.size( f ) < 1000 ) { warning( "missing ", f ); return( NULL ) }
    d <- fread( f, select = c( "area_fips", "own_code", "industry_code", "agglvl_code", "year",
                               "disclosure_code", "annual_avg_estabs", "annual_avg_emplvl", "total_annual_wages" ),
                colClasses = list( character = c( "area_fips", "disclosure_code", "industry_code" ) ) )
    d <- d[ own_code == 5 & grepl( "^[0-9]{5}$", area_fips ) & !grepl( "000$|999$", area_fips ) &
              agglvl_code %in% c( 71L, 75L ) ]
    sup <- d$disclosure_code %in% "N"
    for ( v in c( "annual_avg_estabs", "annual_avg_emplvl", "total_annual_wages" ) )
      set( d, i = which( sup ), j = v, value = NA )
    d[ , .( geoid = area_fips, year = as.integer( year ), estab = as.numeric( annual_avg_estabs ),
            emp = as.numeric( annual_avg_emplvl ), wages = as.numeric( total_annual_wages ) ) ]
  }
  q <- rbindlist( lapply( YEARS, function( y ) {
    a <- rd( y, "10" ); b <- rd( y, "813" )
    if ( is.null( a ) ) return( NULL )
    setnames( a, c( "estab", "emp", "wages" ), c( "qcew_priv_estab", "qcew_priv_emp", "qcew_priv_wages" ) )
    if ( !is.null( b ) ) {
      setnames( b, c( "estab", "emp", "wages" ), c( "qcew_813_estab", "qcew_813_emp", "qcew_813_wages" ) )
      a <- merge( a, b, by = c( "geoid", "year" ), all.x = TRUE )
    }
    a } ), fill = TRUE )
  q[ , qcew_813_suppressed := as.integer( is.na( qcew_813_emp ) ) ]
  for ( k in c( "estab", "emp", "wages" ) )
    set( q, j = paste0( "qcew_privx813_", k ),
         value = q[[ paste0( "qcew_priv_", k ) ]] - fcoalesce( q[[ paste0( "qcew_813_", k ) ]], 0 ) )
  cnt <- grep( "^qcew_(priv|813|privx813)_", names( q ), value = TRUE )
  cnt <- setdiff( cnt, "qcew_813_suppressed" )
  q <- harmonize_counties( q, counts = c( cnt, "qcew_813_suppressed" ) )
  validate_cov( q, "QCEW private" )
  print( q[ , .( counties = .N, priv_emp = sum( qcew_priv_emp, na.rm = TRUE ),
                 priv_wages_bn = sum( qcew_priv_wages, na.rm = TRUE ) / 1e9,
                 emp_813 = sum( qcew_813_emp, na.rm = TRUE ), sup_813 = sum( qcew_813_suppressed ) ), keyby = year ] )
  fwrite( q, file.path( OUT$cov, "cov_qcew.csv" ) )
  fwrite( data.table(
    var = c( "qcew_priv_estab", "qcew_priv_emp", "qcew_priv_wages", "qcew_813_*", "qcew_privx813_*", "qcew_813_suppressed" ),
    desc = c( "private establishments (annual average)", "private employment (annual average)",
              "private total annual wages ($)", "NAICS 813 membership organizations, private (NA = suppressed)",
              "private minus NAICS 813 (suppressed 813 treated as 0)", "1 = county's 813 cell suppressed" ),
    type = c( "B", "B", "B", "B", "B", "flag" ), years = "2000-2024",
    source = "BLS QCEW county annual averages, ownership 5 (private)" ),
    file.path( OUT$cov, "cov_qcew_codebook.csv" ) )
  log_msg( "wrote cov_qcew.csv: ", nrow( q ), " rows" )
}
