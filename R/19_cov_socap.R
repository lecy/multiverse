## 19_cov_socap.R
## Social capital (design 4.3) from Rupasingha, Goetz & Freshwater (2006, with
## updates), Penn State NERCRD: 1997, 2005, 2009, 2014 (no later release).
##
## CAUTION - the published index `sk` is the first principal component of
##   assn  (associational establishments per 1,000: CBP NAICS 813 + recreation),
##   pvote (presidential turnout), respn (Census mail response), and
##   nccs  (nonprofit organizations per 10,000 from NCCS).
## `nccs` IS the dependent variable and `pvote` duplicates the turnout
## covariate, so `sk` is mechanically contaminated. It is kept for validation
## and the kitchen-sink universe only. The analysis measures are:
##   socap_assn_n  association establishments (count, sums across counties;
##                 per capita / log / SAMI forms in the multiverse)
##   socap_respn   Census mail response rate (pop-weighted mean at CBSA)
##   socap_sk2     PC1 of (assn per 1,000, respn) re-estimated each wave
##                 WITHOUT nccs and pvote (county; CBSA = pop-weighted mean)
## Year stamps: data year of the association counts (1997 -> 1997 etc.). A 2000
## value is interpolated linearly from 1997 and 2005 (flag socap_interp2000)
## because 30_assemble only reads 2000-2024.
##
## Outputs: D:/MULTIVERSE/data/cov/cov_socap.csv (+ codebook)

source( "R/00_setup.R" )
source( "R/geo_helpers.R" )
source( "R/13_16_cov_helpers.R" )
suppressPackageStartupMessages( library( readxl ) )

d97 <- file.path( OUT$raw_cov, "socap", "socap_97_05_09.xlsx" )
d14 <- file.path( OUT$raw_cov, "socap", "socap_2014.xlsx" )

## category counts (the components of assn); names differ across waves
CATS <- list( relig = c( "relig", "Religious" ), civic = c( "civic", "Civic" ), bus = c( "bus", "Business" ),
              pol = c( "pol", "Political" ), prof = c( "prof", "Professional" ), labor = c( "labor", "Labor" ),
              bowl = c( "bowl", "Bowling" ), fitness = c( "fitnes", "fitns", "Recreational" ),
              golf = c( "golf", "Golf" ), sport = c( "sport", "Sports" ) )

read_wave <- function( f, sheet, yy, year ) {
  x <- as.data.table( read_excel( f, sheet = sheet ) )
  nm <- names( x ); low <- tolower( nm )
  pick <- function( stems ) { k <- nm[ low %in% tolower( paste0( stems, yy ) ) ]; if ( length( k ) ) x[[ k[1] ]] else NA_real_ }
  pickp <- function( pat ) { k <- grep( pat, nm, value = TRUE, ignore.case = TRUE ); if ( length( k ) ) as.numeric( x[[ k[1] ]] ) else NA_real_ }
  fips <- x[[ grep( "^fips$", nm, ignore.case = TRUE, value = TRUE )[1] ]]
  out <- data.table( geoid = pad( as.integer( fips ), 5 ), year = year )
  for ( k in names( CATS ) ) set( out, j = paste0( "n_", k ), value = as.numeric( pick( CATS[[ k ]] ) ) )
  out[ , socap_assn_n := rowSums( .SD, na.rm = TRUE ), .SDcols = paste0( "n_", names( CATS ) ) ]
  out[ , `:=`( pop_sc = pickp( paste0( "^pop", yy, "$" ) ), assn_pub = pickp( paste0( "^assn", yy, "$" ) ),
               socap_respn = pickp( "^respn" ), pvote = pickp( "^pvote" ),
               nccs = pickp( paste0( "^nccs", yy, "$" ) ), socap_sk = pickp( paste0( "^sk", yy, "$" ) ) ) ]
  out[ !is.na( geoid ) & substr( geoid, 3, 5 ) != "000" ]
}
w <- rbind( read_wave( d97, "1997", "97", 1997L ), read_wave( d97, "2005", "05", 2005L ),
            read_wave( d97, "2009", "09", 2009L ),
            read_wave( d14, excel_sheets( d14 )[1], "2014", 2014L ), fill = TRUE )

## respn is stored as percent in some waves and proportion in others
w[ , socap_respn := ifelse( socap_respn > 1.5, socap_respn / 100, socap_respn ) ]
## check that the category counts reproduce the published assn rate
w[ , assn_rate := 1000 * socap_assn_n / pop_sc ]
chk <- w[ , .( counties = .N, cor_assn = cor( assn_rate, assn_pub, use = "complete.obs" ),
               ratio = median( assn_rate / assn_pub, na.rm = TRUE ),
               cor_sk_nccs = cor( socap_sk, nccs, use = "complete.obs" ),
               cor_sk_pvote = cor( socap_sk, pvote, use = "complete.obs" ) ), keyby = year ]
print( chk )
fwrite( chk, file.path( OUT$diag, "socap_component_check.csv" ) )

## re-estimated index without nccs and pvote (PC1 of z-scored assn rate, respn)
w[ , socap_sk2 := {
  z <- scale( cbind( log1p( assn_rate ), socap_respn ) )
  ok <- stats::complete.cases( z )
  out <- rep( NA_real_, .N )
  if ( sum( ok ) > 10 ) {
    p <- prcomp( z[ ok, ], center = FALSE, scale. = FALSE )
    s <- p$x[ , 1 ]; if ( cor( s, z[ ok, 1 ] ) < 0 ) s <- -s
    out[ ok ] <- s
  }
  out }, by = year ]

keep <- c( "geoid", "year", "socap_assn_n", paste0( "n_", names( CATS ) ), "socap_respn", "socap_sk", "socap_sk2", "pop_sc" )
w <- w[ , ..keep ]
setnames( w, paste0( "n_", names( CATS ) ), paste0( "socap_n_", names( CATS ) ) )
cnt <- c( "socap_assn_n", paste0( "socap_n_", names( CATS ) ), "pop_sc" )
w <- harmonize_counties( w, counts = cnt, rates = c( "socap_respn", "socap_sk", "socap_sk2" ), w = "pop_sc" )

## 2000 = linear interpolation of 1997 and 2005
vv <- setdiff( names( w ), c( "geoid", "year" ) )
a <- w[ year == 1997 ]; b <- w[ year == 2005 ]
m <- merge( a, b, by = "geoid", suffixes = c( ".a", ".b" ) )
y2000 <- m[ , c( list( geoid = geoid, year = 2000L ),
                 lapply( setNames( vv, vv ), function( v ) get( paste0( v, ".a" ) ) + ( 3 / 8 ) * ( get( paste0( v, ".b" ) ) - get( paste0( v, ".a" ) ) ) ) ) ]
w <- rbind( w[ year != 1997 ], y2000 )
w[ , socap_interp2000 := as.integer( year == 2000 ) ]
w[ , pop_sc := NULL ]
setorder( w, geoid, year )
validate_cov( w, "social capital" )
fwrite( w, file.path( OUT$cov, "cov_socap.csv" ) )

fwrite( data.table(
  var = c( "socap_assn_n", "socap_n_*", "socap_respn", "socap_sk", "socap_sk2", "socap_interp2000" ),
  desc = c( "association establishments (sum of 10 categories; CBP)", "category counts",
            "Census mail response rate", "published Rupasingha index (contains nccs = nonprofit density and pvote; validation / kitchen only)",
            "PC1 of log assn rate and respn, re-estimated per wave without nccs and pvote",
            "1 = 2000 interpolated from 1997 and 2005" ),
  type = c( "B", "B", "A", "A", "A", "flag" ),
  years = "1997->2000 (interp), 2005, 2009, 2014; carried forward in 30_assemble",
  source = "Rupasingha, Goetz & Freshwater (NERCRD, Penn State)" ),
  file.path( OUT$cov, "cov_socap_codebook.csv" ) )
log_msg( "wrote cov_socap.csv: ", nrow( w ), " rows" )
