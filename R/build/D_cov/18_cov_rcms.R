## 18_cov_rcms.R
## Religion block (design 4.1): congregations, adherents, Catholic adherents,
## and denominational concentration (HHI of adherent shares across religious
## bodies) from the Religious Congregations & Membership Study:
##   2000  ARDA county file (.SAV, replication/np_density_refactor/Data)
##   2010  ARDA county file (.SAV, same folder)
##   2020  U.S. Religion Census group-by-county detail (usreligioncensus.org)
## Comparability: the number of reporting bodies grows (2000: ~149, 2010: ~236,
## 2020: ~372) and historically Black denominations are largely absent in 2000.
## Group-level HHI is dominated by the few largest bodies, so the drift is
## small, but 2000 is flagged (rel_2000_noblack = 1).
## HHI is computed from group adherents AFTER harmonizing counties and, for
## CBSAs, after summing group adherents across member counties (an HHI cannot
## be averaged), so a CBSA-level file is written alongside the county file.
## Observed years only; interpolation / carry-forward is done in 30_assemble.
##
## Outputs: D:/MULTIVERSE/data/cov/cov_rcms.csv            (county x year)
##          D:/MULTIVERSE/data/cov/cbsa/cbsa_rcms.csv      (CBSA x year HHI)
##          D:/MULTIVERSE/data/cov/cov_rcms_codebook.csv

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )
source( "R/lib/13_16_cov_helpers.R" )
suppressPackageStartupMessages( { library( haven ); library( readxl ) } )

rep_dir <- file.path( DROPBOX, "04 - PAPERS", "01 - In Progress", "38 - resource environments",
                      "replication", "np_density_refactor", "Data" )
f00 <- file.path( rep_dir, "Religious Congregations and Membership Study, 2000 (Counties File).SAV" )
f10 <- file.path( rep_dir, "U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).SAV" )
f20 <- file.path( OUT$raw_cov, "rcms", "2020_USRC_Group_Detail.xlsx" )
stopifnot( file.exists( f00 ), file.exists( f10 ), file.exists( f20 ) )
dir.create( file.path( OUT$cov, "cbsa" ), showWarnings = FALSE )

## ---- long group-level adherents: geoid, year, group, cng, adh ----------------

wide_to_long <- function( x, fips, adh_sfx, cng_sfx, exclude, year, cath ) {
  x <- as.data.table( zap_labels( x ) )
  geoid <- pad( as.integer( x[[ fips ]] ), 5 )
  acol <- grep( paste0( adh_sfx, "$" ), names( x ), value = TRUE )
  acol <- setdiff( acol, exclude )
  grp  <- sub( paste0( adh_sfx, "$" ), "", acol )
  ccol <- paste0( grp, cng_sfx )
  out <- rbindlist( lapply( seq_along( grp ), function( i ) data.table(
    geoid = geoid, year = year, group = grp[ i ],
    cng = if ( ccol[ i ] %in% names( x ) ) as.numeric( x[[ ccol[ i ] ]] ) else NA_real_,
    adh = as.numeric( x[[ acol[ i ] ]] ) ) ) )
  out[ , is_cath := group == cath ]
  out[ !( is.na( adh ) & ( is.na( cng ) | cng == 0 ) ) ]
}

x00 <- read_sav( f00 )
g00 <- wide_to_long( x00, "FIP", "AD", "CG",
                     exclude = c( "TOTAD", "ADJAD", "MAINAD", "EVANAD", "ORTHAD", "OTHERAD" ),
                     year = 2000L, cath = "CATH" )
tot00 <- data.table( geoid = pad( as.integer( x00$FIP ), 5 ), tot_adh = as.numeric( x00$TOTAD ),
                     tot_cng = as.numeric( x00$TOTCG ) )

x10 <- read_sav( f10 )
g10 <- wide_to_long( x10, "FIPS", "ADH", "CNG",
                     exclude = c( "TOTADH", "CATHADH", "EVANADH", "BPRTADH", "MPRTADH", "ORTHADH", "OTHADH" ),
                     year = 2010L, cath = "CTH" )
tot10 <- data.table( geoid = pad( as.integer( x10$FIPS ), 5 ), tot_adh = as.numeric( x10$TOTADH ),
                     tot_cng = as.numeric( x10$TOTCNG ) )

x20 <- as.data.table( read_excel( f20, sheet = "2020 Group by County" ) )
setnames( x20, c( "geoid", "state", "county", "group", "group_name", "cng", "adh", "sh_adh", "sh_pop" ) )
g20 <- x20[ , .( geoid = pad( geoid, 5 ), year = 2020L, group, cng = as.numeric( cng ),
                 adh = as.numeric( adh ), is_cath = group_name == "Catholic Church" ) ]
stopifnot( any( g20$is_cath ) )

## ---- check: groups sum to the published totals ------------------------------

chk <- function( g, tot, yr ) {
  s <- g[ , .( adh = sum( adh, na.rm = TRUE ), cng = sum( cng, na.rm = TRUE ) ), by = geoid ]
  m <- merge( s, tot, by = "geoid" )
  data.table( year = yr, counties = nrow( m ),
              ratio_adh = sum( m$adh ) / sum( m$tot_adh, na.rm = TRUE ),
              ratio_cng = sum( m$cng ) / sum( m$tot_cng, na.rm = TRUE ),
              cor_adh = cor( m$adh, m$tot_adh, use = "complete.obs" ) )
}
s20 <- as.data.table( read_excel( file.path( OUT$raw_cov, "rcms", "2020_USRC_Summaries.xlsx" ),
                                  sheet = "2020 County Summary" ) )
tot20 <- s20[ , .( geoid = pad( FIPS, 5 ), tot_adh = as.numeric( Adherents ),
                   tot_cng = as.numeric( Congregations ) ) ]
check <- rbind( chk( g00, tot00, 2000L ), chk( g10, tot10, 2010L ), chk( g20, tot20, 2020L ) )
print( check )
fwrite( check, file.path( OUT$diag, "rcms_group_sum_check.csv" ) )
if ( any( abs( check$ratio_adh - 1 ) > 0.02 ) )
  warning( "group adherents do not sum to published totals within 2%; see rcms_group_sum_check.csv" )

## ---- harmonize group cells onto 2023 county-equivalents -------------------------
## (harmonize_counties keys on geoid x year, so the group is encoded in `year`)

## published county totals ride along as a pseudo-group (congregation counts
## for some 2010 bodies have no group column, so totals come from TOT*)
tots <- rbind( tot00[ , .( geoid, year = 2000L ) ], tot10[ , .( geoid, year = 2010L ) ],
               tot20[ , .( geoid, year = 2020L ) ] )
tots[ , `:=`( group = "__TOTAL__", is_cath = FALSE,
              cng = c( tot00$tot_cng, tot10$tot_cng, tot20$tot_cng ),
              adh = c( tot00$tot_adh, tot10$tot_adh, tot20$tot_adh ) ) ]
g <- rbind( g00, g10, g20, tots[ grepl( "^[0-9]{5}$", geoid ) ], use.names = TRUE )
g[ , gkey := .GRP, by = .( year, group ) ]
keymap <- unique( g[ , .( gkey, year, group, is_cath ) ] )
h <- harmonize_counties( g[ , .( geoid, year = gkey, cng, adh ) ], counts = c( "cng", "adh" ) )
setnames( h, "year", "gkey" )
h <- merge( h, keymap, by = "gkey" )
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
log_msg( "group cells off the 2023 spine (dropped): ", h[ !geoid %in% sp$geoid, .N ], " in ", paste( unique( h[ !geoid %in% sp$geoid, geoid ] ), collapse = " " ) )
h <- h[ geoid %in% sp$geoid ]

hhi <- function( a ) { a <- a[ is.finite( a ) & a > 0 ]; if ( !length( a ) ) NA_real_ else sum( ( a / sum( a ) )^2 ) }
summ <- function( d, by ) d[ , { T <- group == "__TOTAL__"; G <- !T
  .( rel_cng = sum( cng[ T ], na.rm = TRUE ),
     rel_adh = sum( adh[ T ], na.rm = TRUE ),
     rel_cath_adh = sum( adh[ is_cath ], na.rm = TRUE ),
     rel_hhi = hhi( tapply( adh[ G ], group[ G ], sum, na.rm = TRUE ) ),
     rel_groups = uniqueN( group[ G & adh > 0 & !is.na( adh ) ] ) ) }, by = by ]
cty <- summ( h, c( "geoid", "year" ) )
cty[ , rel_2000_noblack := as.integer( year == 2000 ) ]

h <- merge( h, sp[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
cb <- summ( h, c( "cbsa", "year" ) )[ , .( cbsa, year, rel_hhi ) ]

validate_cov( cty, "RCMS county" )
print( cty[ , .( counties = .N, rel_adh = sum( rel_adh ), rel_cng = sum( rel_cng ),
                 cath_share = sum( rel_cath_adh ) / sum( rel_adh ),
                 med_hhi = median( rel_hhi, na.rm = TRUE ), med_groups = median( rel_groups ) ), keyby = year ] )
fwrite( cty, file.path( OUT$cov, "cov_rcms.csv" ) )
fwrite( cb, file.path( OUT$cov, "cbsa", "cbsa_rcms.csv" ) )

fwrite( data.table(
  var = c( "rel_cng", "rel_adh", "rel_cath_adh", "rel_hhi", "rel_groups", "rel_2000_noblack" ),
  desc = c( "congregations (count)", "adherents (count)", "Catholic adherents (count)",
            "HHI of adherent shares across religious bodies (0-1; higher = more concentrated)",
            "religious bodies with adherents in the county", "1 = 2000 file (historically Black bodies largely missing)" ),
  type = c( "B", "B", "B", "A", "-", "flag" ),
  years = "2000, 2010, 2020 (observed); interpolated/carried forward in 30_assemble",
  source = "RCMS 2000/2010 (ARDA county files); 2020 U.S. Religion Census group detail" ),
  file.path( OUT$cov, "cov_rcms_codebook.csv" ) )
log_msg( "wrote cov_rcms.csv (", nrow( cty ), " rows) and cbsa/cbsa_rcms.csv (", nrow( cb ), " rows)" )
