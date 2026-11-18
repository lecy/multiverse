## 16b_verify_elections.R
## Verify the 2020 and 2024 presidential inputs used in 16_cov_elections.R
## (tonmcg county compilation from news feeds; Alaska hand-entered certified totals).
##
## Independent sources (official, no guestbook):
##   2020 state totals : FEC, Federal Elections 2020, Table 2 (federalelections2020.xlsx)
##   2024 state totals : MIT Election Lab, 2024-elections-official/2024-president-state.csv
##   2024 county totals: MIT Election Lab precinct returns (2024-elections-official/
##                       individual_states/*.zip), US PRESIDENT rows summed to county.
##                       Per precinct x candidate, a TOTAL row is used when present,
##                       otherwise the vote-mode rows are summed. Under/overvotes dropped.
## The MEDSL county file 2000-2024 (Dataverse doi:10.7910/DVN/VOQCHQ) sits behind a
## guestbook, so 2020 is checked at the state level plus an internal consistency
## check (2020 county totals against the 2016-2024 path).
##
## Writes D:/MULTIVERSE/data/diagnostics/elections_check/
##   Rscript R/16b_verify_elections.R

source( "R/00_setup.R" )
suppressPackageStartupMessages( { library( readxl ) } )
dir <- file.path( OUT$raw_cov, "elections" )
VD <- file.path( OUT$diag, "elections_check" ); dir.create( VD, showWarnings = FALSE )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 4 ) else z ) ]
options( width = 200 )

## ---- tonmcg (as read in 16_cov_elections.R) ---------------------------------
rd_t <- function( f, y ) {
  t <- fread( f, colClasses = c( county_fips = "character" ) )
  t[ , .( geoid = sprintf( "%05d", as.integer( county_fips ) ), year = y, total = as.numeric( total_votes ),
          dem = as.numeric( votes_dem ), rep = as.numeric( votes_gop ) ) ] }
tm <- rbind( rd_t( file.path( dir, "tonmcg_2020.csv" ), 2020L ), rd_t( file.path( dir, "tonmcg_2024.csv" ), 2024L ) )
tm[ substr( geoid, 1, 2 ) == "11", geoid := "11001" ]
tm <- tm[ , lapply( .SD, sum ), by = .( geoid, year ), .SDcols = c( "total", "dem", "rep" ) ][ , sf := substr( geoid, 1, 2 ) ]

## ---- official state totals ------------------------------------------------------
st24 <- fread( file.path( dir, "medsl_2024", "2024-president-state.csv" ), colClasses = c( state_fips = "character" ) )
abb <- unique( st24[ , .( state_po, sf = sprintf( "%02d", as.integer( state_fips ) ) ) ] )
o24 <- st24[ , .( total = totalvotes[ 1 ], dem = sum( votes[ party_simplified == "DEMOCRAT" & grepl( "HARRIS", candidate ) ] ),
                  rep = sum( votes[ party_simplified == "REPUBLICAN" & grepl( "TRUMP", candidate ) ] ) ), by = .( sf = sprintf( "%02d", as.integer( state_fips ) ) ) ][ , year := 2024L ]
f20 <- as.data.table( read_excel( file.path( dir, "fec", "federalelections2020.xlsx" ), sheet = "3. Table 2 Electoral & Pop Vote", col_names = FALSE, skip = 4 ) )
setnames( f20, 1:7, c( "state_po", "ev_d", "ev_r", "dem", "rep", "oth", "total" ) )
o20 <- merge( f20[ , state_po := sub( "*", "", state_po, fixed = TRUE ) ][ state_po %in% abb$state_po, .( state_po, dem = as.numeric( dem ), rep = as.numeric( rep ), total = as.numeric( total ) ) ], abb, by = "state_po" )[ , .( sf, total, dem, rep, year = 2020L ) ]
off <- rbind( o20, o24 )
cat( "official national totals (m):\n" ); print( off[ , .( total = sum( total ) / 1e6, dem = sum( dem ) / 1e6, rep = sum( rep ) / 1e6 ), keyby = year ] )

## state comparison: tonmcg sums vs official
ts <- tm[ , .( total = sum( total ), dem = sum( dem ), rep = sum( rep ) ), by = .( sf, year ) ]
sc <- merge( ts, off, by = c( "sf", "year" ), suffixes = c( "_tm", "_off" ), all = TRUE )
sc <- merge( sc, abb, by = "sf", all.x = TRUE )
sc[ , `:=`( pct_total = 100 * ( total_tm / total_off - 1 ), pct_dem = 100 * ( dem_tm / dem_off - 1 ), pct_rep = 100 * ( rep_tm / rep_off - 1 ),
            margin_diff_pts = 100 * ( ( dem_tm - rep_tm ) / total_tm - ( dem_off - rep_off ) / total_off ) ) ]
setorder( sc, year, -pct_total )
fwrite( sc, file.path( VD, "state_vs_official.csv" ) )
cat( "\nstates where the compilation is off by more than 0.5% in total votes or 0.2 points in margin:\n" )
print( r3( sc[ abs( pct_total ) > 0.5 | abs( margin_diff_pts ) > 0.2, .( year, state_po, total_tm, total_off, pct_total, pct_dem, pct_rep, margin_diff_pts ) ] ) )
nat <- sc[ , .( total_tm = sum( total_tm, na.rm = TRUE ) / 1e6, total_off = sum( total_off ) / 1e6 ), keyby = year ][ , pct := 100 * ( total_tm / total_off - 1 ) ]
print( r3( nat ) )

## Alaska: hand-entered values in 16_cov_elections.R vs official
ak_hand <- data.table( year = c( 2020L, 2024L ), total = c( 359530, 338177 ), dem = c( 153778, 140026 ), rep = c( 189951, 184458 ) )
cat( "\nAlaska, hand-entered vs official:\n" ); print( merge( ak_hand, off[ sf == "02" ], by = "year", suffixes = c( "_hand", "_off" ) ) )

## ---- 2024 counties from MEDSL precincts ---------------------------------------
p <- rbindlist( lapply( list.files( file.path( dir, "medsl_2024", "pres" ), full.names = TRUE ), fread, colClasses = "character" ), fill = TRUE )
p <- p[ office == "US PRESIDENT" & !candidate %in% c( "UNDERVOTES", "OVERVOTES", "BLANK", "OVER VOTES", "UNDER VOTES", "TOTAL VOTES", "REGISTERED VOTERS", "BALLOTS CAST" ) ]
cat( "\nexcluded-looking candidates still present:\n" ); print( p[ grepl( "VOTE|BLANK|BALLOT|REGIST|SPOIL|VOID", candidate ), .N, by = candidate ] )
p[ , v := suppressWarnings( as.numeric( votes ) ) ]
p[ , redacted := votes == "*" ]
p[ is.na( v ), v := 0 ]
p[ , has_total := any( mode == "TOTAL" ), by = .( state_po, county_fips, jurisdiction_fips, precinct, candidate ) ]
p <- p[ ( has_total & mode == "TOTAL" ) | !has_total ]
p[ , `:=`( geoid = sprintf( "%05d", as.integer( county_fips ) ), dem = grepl( "HARRIS", candidate ), rep = grepl( "TRUMP", candidate ) ) ]
p[ substr( geoid, 1, 2 ) == "11", geoid := "11001" ]
mc <- p[ !is.na( county_fips ) & county_fips != "", .( total = sum( v ), dem = sum( v[ dem ] ), rep = sum( v[ rep ] ), redacted_rows = sum( redacted ) ), by = .( geoid, state_po ) ]
## check the aggregation against the official state totals first
ms <- merge( mc[ , .( total = sum( total ), dem = sum( dem ), rep = sum( rep ) ), by = .( sf = substr( geoid, 1, 2 ) ) ], o24, by = "sf", suffixes = c( "_prec", "_off" ) )
ms[ , `:=`( pct_total = 100 * ( total_prec / total_off - 1 ), pct_dem = 100 * ( dem_prec / dem_off - 1 ), pct_rep = 100 * ( rep_prec / rep_off - 1 ) ) ]
ms <- merge( ms, abb, by = "sf" ); setorder( ms, -pct_total )
fwrite( ms, file.path( VD, "precinct_agg_vs_official_2024.csv" ) )
cat( "\nprecinct aggregation vs official state totals, 2024 (states off by > 0.5%):\n" )
print( r3( ms[ abs( pct_total ) > 0.5 | abs( pct_dem ) > 0.5 | abs( pct_rep ) > 0.5, .( state_po, total_prec, total_off, pct_total, pct_dem, pct_rep ) ] ) )
good <- ms[ abs( pct_total ) <= 0.5 & abs( pct_dem ) <= 0.5 & abs( pct_rep ) <= 0.5, sf ]
cat( "states whose precinct sums match official totals within 0.5%:", length( good ), "\n" )

## county comparison (states whose precinct sums are reliable)
cc <- merge( tm[ year == 2024L ], mc, by = "geoid", suffixes = c( "_tm", "_prec" ), all = TRUE )
cc[ , sf := substr( geoid, 1, 2 ) ]
cc <- cc[ sf %in% good & sf != "02" ]
cc[ , `:=`( pct_total = 100 * ( total_tm / total_prec - 1 ),
            margin_diff_pts = 100 * ( ( dem_tm - rep_tm ) / total_tm - ( dem_prec - rep_prec ) / total_prec ) ) ]
fwrite( cc, file.path( VD, "county_2024_vs_precincts.csv" ) )
cat( "\n2024 counties: unmatched in tonmcg", cc[ is.na( total_tm ), .N ], "; unmatched in precincts", cc[ is.na( total_prec ), .N ], "\n" )
print( cc[ is.na( total_tm ) | is.na( total_prec ), .( geoid, state_po, total_tm, total_prec ) ][ 1:20 ] )
cq <- cc[ !is.na( total_tm ) & !is.na( total_prec ) ]
dist <- cq[ , .( counties = .N, within_0.5pct = mean( abs( pct_total ) <= 0.5 ), within_2pct = mean( abs( pct_total ) <= 2 ),
                 over_5pct = sum( abs( pct_total ) > 5 ), margin_within_0.5pt = mean( abs( margin_diff_pts ) <= 0.5 ),
                 margin_over_2pt = sum( abs( margin_diff_pts ) > 2 ), median_abs_pct = median( abs( pct_total ) ) ) ]
print( r3( dist ) ); fwrite( dist, file.path( VD, "county_2024_summary.csv" ) )
cat( "\nlargest county discrepancies, 2024:\n" )
print( r3( cq[ order( -abs( pct_total ) ) ][ 1:25, .( geoid, state_po, total_tm, total_prec, pct_total, margin_diff_pts, redacted_rows ) ] ) )
print( r3( cq[ order( -abs( margin_diff_pts ) ) ][ 1:10, .( geoid, state_po, total_tm, total_prec, pct_total, margin_diff_pts ) ] ) )

## ---- metros: what the discrepancies do to turnout and dem_margin -------------------
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )[ cbsa_type == "metro", .( geoid, cbsa, cbsa_title ) ]
## CT: 2024 counties are planning regions in both sources; spine uses 2023 counties
mm <- merge( cq, sp, by = "geoid" )[ , .( total_tm = sum( total_tm ), total_prec = sum( total_prec ), dem_tm = sum( dem_tm ), rep_tm = sum( rep_tm ),
                                          dem_prec = sum( dem_prec ), rep_prec = sum( rep_prec ), counties = .N ), by = .( cbsa, cbsa_title ) ]
mm[ , `:=`( pct_total = 100 * ( total_tm / total_prec - 1 ),
            margin_tm = ( dem_tm - rep_tm ) / total_tm, margin_prec = ( dem_prec - rep_prec ) / total_prec ) ][ , margin_diff_pts := 100 * ( margin_tm - margin_prec ) ]
fwrite( mm, file.path( VD, "metro_2024_vs_precincts.csv" ) )
msum <- mm[ , .( metros = .N, r_log_votes = cor( log( total_tm ), log( total_prec ) ), r_margin = cor( margin_tm, margin_prec ),
                 within_0.5pct = mean( abs( pct_total ) <= 0.5 ), over_2pct = sum( abs( pct_total ) > 2 ), max_abs_pct = max( abs( pct_total ) ),
                 margin_over_0.5pt = sum( abs( margin_diff_pts ) > 0.5 ), max_abs_margin_pts = max( abs( margin_diff_pts ) ) ) ]
## SAMI-form turnout: residual of log votes on log population, same population for both
pop <- fread( file.path( OUT$pop, "cbsa_pop_2000_2025.csv" ), colClasses = list( character = "cbsa" ) )[ year == 2024L, .( cbsa, population ) ]
mm <- merge( mm, pop, by = "cbsa" )
msum[ , r_sami_votes := mm[ , cor( resid( lm( log( total_tm ) ~ log( population ) ) ), resid( lm( log( total_prec ) ~ log( population ) ) ) ) ] ]
print( r3( msum ) ); fwrite( msum, file.path( VD, "metro_2024_summary.csv" ) )
cat( "\nmetros off by more than 1% in votes or 0.5 points in margin, 2024:\n" )
print( r3( mm[ abs( pct_total ) > 1 | abs( margin_diff_pts ) > 0.5 ][ order( -abs( pct_total ) ), .( cbsa_title, counties, total_tm, total_prec, pct_total, margin_diff_pts ) ] ) )

## ---- 2020 internal check: county totals against the 2016 -> 2024 path -------------
el <- fread( file.path( OUT$cov, "cov_elections.csv" ), colClasses = list( character = "geoid" ) )[ year %in% c( 2016, 2020, 2024 ) & !ak_statewide ]
w <- dcast( el, geoid ~ year, value.var = "votes_total" ); setnames( w, c( "geoid", "v16", "v20", "v24" ) )
w <- w[ v16 > 0 & v20 > 0 & v24 > 0 ][ , sf := substr( geoid, 1, 2 ) ]
## 2020 relative to the midpoint of 2016 and 2024 (log), net of the state's own value
w[ , dev := log( v20 ) - ( log( v16 ) + log( v24 ) ) / 2 ][ , dev_state := dev - median( dev ), by = sf ]
w <- merge( w, sp, by = "geoid", all.x = TRUE )
chk20 <- w[ , .( counties = .N, sd_dev = sd( dev_state ), over_10pct = sum( abs( dev_state ) > log( 1.10 ) ), over_20pct = sum( abs( dev_state ) > log( 1.2 ) ) ) ]
print( r3( chk20 ) ); fwrite( w[ order( -abs( dev_state ) ) ], file.path( VD, "county_2020_path_check.csv" ) )
cat( "\n2020 counties furthest off the 2016-2024 path (metro counties):\n" )
print( r3( w[ !is.na( cbsa ) ][ order( -abs( dev_state ) ) ][ 1:20, .( geoid, cbsa_title, v16, v20, v24, dev_state ) ] ) )
## same check for 2024 counties as a benchmark (2024 against 2016-2020 path is not symmetric; use 2012-2016-2020 for 2016 instead)
el2 <- fread( file.path( OUT$cov, "cov_elections.csv" ), colClasses = list( character = "geoid" ) )[ year %in% c( 2012, 2016, 2020 ) & !ak_statewide ]
w2 <- dcast( el2, geoid ~ year, value.var = "votes_total" ); setnames( w2, c( "geoid", "v12", "v16", "v20" ) )
w2 <- w2[ v12 > 0 & v16 > 0 & v20 > 0 ][ , sf := substr( geoid, 1, 2 ) ]
w2[ , dev := log( v16 ) - ( log( v12 ) + log( v20 ) ) / 2 ][ , dev_state := dev - median( dev ), by = sf ]
chk16 <- w2[ , .( counties = .N, sd_dev = sd( dev_state ), over_10pct = sum( abs( dev_state ) > log( 1.10 ) ), over_20pct = sum( abs( dev_state ) > log( 1.2 ) ) ) ]
cat( "\nbenchmark: 2016 (official MEDSL) against the 2012-2020 path:\n" ); print( r3( chk16 ) )
fwrite( rbind( chk20[ , year := 2020L ], chk16[ , year := 2016L ] ), file.path( VD, "path_check_summary.csv" ) )
log_msg( "done" )
