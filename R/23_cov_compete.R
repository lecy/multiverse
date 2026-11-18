## 23_cov_compete.R
## Market competition / niche quality: concentration of nonprofit resources in
## each metro (CBSA) x year, 2000-2024.
##
## Measures follow the Nonprofit Open Data Collective's npcompete package
## (github.com/Nonprofit-Open-Data-Collective/npcompete: HHI, normalized HHI,
## concentration ratios, Gini), recomputed here in data.table because the
## package is pre-release and several of its functions are wrong as written
## (density denominators summed over org rows, CR returned on mixed 0-1 / 100
## scales, the normalized-HHI correction applied once per org, fundraising
## efficiency does not run). Definitions below are the textbook ones.
##
## Universe = the DV universe: active 501(c)(3) filers (max(rev, exp) >= $50k
## in 2010 dollars), located and spliced exactly as in 06_npo_assemble.R
## (CORE 2000-2010 | SOI 2011-2019 | e-file 2020-2024; EIN-only CORE records
## located through the BMF). Invalid NTEE codes (e-file "UND" for ~16% of filers
## 2020-24) are backfilled from the most recent valid BMF code for the EIN.
## Three universes:
##   c3     all active 501(c)(3) filers
##   nohu   hospitals and universities dropped (the hu stratum of the multiverse;
##          same NTEE backfill as 06 since 2026-11-17)
##   local  the community-serving niche: nohu without health (E-H), international (Q),
##          research (U), grantmaking (T), university support orgs (B11), uncoded orgs
##          and Fidelity Charitable. National HQs remain (Feeding America in Chicago,
##          Red Cross in DC, ALSAC/St. Jude in Memphis) but are small shares of those
##          large metros. In c3 and
##          nohu the largest org in many metros is a national entity booked at its
##          headquarters (Kaiser $71B in Oakland, Fidelity Charitable in Boston, Battelle in
##          Columbus, UPMC uncoded in Pittsburgh), so their whole-metro measures mostly
##          record where headquarters sit. Prefer local for niche quality.
## Resources (2022 dollars, negatives set to 0):
##   rev  total revenue
##   con  contributions, gifts and grants (Part VIII 1h; includes government grants)
## Market = the metro (whole sector), and metro x NTEE major group (subsector).
##
## Per market, with shares s_i = x_i / sum(x), n orgs:
##   hhi   sum(s_i^2)                         0-1; >= 1/n by construction
##   nhhi  (hhi - 1/n) / (1 - 1/n)            0-1; normalized HHI (n >= 2)
##   xs    n * hhi = 1 + CV^2 of shares       excess concentration: 1 = equal shares.
##                                            Removes the 1/n floor but RISES with n for
##                                            heavy-tailed size distributions (r = .68 with
##                                            log n, metros 2022)
##   cr1, cr4  share of the largest 1 / 4 orgs
##   gini  Gini of org resources (n/(n-1) small-sample correction, n >= 2)
##   small share of resources held by orgs with size < $1M (2022 $)
## Metro summaries of the subsector markets:
##   shhi  resource-weighted mean of subsector HHIs (concentration of the niche
##         the average dollar sits in)
##   shhio org-weighted mean of subsector HHIs (niche concentration the typical
##         org faces)
##   scr4o org-weighted mean of subsector CR4
##   mix   HHI of the metro's resources across subsectors (portfolio concentration;
##         high = one field, usually health, dominates)
## Count measure (resource-free): mkt_small_n_<univ> = share of active filers < $1M.
##
## CAUTION: n is the count DV and total resources are the spending DV. hhi, cr1, cr4
## and the subsector means (shhio r = -.83 with log n) fall mechanically as n rises;
## xs and gini rise with n. nhhi is the least tied to n (r = -.27 with log n, -.08
## with filer SAMI; local revenue, metros 2022). Concentration also loads on the
## spending DV (one giant org makes both). See diag_mechanical_correlations.csv.
## Splices: HHI medians fall ~10% at 2010 -> 2011 (SOI covers more small filers than
## CORE) and rise in 2020 (small EZ filers missing, as in the counts).
##
## Outputs
##   D:/MULTIVERSE/data/cov/cbsa/cbsa_compete.csv     cbsa x year, mkt_* (read by 30)
##   D:/MULTIVERSE/data/compete/compete_subsector.csv.gz   cbsa x year x univ x res x group
##   D:/MULTIVERSE/data/compete/codebook_compete.csv
##   D:/MULTIVERSE/data/compete/diag_*.csv             splice continuity, correlations

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
OUT$compete <- file.path( BUILD, "data", "compete" )
dir.create( OUT$compete, showWarnings = FALSE )

rd <- function( f ) as.data.table( read_parquet( file.path( OUT$npo, f ) ) )
SMALL <- 1e6          # "small org" cutoff, 2022 dollars
BIG   <- 2e11         # data-error guard, as in 06

## ---- org-year records, spliced (990 / 990-EZ only; foundations excluded) ------

pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )      # written by 06
cols <- c( "source", "subsec_group", "ein", "year", "rev", "exp", "con", "ntee3", "ind", "geoid" )
core <- rd( "orgyear_core.parquet" )[ year %in% 2000:2010 & subsec_group == "c3" ]
soi  <- rd( "orgyear_soi.parquet" )
ef   <- rd( "orgyear_efile.parquet" )

## location for EIN-only CORE records: same rules as 06_npo_assemble.R (BMF county
## for the same EIN-year, else nearest BMF year, else SOI / e-file current geocode)
loc <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ),
              colClasses = list( character = c( "EIN", "geoid", "ntee3", "subsec", "zip5" ) ) )
loc <- loc[ !is.na( geoid ), .( ein = EIN, year_bmf = year, geoid_bmf = geoid, ntee3_bmf = ntee3 ) ]
setkey( loc, ein, year_bmf )
cur <- unique( rbind( soi[ !is.na( geoid ), .( ein, geoid, ntee3, year ) ],
                      ef[  !is.na( geoid ), .( ein, geoid, ntee3, year ) ] )[ order( ein, -year ) ],
               by = "ein" )[ , .( ein, geoid_cur = geoid, ntee3_cur = ntee3 ) ]
core[ , rowid := .I ]
j <- loc[ core[ , .( ein, year_bmf = year, rowid ) ], on = .( ein, year_bmf ), roll = "nearest" ]
core[ j, on = "rowid", `:=`( geoid_bmf = i.geoid_bmf, ntee3_bmf = i.ntee3_bmf ) ]
core <- merge( core, cur, by = "ein", all.x = TRUE )
core[ , geoid := fifelse( !is.na( geoid_bmf ), geoid_bmf, geoid_cur ) ]
core[ , ntee3 := fcoalesce( fifelse( ntee3_bmf == "", NA_character_, ntee3_bmf ),
                            fifelse( ntee3 == "", NA_character_, toupper( substr( ntee3, 1, 3 ) ) ),
                            ntee3_cur ) ]
core[ , ind := NA_character_ ]
## most recent valid BMF NTEE code per EIN: e-file records carry "UND" (undefined)
## for ~16% of filers 2020-24 (10% of revenue, incl. UPMC); ~90% are recoverable
bad_ntee <- function( x ) is.na( x ) | x == "" | toupper( substr( x, 1, 3 ) ) %in% c( "UND", "INV", "Z99" )
ntee_bmf <- unique( loc[ !bad_ntee( ntee3_bmf ) ][ order( ein, -year_bmf ) ], by = "ein" )[ , .( ein, ntee_bmf = toupper( ntee3_bmf ) ) ]
rm( loc, cur, j ); gc( verbose = FALSE )

org <- rbind( core[ , ..cols ],
              soi[ year %in% 2011:2019 & subsec_group == "c3", ..cols ],
              ef[  year %in% 2020:2024 & subsec_group == "c3", ..cols ] )
rm( core, soi, ef ); gc( verbose = FALSE )
org[ , ntee3 := toupper( ntee3 ) ]
org <- merge( org, ntee_bmf, by = "ein", all.x = TRUE )
log_msg( "invalid NTEE before / after BMF backfill: ", round( mean( bad_ntee( org$ntee3 ) ), 4 ), " / ",
         round( mean( bad_ntee( org$ntee3 ) & is.na( org$ntee_bmf ) ), 4 ) )
org[ bad_ntee( ntee3 ) & !is.na( ntee_bmf ), ntee3 := ntee_bmf ]
org[ bad_ntee( ntee3 ), ntee3 := NA_character_ ]
org[ , ntee_bmf := NULL ]
for ( v in c( "rev", "exp", "con" ) ) set( org, i = which( abs( org[[ v ]] ) > BIG ), j = v, value = NA_real_ )
org <- merge( org, pce[ , .( year, to2022, floor_nom ) ], by = "year" )
org[ , size := pmax( rev, exp, na.rm = TRUE ) ]
org <- org[ !is.na( size ) & size >= floor_nom & !is.na( geoid ) ]          # active, located
org[ , hosp_univ := ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) | ind %in% c( "HOS", "UNI" ) ]
pos <- function( x ) pmax( fcoalesce( x, 0 ), 0 )
org[ , `:=`( rev = pos( rev ) * to2022, con = pos( con ) * to2022, size22 = size * to2022 ) ]

## NTEE major groups (NCCS 10 + hospitals and universities split out, as NTMAJ12)
l1 <- substr( org$ntee3, 1, 1 )
org[ , grp := fcase( ntee3 %in% NTEE_HOSP | ind %in% "HOS", "HOS",
                     ntee3 %in% NTEE_UNIV | ind %in% "UNI", "UNI",
                     l1 == "A", "ART", l1 == "B", "EDU", l1 %in% c( "C", "D" ), "ENV",
                     l1 %in% LETTERS[ 5:8 ], "HEA", l1 %in% LETTERS[ 9:16 ], "HMS",
                     l1 == "Q", "INT", l1 %in% LETTERS[ 18:23 ], "PUB", l1 == "X", "REL",
                     l1 == "Y", "MUT", default = "UNK" ) ]
rm( l1 )

spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
org <- merge( org, spine[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
log_msg( "active located c3 filers in CBSAs: ", format( nrow( org ), big.mark = "," ) )
print( org[ , .( n = .N, hosp_univ = sum( hosp_univ ), unk_ntee = round( mean( grp == "UNK" ), 3 ),
                 con_zero = round( mean( con == 0 ), 3 ) ), keyby = year ] )

## universes. local = the community-serving niche: nohu without health (NTEE E-H:
## integrated health systems such as Kaiser (E31) and Cleveland Clinic (E03) escape the
## hospital codes and book national or regional revenue at headquarters), without
## philanthropy / grantmaking intermediaries (NTEE T, incl. national donor-advised
## funds, whose revenue is regranted to other orgs), without international (Q) and
## science / research institutes (U; e.g. Battelle, a $12B federal lab contractor booked
## in Columbus), which serve national or foreign markets, and without uncoded orgs.
## Fidelity Investments Charitable Gift Fund (national DAF, $15B, coded S50) is dropped by EIN.
NATIONAL_EIN <- c( "110303001" )
## B11 (single-organization support for education: mostly university endowment
## associations, e.g. KU Endowment in Lawrence) goes with the universities.
org[ , local := !hosp_univ & !grp %in% c( "HEA", "UNK" ) & !substr( ntee3, 1, 1 ) %in% c( "Q", "T", "U" ) &
                !ntee3 %in% "B11" &
                !ind %in% c( "HEL", "HOS" ) & !ein %in% NATIONAL_EIN ]
UNIV <- list( c3 = rep( TRUE, nrow( org ) ), nohu = !org$hosp_univ, local = org$local )
long <- rbindlist( lapply( names( UNIV ), function( u ) {
  k <- UNIV[[ u ]]
  rbind( org[ k, .( cbsa, year, grp, univ = u, res = "rev", x = rev, size22 ) ],
         org[ k, .( cbsa, year, grp, univ = u, res = "con", x = con, size22 ) ] ) } ) )
small_n <- rbindlist( lapply( names( UNIV ), function( u )
  org[ UNIV[[ u ]], .( mkt_small_n = mean( size22 < SMALL ) ), by = .( cbsa, year ) ][ , univ := u ] ) )
## largest org per metro x universe x year (for the diagnostics)
topn <- rbindlist( lapply( names( UNIV ), function( u )
  org[ UNIV[[ u ]] ][ order( -rev ), .( ein = ein[1], ntee3 = ntee3[1], rev_top = rev[1] ), by = .( cbsa, year ) ][ , univ := u ] ) )
rm( UNIV )
rm( org ); gc( verbose = FALSE )

## ---- market statistics ----------------------------------------------------------

## x sorted ascending within each market; i = rank
mkt_stats <- function( d, by ) {
  setorderv( d, c( by, "x" ) )
  d[ , i := seq_len( .N ), by = by ]
  d[ , {
    n <- .N; tot <- sum( x )
    hhi <- if ( tot > 0 ) sum( ( x / tot )^2 ) else NA_real_
    .( n = n, total = tot, hhi = hhi,
       nhhi = if ( n >= 2 && tot > 0 ) ( hhi - 1 / n ) / ( 1 - 1 / n ) else NA_real_,
       xs = n * hhi,
       cr1 = if ( tot > 0 ) x[ n ] / tot else NA_real_,
       cr4 = if ( tot > 0 ) sum( x[ max( 1, n - 3 ):n ] ) / tot else NA_real_,
       gini = if ( n >= 2 && tot > 0 ) ( 2 * sum( i * x ) / ( n * tot ) - ( n + 1 ) / n ) * n / ( n - 1 ) else NA_real_,
       small = if ( tot > 0 ) sum( x[ size22 < SMALL ] ) / tot else NA_real_ ) }, by = by ]
}

whole <- mkt_stats( long, c( "univ", "res", "cbsa", "year" ) )
sub   <- mkt_stats( long, c( "univ", "res", "cbsa", "year", "grp" ) )
rm( long ); gc( verbose = FALSE )
fwrite( sub, file.path( OUT$compete, "compete_subsector.csv.gz" ) )

## metro summaries of the subsector markets (UNK = no NTEE code: left out)
subm <- sub[ grp != "UNK" & total > 0, .(
  shhi  = sum( hhi * total ) / sum( total ),
  shhio = sum( hhi * n ) / sum( n ),
  scr4o = sum( cr4 * n ) / sum( n ),
  mix   = sum( ( total / sum( total ) )^2 ) ), by = .( univ, res, cbsa, year ) ]

m <- merge( whole[ , !c( "n", "total" ) ], subm, by = c( "univ", "res", "cbsa", "year" ), all.x = TRUE )
mv <- c( "hhi", "nhhi", "xs", "cr1", "cr4", "gini", "small", "shhi", "shhio", "scr4o", "mix" )
wide <- dcast( m, cbsa + year ~ res + univ, value.var = mv, sep = "_" )
setnames( wide, setdiff( names( wide ), c( "cbsa", "year" ) ),
          paste0( "mkt_", setdiff( names( wide ), c( "cbsa", "year" ) ) ) )
sn <- dcast( small_n, cbsa + year ~ univ, value.var = "mkt_small_n" )
setnames( sn, c( "c3", "nohu", "local" ), paste0( "mkt_small_n_", c( "c3", "nohu", "local" ) ) )
wide <- merge( wide, sn, by = c( "cbsa", "year" ), all = TRUE )
setorder( wide, cbsa, year )
fwrite( wide, file.path( OUT$cov, "cbsa", "cbsa_compete.csv" ) )
log_msg( "cbsa_compete.csv: ", nrow( wide ), " cbsa-years x ", ncol( wide ) - 2, " measures" )

## ---- codebook ---------------------------------------------------------------------

lab <- c( hhi = "Herfindahl-Hirschman index of org shares (0-1), whole metro",
          nhhi = "normalized HHI (hhi - 1/n)/(1 - 1/n), whole metro",
          xs = "excess concentration n * HHI = 1 + CV^2 of shares, whole metro",
          cr1 = "share of the largest org, whole metro",
          cr4 = "share of the 4 largest orgs, whole metro",
          gini = "Gini of org resources (small-sample corrected), whole metro",
          small = "share of resources held by orgs with size < $1M (2022 $)",
          shhi = "resource-weighted mean HHI of metro x NTEE major-group markets",
          shhio = "org-weighted mean HHI of metro x NTEE major-group markets",
          scr4o = "org-weighted mean CR4 of metro x NTEE major-group markets",
          mix = "HHI of metro resources across NTEE major groups" )
ulab <- c( c3 = "all 501(c)(3)", nohu = "excl. hospitals & universities",
           local = "excl. hospitals, universities and their support orgs (B11), health (E-H), international (Q), research (U), grantmakers (T), uncoded" )
cb <- CJ( metric = mv, res = c( "rev", "con" ), univ = names( ulab ), sorted = FALSE )
cb[ , `:=`( var = paste( "mkt", metric, res, univ, sep = "_" ),
            label = paste0( lab[ metric ], "; ", fifelse( res == "rev", "total revenue", "contributions" ),
                            "; ", ulab[ univ ] ) ) ]
cb <- rbind( cb[ , .( var, label ) ],
             data.table( var = paste0( "mkt_small_n_", names( ulab ) ),
                         label = paste0( "share of active filers with size < $1M (2022 $); ", ulab ) ) )
cb[ , source := "23_cov_compete.R: active 501(c)(3) filers, spliced CORE/SOI/e-file, 2022 dollars" ]
fwrite( cb, file.path( OUT$compete, "codebook_compete.csv" ) )

## ---- diagnostics ------------------------------------------------------------------

npo <- fread( file.path( OUT$npo, "npo_cbsa_year.csv" ), colClasses = list( character = "cbsa" ) )
d <- merge( wide, npo[ , .( cbsa, year, cbsa_type, population, n_filer_c3, n_filer_c3_nohu, exp_c3, exp_c3_nohu ) ],
            by = c( "cbsa", "year" ) )
key <- c( "mkt_hhi_rev_nohu", "mkt_nhhi_rev_nohu", "mkt_xs_rev_nohu", "mkt_cr4_rev_nohu", "mkt_gini_rev_nohu",
          "mkt_small_rev_nohu", "mkt_shhi_rev_nohu", "mkt_shhio_rev_nohu", "mkt_mix_rev_nohu",
          "mkt_hhi_con_nohu", "mkt_xs_con_nohu", "mkt_gini_con_nohu", "mkt_shhio_con_nohu",
          "mkt_hhi_rev_c3", "mkt_xs_rev_c3", "mkt_gini_rev_c3", "mkt_small_n_nohu",
          "mkt_hhi_rev_local", "mkt_nhhi_rev_local", "mkt_xs_rev_local", "mkt_cr4_rev_local", "mkt_gini_rev_local",
          "mkt_shhio_rev_local", "mkt_mix_rev_local", "mkt_hhi_con_local", "mkt_nhhi_con_local",
          "mkt_gini_con_local", "mkt_small_n_local" )
fwrite( topn, file.path( OUT$compete, "largest_org_by_metro_year.csv.gz" ) )

## splice continuity: metro medians by year (look at 2010 -> 2011 and 2019 -> 2020)
med <- d[ cbsa_type == "metro", lapply( .SD, function( z ) round( median( z, na.rm = TRUE ), 4 ) ),
          keyby = year, .SDcols = key ]
fwrite( med, file.path( OUT$compete, "diag_metro_medians_by_year.csv" ) )
print( med[ , c( "year", "mkt_hhi_rev_nohu", "mkt_gini_rev_nohu", "mkt_hhi_rev_local", "mkt_nhhi_rev_local",
                 "mkt_gini_rev_local", "mkt_hhi_con_local", "mkt_shhio_rev_local", "mkt_small_n_local" ), with = FALSE ] )

## mechanical links: correlation with log population, log filer count and log
## spending (the DVs) and with filer SAMI (log n on log P residual), metros, 2012 and 2022
mech <- rbindlist( lapply( c( 2012, 2022 ), function( y ) {
  w <- d[ cbsa_type == "metro" & year == y ]
  w[ , sami_n := resid( lm( log( n_filer_c3_nohu ) ~ log( population ) ) ) ]
  w[ , sami_exp := resid( lm( log( exp_c3_nohu ) ~ log( population ) ) ) ]
  rbindlist( lapply( key, function( v ) {
    x <- w[[ v ]]; tr <- if ( grepl( "_(hhi|xs|shhi|shhio|cr1)_", v ) ) log( x ) else x
    ok <- is.finite( tr )
    data.table( year = y, var = v, transform = fifelse( identical( tr, x ), "level", "log" ),
                r_logP = cor( tr[ ok ], log( w$population[ ok ] ) ),
                r_logN = cor( tr[ ok ], log( w$n_filer_c3_nohu[ ok ] ) ),
                r_logExp = cor( tr[ ok ], log( w$exp_c3_nohu[ ok ] ) ),
                r_samiN = cor( tr[ ok ], w$sami_n[ ok ] ),
                r_samiExp = cor( tr[ ok ], w$sami_exp[ ok ] ) ) } ) ) } ) )
mech[ , ( 4:8 ) := lapply( .SD, round, 3 ), .SDcols = 4:8 ]
fwrite( mech, file.path( OUT$compete, "diag_mechanical_correlations.csv" ) )
print( mech[ year == 2022 ] )

## within-metro share of variance, metros 2010-2022
wv <- d[ cbsa_type == "metro" & year %in% 2010:2022 ]
within <- rbindlist( lapply( key, function( v ) {
  x <- wv[[ v ]]; if ( grepl( "_(hhi|xs|shhi|shhio|cr1)_", v ) ) x <- log( x )
  ok <- is.finite( x ); g <- wv$cbsa[ ok ]; x <- x[ ok ]
  data.table( var = v, within_share = round( 1 - var( ave( x, g ) ) / var( x ), 3 ) ) } ) )
fwrite( within, file.path( OUT$compete, "diag_within_share.csv" ) )
print( within )

## correlations among the measures, metros 2022
cm <- round( cor( d[ cbsa_type == "metro" & year == 2022, ..key ], use = "pairwise.complete.obs" ), 2 )
fwrite( as.data.table( cm, keep.rownames = "var" ), file.path( OUT$compete, "diag_correlations_2022.csv" ) )

## most and least concentrated metros, 2022 (local universe, revenue, normalized HHI)
meta <- unique( spine[ !is.na( cbsa ) & cbsa != "", .( cbsa, cbsa_title ) ] )
ext <- merge( d[ cbsa_type == "metro" & year == 2022,
                 .( cbsa, population, n_filer_c3_nohu, mkt_hhi_rev_nohu, mkt_cr1_rev_nohu, mkt_hhi_rev_local,
                    mkt_nhhi_rev_local, mkt_cr1_rev_local, mkt_gini_rev_local, mkt_mix_rev_local ) ], meta, by = "cbsa" )
ext <- merge( ext, topn[ univ == "local" & year == 2022, .( cbsa, top_ein = ein, top_ntee = ntee3,
                                                            top_rev_m = round( rev_top / 1e6 ) ) ], by = "cbsa", all.x = TRUE )
setorder( ext, -mkt_nhhi_rev_local )
fwrite( ext, file.path( OUT$compete, "diag_metros_2022.csv" ) )
print( rbind( head( ext, 12 ), tail( ext, 4 ) )[ , .( cbsa_title = substr( cbsa_title, 1, 26 ), pop_k = round( population / 1e3 ),
                                                      cr1_nohu = round( mkt_cr1_rev_nohu, 2 ), nhhi_loc = round( mkt_nhhi_rev_local, 3 ),
                                                      cr1_loc = round( mkt_cr1_rev_local, 2 ), gini_loc = round( mkt_gini_rev_local, 3 ),
                                                      top_ein, top_ntee, top_rev_m ) ] )
## largest local-universe orgs nationally, 2022 (national entities booked at headquarters?)
print( topn[ univ == "local" & year == 2022 ][ order( -rev_top ) ][ 1:20 ][
  meta, on = "cbsa", nomatch = 0 ][ order( -rev_top ), .( ein, ntee3, rev_bn = round( rev_top / 1e9, 2 ), cbsa_title = substr( cbsa_title, 1, 26 ) ) ] )
log_msg( "done" )
