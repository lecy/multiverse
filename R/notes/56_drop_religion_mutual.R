## 56_drop_religion_mutual.R
## Does dropping religion-related (NTEE X) and mutual/membership-benefit (NTEE Y)
## 501(c)(3) filers change anything? 2022 e-file, same $50k real floor and
## locations as the panel (06_npo_assemble.R). Congregations do not file, so
## "religion-related" here means ministries, religious media, missions, etc.
##   Part 1  shares, scaling exponents, SAMI agreement, metros that move most
##   Part 2  baseline cross-section with and without them
##   Part 3  matched sample of multiverse specifications (2022 cross-sections),
##           original outcome vs the same outcome without X and Y
## Single process. Writes D:/MULTIVERSE/data/followup/p10_*.

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( arrow ); library( ggplot2 ) } )
FU <- file.path( BUILD, "data", "followup" )
N_COUNT <- 1200L; N_EXP <- 800L
## with MV_MODEL=lean (13-covariate default, run v11) outputs are p10_v11_* and part 2 uses the 13 covariates
P10 <- if ( MV_MODEL == "lean" ) "p10_v11_" else "p10_"

pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )
ef <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ),
        col_select = c( "ein", "year", "subsec_group", "rev", "exp", "ntee3", "ind", "geoid" ) ) )[
        year == 2022 & subsec_group == "c3" & !is.na( geoid ) ]
ef <- merge( ef, pce[ , .( year, floor_nom ) ], by = "year" )[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "ntee3" ),
              colClasses = list( character = c( "EIN", "ntee3" ) ) )[ !is.na( ntee3 ) & ntee3 != "" ]
bmf <- unique( bmf[ order( EIN, -year ) ], by = "EIN" )[ , .( ein = EIN, ntee_bmf = toupper( substr( ntee3, 1, 3 ) ) ) ]
ef <- merge( ef, bmf, by = "ein", all.x = TRUE )
ef[ , ntee := fcoalesce( fifelse( is.na( ntee3 ) | ntee3 %in% c( "", "UND" ), NA_character_, toupper( substr( ntee3, 1, 3 ) ) ), ntee_bmf ) ]
ef[ , rel := ind %in% "REL" | ( ( is.na( ind ) | ind == "UNU" ) & substr( ntee, 1, 1 ) %in% "X" ) ]
ef[ , mmb := ind %in% "MMB" | ( ( is.na( ind ) | ind == "UNU" ) & substr( ntee, 1, 1 ) %in% "Y" ) ]
ef[ , hu := ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) | ind %in% c( "HOS", "UNI" ) ]
ef <- merge( ef, sp[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
pos <- function( x ) pmax( x, 0, na.rm = TRUE )
agg <- ef[ , .( n_all = .N, n_rel = sum( rel ), n_mmb = sum( mmb ),
                n_filer_c3_xrm = sum( !rel & !mmb ),
                exp_nohu = sum( pos( exp )[ !hu ] ), exp_c3_nohu_xrm = sum( pos( exp )[ !hu & !rel & !mmb ] ),
                exp_rel = sum( pos( exp )[ rel ] ), exp_mmb = sum( pos( exp )[ mmb ] ) ), by = cbsa ]

d <- load_panel()
p22 <- merge( d[ year == 2022 ], agg, by = "cbsa", all.x = TRUE )
cat( "check vs panel (2022): filers", sum( p22$n_filer_c3 ), "vs", sum( p22$n_all, na.rm = TRUE ),
     "; exp excl h/u (bn)", round( sum( p22$exp_c3_nohu ) / 1e9 ), "vs", round( sum( p22$exp_nohu, na.rm = TRUE ) / 1e9 ), "\n" )

## ---- Part 1 -------------------------------------------------------------------------
pm <- p22[ cbsa_type == "metro" ]
sh <- data.table(
  share_filers_rel = sum( pm$n_rel ) / sum( pm$n_all ), share_filers_mmb = sum( pm$n_mmb ) / sum( pm$n_all ),
  share_exp_rel = sum( pm$exp_rel ) / sum( pm$exp_nohu ), share_exp_mmb = sum( pm$exp_mmb ) / sum( pm$exp_nohu ),
  metro_share_rel_p10 = quantile( pm$n_rel / pm$n_all, .1 ), metro_share_rel_med = median( pm$n_rel / pm$n_all ),
  metro_share_rel_p90 = quantile( pm$n_rel / pm$n_all, .9 ), metro_share_rel_max = max( pm$n_rel / pm$n_all ),
  cor_rel_share_logP = cor( pm$n_rel / pm$n_all, log( pm$population ) ) )
print( round( sh, 4 ) ); fwrite( sh, file.path( FU, paste0( P10, "shares.csv" ) ) )
sami <- function( y, P ) { ok <- y > 0; z <- rep( NA_real_, length( y ) ); z[ ok ] <- resid( lm( log( y[ ok ] ) ~ log( P[ ok ] ) ) ); z }
b <- function( y, P ) coef( lm( log( y[ y > 0 ] ) ~ log( P[ y > 0 ] ) ) )[ 2 ]
cmp <- rbindlist( lapply( c( "metro", "cbsa" ), function( u ) { z <- if ( u == "metro" ) pm else p22
  data.table( unit = u,
    beta_n = b( z$n_filer_c3, z$population ), beta_n_xrm = b( z$n_filer_c3_xrm, z$population ),
    beta_e = b( z$exp_c3_nohu, z$population ), beta_e_xrm = b( z$exp_c3_nohu_xrm, z$population ),
    cor_sami_n = cor( sami( z$n_filer_c3, z$population ), sami( z$n_filer_c3_xrm, z$population ), use = "complete.obs" ),
    cor_sami_e = cor( sami( z$exp_c3_nohu, z$population ), sami( z$exp_c3_nohu_xrm, z$population ), use = "complete.obs" ),
    spearman_sami_n = cor( sami( z$n_filer_c3, z$population ), sami( z$n_filer_c3_xrm, z$population ), use = "complete.obs", method = "spearman" ) ) } ) )
print( cmp[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 4 ) else z ) ] ); fwrite( cmp, file.path( FU, paste0( P10, "scaling_sami.csv" ) ) )
pm[ , `:=`( s0 = sami( n_filer_c3, population ), s1 = sami( n_filer_c3_xrm, population ) ) ]
pm[ , `:=`( pr0 = 100 * rank( s0 ) / .N, pr1 = 100 * rank( s1 ) / .N, rel_share = n_rel / n_all ) ]
mv <- pm[ order( -abs( pr1 - pr0 ) ) ][ 1:12, .( cbsa_title, population, rel_share = round( rel_share, 3 ), n_rel,
                                                pctile_all = round( pr0 ), pctile_without = round( pr1 ) ) ]
print( mv ); fwrite( mv, file.path( FU, paste0( P10, "metros_moving_most.csv" ) ) )
cat( "median |percentile change|:", round( median( abs( pm$pr1 - pm$pr0 ) ), 2 ), "; 90th:", round( quantile( abs( pm$pr1 - pm$pr0 ), .9 ), 2 ), "\n" )
top_rel <- pm[ order( -rel_share ) ][ 1:10, .( cbsa_title, rel_share = round( rel_share, 3 ), n_rel ) ]
print( top_rel ); fwrite( top_rel, file.path( FU, paste0( P10, "most_religious_share.csv" ) ) )

## ---- Part 2: baseline ----------------------------------------------------------------
pm[ , `:=`( l_pf = flog( pf_assets, population ), l_gov = flog( gov_earn_sl, population ) ) ]
if ( MV_MODEL == "lean" ) pm[ , `:=`( l_med = flog( tr_medicaid, population ), l_hist = flog( hist_pre1970, population ) ) ]
RHS <- if ( MV_MODEL == "lean" ) "logP + poverty_rate + pct_65plus + gini_m + pct_nonwhite + rel_hhi + age_sd + l_pf + pct_ba + turnout + lag_g10 + l_gov + l_med + l_hist" else
  "logP + race_hhi + l_pf + turnout + l_gov + poverty_rate + pct_ba"
bl <- rbindlist( lapply( c( "n_filer_c3", "n_filer_c3_xrm", "exp_c3_nohu", "exp_c3_nohu_xrm" ), function( y ) {
  r <- fixest::feols( as.formula( paste( "log(", y, ") ~", RHS ) ), pm, vcov = "hetero" )
  ct <- fixest::coeftable( r ); dfr <- fixest::degrees_freedom( r, "resid" )
  data.table( outcome = y, term = rownames( ct ), est = ct[ , 1 ], partial_r = ct[ , 3 ] / sqrt( ct[ , 3 ]^2 + dfr ) ) } ) )
print( dcast( bl[ term != "(Intercept)" ], term ~ outcome, value.var = "partial_r" )[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
fwrite( bl, file.path( FU, paste0( P10, "baseline.csv" ) ) )

## ---- Part 3: matched multiverse sample -------------------------------------------------
dd <- merge( d, agg[ , .( cbsa, n_filer_c3_xrm, exp_c3_nohu_xrm ) ][ , year := 2022L ], by = c( "cbsa", "year" ), all.x = TRUE )
g <- build_grid( "principled" )[ is.na( dup_of ) & ident == "cs" & cs_year == 2022L ]
set.seed( 2026 )
smp <- rbind( g[ outcome == "n_filer_c3" ][ sample( .N, N_COUNT ) ], g[ outcome == "exp_c3_nohu" ][ sample( .N, N_EXP ) ] )
log_msg( "matched sample: ", nrow( smp ), " specifications x 2 outcomes" )
res <- rbindlist( lapply( seq_len( nrow( smp ) ), function( i ) {
  s <- smp[ i ]; a <- run_spec( s, dd ); s2 <- copy( s ); s2[ , outcome := paste0( outcome, "_xrm" ) ]; bb <- run_spec( s2, dd )
  if ( is.null( a ) || is.null( bb ) ) return( NULL )
  m <- merge( a[ , .( term, t0 = stat, p0 = p, n0 = n ) ], bb[ , .( term, t1 = stat, p1 = p, n1 = n ) ], by = "term" )
  k <- nrow( a ); m[ , `:=`( r0 = t0 / sqrt( t0^2 + n0 - k - 1 ), r1 = t1 / sqrt( t1^2 + n1 - k - 1 ),
                             spec_id = s$spec_id, outcome = s$outcome, dv_form = s$dv_form, est = s$est ) ] } ) )
fwrite( res, file.path( FU, paste0( P10, "matched_specs.csv" ) ) )
summ <- res[ term != "logP", .( pairs = .N, cor_r = cor( r0, r1 ), mean_abs_diff = mean( abs( r1 - r0 ) ),
                                p90_abs_diff = quantile( abs( r1 - r0 ), .9 ), sign_flips = mean( sign( r0 ) != sign( r1 ) ),
                                sig_changes = mean( ( p0 < .05 ) != ( p1 < .05 ) ) ), keyby = .( outcome, term ) ]
print( summ[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ] )
fwrite( summ, file.path( FU, paste0( P10, "matched_summary.csv" ) ) )
tot <- res[ term != "logP", .( pairs = .N, cor_r = cor( r0, r1 ), mean_abs_diff = mean( abs( r1 - r0 ) ),
                               sign_flips = mean( sign( r0 ) != sign( r1 ) ), sig_changes = mean( ( p0 < .05 ) != ( p1 < .05 ) ) ), keyby = outcome ]
print( tot[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 4 ) else z ) ] )
fwrite( tot, file.path( FU, paste0( P10, "matched_overall.csv" ) ) )
log_msg( "done" )
