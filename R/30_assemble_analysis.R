## 30_assemble_analysis.R
## Build the analysis panels (county x year and CBSA x year, 2000-2024) from
##   data/npo/npo_county_year.csv   (outcomes, population, foundations, BMF)
##   data/cov/cov_*.csv             (one file per covariate source)
## Aggregation to CBSA sums numerators/denominators; medians and Gini are
## population-weighted means (flagged in the codebook).
## Periodic covariates get *_lin (linear) and *_locf (carry-forward) versions;
## the bare name is the default (linear), and *_obs marks observed years.
##
## Outputs: D:/MULTIVERSE/data/analysis/{county,cbsa}_year.parquet
##          D:/MULTIVERSE/data/analysis/codebook_analysis.csv

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
OUT$analysis <- file.path( BUILD, "data", "analysis" )
dir.create( OUT$analysis, showWarnings = FALSE )

spine <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
npo   <- fread( file.path( OUT$npo, "npo_county_year.csv" ), colClasses = list( character = "geoid" ) )

## ---- covariates: read every source present --------------------------------

cov_files <- list.files( OUT$cov, pattern = "^cov_[a-z_]+\\.csv$", full.names = TRUE )
cov_files <- cov_files[ !grepl( "codebook", cov_files ) ]
cov <- NULL
for ( f in cov_files ) {
  hdr <- names( fread( f, nrows = 0 ) )
  if ( !all( c( "geoid", "year" ) %in% hdr ) ) {
    log_msg( basename( f ), ": skipped (not keyed geoid x year)" ); next
  }
  d <- fread( f, colClasses = list( character = "geoid" ) )
  d <- d[ year %in% YEARS ]
  ## drop non-numeric helper columns except a few flags
  keepc <- c( "geoid", "year", names( d )[ vapply( d, is.numeric, logical( 1 ) ) ] )
  d <- d[ , unique( keepc ), with = FALSE ]
  dup <- intersect( setdiff( names( d ), c( "geoid", "year" ) ), names( cov ) )
  if ( length( dup ) ) d[ , ( dup ) := NULL ]
  cov <- if ( is.null( cov ) ) d else merge( cov, d, by = c( "geoid", "year" ), all = TRUE )
  log_msg( basename( f ), ": ", ncol( d ) - 2, " variables" )
}

cy <- merge( npo, cov, by = c( "geoid", "year" ), all.x = TRUE )
cy <- merge( cy, spine[ , .( geoid, cbsa, cbsa_type, state_fips ) ], by = "geoid", all.x = TRUE )

## ---- columns that are medians / indices (weighted mean, not sum) ----------

## medians / indices: population-weighted mean across counties
WMEAN <- intersect( c( "gini", "mhi", "socap_respn", "socap_sk", "socap_sk2" ), names( cy ) )
## flags and source markers: not aggregated
FLAGS <- c( "src_990", "src_pf", "bmf_months_off", "post_splice_soi", "post_splice_efile",
            "pf_interp", "acs_mid_year", "ak_statewide", "mhi_ct_approx", "post_tcja",
            "irs_era_pre2010", "u18_method", "gini_method",
            "bmf_src_monthly", "entry_src_monthly", "rel_2000_noblack", "rel_groups",
            "socap_interp2000", "patents_truncated" )
## rates, shares, and indices published by the sources: never summed; the
## ones the analysis uses are recomputed from numerators in derive()
RATE_PAT <- "^pct_|_rate|_share|share$|_hhi$|^race_diversity$|^age_(mean|sd|hhi)$|competitiveness|per_return|per_claim|^dem_margin$"
id    <- c( "geoid", "year", "cbsa", "cbsa_type", "state_fips" )
num   <- setdiff( names( cy )[ vapply( cy, is.numeric, logical( 1 ) ) ], c( id, FLAGS, WMEAN ) )
num   <- num[ !grepl( RATE_PAT, num ) ]

## ---- derived variables (same formulas at every unit) ------------------------

derive <- function( d ) {
  has <- function( ... ) all( c( ... ) %in% names( d ) )
  sdiv <- function( a, b ) ifelse( is.finite( a / b ), a / b, NA_real_ )
  if ( has( "race_den", "n_nhwhite" ) ) {
    g <- intersect( c( "n_nhwhite", "n_nhblack", "n_nhaian", "n_nhasian", "n_nhnhpi", "n_nhtwo", "n_hisp" ),
                    names( d ) )
    d[ , race_hhi := Reduce( `+`, lapply( g, function( v ) ( get( v ) / race_den )^2 ) ) ]
    d[ , pct_nonwhite := 1 - n_nhwhite / race_den ]
  }
  if ( has( "pop_65plus", "pop_total" ) ) d[ , pct_65plus := sdiv( pop_65plus, pop_total ) ]
  if ( has( "pop_age_00_04" ) ) {
    bins <- grep( "^pop_age_", names( d ), value = TRUE )
    mids <- c( seq( 2.5, 82.5, 5 ), 87.5 )[ seq_along( bins ) ]
    tot <- Reduce( `+`, lapply( bins, function( b ) d[[ b ]] ) )
    m1  <- Reduce( `+`, Map( function( b, m ) d[[ b ]] * m, bins, mids ) ) / tot
    m2  <- Reduce( `+`, Map( function( b, m ) d[[ b ]] * m^2, bins, mids ) ) / tot
    d[ , age_sd := sqrt( pmax( m2 - m1^2, 0 ) ) ]
  }
  if ( has( "edu_ba_num", "edu_den" ) ) d[ , pct_ba := sdiv( edu_ba_num, edu_den ) ]
  if ( has( "fb_num", "fb_den" ) ) d[ , pct_foreign_born := sdiv( fb_num, fb_den ) ]
  if ( has( "personal_income", "bea_pop" ) ) d[ , pcpi := sdiv( personal_income, bea_pop ) ]
  if ( has( "gov_emp_sl", "emp_total" ) ) d[ , gov_emp_share := sdiv( gov_emp_sl, emp_total ) ]
  if ( has( "gov_earn_sl", "earn_total" ) ) d[ , gov_earn_share := sdiv( gov_earn_sl, earn_total ) ]
  ## first matching column, extracted outside data.table scope (avoids
  ## local-name / column-name collisions such as `lf`)
  col <- function( cands ) { k <- intersect( cands, names( d ) ); if ( length( k ) ) d[[ k[1] ]] else NULL }
  pov_n <- col( c( "pov_n", "pov_num", "poverty_count" ) ); pov_u <- col( c( "pov_universe", "pov_den" ) )
  if ( !is.null( pov_n ) && !is.null( pov_u ) ) set( d, j = "poverty_rate", value = sdiv( pov_n, pov_u ) )
  if ( has( "mhi" ) ) set( d, j = "med_hh_income", value = d$mhi )
  un_v <- col( c( "unemp", "unemployed" ) ); lf_v <- col( c( "lf", "labor_force" ) )
  if ( !is.null( un_v ) && !is.null( lf_v ) ) set( d, j = "unemp_rate", value = sdiv( un_v, lf_v ) )
  ca_v <- col( c( "a19700", "contrib_amount" ) ); cr_v <- col( c( "n1", "returns_total" ) )
  ag_v <- col( c( "a00100", "agi" ) )
  if ( !is.null( ca_v ) ) set( d, j = "contrib_total", value = ca_v )      # already in dollars (15_cov_irs_soi.R)
  if ( !is.null( ca_v ) && !is.null( cr_v ) ) set( d, j = "contrib_per_return", value = sdiv( ca_v, cr_v ) )
  if ( !is.null( ca_v ) && !is.null( ag_v ) ) set( d, j = "contrib_share_agi", value = sdiv( ca_v, ag_v ) )
  ## religion (RCMS), urban economy
  if ( has( "rel_adh", "population" ) ) set( d, j = "pct_adherent", value = sdiv( d$rel_adh, d$population ) )
  if ( has( "rel_cath_adh", "rel_adh" ) ) set( d, j = "pct_catholic", value = sdiv( d$rel_cath_adh, d$rel_adh ) )
  if ( has( "gdp_real", "emp_total" ) ) set( d, j = "gdp_per_worker", value = sdiv( d$gdp_real, d$emp_total ) )
  if ( has( "qcew_priv_wages", "qcew_priv_emp" ) )
    set( d, j = "qcew_avg_pay", value = sdiv( d$qcew_priv_wages, d$qcew_priv_emp ) )
  vt_v <- col( c( "votes_total", "totalvotes" ) )
  if ( !is.null( vt_v ) && has( "pop_18plus" ) ) set( d, j = "turnout", value = sdiv( vt_v, d$pop_18plus ) )
  if ( !is.null( vt_v ) && has( "votes_dem", "votes_rep" ) )
    set( d, j = "dem_margin", value = sdiv( d$votes_dem - d$votes_rep, vt_v ) )
  d
}

## ---- periodic covariates: observed flag, linear, carry-forward -------------

## series whose last release precedes 2024 (RCMS 2020, Rupasingha 2014): the
## default also carries the last observation forward (design 4: "carried
## forward and flagged"; *_obs marks observed years)
CARRY_END <- c( "rel_hhi", "pct_adherent", "pct_catholic", "rel_adh", "rel_cng", "rel_cath_adh",
                "socap_assn_n", "socap_respn", "socap_sk", "socap_sk2",
                ## government (22_cov_government.R, 12_cov_bea.R): Census of
                ## Governments census years 2002-2022 (interpolated between,
                ## carried to 2023-24, missing 2000-01); BEA transfers end 2022
                "cog_curop", "cog_capital", "cog_assist", "cog_interest", "cog_educ_curop",
                "cog_welfare", "cog_health_curop", "cog_salaries", "cog_fed_igr", "cog_state_igr", "cog_dge",
                "tr_total", "tr_gov", "tr_socsec", "tr_medical", "tr_medicare", "tr_medicaid",
                "tr_income_maint", "tr_snap", "tr_unemp", "tr_veterans", "tr_education", "tr_nonprofit" )

## direct vs indirect government measures (notes/government-funding-measures.md),
## computed after interpolation so census-year series have every year
derive_gov <- function( d ) {
  has <- function( ... ) all( c( ... ) %in% names( d ) )
  sdiv <- function( a, b ) ifelse( is.finite( a / b ), a / b, NA_real_ )
  if ( has( "tr_medicare", "tr_medicaid", "tr_income_maint" ) )
    set( d, j = "tr_medmaint", value = d$tr_medicare + d$tr_medicaid + d$tr_income_maint )
  if ( has( "acs_cow_den", "acs_cow_lgov", "acs_cow_sgov", "acs_cow_fgov" ) ) {
    set( d, j = "acs_pct_gov", value = sdiv( d$acs_cow_lgov + d$acs_cow_sgov + d$acs_cow_fgov, d$acs_cow_den ) )
    set( d, j = "acs_pct_lgov", value = sdiv( d$acs_cow_lgov, d$acs_cow_den ) )
  }
  if ( has( "qcew_lgov_emp", "qcew_sgov_emp", "qcew_fgov_emp", "qcew_priv_emp" ) )
    set( d, j = "qcew_lgov_share", value = sdiv( d$qcew_lgov_emp,
         d$qcew_lgov_emp + d$qcew_sgov_emp + d$qcew_fgov_emp + d$qcew_priv_emp ) )
  if ( has( "qcew_lgov_wages", "cog_curop" ) )
    set( d, j = "gov_labor_intensity", value = sdiv( d$qcew_lgov_wages, d$cog_curop ) )
  if ( has( "tr_medmaint", "qcew_lgov_wages", "qcew_sgov_wages" ) )
    set( d, j = "gov_indirect_share", value = sdiv( d$tr_medmaint, d$tr_medmaint + d$qcew_lgov_wages + d$qcew_sgov_wages ) )
  d
}
fill_periodic <- function( d, unit ) {
  vars <- intersect( c( "pct_ba", "pct_foreign_born", "gini", "turnout", "dem_margin",
                        "contrib_total", "contrib_per_return", "contrib_share_agi", CARRY_END ), names( d ) )
  setorderv( d, c( unit, "year" ) )
  for ( v in vars ) {
    d[ , paste0( v, "_obs" ) := !is.na( get( v ) ) ]
    d[ , paste0( v, "_lin" ) := {
      x <- as.numeric( get( v ) ); ok <- !is.na( x )
      if ( sum( ok ) >= 2 ) approx( year[ ok ], x[ ok ], year, rule = 1 )$y else x }, by = unit ]
    d[ , paste0( v, "_locf" ) := nafill( as.numeric( get( v ) ), type = "locf" ), by = unit ]
    if ( v %in% CARRY_END ) d[ , ( v ) := fcoalesce( get( paste0( v, "_lin" ) ), get( paste0( v, "_locf" ) ) ) ]
    else d[ , ( v ) := get( paste0( v, "_lin" ) ) ]
  }
  d
}

## ---- county file -------------------------------------------------------------

county <- derive( copy( cy ) )
county <- derive_gov( fill_periodic( county, "geoid" ) )
write_parquet( county, file.path( OUT$analysis, "county_year.parquet" ) )

## ---- CBSA file ---------------------------------------------------------------

cb <- cy[ !is.na( cbsa ) & cbsa != "" ]
wpop <- "population"
agg_sum <- cb[ , lapply( .SD, function( x ) if ( all( is.na( x ) ) ) NA_real_ else as.numeric( sum( x, na.rm = TRUE ) ) ),
               by = .( cbsa, year ), .SDcols = num ]
agg_w <- if ( length( WMEAN ) )
  cb[ , lapply( .SD, function( x ) weighted.mean( x, get( wpop ), na.rm = TRUE ) ),
      by = .( cbsa, year ), .SDcols = WMEAN ] else NULL
## CBSA state = state of its most populous county (for state FE / RE / clustering)
st <- cb[ year == 2020, .( state_fips = state_fips[ which.max( population ) ] ), by = cbsa ]
meta <- unique( spine[ !is.na( cbsa ) & cbsa != "", .( cbsa, cbsa_title, cbsa_type ) ] )
flags <- cb[ , .( src_990 = first( src_990 ), src_pf = first( src_pf ), pf_interp = first( pf_interp ),
                  n_counties = uniqueN( geoid ) ), by = .( cbsa, year ) ]
fl_max <- intersect( c( "bmf_src_monthly", "entry_src_monthly", "rel_2000_noblack", "socap_interp2000",
                        "patents_truncated" ), names( cb ) )
if ( length( fl_max ) )
  flags <- merge( flags, cb[ , lapply( .SD, function( x ) suppressWarnings( max( as.numeric( x ), na.rm = TRUE ) ) ),
                             by = .( cbsa, year ), .SDcols = fl_max ], by = c( "cbsa", "year" ) )
cbsa <- Reduce( function( a, b ) merge( a, b, by = intersect( names( a ), names( b ) ), all.x = TRUE ),
                Filter( Negate( is.null ), list( agg_sum, agg_w, flags ) ) )
for ( v in fl_max ) set( cbsa, i = which( !is.finite( cbsa[[ v ]] ) ), j = v, value = NA )
## CBSA-level sources (computed at CBSA directly: RCMS HHI from summed group
## adherents; BLS MSA files): data/cov/cbsa/cbsa_*.csv, keyed cbsa x year
for ( f in list.files( file.path( OUT$cov, "cbsa" ), pattern = "^cbsa_[a-z_]+\\.csv$", full.names = TRUE ) ) {
  x <- fread( f, colClasses = list( character = "cbsa" ) )[ year %in% YEARS ]
  dup <- intersect( setdiff( names( x ), c( "cbsa", "year" ) ), names( cbsa ) )
  if ( length( dup ) ) cbsa[ , ( dup ) := NULL ]
  cbsa <- merge( cbsa, x, by = c( "cbsa", "year" ), all.x = TRUE )
  log_msg( basename( f ), ": ", ncol( x ) - 2, " CBSA-level variables" )
}
cbsa <- merge( cbsa, st, by = "cbsa", all.x = TRUE )
cbsa <- merge( cbsa, meta, by = "cbsa", all.x = TRUE )
cbsa <- derive( cbsa )
cbsa <- derive_gov( fill_periodic( cbsa, "cbsa" ) )
write_parquet( cbsa, file.path( OUT$analysis, "cbsa_year.parquet" ) )

## ---- coverage report -----------------------------------------------------------

key <- intersect( c( "population", "n_filer_c3", "exp_c3", "n_bmf_c3pc", "n_pf", "pf_assets",
                     "race_hhi", "pct_nonwhite", "pct_65plus", "age_sd", "pct_ba", "pct_foreign_born",
                     "gini", "pcpi", "gov_earn_sl", "gov_emp_share", "poverty_rate", "unemp_rate",
                     "contrib_total", "turnout", "dem_margin", "rel_hhi", "pct_adherent", "socap_assn_n",
                     "socap_sk2", "bls_npo_emp", "bls_wage_ratio_adj", "n_entry_c3pc", "gdp_real",
                     "qcew_priv_wages", "patents_frac" ), names( cbsa ) )
covg <- cbsa[ cbsa_type == "metro", lapply( .SD, function( x ) sum( !is.na( x ) ) ),
              keyby = year, .SDcols = key ]
fwrite( covg, file.path( OUT$diag, "analysis_coverage_metro.csv" ) )
print( covg )
log_msg( "CBSA file: ", nrow( cbsa ), " rows x ", ncol( cbsa ), " cols; county file: ",
         nrow( county ), " x ", ncol( county ) )
