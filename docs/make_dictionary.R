## docs/make_dictionary.R
## Writes docs/variable_dictionary.csv: one row per variable in the final analysis
## tables (data/analysis/county_year.parquet, cbsa_year.parquet), with a short
## description, the build step that makes it, and which table carries it.
## Gap-fill variants (<v>_obs, <v>_lin, <v>_locf; see 30_assemble_analysis.R) are
## folded into their base variable's row. Descriptions come from the covariate
## codebooks the build writes (data/cov/*codebook*.csv) and, for everything else,
## from the table below. Run from the project folder after a rebuild of 30:
##   Rscript docs/make_dictionary.R

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )

A <-file.path( BUILD, "data", "analysis" )
cty <- names( open_dataset( file.path( A, "county_year.parquet" ) ) )
cbs <- names( open_dataset( file.path( A, "cbsa_year.parquet" ) ) )

## ---- curated descriptions ----------------------------------------------------------
## step: build script that creates the variable (30 = derived at assembly)
cur <- fread( text = '
variable|group|step|description
geoid|keys|01|County-equivalent FIPS code, 2023 delineation (Connecticut planning regions)
cbsa|keys|01|CBSA code, OMB 2023 delineation
cbsa_title|keys|01|CBSA name
cbsa_type|keys|01|Metropolitan or micropolitan statistical area
state_fips|keys|01|State FIPS code (CBSA: state of the principal county)
n_counties|keys|30|Number of county-equivalents in the CBSA
year|keys|01|Calendar / tax year
population|population|01|Resident population, Census PEP July 1
src_990|nonprofit flags|06|Source of the 990/990-EZ series that year: CORE 2000-10, SOI 2011-19, e-file 2020-24
src_pf|nonprofit flags|06|Source of the 990-PF series that year
pf_interp|nonprofit flags|06|1 = foundation values interpolated (2016-18)
post_splice_soi|nonprofit flags|06|1 = year on or after the CORE -> SOI splice (2011)
post_splice_efile|nonprofit flags|06|1 = year on or after the SOI -> e-file splice (2020)
n_filer_all|nonprofit filers|06|Active filers, all 501(c) subsections (max(revenue, expenses) >= $50k in 2010 $)
n_filer_c3|nonprofit filers|06|Active 501(c)(3) 990/990-EZ filers (same floor; 990-PF excluded)
n_filer_c3_nohu|nonprofit filers|06|Active c3 filers excluding hospitals (NTEE E20-E24) and universities (B40-B43, B50)
n_filer_c3_hs|nonprofit filers|06|Active c3 filers in human services (NTEE major groups I-P)
n_filer_c3_nofloor|nonprofit filers|06|All c3 990/990-EZ records, no size floor
exp_all|nonprofit resources|06|Total expenses of active filers, all subsections (2022 $)
exp_c3|nonprofit resources|06|Total expenses of active c3 filers (2022 $)
exp_c3_nohu|nonprofit resources|06|Total expenses of active c3 filers excluding hospitals and universities (2022 $)
exp_c3_hs|nonprofit resources|06|Total expenses of active human-services c3 filers (2022 $)
rev_c3|nonprofit resources|06|Total revenue of active c3 filers (2022 $)
rev_c3_nohu|nonprofit resources|06|Total revenue, excluding hospitals and universities (2022 $)
grants_org_c3|nonprofit resources|09|Grants paid to domestic organizations and governments (990 Part IX line 1; 2012+; 2022 $)
exp_c3_net|nonprofit resources|06|exp_c3 minus grants_org_c3 (pass-through removed; 2012+)
exp_c3_nohu_net|nonprofit resources|06|exp_c3_nohu minus grants paid (2012+)
rev_c3_net|nonprofit resources|06|rev_c3 minus grants paid (2012+)
rev_c3_nohu_net|nonprofit resources|06|rev_c3_nohu minus grants paid (2012+)
con_c3|nonprofit resources|06|Contributions, gifts and grants received by active c3 filers (2022 $)
assets_c3|nonprofit resources|06|Total assets, end of year, active c3 filers (2022 $)
n_pf|foundations|06|Private foundations filing a 990-PF (no floor)
pf_assets|foundations|06|Foundation assets, book value end of year (2022 $)
pf_grants|foundations|06|Foundation contributions and grants paid (2022 $)
n_bmf_all|registered orgs|02b|Registered exempt organizations, all subsections (reconstructed BMF stock)
n_bmf_c3|registered orgs|02b|Registered 501(c)(3) organizations (reconstructed stock)
n_bmf_c3pc|registered orgs|02b|Registered 501(c)(3) public charities (foundation code not 02/03/04)
n_bmf_c3pc_nohu|registered orgs|02b|Registered public charities excluding hospitals and universities
n_bmf_c3pc_hs|registered orgs|02b|Registered human-services public charities (NTEE I-P)
n_bmf_pf|registered orgs|02b|Registered private foundations
n_bmf_all_snap|registered orgs|02|Raw BMF snapshot count, all subsections (no gap filling)
n_bmf_c3_snap|registered orgs|02|Raw snapshot count, 501(c)(3)
n_bmf_c3pc_snap|registered orgs|02|Raw snapshot count, c3 public charities
n_bmf_c3pc_nohu_snap|registered orgs|02|Raw snapshot count, public charities excluding hospitals and universities
n_bmf_c3pc_hs_snap|registered orgs|02|Raw snapshot count, human-services public charities
n_bmf_pf_snap|registered orgs|02|Raw snapshot count, private foundations
bmf_months_off|registered orgs|02|Months between the snapshot used and Dec 31 of the year
bmf_src_monthly|registered orgs|02b|1 = 2023-24 counts from the NCCS monthly BMF
n_entry_c3|entries|08|New 501(c)(3) registrations by ruling year
n_entry_c3pc|entries|08|New public-charity registrations
n_entry_c3pc_nohu|entries|08|New public-charity registrations excluding hospitals and universities
n_entry_pf|entries|08|New private-foundation registrations (NA for 2016-21)
entry_src_monthly|entries|08|1 = 2022-24 entries from the Dec 2024 monthly BMF (survivors only)
pcpi|economy|30|Per-capita personal income = personal_income / bea_pop
gov_emp_share|government|30|State and local government share of jobs = gov_emp_sl / emp_total
gov_earn_share|government|30|State and local government share of earnings = gov_earn_sl / earn_total
poverty_rate|need|30|Poverty rate (share) = pov_n / pov_universe
med_hh_income|need|30|Median household income, nominal $ (copy of mhi; CBSA: population-weighted)
unemp_rate|economy|30|Unemployment rate (share) = unemp / lf
contrib_total|giving|30|Itemized charitable contributions, $ (= a19700)
contrib_per_return|giving|30|Itemized contributions per tax return
contrib_share_agi|giving|30|Itemized contributions / AGI
pct_adherent|religion|30|Religious adherents / population
pct_catholic|religion|30|Catholic adherents / all adherents
gdp_per_worker|economy|30|Real GDP per job = gdp_real / emp_total
qcew_avg_pay|economy|30|Average private annual pay = qcew_priv_wages / qcew_priv_emp
turnout|civic|30|Presidential votes / population 18+
tr_medmaint|government|30|Medicare + Medicaid + income-maintenance transfers
acs_pct_gov|government|30|Share of employed residents working for any government (ACS)
acs_pct_lgov|government|30|Share of employed residents working for local government (ACS)
qcew_lgov_share|government|30|Local government share of QCEW employment (government + private)
gov_labor_intensity|government|30|Local government wages / local current operations
gov_indirect_share|government|30|Indirect government (Medicare, Medicaid, income maintenance) / (indirect + state and local wages)
patents_frac|innovation|21|Utility patents by application year, fractional inventor counts
patents_corp|innovation|21|Patents with a corporate assignee (fractional)
patents_univnp|innovation|21|Patents with a university or nonprofit assignee (fractional)
patents_other|innovation|21|Patents with another or no assignee (fractional)
patents_truncated|innovation|21|1 = application year 2021+ (grant lag truncates counts)
hist_pre1970|history|25|Share of the 2000 public-charity stock ruled before 1970 (NA where > 0.6)
lag_g5|history|26|Population growth kernel, 5-year lag
lag_g10|history|26|Population growth kernel, 10-year lag
gini_m|need|27|Metro household-income Gini from summed ACS brackets (2010-24)
bls_npo_estab|BLS nonprofit|07|501(c)(3) establishments, QCEW-IRS match (MSA, 2013-22)
bls_npo_emp|BLS nonprofit|07|501(c)(3) annual average employment
bls_npo_wages|BLS nonprofit|07|501(c)(3) total annual wages ($)
bls_priv_estab|BLS nonprofit|07|All private establishments
bls_priv_emp|BLS nonprofit|07|All private employment
bls_priv_wages|BLS nonprofit|07|All private total annual wages ($)
bls_pct_npo|BLS nonprofit|07|Nonprofit share of private employment
bls_wage_ratio|BLS nonprofit|07|Nonprofit / private average annual wage (raw)
bls_wage_ratio_adj|BLS nonprofit|07|Composition-adjusted wage ratio (fixed 2018 national sector weights)
bls_wage_ratio_cov|BLS nonprofit|07|Share of national sector weight available for the adjusted ratio
bls_pct_npo_61|BLS nonprofit|07|Nonprofit share of employment, educational services (NAICS 61)
bls_pct_npo_62|BLS nonprofit|07|Nonprofit share of employment, health care and social assistance (62)
bls_pct_npo_624|BLS nonprofit|07|Nonprofit share of employment, social assistance (624)
bls_pct_npo_71|BLS nonprofit|07|Nonprofit share of employment, arts and recreation (71)
bls_delin_flag|BLS nonprofit|07|1 = MSA code absent from the 2023 delineation or its county set changed
socap_n_relig|social capital|19|Religious organizations (CBP establishments)
socap_n_civic|social capital|19|Civic and social associations
socap_n_bus|social capital|19|Business associations
socap_n_pol|social capital|19|Political organizations
socap_n_prof|social capital|19|Professional organizations
socap_n_labor|social capital|19|Labor organizations
socap_n_bowl|social capital|19|Bowling centers
socap_n_fitness|social capital|19|Fitness and recreational sports centers
socap_n_golf|social capital|19|Golf courses and country clubs
socap_n_sport|social capital|19|Sports teams and clubs
qcew_813_estab|economy|20|NAICS 813 membership-organization establishments, private (NA = suppressed)
qcew_813_emp|economy|20|NAICS 813 employment, private
qcew_813_wages|economy|20|NAICS 813 wages, private ($)
qcew_privx813_estab|economy|20|Private establishments excluding NAICS 813
qcew_privx813_emp|economy|20|Private employment excluding NAICS 813
qcew_privx813_wages|economy|20|Private wages excluding NAICS 813 ($)
', sep = "|" )

## ---- codebooks written by the covariate steps ------------------------------------
step_of <- c( elections = "16", irs_soi = "15", laus = "14", saipe = "13", acs = "17", bea = "12",
              gov = "22", pep_char = "11", qcew = "20", rcms = "18", socap = "19" )
grp_of  <- c( elections = "civic", irs_soi = "giving", laus = "economy", saipe = "need", acs = "demography",
              bea = "economy", gov = "government", pep_char = "demography", qcew = "economy",
              rcms = "religion", socap = "social capital" )
cb <- rbindlist( lapply( list.files( OUT$cov, "codebook", full.names = TRUE ), function( f ) {
  x <- fread( f ); setnames( x, 1:2, c( "variable", "description" ) )
  k <- sub( "^(cov_)?(codebook_)?", "", sub( "(_codebook)?\\.csv$", "", basename( f ) ) )
  x[ , .( variable = trimws( sub( "\\(derived\\)", "", variable ) ), description, step = step_of[ k ], group = grp_of[ k ] ) ]
} ) )
cb <- cb[ !grepl( "\\*", variable ) ]

## market competition: mkt_<metric>_<resource>_<universe> (labels as in 23's codebook)
lab <- c( hhi = "HHI of organization shares (0-1), whole metro", nhhi = "Normalized HHI (hhi - 1/n)/(1 - 1/n)",
          xs = "Excess concentration n x HHI", cr1 = "Share of the largest organization",
          cr4 = "Share of the 4 largest organizations", gini = "Gini of organization resources",
          small = "Share of resources held by organizations under $1M (2022 $)",
          shhi = "Resource-weighted mean HHI of metro x NTEE major-group markets",
          shhio = "Organization-weighted mean HHI of metro x NTEE major-group markets",
          scr4o = "Organization-weighted mean CR4 of metro x NTEE major-group markets",
          mix = "HHI of metro resources across NTEE major groups" )
ulab <- c( c3 = "all 501(c)(3)", nohu = "excl. hospitals and universities",
           local = "excl. hospitals, universities, support orgs (B11), health (E-H), international (Q), research (U), grantmakers (T), uncoded" )
mk <- CJ( m = names( lab ), r = c( "rev", "con" ), u = names( ulab ), sorted = FALSE )
mk <- rbind( mk[ , .( variable = paste( "mkt", m, r, u, sep = "_" ),
                      description = paste0( lab[ m ], "; ", fifelse( r == "rev", "revenue", "contributions" ), "; ", ulab[ u ] ) ) ],
             data.table( variable = paste0( "mkt_small_n_", names( ulab ) ),
                         description = paste0( "Share of active filers under $1M (2022 $); ", ulab ) ) )
mk[ , `:=`( step = "23", group = "market competition" ) ]

desc <- unique( rbind( cur[ , .( variable, description, step, group ) ], cb, mk, fill = TRUE ), by = "variable" )

## ---- assemble -----------------------------------------------------------------------
all <- unique( c( cty, cbs ) )
base <- sub( "_(obs|lin|locf)$", "", all )
isvar <- all != base & base %in% all
d <- data.table( variable = all[ !isvar ] )
d[ , in_county := variable %in% cty ][ , in_cbsa := variable %in% cbs ]
d[ , gapfill := variable %in% base[ isvar ] ]
d <- merge( d, desc, by = "variable", all.x = TRUE, sort = FALSE )
d[ gapfill == TRUE, description := paste0( description, " [interpolated; also _obs (observed flag), _lin (linear), _locf (carried forward)]" ) ]
miss <- d[ is.na( description ), variable ]
if ( length( miss ) ) warning( "no description for: ", paste( miss, collapse = ", " ) )
d[ , order := match( variable, all ) ]
setorder( d, order )
d[ , table := fcase( in_county & in_cbsa, "both", in_county, "county", default = "cbsa" ) ]
out <- d[ , .( variable, group, step, table, gapfill, description ) ]
fwrite( out, "docs/variable_dictionary.csv" )
cat( "wrote docs/variable_dictionary.csv:", nrow( out ), "variables (",
     sum( isvar ), "gap-fill variants folded );", length( cty ), "county and", length( cbs ), "CBSA columns\n" )
