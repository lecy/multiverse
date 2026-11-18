## 45_summarize_run.R
## First-pass results for a completed universe run (design doc section 5).
##   Goal 1 - which covariates are specification-sensitive:
##            sign stability, share significant, IQR of standardized beta,
##            variance decomposition across fork dimensions (marginal eta^2)
##   Goal 2 - how much results depend on DV form:
##            median standardized beta by DV form; Spearman correlation of
##            t-statistics across DV forms; meta-hypothesis: does a covariate's
##            R^2 on log P predict its DV-form sensitivity? (and, since
##            2026-11-15, its predictor-form sensitivity)
## Usage: Rscript R/45_summarize_run.R [principled|kitchen|principled_v7 ...]
##   The argument names the run folder data/spec/run_<arg>/; outputs go there.
## 2026-11-15 conventions, aligned with 58_outline_analyses.R part A so the two agree:
##   - partial r counts degrees of freedom in independent places, not place-years
##     (pooled = 8 years, within = 13), because panel SEs are clustered by metro
##   - pooled state random-intercept fits are left out (model-based SEs treat
##     metro-years as independent, so their t is not comparable)
##   - size dependence uses flog() (per-capita floor) for extensive covariates,
##     as the models do from run v6 on

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( library( arrow ) )

UNIVERSE <- if ( length( commandArgs( TRUE ) ) ) commandArgs( TRUE )[1] else "principled"
run_dir <- file.path( OUT$spec, paste0( "run_", UNIVERSE ) )
specs <- as.data.table( read_parquet( file.path( run_dir, "specs.parquet" ) ) )
co    <- as.data.table( read_parquet( file.path( run_dir, "coefs.parquet" ) ) )
FORK_DIMS <- c( "outcome", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year",
                "outlier", "drop", "op" )

d <- merge( co[ term != "logP" ], specs[ , c( "spec_id", "construct", "hu", "ident", FORK_DIMS ), with = FALSE ],
            by = "spec_id" )
## Common currency across DV forms: partial correlation r = t / sqrt(t^2 + df).
## Standardized betas divide by sd(transformed y); for log-log that sd is
## dominated by city size, so identical (FWL) coefficients look ~3x smaller
## than under SAMI. Partial r is scale-free and equal across FWL twins.
## df ~ n_units - (terms in the spec + 1); fixed-effect df are ignored (n >> FE).
k_terms <- co[ , .( k = .N ), by = spec_id ]
rm( co ); gc()
d <- merge( d, k_terms, by = "spec_id" )
d <- d[ !( ident == "pooled" & est == "state_re" ) ]
d[ , n_units := n / fcase( ident == "pooled", 8, ident %in% c( "within", "within_long" ), 13, default = 1 ) ]
d[ , std_beta_raw := std_beta ]
d[ , std_beta := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]   # metric used below
d <- d[ is.finite( std_beta ) ]
d[ , stratum := paste( construct, hu, ident, sep = "/" ) ]

## ---- Goal 1: stability ---------------------------------------------------------

stab <- d[ , {
  med <- median( std_beta )
  .( specs = .N, median_partial_r = med, median_std_beta = median( std_beta_raw ),
     iqr_std_beta = IQR( std_beta ),
     sign_stability = mean( sign( std_beta ) == sign( med ) ),
     share_pos = mean( std_beta > 0 ),
     share_sig05 = mean( p < 0.05, na.rm = TRUE ),
     share_sig05_same_sign = mean( p < 0.05 & sign( std_beta ) == sign( med ), na.rm = TRUE ),
     share_sig05_state = mean( p_state < 0.05, na.rm = TRUE ) ) },
  by = .( stratum, construct, hu, ident, term ) ][ order( stratum, -sign_stability ) ]
fwrite( stab, file.path( run_dir, "summary_stability.csv" ) )

## marginal eta^2 of std_beta on each fork dimension (Young-Holsteen style
## "model influence": which fork moves the coefficient)
eta2 <- function( y, g ) {
  g <- as.factor( g ); if ( nlevels( g ) < 2 ) return( NA_real_ )
  m <- tapply( y, g, mean ); n <- tapply( y, g, length )
  sum( n * ( m - mean( y ) )^2 ) / sum( ( y - mean( y ) )^2 )
}
infl <- d[ , c( .( specs = .N ), lapply( setNames( FORK_DIMS, FORK_DIMS ),
                                         function( f ) eta2( std_beta, get( f ) ) ) ),
           by = .( stratum, term ) ]
fwrite( infl, file.path( run_dir, "summary_fork_influence.csv" ) )

## ---- Goal 2: DV form ---------------------------------------------------------------

by_dv <- dcast( d[ , .( med = median( std_beta ) ), by = .( stratum, term, dv_form ) ],
                stratum + term ~ dv_form, value.var = "med" )
fwrite( by_dv, file.path( run_dir, "summary_by_dv_form.csv" ) )

## Spearman correlation of covariate t-stats across DV forms (same other forks)
key <- setdiff( c( "stratum", "term", FORK_DIMS ), "dv_form" )
w <- dcast( d, as.formula( paste( paste( key, collapse = " + " ), "~ dv_form" ) ),
            value.var = "stat", fun.aggregate = mean )
dvs <- intersect( FORKS$dv_form, names( w ) )
rho <- rbindlist( lapply( split( w, by = "stratum" ), function( x ) {
  cm <- cor( x[ , ..dvs ], method = "spearman", use = "pairwise.complete.obs" )
  data.table( stratum = x$stratum[1], a = rep( dvs, length( dvs ) ), b = rep( dvs, each = length( dvs ) ),
              rho = as.vector( cm ) )[ a < b ] } ) )
fwrite( rho, file.path( run_dir, "summary_dvform_tstat_spearman.csv" ) )

## meta-hypothesis: size dependence (R^2 of covariate on log P, metros 2022)
## vs DV-form sensitivity (SD of the per-DV-form median standardized beta)
panel <- load_panel()
m22 <- panel[ year == 2022 & cbsa_type == "metro" ]
r2p <- rbindlist( lapply( COVS$var, function( v ) {
  x <- m22[[ v ]]; if ( is.null( x ) ) return( NULL )
  xx <- if ( COVS[ var == v, type ] == "B" ) flog( x, m22$population ) else x
  ok <- is.finite( xx ) & is.finite( m22$logP )
  if ( sum( ok ) < 10 ) return( NULL )
  data.table( term = v, type = COVS[ var == v, type ], r2_logP = summary( lm( xx[ ok ] ~ m22$logP[ ok ] ) )$r.squared )
} ) )
## predictor-form sensitivity added 2026-11-15: the outline finds predictor form, not
## DV form, is where size dependence bites (D6), so the meta-hypothesis is tested on both
sens <- d[ , .( dvform_sd = sd( tapply( std_beta, dv_form, median ) ),
                dvform_signflip = length( unique( sign( tapply( std_beta, dv_form, median ) ) ) ) > 1,
                predform_sd = sd( tapply( std_beta, pred_form, median ) ),
                predform_signflip = length( unique( sign( tapply( std_beta, pred_form, median ) ) ) ) > 1 ),
           by = .( stratum, term ) ]
meta <- merge( sens, r2p, by = "term" )
fwrite( meta, file.path( run_dir, "summary_meta_hypothesis.csv" ) )
meta_test <- meta[ , .( covariates = .N,
                        spearman_r2_vs_sensitivity = cor( r2_logP, dvform_sd, method = "spearman" ),
                        spearman_r2_vs_predform = cor( r2_logP, predform_sd, method = "spearman" ),
                        spearman_r2_vs_predform_typeB = if ( sum( type == "B" ) >= 3 )
                          cor( r2_logP[ type == "B" ], predform_sd[ type == "B" ], method = "spearman" ) else NA_real_ ),
                   by = stratum ]
fwrite( meta_test, file.path( run_dir, "summary_meta_test.csv" ) )

## ---- console headline ----------------------------------------------------------------

cat( "\n== Stability (count / incl / cs), default-measure covariates ==\n" )
print( stab[ stratum == "count/incl/cs" & term %in% c( "pct_nonwhite", "race_hhi", "pf_assets", "turnout", "gov_earn_sl",
                                                      "poverty_rate", "pct_ba" ),
             .( term, specs, median_r = round( median_partial_r, 3 ), iqr_r = round( iqr_std_beta, 3 ),
                sign_stab = round( sign_stability, 3 ), sig05 = round( share_sig05, 3 ) ) ] )
cat( "\n== Stability (exp / excl / cs) ==\n" )
print( stab[ stratum == "exp/excl/cs" & term %in% c( "pct_nonwhite", "race_hhi", "pf_assets", "turnout", "gov_earn_sl",
                                                    "poverty_rate", "pct_ba" ),
             .( term, specs, median_r = round( median_partial_r, 3 ), iqr_r = round( iqr_std_beta, 3 ),
                sign_stab = round( sign_stability, 3 ), sig05 = round( share_sig05, 3 ) ) ] )
cat( "\n== Dominant fork per covariate (count / incl / cs) ==\n" )
ii <- infl[ stratum == "count/incl/cs" ]
ii[ , top_fork := FORK_DIMS[ apply( as.matrix( .SD ), 1, which.max ) ], .SDcols = FORK_DIMS ]
ii[ , top_eta2 := apply( as.matrix( .SD ), 1, max, na.rm = TRUE ), .SDcols = FORK_DIMS ]
print( ii[ , .( term, top_fork, top_eta2 = round( top_eta2, 2 ), dv_form = round( dv_form, 2 ),
                pred_form = round( pred_form, 2 ) ) ][ order( -top_eta2 ) ] )
cat( "\n== Meta-hypothesis: Spearman(R2 on log P, DV-form sensitivity) ==\n" )
print( meta_test )
