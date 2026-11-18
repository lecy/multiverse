## 86_sensitivity_tcja_size.R
## Two open checks from the outline: are they forks, or sensitivity notes?
##
## Part A: the 2018 tax law (TCJA) in the multiverse. Contribution measures enter only as
## philanthropy alternatives (philanthropy:contrib_total / contrib_per_return /
## contrib_share_agi: 54,252 of 498,432 specs). Every cross-section (2019, 2022) is after
## the law; pooled (2015-22) and within (2010-22) specs span it. Each contribution spec is
## re-fit on a pre-2018 window (cs: 2017 instead of 2022; pooled: 2015-17; within: 2010-17),
## and so is its default-model twin (foundation assets), which separates the effect of the
## law from the effect of a shorter window:
##   law effect on a term = (pre - full) in the contribution spec  -  (pre - full) in the twin
## Reported for the contribution term itself and for the benchmark covariates.
##
## Part B: city size. 83_sample_exclusions.R found that keeping only metros >= 250k turns
## poverty positive for filers. Here the same 1,470 default-model specs are fit on metros
## < 250k and >= 250k separately (metro unit only; about 194 metros each): does each
## benchmark differ by size, and is the difference larger than sampling noise?
##
## Partial r with df counted in places (pooled / years, within / years), as in 58 part A.
## Outputs D:/MULTIVERSE/data/sensitivity/. Usage: Rscript R/86_sensitivity_tcja_size.R [A] [B] [workers]

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
suppressPackageStartupMessages( { library( parallel ); library( arrow ) } )
args <- commandArgs( TRUE )
PARTS <- intersect( args, c( "A", "B" ) ); if ( !length( PARTS ) ) PARTS <- c( "A", "B" )
WORKERS <- suppressWarnings( as.integer( args[ grepl( "^[0-9]+$", args ) ] ) ); if ( !length( WORKERS ) ) WORKERS <- 12L
## with MV_MODEL=lean the grid is v11 (13-covariate default) and outputs go to sensitivity_v11/
OD <- file.path( BUILD, "data", if ( MV_MODEL == "lean" ) "sensitivity_v11" else "sensitivity" ); dir.create( OD, showWarnings = FALSE )
BENCH <- if ( MV_MODEL == "lean" ) unlist( lapply( BLOCKS, `[[`, "default" ), use.names = FALSE ) else c( "pct_ba", "pf_assets", "pct_nonwhite", "turnout", "poverty_rate" )
CONTRIB <- c( "contrib_total", "contrib_per_return", "contrib_share_agi" )
KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit", "est", "cs_year", "outlier" )

grid <- build_grid( "principled" )[ is.na( dup_of ) ]
panel <- load_panel()

## parallel fit of a job table: spec_id, sample (a label), cs_year override, max_year, keep (cbsa subset name)
fit_jobs <- function( jobs, keepsets = list() ) {
  run_job <- function( i ) {
    j <- jobs[ i ]; s <- grid[ spec_id == j$spec_id ]
    if ( !is.na( j$cs_year_new ) ) s[ , cs_year := j$cs_year_new ]
    p <- panel[ year <= j$max_year ]
    if ( !is.na( j$keep ) ) p <- p[ cbsa %in% keepsets[[ j$keep ]] ]
    r <- tryCatch( run_spec( s, p ), error = function( e ) NULL )
    if ( is.null( r ) ) return( NULL )
    r[ , `:=`( job = j$job, variant = j$variant, k = .N ) ]; r[] }
  environment( run_job ) <- globalenv()                 # workers look up jobs/keepsets/grid/panel globally
  assign( "jobs", jobs, envir = globalenv() ); assign( "keepsets", keepsets, envir = globalenv() )
  assign( "run_job", run_job, envir = globalenv() )
  cl <- makePSOCKcluster( WORKERS ); on.exit( try( stopCluster( cl ), silent = TRUE ), add = TRUE )
  invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1" )
    setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
    source( "R/00_setup.R" ); source( "R/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
  clusterExport( cl, c( "panel", "grid", "jobs", "keepsets", "run_job" ), envir = globalenv() )
  res <- rbindlist( parLapplyLB( cl, seq_len( nrow( jobs ) ), run_job, chunk.size = 50 ), fill = TRUE )
  stopCluster( cl ); res
}
add_pr <- function( r, years ) {
  r[ , n_units := n / years ]
  r[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]; r[] }

## ---- Part A: TCJA window -------------------------------------------------------------------
if ( "A" %in% PARTS ) {
  cs <- grid[ op %like% "^philanthropy:contrib" & drop == "none" & !( ident == "pooled" & est == "state_re" ) &
              !( ident == "cs" & cs_year != 2022L ) ]
  cs[ , contrib := sub( "philanthropy:", "", op ) ]
  tw <- merge( cs[ , c( "spec_id", "contrib", KEYS ), with = FALSE ],
               grid[ op == "default" & drop == "none", c( "spec_id", KEYS ), with = FALSE ], by = KEYS, suffixes = c( "", "_twin" ) )
  log_msg( "A: ", nrow( cs ), " contribution specs; with default twins: ", nrow( tw ) )
  ids <- unique( c( tw$spec_id, tw$spec_id_twin ) )
  sp <- grid[ spec_id %in% ids ]
  jobs <- rbind( sp[ , .( spec_id, variant = "full", cs_year_new = NA_integer_, max_year = 9999L, keep = NA_character_ ) ],
                 sp[ , .( spec_id, variant = "pre2018", cs_year_new = fifelse( ident == "cs", 2017L, NA_integer_ ),
                          max_year = 2017L, keep = NA_character_ ) ] )
  jobs[ , job := .I ]
  log_msg( "A: fits to run: ", nrow( jobs ) )
  t0 <- proc.time(); r <- fit_jobs( jobs ); log_msg( "A: fits in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min" )
  r <- merge( r, grid[ , .( spec_id, ident ) ], by = "spec_id" )
  r[ , years := fcase( ident == "cs", 1, ident == "pooled" & variant == "full", 8, ident == "pooled", 3,
                       ident == "within" & variant == "full", 13, ident == "within", 8 ) ]
  r <- add_pr( r, r$years )
  write_parquet( r, file.path( OD, "a_coefs.parquet" ) )
  w <- dcast( r[ term != "logP" ], spec_id + term ~ variant, value.var = "pr" )
  w[ , d := pre2018 - full ]
  ## contribution term: its own change, and the twin's foundation-asset change as the window benchmark
  a <- merge( tw, w[ term %in% CONTRIB, .( spec_id, cterm = term, c_full = full, c_pre = pre2018, c_d = d ) ], by = "spec_id" )
  a <- merge( a, w[ term == "pf_assets", .( spec_id_twin = spec_id, pf_d = d, pf_full = full ) ], by = "spec_id_twin" )
  a1 <- a[ , .( specs = .N, full = median( c_full ), pre2018 = median( c_pre ), median_change = median( c_d ),
                window_benchmark = median( pf_d ), law_effect = median( c_d - pf_d ),
                sign_flips = mean( sign( c_full ) != sign( c_pre ) & pmax( abs( c_full ), abs( c_pre ) ) > .05 ),
                sig_full = NA_real_, cor = cor( c_full, c_pre ) ), keyby = .( ident, construct, cterm ) ]
  ## significance shares
  sg <- dcast( r[ term %in% CONTRIB ], spec_id + term ~ variant, value.var = "p" )
  sg <- sg[ , .( sig_full = mean( full < .05, na.rm = TRUE ), sig_pre = mean( pre2018 < .05, na.rm = TRUE ) ),
            keyby = .( spec_id %in% cs[ construct == "count", spec_id ], term ) ]
  ## benchmarks inside contribution specs vs inside their twins
  b <- merge( tw, w[ term %in% BENCH, .( spec_id, term, b_d = d, b_full = full ) ], by = "spec_id" )
  b <- merge( b, w[ term %in% BENCH, .( spec_id_twin = spec_id, term, t_d = d ) ], by = c( "spec_id_twin", "term" ) )
  b1 <- b[ , .( pairs = .N, median_change_contrib_specs = median( b_d ), median_change_twins = median( t_d ),
                law_effect = median( b_d - t_d ), median_abs_law_effect = median( abs( b_d - t_d ) ) ), keyby = .( ident, construct, term ) ]
  fwrite( a1, file.path( OD, "a_contrib_terms.csv" ) ); fwrite( b1, file.path( OD, "a_benchmarks.csv" ) ); fwrite( sg, file.path( OD, "a_significance.csv" ) )
  r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
  cat( "\n== A: contribution terms, full window vs pre-2018 ==\n" ); print( r3( a1 ), nrows = 100 )
  cat( "\n== A: significance (rows: counts TRUE/FALSE) ==\n" ); print( r3( sg ) )
  cat( "\n== A: benchmarks in contribution specs, net of the window effect in twins ==\n" ); print( r3( b1 ), nrows = 100 )
}

## ---- Part B: size split ----------------------------------------------------------------------
if ( "B" %in% PARTS ) {
  OUTS <- c( "n_filer_c3", "n_bmf_c3pc", "exp_c3_nohu" )
  sp <- grid[ drop == "none" & op == "default" & outlier == "none" & outcome %in% OUTS & cs_year == 2022L & unit == "metro" &
              ident != "within" & !( ident == "pooled" & est == "state_re" ) ]
  p22 <- panel[ year == 2022 & cbsa_type == "metro" ]
  keepsets <- list( lt250k = p22[ population < 2.5e5, cbsa ], ge250k = p22[ population >= 2.5e5, cbsa ], all = p22$cbsa )
  log_msg( "B: ", nrow( sp ), " specs; metros <250k: ", length( keepsets$lt250k ), ", >=250k: ", length( keepsets$ge250k ) )
  jobs <- rbindlist( lapply( names( keepsets ), function( k ) sp[ , .( spec_id, variant = k, cs_year_new = NA_integer_, max_year = 9999L, keep = k ) ] ) )
  jobs[ , job := .I ]
  r <- fit_jobs( jobs, keepsets )
  r <- merge( r, grid[ , .( spec_id, ident, outcome, construct ) ], by = "spec_id" )
  r <- add_pr( r, fifelse( r$ident == "pooled", 8, 1 ) )
  r[ , se_pr := ( 1 - pr^2 ) / sqrt( pmax( n_units - k - 1, 1 ) ) ]       # approximate SE of a partial r
  write_parquet( r, file.path( OD, "b_coefs.parquet" ) )
  w <- dcast( r[ term %in% BENCH ], spec_id + outcome + ident + term ~ variant, value.var = c( "pr", "se_pr" ) )
  w[ , `:=`( diff = pr_ge250k - pr_lt250k, z = ( pr_ge250k - pr_lt250k ) / sqrt( se_pr_ge250k^2 + se_pr_lt250k^2 ) ) ]
  b1 <- w[ , .( specs = .N, all = median( pr_all ), small = median( pr_lt250k ), large = median( pr_ge250k ),
                diff = median( diff ), share_abs_z_gt2 = mean( abs( z ) > 2 ), sign_differs = mean( sign( pr_lt250k ) != sign( pr_ge250k ) ) ),
           keyby = .( outcome, ident, term ) ]
  fwrite( b1, file.path( OD, "b_size_split.csv" ) )
  r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
  cat( "\n== B: benchmarks in metros < 250k vs >= 250k ==\n" ); print( r3( b1 ), nrows = 100 )
}
log_msg( "done" )
