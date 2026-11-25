## 40_multiverse.R
## Multiverse configuration, specification grid, and model engine, for the scripts.
## Sourced after 00_setup.R by 44_run_universe.R and the research-note scripts.
##
## The engine lives in the package (npdensity: R/mv_config.R, R/mv_engine.R):
##   mv_spec_config( model )            covariate catalog, blocks, outcomes, forks
##   mv_grid( universe, cfg = )         the specification grid
##   mv_run_spec( s, panel, cfg )       one specification
##   mv_run_universe( grid, panel, dir, cfg, batch, workers )   resumable batches
## This file keeps the names the scripts use. It copies the configuration into the
## globals COVS, BLOCKS, OUTCOMES, FORKS, IDENT, TIME_INVARIANT_VARS and
## TIME_INVARIANT_BLOCKS, and the functions below read those globals at call time.
## A script that changes a global (41_composites.R, 89_need_socap_diversity.R add
## alternative measures) therefore changes the grid and the fits, as before.
##
## Model: Sys.setenv( MV_MODEL = "lean" ) before sourcing gives the 13-covariate model of
## run v11; unset (or "default") gives the block set of runs v1-v10.

suppressPackageStartupMessages( { library( data.table ); library( fixest ); library( lme4 ) } )
setFixest_notes( FALSE ); setFixest_nthreads( 1 )

MV_MODEL <- Sys.getenv( "MV_MODEL", "default" )
local( {
  cfg <- mv_spec_config( MV_MODEL )
  assign( "COVS", cfg$covs, globalenv() ); assign( "BLOCKS", cfg$blocks, globalenv() )
  assign( "OUTCOMES", cfg$outcomes, globalenv() ); assign( "FORKS", cfg$forks, globalenv() )
  assign( "IDENT", cfg$ident, globalenv() )
  assign( "TIME_INVARIANT_VARS", cfg$time_invariant_vars, globalenv() )
  assign( "TIME_INVARIANT_BLOCKS", cfg$time_invariant_blocks, globalenv() )
} )
BLOCKS_LEAN <- mv_spec_config( "lean" )$blocks

## the configuration as the globals hold it now
mv_cfg <- function() list( model = MV_MODEL, covs = COVS, blocks = BLOCKS, outcomes = OUTCOMES,
                           forks = FORKS, ident = IDENT, time_invariant_vars = TIME_INVARIANT_VARS,
                           time_invariant_blocks = TIME_INVARIANT_BLOCKS )

ADDON_BLOCKS <- function() mv_addon_blocks( mv_cfg() )
op_variants  <- function() mv_op_variants( mv_cfg() )
block_sets   <- function() mv_block_sets( mv_cfg() )
build_grid   <- function( universe = c( "principled", "kitchen" ) ) mv_grid( match.arg( universe ), cfg = mv_cfg() )
covars_for   <- function( s ) mv_covars( s, mv_cfg() )
run_spec     <- function( s, panel ) mv_run_spec( s, panel, mv_cfg() )
load_panel   <- function( path = file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) mv_load_panel( path )

## winsor(), resid_logP() (the SAMI first stage), slog(), flog() and DIVISION are in the
## package too: ?npdensity::transforms, ?npdensity::mv_spec_config.
