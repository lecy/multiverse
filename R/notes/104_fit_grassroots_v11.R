## 104_fit_grassroots_v11.R
## Grassroots as a multiverse outcome. Run v11 has filers (n_filer_c3) and the registered stock
## (n_bmf_c3pc) as count outcomes; grassroots = registered public charities that do not file
## above the floor = n_bmf_c3pc - n_filer_c3. Every v11 registered-stock spec is refit with grassroots
## as the outcome (same forks, same covariates), so summaries can report filers, grassroots and
## spending separately instead of pooling filers with registrations (97 / 98 with MV_SPLIT=1).
## Spec ids are the twin's id + 1e7; outcome "n_grass", construct "count", hu "incl".
## Output: D:/MULTIVERSE/data/spec/run_principled_v11_grass/{specs,coefs}.parquet.
## Usage: Rscript R/notes/104_fit_grassroots_v11.R [workers]

Sys.setenv( MV_MODEL = "lean" )
source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( arrow ); library( parallel ) } )
args <- commandArgs( TRUE ); WORKERS <- if ( length( args ) ) as.integer( args[ 1 ] ) else 4L
RUN <- file.path( OUT$spec, "run_principled_v11" )
GD <- file.path( OUT$spec, "run_principled_v11_grass" ); dir.create( GD, showWarnings = FALSE )
OFF <- 1e7

gs <- as.data.table( read_parquet( file.path( RUN, "specs.parquet" ) ) )[ status == "ok" & outcome == "n_bmf_c3pc" ]
gs[ , `:=`( twin_id = spec_id, spec_id = spec_id + OFF, outcome = "n_grass" ) ]
log_msg( "registered-stock specs to refit: ", nrow( gs ) )
panel <- load_panel(); panel[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
run_one <- function( i ) {
  r <- tryCatch( run_spec( gs[ i ], panel ), error = function( e ) NULL )
  if ( is.null( r ) || !nrow( r ) ) return( NULL )
  r[ , spec_id := gs$spec_id[ i ] ][] }
environment( run_one ) <- globalenv()
for ( v in c( "gs", "panel", "run_one" ) ) assign( v, get( v ), envir = globalenv() )
cl <- makePSOCKcluster( WORKERS )
invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1", MV_MODEL = "lean" )
  setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
  source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
clusterExport( cl, c( "gs", "panel", "run_one" ) )
t0 <- proc.time()
co <- rbindlist( parLapplyLB( cl, seq_len( nrow( gs ) ), run_one, chunk.size = 50 ), fill = TRUE )
stopCluster( cl )
log_msg( "fits: ", uniqueN( co$spec_id ), " of ", nrow( gs ), " in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min" )
gs[ , status := fifelse( spec_id %in% co$spec_id, "ok", "no fit" ) ]
write_parquet( gs, file.path( GD, "specs.parquet" ) ); write_parquet( co, file.path( GD, "coefs.parquet" ) )
log_msg( "done" )
