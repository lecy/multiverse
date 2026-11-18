## 103_ident_by_population.R
## D3 Evidence 1 split by outcome population (filers, grassroots, registered stock, spending) instead of
## pooling filers and registrations into one "counts" group. Default-model specs of run v11
## (op default, drop none; pooled state RE excluded, as in 97). Grassroots (registered public charities
## that do not file above the floor = n_bmf_c3pc - n_filer_c3) are not a v11 outcome, so the
## registered-stock specs are refit with grassroots as the outcome (same forks).
## Partial r with df in metros (pooled / 8, within / 13). Per term x population x identification:
## median partial r, share of specs significant (p < .05) with the median's sign; within - cross-section.
## Outputs D:/MULTIVERSE/data/outline/v11/mv/ident_by_population*.csv. Usage: Rscript R/notes/103_ident_by_population.R [workers]

Sys.setenv( MV_MODEL = "lean" )
source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
suppressPackageStartupMessages( { library( arrow ); library( parallel ) } )
args <- commandArgs( TRUE ); WORKERS <- if ( length( args ) ) as.integer( args[ 1 ] ) else 4L
RUN <- file.path( OUT$spec, "run_principled_v11" )
OD <- file.path( BUILD, "data", "outline", "v11", "mv" )
POP <- c( n_filer_c3 = "Filers", n_grass = "Grassroots", n_bmf_c3pc = "Registered stock", exp_c3_nohu = "Spending excl. H&U" )

specs <- as.data.table( read_parquet( file.path( RUN, "specs.parquet" ) ) )[ status == "ok" & op == "default" & drop == "none" &
           outcome %in% names( POP ) & !( ident == "pooled" & est == "state_re" ) ]
co <- as.data.table( read_parquet( file.path( RUN, "coefs.parquet" ) ) )[ spec_id %in% specs$spec_id ]
log_msg( "default specs: ", nrow( specs ), " (", paste( specs[ , .N, by = outcome ][ , paste( outcome, N ) ], collapse = ", " ), ")" )

## grassroots: refit the registered-stock specs
gs <- copy( specs[ outcome == "n_bmf_c3pc" ] )[ , outcome := "n_grass" ]
panel <- load_panel(); panel[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
run_one <- function( i ) { r <- tryCatch( run_spec( gs[ i ], panel ), error = function( e ) NULL ); r }
environment( run_one ) <- globalenv(); assign( "gs", gs, envir = globalenv() ); assign( "panel", panel, envir = globalenv() )
assign( "run_one", run_one, envir = globalenv() )
cl <- makePSOCKcluster( WORKERS )
invisible( clusterEvalQ( cl, { Sys.setenv( OMP_NUM_THREADS = "1", MV_MODEL = "lean" )
  setwd( Sys.getenv( "MV_PROJECT_ROOT" ) )
  source( "R/lib/00_setup.R" ); source( "R/lib/40_multiverse.R" ); data.table::setDTthreads( 1L ); NULL } ) )
clusterExport( cl, c( "gs", "panel", "run_one" ) )
t0 <- proc.time(); gco <- rbindlist( parLapplyLB( cl, seq_len( nrow( gs ) ), run_one, chunk.size = 20 ), fill = TRUE ); stopCluster( cl )
log_msg( "grassroots fits: ", uniqueN( gco$spec_id ), " in ", round( ( proc.time() - t0 )[[ 3 ]] / 60, 1 ), " min" )
gco[ , spec_id := -spec_id ]                                  # keep apart from the registered-stock twins
gs[ , spec_id := -spec_id ]

co <- rbind( co, gco, fill = TRUE ); specs <- rbind( specs, gs, fill = TRUE )
co <- merge( co, co[ , .( k = .N ), by = spec_id ], by = "spec_id" )
co <- merge( co, specs[ , .( spec_id, ident, outcome ) ], by = "spec_id" )
co[ , n_units := n / fcase( ident == "pooled", 8, ident == "within", 13, default = 1 ) ]
co[ , pr := stat / sqrt( stat^2 + pmax( n_units - k - 1, 1 ) ) ]
co <- co[ term != "logP" ]
co[ term == "rel_hhi", `:=`( term = "rel_frac", pr = -pr ) ]

s <- co[ , { m <- median( pr ); .( specs = .N, median_pr = m, sig_same = mean( p < .05 & sign( pr ) == sign( m ) ),
                                   sig_any = mean( p < .05 ), same_sign = mean( sign( pr ) == sign( m ) ) ) },
         keyby = .( term, outcome, ident ) ]
w <- dcast( s, term + outcome ~ ident, value.var = c( "median_pr", "sig_same" ) )
w[ , `:=`( within_minus_cs = median_pr_within - median_pr_cs, within_minus_pooled = median_pr_within - median_pr_pooled ) ]
w[ , population := POP[ outcome ] ]
fwrite( s, file.path( OD, "ident_by_population_long.csv" ) ); fwrite( w, file.path( OD, "ident_by_population.csv" ) )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]
print( r3( w[ order( outcome, term ) ] ), nrows = 100 )
log_msg( "done" )
