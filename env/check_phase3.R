## env/check_phase3.R
## Check for the Phase 3 folder move (REPRODUCIBILITY-PLAN.md): every R script
## parses, every script path written in the code or the docs exists, pipeline.csv
## points at existing scripts, and the shared setup and engine still load.
## Run from the project folder: Rscript env/check_phase3.R  (exits 1 on a failure)

fail <- character( 0 )
tracked <- system2( "git", c( "ls-files" ), stdout = TRUE )
tracked <- tracked[ file.exists( tracked ) ]

## 1. every R script parses
rs <- c( list.files( "R", "\\.R$", recursive = TRUE, full.names = TRUE ), list.files( "env", "\\.R$", full.names = TRUE ), "run.R" )
for ( f in rs ) tryCatch( parse( f ), error = function( e ) fail <<- c( fail, paste( "parse:", f, conditionMessage( e ) ) ) )
cat( "parsed", length( rs ), "R scripts\n" )

## 2. every R/... .R path written in code, docs and slides exists
txt <- tracked[ !grepl( "\\.(png|pdf|docx|jpe?g|svg|parquet|pptx|xlsx|zip)$", tracked ) & tracked != "env/phase3_moves.csv" ]
refs <- unique( unlist( lapply( txt, function( f ) {
  x <- tryCatch( readLines( f, warn = FALSE, encoding = "UTF-8" ), error = function( e ) character( 0 ) )
  m <- regmatches( x, gregexpr( "(?<![A-Za-z0-9_./-])R/[A-Za-z0-9_/]+\\.R\\b", x, perl = TRUE ) )
  unlist( m ) } ) ) )
planned <- c( "R/paper/make_figures.R", "R/paper/make_numbers.R" )   # Phase 6, named in the plan
bad <- setdiff( refs[ !file.exists( refs ) ], planned )
cat( "script paths referenced:", length( refs ), "; missing:", length( bad ), "\n" )
if ( length( bad ) ) fail <- c( fail, paste( "missing script:", bad ) )

## 3. no script left at the top of R/
top <- list.files( "R", "\\.R$" )
if ( length( top ) ) fail <- c( fail, paste( "still in R/:", top ) )

## 4. pipeline.csv scripts exist
p <- read.csv( "pipeline.csv", colClasses = "character" )
if ( !all( file.exists( p$script ) ) ) fail <- c( fail, paste( "pipeline script missing:", p$script[ !file.exists( p$script ) ] ) )

## 5. setup, helpers and engine load
ok <- tryCatch( {
  suppressMessages( { source( "R/lib/00_setup.R" ); source( "R/lib/geo_helpers.R" ); source( "R/lib/40_multiverse.R" ) } )
  exists( "load_panel" ) && exists( "build_grid" )
}, error = function( e ) { fail <<- c( fail, paste( "load:", conditionMessage( e ) ) ); FALSE } )
cat( "setup + engine load:", ok, "\n" )

if ( length( fail ) ) { cat( "\nFAIL\n", paste( fail, collapse = "\n" ), "\n" ); quit( status = 1 ) }
cat( "\nPASS\n" )
