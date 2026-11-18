## 09a_efile_grants.R
## Grant payments and prior-year totals from the NCCS e-file release
## (panel990), tax years 2020-2024, for the revenue/expense and pass-through
## analysis (09_npo_grants.R).
##   990:    Part IX lines 1-3 (grants to domestic orgs & governments, domestic
##           individuals, foreign) and Part I CY/PY revenue and expenses
##   990-EZ: Part I line 10 (grants and similar amounts paid; no org/individual
##           split), via the harmonized F9_01_EXP_GRANT_SIMILAR_CY
## Deduplicated with the same rule as 05_npo_efile.R (panel_deduplicate defaults).
## Output: D:/MULTIVERSE/raw/panel990/efile_grants_<year>.parquet

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( { library( arrow ); library( panel990 ) } )
args <- commandArgs( TRUE )
YRS  <- if ( length( args ) ) as.integer( strsplit( args[1], "," )[[1]] ) else 2020:2024
options( timeout = 7200 )

COLS <- c( "F9_00_RETURN_TYPE", "F9_00_TAX_PERIOD_END_DATE", "F9_00_RETURN_GROUP_X",
           "F9_00_RETURN_AMENDED_X", "F9_00_RETURN_TIME_STAMP", "F9_00_EXEMPT_STAT_501C3_X",
           "F9_01_REV_TOT_CY", "F9_01_REV_TOT_PY", "F9_01_EXP_TOT_CY", "F9_01_EXP_TOT_PY",
           "F9_01_EXP_GRANT_SIMILAR_CY", "F9_01_EXP_GRANT_SIMILAR_PY",
           "F9_09_EXP_GRANT_US_ORG_TOT", "F9_09_EXP_GRANT_US_INDIV_TOT", "F9_09_EXP_GRANT_FRGN_TOT",
           "F9_09_EXP_TOT_TOT" )

for ( y in YRS ) {
  out <- file.path( RAW$panel990, sprintf( "efile_grants_%d.parquet", y ) )
  if ( file.exists( out ) ) next
  log_msg( "e-file grants ", y )
  p <- panelize( tables = c( "P00", "P01", "P09" ), years = y, columns = COLS,
                 bmf = FALSE, backend = "duckdb", cache = "retain", path = RAW$panel990,
                 unique_rows = FALSE, verbose = TRUE )
  d <- as.data.table( panel_data( p ) )
  n0 <- nrow( d )
  log_msg( "  columns: ", paste( names( d ), collapse = " " ) )
  d <- as.data.table( panel_deduplicate( d ) )
  log_msg( sprintf( "  %d: %s filings -> %s EIN-years", y, format( n0, big.mark = "," ),
                    format( nrow( d ), big.mark = "," ) ) )
  write_parquet( d, out )
  rm( d, p ); gc()
}
log_msg( "done" )
