## 10b_ntee_1023ez_check.R
## Diagnostic (2026-11-17): are EINs with missing NTEE codes (e-file active c3 filers 2020-24;
## Dec 2023/2024 monthly-BMF public charities) in the IRS Form 1023-EZ approval files, and do
## the self-reported 1023-EZ codes agree with the legacy-BMF backfill? Not part of the build.
## Input: D:/MULTIVERSE/raw/irs_1023ez/ (NCCS mirror raw/1023-ez-approvals/f1023ez-approvals-baseline-2014-2023.zip;
## the IRS 2024 link returned 404 on 2026-11-17). Output: data/diagnostics/ntee_missing_vs_1023ez.csv
source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
bad <- bad_ntee                       # shared rule, R/lib/00_setup.R
valid_code <- function( x ) grepl( "^[A-Z][0-9]{2}", x ) & !bad( x )

## ---- 1023-EZ approvals ------------------------------------------------------
dir <- file.path( BUILD, "raw", "irs_1023ez", "baseline" )
ez <- rbindlist( lapply( list.files( dir, "\\.csv$", full.names = TRUE ), function( f ) {
  h <- names( fread( f, nrows = 0 ) )
  k <- c( h[ tolower( h ) == "ein" ], "Nteecode", "Signaturedate", "State" )
  x <- fread( f, select = k, colClasses = "character", showProgress = FALSE )
  setnames( x, c( "ein", "ntee_raw", "sigdate", "state" ) )
  x[ , file_year := as.integer( sub( ".*(20[0-9]{2}).*", "\\1", basename( f ) ) ) ]
} ) )
ez[ , ein := pad( gsub( "[^0-9]", "", ein ), 9 ) ]
ez[ , ntee_ez := toupper( substr( gsub( "[^A-Za-z0-9]", "", ntee_raw ), 1, 3 ) ) ]
cat( "1023-EZ approvals:", nrow( ez ), "rows,", uniqueN( ez$ein ), "EINs; valid NTEE:",
     round( 100 * mean( valid_code( ez$ntee_ez ) ), 1 ), "%\n" )
print( ez[ , .( rows = .N, valid_ntee = round( 100 * mean( valid_code( ntee_ez ) ), 1 ) ), keyby = file_year ] )
cat( "examples of raw NTEE values:\n" ); print( head( ez[ , .N, by = ntee_raw ][ order( -N ) ], 12 ) )
ez <- unique( ez[ order( ein, -file_year ) ], by = "ein" )        # latest approval per EIN
ezv <- ez[ valid_code( ntee_ez ) ]

## ---- legacy-snapshot backfill source (as in 06 / 02b) ------------------------
best <- read_ntee_best()[ , .( ein = EIN, ntee_bmf = ntee_best ) ]   # lookup written by 02

summ <- function( d, label ) {
  d <- merge( d, best, by = "ein", all.x = TRUE )
  d <- merge( d, ezv[ , .( ein, ntee_ez, ez_year = file_year ) ], by = "ein", all.x = TRUE )
  d[ , `:=`( in_bmf = !is.na( ntee_bmf ), in_ez = !is.na( ntee_ez ) ) ]
  out <- d[ , .( set = label, missing = .N,
                 bmf_recovers = sum( in_bmf ), ez_has_code = sum( in_ez ),
                 ez_only = sum( in_ez & !in_bmf ), still_missing_after_both = sum( !in_ez & !in_bmf ),
                 both = sum( in_ez & in_bmf ),
                 agree_3char = sum( in_ez & in_bmf & ntee_ez == ntee_bmf ),
                 agree_major = sum( in_ez & in_bmf & substr( ntee_ez, 1, 1 ) == substr( ntee_bmf, 1, 1 ) ) ),
            by = year ]
  list( summary = out, detail = d )
}

## ---- A. e-file active 501(c)(3) filers, 2020-2024 ---------------------------
ef <- as.data.table( read_parquet( file.path( OUT$npo, "orgyear_efile.parquet" ) ) )[ year %in% 2020:2024 & subsec_group == "c3" ]
pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )[ , .( year, floor_nom ) ]
ef <- merge( ef, pce, by = "year" )
ef <- ef[ pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
A <- summ( ef[ bad( ntee3 ), .( ein, year ) ], "e-file active c3 filers" )

## ---- B. monthly BMF Dec 2023 / Dec 2024 public charities ---------------------
mb <- rbindlist( lapply( 2023:2024, function( y ) {
  m <- fread( file.path( BUILD, "raw", "bmf_monthly", sprintf( "bmf_%d_12_processed.csv", y ) ),
              select = c( "ein", "subsection_code", "foundation_code", "ntee_code_clean", "org_addr_state", "ruling_date_ym_str" ),
              colClasses = "character" )
  m <- unique( m, by = "ein" )[ org_addr_state %in% c( state.abb, "DC" ) &
                                 suppressWarnings( as.integer( subsection_code ) ) == 3L &
                                 !suppressWarnings( as.integer( foundation_code ) ) %in% 2:4 ]
  m[ , .( ein = pad( gsub( "[^0-9]", "", ein ), 9 ), year = y,
          ntee = fifelse( ntee_code_clean %in% c( "UNDEFINED", "INVALID" ), "", ntee_code_clean ),
          ryear = suppressWarnings( as.integer( substr( ruling_date_ym_str, 1, 4 ) ) ) ) ]
} ) )
B <- summ( mb[ bad( ntee ), .( ein, year, ryear ) ], "monthly BMF public charities" )

res <- rbind( A$summary, B$summary )
res[ , `:=`( pct_ez = round( 100 * ez_has_code / missing, 1 ), pct_ez_only = round( 100 * ez_only / missing, 1 ),
             pct_after_both = round( 100 * still_missing_after_both / missing, 1 ),
             agree3_pct = round( 100 * agree_3char / both, 1 ), agree_major_pct = round( 100 * agree_major / both, 1 ) ) ]
setorder( res, set, year )
print( res )
fwrite( res, file.path( OUT$diag, "ntee_missing_vs_1023ez.csv" ) )

## residual (after legacy BMF backfill) by ruling year: where does 1023-EZ help?
r <- B$detail[ year == 2024 & !in_bmf ]
print( r[ , .( still_missing_after_bmf = .N, ez_has_code = sum( in_ez ), pct = round( 100 * mean( in_ez ), 1 ) ),
          keyby = .( ruling = fcase( is.na( ryear ), "unknown", ryear < 2014, "before 2014", ryear > 2023, "2024 (no EZ file)", default = as.character( ryear ) ) ) ] )
## NTEE major groups the 1023-EZ codes assign to the residual
print( B$detail[ year == 2024 & !in_bmf & in_ez, .N, keyby = .( major = substr( ntee_ez, 1, 1 ) ) ][ , pct := round( 100 * N / sum( N ), 1 ) ][] )
cat( "residual human services (I-P) per 1023-EZ, Dec 2024:",
     B$detail[ year == 2024 & !in_bmf & in_ez & substr( ntee_ez, 1, 1 ) %in% LETTERS[ 9:16 ], .N ], "\n" )
