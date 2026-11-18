## 10_source_coverage.R
## Coverage tables for DATA-MANIFEST.md: filer universe by year, return type
## (990 / 990-EZ / 990-PF) by year, and the share of each year's records that
## were e-filed (matched to the NCCS e-file release) vs paper-only.
## Run after 06_npo_assemble.R. Read-only with respect to the database.
##
## Extra inputs (downloaded once, cached on D:):
##   - CORE PZ 2000-2010 return type (F9_00_RETURN_TYPE is not kept by 03):
##     raw/nccs-core-hrmn/core_pz_<year>_formtype.parquet
##   - e-file headers TY2009-2019 (efile_v3_1 / efilepf_v3_1, same layout as
##     panel990's cache): raw/panel990/<year>/F9-P00-T00-HEADER-<year>.parquet,
##     raw/panel990/990PF/<year>/F9-P00-T00-HEADER-<year>.parquet
##
## E-file match: EIN x tax-period-end month (YYYYMM), so fiscal-year filers
## match regardless of how each source assigns the tax year.
##
## Outputs (data/diagnostics/):
##   manifest_universe_by_year.csv   national totals of the spliced outcome series
##   manifest_forms_by_year.csv      records in the spliced source by return type, all / active
##   manifest_efile_share_by_year.csv share of records with an e-filed return, by type

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
options( timeout = 7200 )

ym <- function( x ) substr( gsub( "[^0-9]", "", as.character( x ) ), 1, 6 )
ein9 <- function( x ) pad( gsub( "[^0-9]", "", x ), 9 )

## ---- 1. CORE PZ return type, 2000-2010 ---------------------------------------
CORE <- "https://nccsdata.s3.us-east-1.amazonaws.com/harmonized/core/501c3-pz"
core_ft <- rbindlist( lapply( 2000:2010, function( y ) {
  out <- file.path( RAW$core_hrmn, sprintf( "core_pz_%d_formtype.parquet", y ) )
  if ( !file.exists( out ) ) {
    tmp <- tmpfile( ".csv" ); on.exit( unlink( tmp ), add = TRUE )
    log_msg( "CORE PZ ", y, ": downloading for return type" )
    utils::download.file( sprintf( "%s/CORE-%d-501C3-CHARITIES-PZ-HRMN.csv", CORE, y ), tmp, mode = "wb", quiet = TRUE )
    ## CORE PZ files before TY2008 carry no return-type field (990 and EZ pooled)
    h <- names( fread( tmp, nrows = 0 ) )
    x <- fread( tmp, select = intersect( c( "EIN2", "TAX_YEAR", "F9_00_TAX_PERIOD_END_DATE", "F9_00_RETURN_TYPE" ), h ),
                colClasses = "character", showProgress = FALSE )
    if ( !"F9_00_RETURN_TYPE" %in% names( x ) ) x[ , F9_00_RETURN_TYPE := NA_character_ ]
    unlink( tmp )
    write_parquet( x, out )
  }
  x <- as.data.table( read_parquet( out ) )
  x[ , .( ein = ein9( EIN2 ), taxpd = ym( F9_00_TAX_PERIOD_END_DATE ), rtype = toupper( F9_00_RETURN_TYPE ) ) ]
} ) )
core_ft <- unique( core_ft, by = c( "ein", "taxpd" ) )
log_msg( "CORE return types: ", paste( capture.output( print( core_ft[ , .N, by = rtype ] ) ), collapse = " | " ) )

## ---- 2. e-file headers, TY2009-2024 ------------------------------------------
EF <- "https://nccs-efile.s3.us-east-1.amazonaws.com/public"
hdr <- function( y, pf = FALSE ) {
  dir <- if ( pf ) file.path( RAW$panel990, "990PF", y ) else file.path( RAW$panel990, y )
  dir.create( dir, recursive = TRUE, showWarnings = FALSE )
  f <- file.path( dir, sprintf( "F9-P00-T00-HEADER-%d.parquet", y ) )
  if ( !file.exists( f ) ) {
    log_msg( "e-file header ", if ( pf ) "PF " else "", y )
    utils::download.file( sprintf( "%s/%s/F9-P00-T00-HEADER-%d.parquet", EF,
                                   if ( pf ) "efilepf_v3_1" else "efile_v3_1", y ), f, mode = "wb", quiet = TRUE )
  }
  x <- as.data.table( read_parquet( f, col_select = c( "EIN2", "TAX_YEAR", "TAX_PERIOD_END_DATE", "RETURN_TYPE" ) ) )
  x[ , .( ein = ein9( EIN2 ), taxpd = ym( TAX_PERIOD_END_DATE ), ef_type = toupper( RETURN_TYPE ), ef_ty = as.integer( TAX_YEAR ) ) ]
}
ef  <- unique( rbindlist( lapply( 2009:2024, hdr ) ), by = c( "ein", "taxpd" ) )
efp <- unique( rbindlist( lapply( 2009:2024, hdr, pf = TRUE ) ), by = c( "ein", "taxpd" ) )
fwrite( rbind( ef[ , .( returns = .N ), keyby = .( tax_year = ef_ty, return_type = ef_type ) ][ , release := "efile_v3_1" ],
               efp[ , .( returns = .N ), keyby = .( tax_year = ef_ty, return_type = ef_type ) ][ , release := "efilepf_v3_1" ] ),
        file.path( OUT$diag, "manifest_efile_release_counts.csv" ) )

## ---- 3. records of the spliced source, by return type ------------------------
pce <- fread( file.path( OUT$diag, "pce_deflator.csv" ) )[ , .( year, floor_nom ) ]
rd <- function( f, yrs ) as.data.table( read_parquet( file.path( OUT$npo, f ) ) )[ year %in% yrs ]
core <- rd( "orgyear_core.parquet", 2000:2015 )
soi  <- rd( "orgyear_soi.parquet", 2011:2019 )
efl  <- rd( "orgyear_efile.parquet", 2020:2024 )
spf  <- rd( "orgyear_soi_pf.parquet", 2019:2020 )
epf  <- rd( "orgyear_efile_pf.parquet", 2021:2024 )

## CORE: public-charity PZ for 2000-2010; foundations (pf) for 2000-2015
cpz <- core[ form %in% c( "pz", "ce" ) & year <= 2010 ]
cpz[ , taxpd := ym( taxpd_end ) ]
cpz[ core_ft, on = .( ein, taxpd ), rtype := i.rtype ]
## CORE codes the return type as an IRS document code: 93 (and a few 90) = Form 990,
## 92 (and a few 9) = 990-EZ. Checked 2026-11-17 against the e-file return type of
## the same EIN-period (TY2009-10): 93 -> 990 in 111,398 of 111,408; 92 -> 990EZ in
## 44,058 of 44,079; code-92 gross receipts track the EZ limits ($1M/$500k/$200k).
cpz[ , rtype := fcase( form == "ce", "990/990EZ",
                       year < 2008, "990/990EZ",
                       rtype %in% c( "93", "90", "990" ), "990",
                       rtype %in% c( "92", "9", "990EZ", "EZ" ), "990EZ",
                       default = "990/990EZ" ) ]
soi[ , rtype := fifelse( form == "EZ", "990EZ", "990" ) ]
efl[ , rtype := toupper( form ) ]
pz <- rbindlist( list(
  cpz[ , .( source, year, subsec_group, ein, taxpd = taxpd, rtype, rev, exp ) ],
  soi[ , .( source, year, subsec_group, ein, taxpd = ym( taxpd_end ), rtype, rev, exp ) ],
  efl[ , .( source, year, subsec_group, ein, taxpd = ym( taxpd_end ), rtype, rev, exp ) ] ) )
pfr <- rbindlist( list(
  core[ form == "pf" & year <= 2015, .( source, year, ein, taxpd = ym( taxpd_end ), rtype = "990PF", rev, exp ) ],
  spf[ , .( source, year, ein, taxpd = ym( taxpd_end ), rtype = "990PF", rev, exp ) ],
  epf[ , .( source, year, ein, taxpd = ym( taxpd_end ), rtype = "990PF", rev, exp ) ] ) )
pfr[ , subsec_group := "pf" ]
rec <- rbind( pz, pfr )
rec <- merge( rec, pce, by = "year" )
rec[ , active := pmax( rev, exp, na.rm = TRUE ) >= floor_nom ]
rec[ is.na( active ), active := FALSE ]

## e-file match
rec[ , efiled := FALSE ]
rec[ subsec_group != "pf", efiled := paste( ein, taxpd ) %chin% ef[ , paste( ein, taxpd ) ] ]
rec[ subsec_group == "pf", efiled := paste( ein, taxpd ) %chin% efp[ , paste( ein, taxpd ) ] ]
rec[ source == "EFILE", efiled := TRUE ]

forms <- rec[ , .( records = .N, active = sum( active ) ), keyby = .( year, source, subsec_group, rtype ) ]
fwrite( forms, file.path( OUT$diag, "manifest_forms_by_year.csv" ) )

share <- rec[ , .( records = .N, efiled = sum( efiled ), share_efiled = round( mean( efiled ), 4 ),
                   active = sum( active ), active_efiled = sum( active & efiled ),
                   share_active_efiled = round( sum( active & efiled ) / sum( active ), 4 ),
                   exp_share_efiled = round( sum( pmax( exp, 0 )[ efiled & active ], na.rm = TRUE ) /
                                               sum( pmax( exp, 0 )[ active ], na.rm = TRUE ), 4 ) ),
              keyby = .( year, source, group = fifelse( subsec_group == "pf", "990PF",
                                                  fifelse( subsec_group == "c3", paste0( "c3 ", rtype ), paste0( "other ", rtype ) ) ) ) ]
fwrite( share, file.path( OUT$diag, "manifest_efile_share_by_year.csv" ) )

## ---- 4. national totals of the spliced outcome series ------------------------
cy <- fread( file.path( OUT$npo, "npo_county_year.csv" ) )
num <- c( "n_bmf_c3pc", "n_bmf_c3pc_snap", "n_bmf_c3pc_nohu", "n_filer_c3", "n_filer_c3_nohu", "n_filer_c3_hs",
          "n_filer_c3_nofloor", "exp_c3", "exp_c3_nohu", "exp_c3_net", "n_pf", "pf_assets", "n_entry_c3pc" )
u <- cy[ , c( lapply( .SD, function( x ) sum( x, na.rm = TRUE ) ),
              .( src_990 = first( src_990 ), src_pf = first( src_pf ) ) ), by = year, .SDcols = num ]
u[ , n_grass := n_bmf_c3pc - n_filer_c3 ]
u[ , share_filer := round( n_filer_c3 / n_bmf_c3pc, 4 ) ]
u[ , hu_share_exp := round( 1 - exp_c3_nohu / exp_c3, 4 ) ]
u[ year < 2012, exp_c3_net := NA ]
u[ year < 2000 | year > 2024, n_entry_c3pc := NA ]
for ( v in c( "exp_c3", "exp_c3_nohu", "exp_c3_net", "pf_assets" ) ) set( u, j = v, value = round( u[[ v ]] / 1e9, 1 ) )
setorder( u, year )
fwrite( u, file.path( OUT$diag, "manifest_universe_by_year.csv" ) )

print( u[ , .( year, src_990, src_pf, n_bmf_c3pc, n_filer_c3, n_grass, share_filer, n_filer_c3_nohu,
               exp_c3_bn = exp_c3, exp_c3_nohu_bn = exp_c3_nohu, hu_share_exp, n_pf ) ] )
print( dcast( forms[ subsec_group != "other" ], year + source ~ rtype, value.var = "active", fill = 0 ) )
print( dcast( share[ grepl( "^c3|990PF", group ) ], year + source ~ group, value.var = "share_active_efiled" ) )
log_msg( "done" )
