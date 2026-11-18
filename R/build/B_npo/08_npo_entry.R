## 08_npo_entry.R
## Entry series (design 2.5 n_entry): new 501(c)(3) registrations by county
## and IRS ruling year.
##   2000-2021  Long BMF Panel 1995-2022 (startup cohorts: every EIN that
##              appears in any BMF vintage 1995-2022, with its FIRST ruling
##              date, so reinstatements after the 2010-11 auto-revocations are
##              not re-counted as entries; location = county in the ruling
##              year)
##   2022-2024  rulings found in the December 2024 monthly BMF (02b); the
##              long panel's 2022 cohort is truncated at 2022-09
## The monthly-BMF cohorts count organizations still registered at the end
## of 2024 (1-3 years of attrition). The 2019-2021 overlap between the two
## sources gives the survivor ratio, reported in
## data/diagnostics/entry_splice.csv; years from the monthly file carry
## entry_src_monthly = 1.
## Known breaks: Form 1023-EZ (July 2014) sharply raises small-org entry.
##
## Output: data/npo/npo_entry_county_year.csv

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )

f <- file.path( DATA_HUB, "BMF", "bmf-long-panel-1995-2022.rds" )
stopifnot( file.exists( f ) )
log_msg( "reading long BMF panel" )
x <- as.data.table( readRDS( f ) )
keep <- c( "ein", "LongBMF19952022PanelOrg", "ruledate1", "ruleyear", "fips_ruleyear", "zip5_ruleyear",
           "city_ruleyear", "state_ruleyear", "subseccd_ruleyear", "fndncd_ruleyear", "nteefinal", "outnccs" )
x <- x[ , intersect( keep, names( x ) ), with = FALSE ]
gc()
log_msg( "panel rows: ", nrow( x ) )

x <- x[ LongBMF19952022PanelOrg == 1 & ( is.na( outnccs ) | outnccs != "OUT" ) &
          as.integer( subseccd_ruleyear ) == 3L ]
x[ , ryear := as.integer( floor( ruledate1 / 100 ) ) ]
x[ is.na( ryear ), ryear := as.integer( ruleyear ) ]
x[ , pf := pad( as.integer( fndncd_ruleyear ), 2 ) %in% c( "02", "03", "04" ) ]
## the panel's ruling-year attributes are missing for cohorts the 2016-2018
## snapshots omitted: foundation status from any BMF snapshot of the EIN
lg <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "pf" ),
             colClasses = list( character = "EIN" ) )
lg[ , pf := as.logical( pf ) ]
pf_any <- lg[ , .( pf_any = any( pf, na.rm = TRUE ) ), by = EIN ]
x[ , EIN := pad( as.integer( ein ), 9 ) ]
x[ pf_any, on = "EIN", pf_any := i.pf_any ]
log_msg( "foundation status filled from snapshots: ", x[ is.na( fndncd_ruleyear ) & pf_any == TRUE, .N ] )
x[ is.na( fndncd_ruleyear ) & pf_any == TRUE, pf := TRUE ]
x[ , ntee3 := toupper( substr( nteefinal, 1, 3 ) ) ]
x[ , geoid := fifelse( !is.na( fips_ruleyear ) & fips_ruleyear > 0, pad( as.integer( fips_ruleyear ), 5 ), NA_character_ ) ]
x[ , zip5 := fifelse( !is.na( zip5_ruleyear ) & zip5_ruleyear > 0, pad( as.integer( zip5_ruleyear ), 5 ), NA_character_ ) ]
x[ , geoid := ct_to_planning_region( geoid, city_ruleyear ) ]
zipmap <- fread( file.path( OUT$geo, "zip_county_modal_bmf.csv" ), colClasses = list( character = c( "zip5", "geoid_z" ) ) )
x[ zipmap, on = "zip5", geoid_z := i.geoid_z ]
x[ , loc_method := fifelse( !is.na( geoid ), "fips_ruleyear", fifelse( !is.na( geoid_z ), "zip_modal", "unlocated" ) ) ]
x[ is.na( geoid ), geoid := geoid_z ]
x[ , geoid := recode_county( geoid ) ]
print( x[ , .N, by = loc_method ] )

tab <- function( d, src ) d[ !is.na( geoid ), .(
  n_entry_c3        = .N,
  n_entry_c3pc      = sum( !pf ),
  n_entry_c3pc_nohu = sum( !pf & !ntee3 %in% c( NTEE_HOSP, NTEE_UNIV ) ),
  n_entry_pf        = sum( pf ) ), by = .( geoid, year = ryear ) ][ , src := src ]
panel <- tab( x[ ryear %in% 1995:2022 ], "long_panel" )
rm( x ); gc()

m <- fread( file.path( BUILD, "raw", "bmf_monthly", "c3_rulings_2024.csv.gz" ),
            colClasses = list( character = c( "EIN", "geoid", "ruling_ym" ) ) )
m[ , ryear := as.integer( substr( ruling_ym, 1, 4 ) ) ]
m[ , pf := as.logical( pf ) ]
## reinstated organizations carry a new ruling date in the IRS file: drop
## EINs already present in a legacy snapshot before their ruling year
first_seen <- lg[ , .( first_seen = min( year ) ), by = EIN ]
m[ first_seen, on = "EIN", first_seen := i.first_seen ]
log_msg( "monthly-BMF rulings 2015-2024 dropped as pre-existing (reinstated): ",
         m[ ryear %in% 2015:2024 & !is.na( first_seen ) & first_seen < ryear, .N ] )
m <- m[ is.na( first_seen ) | first_seen >= ryear ]
rm( lg ); gc()
monthly <- tab( m[ ryear %in% 2015:2024 ], "monthly_2024_12" )

## overlap: survivors in Dec 2024 / all entrants seen in the panel
ov <- merge( panel[ , .( panel = sum( n_entry_c3pc ) ), by = year ],
             monthly[ , .( monthly = sum( n_entry_c3pc ) ), by = year ], by = "year", all = TRUE )
ov[ , survivor_ratio := monthly / panel ]
print( ov[ year >= 2010 ] )
fwrite( ov, file.path( OUT$diag, "entry_splice.csv" ) )

ent <- rbind( panel[ year %in% 2000:2021 ], monthly[ year %in% 2022:2024 ] )
## new foundations ruled 2016-2021 are absent from the legacy BMF files the
## long panel is built on (they first appear in the 2023 monthly file)
ent[ year %in% 2016:2021, n_entry_pf := NA_integer_ ]
ent[ , entry_src_monthly := as.integer( src != "long_panel" ) ][ , src := NULL ]
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
log_msg( "entries off the 2023 spine (dropped): ", ent[ !geoid %in% sp$geoid, sum( n_entry_c3 ) ] )
ent <- ent[ geoid %in% sp$geoid ]
setorder( ent, geoid, year )
fwrite( ent, file.path( OUT$npo, "npo_entry_county_year.csv" ) )
print( ent[ , .( counties = .N, n_entry_c3pc = sum( n_entry_c3pc ), n_entry_pf = sum( n_entry_pf ) ), keyby = year ] )
log_msg( "wrote npo_entry_county_year.csv: ", nrow( ent ), " county-years" )
