## 21_patents.R
## Utility patents by inventor county and application year (design 5.2).
## NOT YET RUN: PatentsView bulk files moved to the USPTO Open Data Portal in
## March 2026 and now require a signed-in USPTO.gov account (the old open S3
## links return 403). Put these files (zipped or unzipped .tsv) in
##   D:/MULTIVERSE/raw/patentsview/
##     g_patent.tsv(.zip)                 patent_id, patent_type, patent_date
##     g_application.tsv(.zip)            patent_id, filing_date
##     g_inventor_disambiguated.tsv(.zip) patent_id, inventor_sequence, location_id
##     g_location_disambiguated.tsv(.zip) location_id, latitude, longitude, disambig_country
##     g_assignee_disambiguated.tsv(.zip) patent_id, assignee_sequence, assignee_type,
##                                        disambig_assignee_organization
## (a March 2025 mirror of the same tables is on ICPSR DataLumos, project 223582).
##
## Construction
##   - utility patents only; year = application (filing) year, 2000-2024.
##     Grants lag filing by ~2-3 years, so the last 3 application years are
##     truncated (flag patents_truncated = 1 for 2021+; grants observed through 2025).
##   - fractional counting: each inventor carries 1 / (inventors on the
##     patent), including foreign inventors; US inventors are located by
##     lat/long -> 2023 county (point in polygon).
##   - assignee class from the first assignee: university / nonprofit /
##     hospital (organization-name rules), corporate (types 2, 3), other
##     (individual, government, unassigned).
## Output: D:/MULTIVERSE/data/cov/cov_patents.csv
##   patents_frac, patents_corp, patents_univnp, patents_other, patents_truncated

source( "R/lib/00_setup.R" )
source( "R/lib/geo_helpers.R" )

pdir <- file.path( BUILD, "raw", "patentsview" )
need <- c( "g_patent", "g_application", "g_inventor_disambiguated", "g_location_disambiguated",
           "g_assignee_disambiguated" )
## look in raw/patentsview and raw/patents, including subfolders (bundled
## downloads unpack into nested folders); prefer unzipped .tsv
dirs <- file.path( BUILD, "raw", c( "patentsview", "patents" ) )
allf <- unlist( lapply( dirs[ dir.exists( dirs ) ], list.files, recursive = TRUE, full.names = TRUE ) )
path_of <- function( t ) {
  f <- c( allf[ basename( allf ) == paste0( t, ".tsv" ) ], allf[ basename( allf ) == paste0( t, ".tsv.zip" ) ] )
  if ( length( f ) ) f[1] else NA_character_
}
paths <- vapply( need, path_of, character( 1 ) )
if ( anyNA( paths ) ) {
  message( "PatentsView files missing: ", paste( need[ is.na( paths ) ], collapse = ", " ),
           "\nSee the header of this script. Skipping patents." )
  quit( save = "no", status = 0 )
}
rd <- function( t, select ) {
  f <- paths[[ t ]]
  cmd <- if ( grepl( "\\.zip$", f ) ) sprintf( 'unzip -p "%s"', f ) else NULL
  if ( is.null( cmd ) ) fread( f, select = select, sep = "\t", quote = "\"", colClasses = "character" )
  else fread( cmd = cmd, select = select, sep = "\t", quote = "\"", colClasses = "character" )
}

log_msg( "patents" )
pat <- rd( "g_patent", c( "patent_id", "patent_type" ) )[ patent_type == "utility" ]
app <- rd( "g_application", c( "patent_id", "filing_date" ) )
app[ , ayear := as.integer( substr( filing_date, 1, 4 ) ) ]
pat <- merge( pat, unique( app[ , .( patent_id, ayear ) ], by = "patent_id" ), by = "patent_id" )
pat <- pat[ ayear %in% YEARS ]
log_msg( "utility patents with application year 2000-2024: ", nrow( pat ) )

asg <- rd( "g_assignee_disambiguated", c( "patent_id", "assignee_sequence", "assignee_type",
                                          "disambig_assignee_organization" ) )
asg <- asg[ as.integer( assignee_sequence ) == 0L ]
UNIV <- "UNIVERSIT|COLLEGE|INSTITUTE OF TECHNOLOGY|POLYTECHNIC|SCHOOL OF MEDICINE|BOARD OF REGENTS|REGENTS OF|RESEARCH FOUNDATION|FOUNDATION|HOSPITAL|MEDICAL CENTER|CLINIC|CHILDREN'S|HEALTH SYSTEM|INSTITUTE FOR|RESEARCH INSTITUTE|MUSEUM"
asg[ , org := toupper( disambig_assignee_organization ) ]
asg[ , aclass := fcase( grepl( UNIV, org ), "univnp",
                        assignee_type %in% c( "2", "3" ), "corp",
                        default = "other" ) ]
pat <- merge( pat, asg[ , .( patent_id, aclass ) ], by = "patent_id", all.x = TRUE )
pat[ is.na( aclass ), aclass := "other" ]
rm( asg, app ); gc()

inv <- rd( "g_inventor_disambiguated", c( "patent_id", "inventor_sequence", "location_id" ) )
inv <- inv[ patent_id %in% pat$patent_id ]
inv[ , w := 1 / .N, by = patent_id ]
loc <- rd( "g_location_disambiguated", c( "location_id", "latitude", "longitude", "disambig_country" ) )
loc <- loc[ disambig_country == "US" ]
loc[ , `:=`( lat = as.numeric( latitude ), lon = as.numeric( longitude ) ) ]
loc <- loc[ is.finite( lat ) & is.finite( lon ) ]
log_msg( "US inventor locations to place: ", nrow( loc ) )
loc[ , geoid := county_from_latlon( lat, lon ) ]
inv <- merge( inv, loc[ !is.na( geoid ), .( location_id, geoid ) ], by = "location_id" )
inv <- merge( inv, pat[ , .( patent_id, ayear, aclass ) ], by = "patent_id" )

out <- inv[ , .( patents_frac = sum( w ),
                 patents_corp = sum( w[ aclass == "corp" ] ),
                 patents_univnp = sum( w[ aclass == "univnp" ] ),
                 patents_other = sum( w[ aclass == "other" ] ) ), by = .( geoid, year = ayear ) ]
out[ , patents_truncated := as.integer( year >= 2021 ) ]   # grants observed through 2025
setorder( out, geoid, year )
print( out[ , .( counties = .N, patents = sum( patents_frac ), univnp_share = sum( patents_univnp ) / sum( patents_frac ) ),
            keyby = year ] )
fwrite( out, file.path( OUT$cov, "cov_patents.csv" ) )
log_msg( "wrote cov_patents.csv: ", nrow( out ), " county-years" )
