## 25_cov_endowment.R
## Historical endowment covariate (moved from "55_endowment_fork.R build" in Phase 2
## of the reproducibility plan): density of 501(c)(3) public charities in the 2000
## BMF stock that were ruled before 1970 (see 52_endowment_change.R). Metros where
## > 60% of the 2000 stock predates 1970 are group-exemption artifacts (Mankato,
## Memphis) and are set to NA. Time-invariant; 30_assemble_analysis.R merges
## hist_pre1970 into cbsa_year.parquet.
##
## Inputs:  data/npo/ein_location_bmf.csv.gz (06), data/geo/county_spine.csv (01)
## Output:  data/cov/cov_endowment.csv (one row per CBSA)

source( "R/00_setup.R" )
COV_FILE <- file.path( OUT$cov, "cov_endowment.csv" )
SHARE_MAX <- 0.6

bmf <- fread( file.path( OUT$npo, "ein_location_bmf.csv.gz" ), select = c( "EIN", "year", "geoid", "subsec", "pf", "ryear" ),
              colClasses = list( character = c( "EIN", "geoid", "subsec" ) ) )
b0 <- bmf[ year == 2000 & subsec %in% c( "03", "3" ) & pf == FALSE & !is.na( geoid ) ]
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = list( character = c( "geoid", "cbsa" ) ) )
b0 <- merge( b0, sp[ !is.na( cbsa ) & cbsa != "", .( geoid, cbsa ) ], by = "geoid" )
e <- b0[ , .( n_bmf2000 = .N, n_pre1970 = sum( ryear < 1970, na.rm = TRUE ),
              n_pre1960 = sum( ryear < 1960, na.rm = TRUE ) ), by = cbsa ]
e[ , share_pre1970 := n_pre1970 / n_bmf2000 ]
e[ , artifact := share_pre1970 > SHARE_MAX ]
e[ , hist_pre1970 := fifelse( artifact, NA_real_, as.numeric( n_pre1970 ) ) ]
fwrite( e, COV_FILE )
log_msg( "CBSAs: ", nrow( e ), "; flagged as group-exemption artifacts: ", e[ artifact == TRUE, .N ] )
print( e[ artifact == TRUE ][ order( -share_pre1970 ) ] )
