## 07_npo_bls.R
## BLS nonprofit research data (QCEW establishments matched to IRS 501(c)(3)
## status), MSA level, 2013-2022 (design 2.7). BLS plans the next update for
## 2029, so 2022 is the last year.
##
##   bls_npo_estab / bls_npo_emp / bls_npo_wages  501(c)(3) establishments,
##        annual average employment, total annual wages ($; BLS reports $000s)
##   bls_priv_*      all private establishments (same definitions)
##   bls_pct_npo     nonprofit share of private employment
##   bls_wage_ratio  nonprofit / private average annual wage (raw; driven by
##                   industry mix)
##   bls_wage_ratio_adj  composition-adjusted ratio: sector wage ratios
##                   (2-digit NAICS) weighted by FIXED national nonprofit
##                   employment shares (2018), renormalized over the sectors
##                   the MSA reports; bls_wage_ratio_cov = share of national
##                   weight available (low = mostly suppressed)
##   bls_pct_npo_62 / _61 / _624 / _71  nonprofit employment share within
##                   health care & social assistance, education, social
##                   assistance, arts & recreation (where not suppressed)
## MSA files are used directly (county sums would be biased by non-random
## county suppression). QCEW MSA codes are CBSA/10 on the OMB delineation in
## force (2013 vintage for 2013-2017, 2018 vintage for 2018-2022); they are
## matched to 2023 CBSA codes and flagged when the code is absent or the
## member-county set changed (bls_delin_flag).
##
## Outputs: D:/MULTIVERSE/data/cov/cbsa/cbsa_bls.csv  (merged at CBSA level in 30_assemble)
##          (validation against the 990 data: 07b_bls_validation.R, after 30)

source( "R/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )
dir.create( file.path( OUT$cov, "cbsa" ), showWarnings = FALSE )

files <- file.path( RAW$bls, sprintf( "qcew-nonprofits-%d.csv", 2013:2022 ) )
stopifnot( all( file.exists( files ) ) )
num <- function( x ) suppressWarnings( as.numeric( gsub( "[^0-9.eE-]", "", x ) ) )
b <- rbindlist( lapply( files, fread, colClasses = "character" ), fill = TRUE )
b[ , year := as.integer( year ) ]
for ( v in c( "priv_estab", "priv_employ", "priv_wages", "npo_estab", "npo_employ", "npo_wages",
              "percent_npo", "wage_ratio_npo" ) ) set( b, j = v, value = num( b[[ v ]] ) )
b[ , naics := sub( "-[0-9]{2}$", "", naics ) ]          # ranges such as 31-33 -> 31
b[ , lvl := substr( ind_level, 1, 1 ) ]

## ---- MSA totals -------------------------------------------------------------------

m <- b[ grepl( "^3_", geo_level ) ]
m[ , cbsa := paste0( pad( as.integer( fips ), 4 ), "0" ) ]
## 2023 delineation: renamed codes (1:1) and metros absorbed into a neighbor
## (totals summed into the 2023 CBSA; ratios recomputed; industry detail NA)
RENAME <- c( "17460" = "17410",   # Cleveland-Elyria -> Cleveland
             "19380" = "19430",   # Dayton -> Dayton-Kettering-Beavercreek
             "39140" = "39150",   # Prescott -> Prescott Valley-Prescott
             "15680" = "30500",   # California-Lexington Park -> Lexington Park
             "45540" = "48680" )  # The Villages -> Wildwood-The Villages
ABSORB <- c( "31460" = "23420",   # Madera -> Fresno
             "36140" = "12100" )  # Ocean City -> Atlantic City-Hammonton
m[ , bls_code_orig := cbsa ]
m[ cbsa %in% names( RENAME ), cbsa := RENAME[ cbsa ] ]
absorbed <- unique( c( names( ABSORB ), ABSORB ) )
m[ cbsa %in% names( ABSORB ), cbsa := ABSORB[ cbsa ] ]
tot <- m[ lvl == "1", .( bls_title = geo_title[ which.max( priv_employ ) ],
                         bls_npo_estab = sum( npo_estab ), bls_npo_emp = sum( npo_employ ),
                         bls_npo_wages = sum( npo_wages ) * 1000,
                         bls_priv_estab = sum( priv_estab ), bls_priv_emp = sum( priv_employ ),
                         bls_priv_wages = sum( priv_wages ) * 1000,
                         bls_recoded = any( bls_code_orig != cbsa ),
                         bls_merged = .N > 1 ), by = .( cbsa, year ) ]
tot[ , bls_pct_npo := bls_npo_emp / bls_priv_emp ]
tot[ , bls_wage_ratio := ( bls_npo_wages / bls_npo_emp ) / ( bls_priv_wages / bls_priv_emp ) ]

## ---- composition-adjusted wage ratio (2-digit sectors) ---------------------------

us <- b[ grepl( "^1_", geo_level ) & lvl == "2" & year == 2018, .( naics, w = npo_employ ) ]
us <- us[ is.finite( w ) & w > 0 ][ , w := w / sum( w ) ]
sec <- m[ lvl == "2" & naics %in% us$naics & is.finite( npo_employ ) & npo_employ > 0 &
            is.finite( priv_employ ) & priv_employ > 0 & is.finite( npo_wages ) & is.finite( priv_wages ),
          .( cbsa, year, naics, r = ( npo_wages / npo_employ ) / ( priv_wages / priv_employ ) ) ]
sec <- merge( sec, us, by = "naics" )
adj <- sec[ , .( bls_wage_ratio_adj = sum( w * r ) / sum( w ), bls_wage_ratio_cov = sum( w ) ), by = .( cbsa, year ) ]

## within-industry market shares
shr <- dcast( m[ naics %in% c( "62", "61", "624", "71" ) & is.finite( npo_employ ) & priv_employ > 0,
                 .( cbsa, year, v = paste0( "bls_pct_npo_", naics ), s = npo_employ / priv_employ ) ],
              cbsa + year ~ v, value.var = "s", fun.aggregate = function( x ) if ( length( x ) == 1L ) x else NA_real_ )

bls <- Reduce( function( a, x ) merge( a, x, by = c( "cbsa", "year" ), all.x = TRUE ), list( tot, adj, shr ) )
## industry detail is not additive across absorbed metros
detail <- c( "bls_wage_ratio_adj", "bls_wage_ratio_cov", grep( "^bls_pct_npo_", names( bls ), value = TRUE ) )
bls[ bls_merged == TRUE, ( detail ) := NA_real_ ]

## ---- delineation match to 2023 CBSAs -------------------------------------------

sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
c23 <- unique( sp[ !is.na( cbsa ) & cbsa != "", .( cbsa, cbsa_title, cbsa_type ) ] )
bls <- merge( bls, c23, by = "cbsa", all.x = TRUE )
## title check: first principal city must agree
city <- function( x ) tolower( sub( "[-,].*$", "", x ) )
bls[ , bls_delin_flag := fcase( is.na( cbsa_title ), "code_not_in_2023",
                                bls_merged, "absorbed_metro_summed",
                                bls_recoded, "code_renamed_2023",
                                city( bls_title ) != city( cbsa_title ), "title_differs",
                                default = "ok" ) ]
bls[ , c( "bls_recoded", "bls_merged" ) := NULL ]
print( bls[ , .N, keyby = .( year, bls_delin_flag ) ] )
print( unique( bls[ bls_delin_flag != "ok", .( cbsa, bls_title, cbsa_title, bls_delin_flag ) ] ) )
out <- bls[ !is.na( cbsa_title ) ]
out[ , c( "bls_title", "cbsa_title", "cbsa_type" ) := NULL ]
out[ , bls_delin_flag := as.integer( bls_delin_flag != "ok" ) ]
fwrite( out, file.path( OUT$cov, "cbsa", "cbsa_bls.csv" ) )
log_msg( "wrote cbsa_bls.csv: ", nrow( out ), " CBSA-years, ", uniqueN( out$cbsa ), " CBSAs" )
