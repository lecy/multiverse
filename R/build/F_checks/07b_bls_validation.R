## 07b_bls_validation.R
## Validation of the 990 data against the BLS nonprofit research data (split from
## 07_npo_bls.R in Phase 2 of the reproducibility plan, so that 07 no longer reads
## the assembled panel it feeds). Run after 30_assemble_analysis.R.
##   990 expenses vs BLS nonprofit payroll, filers vs establishments, by year
##   metros where 990 expenses most exceed local payroll (headquarters attribution)
##
## Inputs:  data/cov/cbsa/cbsa_bls.csv (07), data/analysis/cbsa_year.parquet (30),
##          data/geo/county_spine.csv
## Outputs: data/diagnostics/bls_vs_990.csv, bls_vs_990_metro.csv

source( "R/lib/00_setup.R" )
suppressPackageStartupMessages( library( arrow ) )

out <- fread( file.path( OUT$cov, "cbsa", "cbsa_bls.csv" ), colClasses = list( character = "cbsa" ) )
sp <- fread( file.path( OUT$geo, "county_spine.csv" ), colClasses = "character" )
c23 <- unique( sp[ !is.na( cbsa ) & cbsa != "", .( cbsa, cbsa_title, cbsa_type ) ] )

## ---- validation: 990 expenses vs BLS payroll; filers vs establishments ------------

pan <- as.data.table( read_parquet( file.path( BUILD, "data", "analysis", "cbsa_year.parquet" ) ) )
v <- merge( out, pan[ , .( cbsa, year, population, exp_c3, exp_c3_nohu, n_filer_c3, cbsa_type ) ],
            by = c( "cbsa", "year" ) )
v <- v[ cbsa_type == "metro" & bls_npo_emp > 0 & exp_c3 > 0 ]
sami <- function( y, p ) { ok <- is.finite( log( y ) ) & is.finite( log( p ) ); r <- rep( NA_real_, length( y ) )
                           r[ ok ] <- resid( lm( log( y[ ok ] ) ~ log( p[ ok ] ) ) ); r }
v[ , `:=`( s_exp = sami( exp_c3, population ), s_wag = sami( bls_npo_wages, population ),
           s_emp = sami( bls_npo_emp, population ), s_n = sami( n_filer_c3, population ),
           s_est = sami( bls_npo_estab, population ) ), by = year ]
beta <- function( y, p ) unname( coef( lm( log( y ) ~ log( p ) ) )[2] )
val <- v[ , .( metros = .N,
               cor_log_exp_wages = cor( log( exp_c3 ), log( bls_npo_wages ) ),
               median_wages_over_exp = median( bls_npo_wages / exp_c3 ),
               cor_sami_exp_wages = cor( s_exp, s_wag ),
               cor_sami_n_estab = cor( s_n, s_est ),
               beta_npo_emp = beta( bls_npo_emp, population ),
               beta_npo_wages = beta( bls_npo_wages, population ),
               beta_exp = beta( exp_c3, population ),
               beta_n_filer = beta( n_filer_c3, population ),
               median_pct_npo = median( bls_pct_npo, na.rm = TRUE ),
               r2_pct_npo_logP = summary( lm( bls_pct_npo ~ log( population ) ) )$r.squared ), keyby = year ]
print( val, digits = 3 )
fwrite( val, file.path( OUT$diag, "bls_vs_990.csv" ) )
## headquarters attribution: metros where 990 expenses most exceed local payroll
hq <- v[ , .( years = .N, log_exp_over_wages = mean( log( exp_c3 / bls_npo_wages ) ),
              log_exp_nohu_over_wages = mean( log( exp_c3_nohu / bls_npo_wages ), na.rm = TRUE ) ), by = cbsa ]
hq <- merge( hq, c23[ , .( cbsa, cbsa_title ) ], by = "cbsa" )[ order( -log_exp_over_wages ) ]
fwrite( hq, file.path( OUT$diag, "bls_vs_990_metro.csv" ) )
cat( "\nLargest 990-expense / BLS-payroll gaps (headquarters attribution):\n" ); print( head( hq, 10 ) )
