## 84_tcja_contrib.R
## Does the 2018 tax law (TCJA) break the individual-giving covariates?
## TCJA roughly doubled the standard deduction from TY2018: the itemizer share fell from
## about 21% to 6%, so itemized contributions after 2018 cover a smaller, richer slice of
## filers. Every multiverse cross-section (2019, 2022) is post-TCJA, the pooled stratum
## (2015-22) straddles the break, and the within stratum (2010-22) identifies off it.
##   Part 1  the break in levels and in the cross-metro ranking: national itemizer share
##           and contributions per return by year; year-to-year correlation of each
##           contribution measure across metros (does 2017 -> 2018 stand out?)
##   Part 2  partial r of each contribution measure in the default model, re-fit as a
##           cross-section in every year 2010-2022 (metros; log Y + log P; OLS and
##           division FE; filers and spending excluding hospitals and universities)
##   Part 3  within-metro (2010-22) default model with each contribution measure, with
##           and without the measure interacted with post-2018 (does the break carry the
##           within estimate?)
## Outputs D:/MULTIVERSE/data/tcja/. Usage: Rscript R/84_tcja_contrib.R

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )
## with MV_MODEL=lean the default model is the 13-covariate one (run v11); outputs go to tcja_v11/
OD <- file.path( BUILD, "data", if ( MV_MODEL == "lean" ) "tcja_v11" else "tcja" ); dir.create( OD, showWarnings = FALSE )
panel <- load_panel()
CM <- c( contrib_total = "philanthropy:contrib_total", contrib_per_return = "philanthropy:contrib_per_return",
         contrib_share_agi = "philanthropy:contrib_share_agi" )
m <- panel[ cbsa_type == "metro" & year %in% 2010:2022 ]

## ---- part 1: the break ---------------------------------------------------------------
soi <- fread( file.path( OUT$cov, "cov_irs_soi.csv" ), colClasses = list( character = "geoid" ) )
nat <- soi[ year %in% 2010:2022, .( itemizer_share = sum( n04470, na.rm = TRUE ) / sum( n1, na.rm = TRUE ),
                                    claim_share = sum( n19700, na.rm = TRUE ) / sum( n1, na.rm = TRUE ),
                                    contrib_per_return = sum( a19700, na.rm = TRUE ) / sum( n1, na.rm = TRUE ),
                                    contrib_share_agi = sum( a19700, na.rm = TRUE ) / sum( a00100, na.rm = TRUE ) ), by = year ][ order( year ) ]
fwrite( nat, file.path( OD, "t1_national.csv" ) )
## year-to-year rank stability across metros (contrib_total in per-capita log form)
m[ , contrib_total_pc := log( contrib_total / population ) ]
yy <- rbindlist( lapply( c( "contrib_total_pc", "contrib_per_return", "contrib_share_agi", "pct_ba", "pcpi" ), function( v ) {
  w <- dcast( m[ , .( cbsa, year, x = get( v ) ) ], cbsa ~ year, value.var = "x" )
  rbindlist( lapply( 2011:2022, function( t ) data.table( var = v, from = t - 1, to = t,
    rho = cor( w[[ as.character( t - 1 ) ]], w[[ as.character( t ) ]], method = "spearman", use = "complete.obs" ) ) ) ) } ) )
fwrite( yy, file.path( OD, "t1_rank_stability.csv" ) )

## ---- part 2: cross-section by year -----------------------------------------------------
base <- list( drop = "none", dv_form = "loglog", pred_form = "log", rhs_logP = TRUE, unit = "metro",
              ident = "cs", outlier = "none" )
specs <- CJ( op = c( "default", unname( CM ) ), est = c( "ols", "div_fe" ),
             outcome = c( "n_filer_c3", "exp_c3_nohu" ), cs_year = 2010:2022 )
specs[ , op_block := fifelse( op == "default", NA_character_, "philanthropy" ) ]
specs[ , spec_id := .I ]
res <- rbindlist( lapply( seq_len( nrow( specs ) ), function( i ) {
  s <- c( base, as.list( specs[ i ] ) )
  r <- run_spec( s, panel ); if ( is.null( r ) ) return( NULL )
  r[ , k := .N ]; r } ) )
res <- merge( res, specs, by = "spec_id" )
res[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
xs <- res[ term %in% c( names( CM ), "pf_assets", "pct_ba", "turnout", "poverty_rate", "pct_nonwhite" ) ]
fwrite( xs, file.path( OD, "t2_cs_by_year.csv" ) )
## pre (2010-17) vs post (2018-22) mean partial r by measure
pp <- xs[ ( op != "default" & term %in% names( CM ) ) | op == "default",
          .( pre = mean( pr[ cs_year <= 2017 ] ), post = mean( pr[ cs_year >= 2018 ] ),
             y2017 = pr[ cs_year == 2017 ], y2018 = pr[ cs_year == 2018 ], y2022 = pr[ cs_year == 2022 ],
             sd_pre = sd( pr[ cs_year <= 2017 ] ) ),
          by = .( term, op, est, outcome ) ]
pp[ , jump_in_sd := ( post - pre ) / sd_pre ]
fwrite( pp, file.path( OD, "t2_pre_post.csv" ) )

## ---- part 3: within metros, with and without a post-2018 slope -------------------------
wres <- rbindlist( lapply( names( CM ), function( v ) rbindlist( lapply( c( "n_filer_c3", "exp_c3_nohu" ), function( y ) {
  s <- modifyList( base, list( op = CM[[ v ]], op_block = "philanthropy", est = "ols", outcome = y, cs_year = 2022L,
                               ident = "within", spec_id = 0L ) )
  r0 <- run_spec( s, panel )[ term == v, .( term, model = "within", estimate, stat, n ) ]
  ## the same model with the measure interacted with post-2018, fit directly
  xsv <- setdiff( covars_for( s ), TIME_INVARIANT_VARS )
  d <- panel[ year %in% 2010:2022 & cbsa_type == "metro", c( "cbsa", "year", "population", "logP", y, xsv ), with = FALSE ]
  d <- d[ complete.cases( d ) & get( y ) > 0 ]
  ## same model as run_spec(): log Y on logged extensive / level intensive covariates + log P
  d[ , yv := log( get( y ) ) ]
  for ( u in xsv ) if ( COVS[ var == u, type ] == "B" ) set( d, j = u, value = flog( d[[ u ]], d$population ) )
  d[ , post := as.integer( year >= 2018 ) ]
  d[ , xpost := get( v ) * post ]
  f <- as.formula( paste( "yv ~", paste( c( xsv, "xpost", "logP" ), collapse = " + " ), "| cbsa + year" ) )
  r1 <- feols( f, data = d, cluster = ~cbsa )
  ct <- coeftable( r1 )[ c( v, "xpost" ), ]
  rbind( r0, data.table( term = c( v, paste0( v, " x post2018" ) ), model = "within + post slope",
                         estimate = ct[ , 1 ], stat = ct[ , 3 ], n = nobs( r1 ) ) )[ , outcome := y ] } ) ) ) )
fwrite( wres, file.path( OD, "t3_within_post.csv" ) )

cat( "\n== National series ==\n" ); print( nat, digits = 3 )
cat( "\n== Rank stability across metros, 2016-17 / 2017-18 / 2018-19 ==\n" )
print( dcast( yy[ to %in% 2016:2019 ], var ~ to, value.var = "rho" ), digits = 3 )
cat( "\n== Partial r pre vs post 2018 (cross-section by year) ==\n" ); print( pp[ order( outcome, est, term ) ], digits = 2 )
cat( "\n== Within metros ==\n" ); print( wres, digits = 3 )
