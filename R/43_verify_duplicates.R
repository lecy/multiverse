## 43_verify_duplicates.R
## The trimmed grid drops algebraic duplicates (rows with dup_of != NA). This
## runs a random sample of them next to their canonical twin and checks that
## the covariate coefficients agree (FWL / reparameterization). Any mismatch
## means the engine is not doing what the SAMI note says it should.
## Output: D:/MULTIVERSE/data/spec/verify_duplicates.csv

source( "R/00_setup.R" )
source( "R/40_multiverse.R" )

panel <- load_panel()
grid  <- build_grid( "principled" )
forkcols <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
               "est", "cs_year", "outlier", "drop", "op" )

twin_of <- function( s ) {
  t <- copy( s )
  if ( t$dv_form == "logpc" & t$rhs_logP ) t$dv_form <- "loglog"
  if ( t$dv_form == "pois_off" & t$rhs_logP ) t$dv_form <- "pois_free"
  if ( t$ident == "cs" & t$dv_form == "sami" & t$rhs_logP ) t$dv_form <- "loglog"
  if ( t$ident == "cs" & t$pred_form == "sami" & ( t$rhs_logP | t$dv_form %in% c( "loglog", "pois_free" ) ) )
    t$pred_form <- "log"
  if ( t$dv_form %in% c( "loglog", "pois_free" ) ) t$rhs_logP <- TRUE
  grid[ t[ , ..forkcols ], on = forkcols ]
}

set.seed( 43 )
dups <- grid[ !is.na( dup_of ) ][ , .SD[ sample( .N, min( .N, 60 ) ) ], by = dup_of ]
res <- rbindlist( lapply( seq_len( nrow( dups ) ), function( i ) {
  s <- dups[ i ]; t <- twin_of( s )
  if ( nrow( t ) != 1 || !is.na( t$dup_of ) ) return( data.table( spec_id = s$spec_id, rule = s$dup_of,
                                                                 twin_found = FALSE ) )
  a <- run_spec( s, panel ); b <- run_spec( t, panel )
  if ( is.null( a ) || is.null( b ) ) return( data.table( spec_id = s$spec_id, rule = s$dup_of, twin_found = TRUE,
                                                          fitted = FALSE ) )
  m <- merge( a[ term != "logP", .( term, ea = estimate, ta = stat ) ],
              b[ term != "logP", .( term, eb = estimate, tb = stat ) ], by = "term" )
  data.table( spec_id = s$spec_id, twin_id = t$spec_id, rule = s$dup_of, twin_found = TRUE, fitted = TRUE,
              dv_form = s$dv_form, pred_form = s$pred_form, est = s$est,
              max_rel_diff_coef = max( abs( m$ea - m$eb ) / pmax( abs( m$eb ), 1e-12 ) ),
              max_abs_diff_t = max( abs( m$ta - m$tb ) ) )
} ), fill = TRUE )
fwrite( res, file.path( OUT$spec, "verify_duplicates.csv" ) )
print( res[ , .( n = .N, twins = sum( twin_found ), fitted = sum( fitted, na.rm = TRUE ),
                 max_rel_diff_coef = signif( max( max_rel_diff_coef, na.rm = TRUE ), 3 ),
                 p99_rel_diff = signif( quantile( max_rel_diff_coef, .99, na.rm = TRUE ), 3 ),
                 max_t_diff = signif( max( max_abs_diff_t, na.rm = TRUE ), 3 ) ), by = rule ] )
print( res[ max_rel_diff_coef > 1e-6 ][ order( -max_rel_diff_coef ) ][ 1:10 ] )
