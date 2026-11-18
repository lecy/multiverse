## 54_summarize_composites.R
## Composite-index options (41_composites.R) vs the measures they replace,
## across the principled universe (base run v3 + run_principled_cmp).
##   A  stability by option: every measure option of each block (default,
##      single-item alternatives, composites), full model only (drop = none):
##      median partial r, IQR, sign stability, share p < .05 (primary and
##      state-clustered SE)
##   B  paired: each composite spec vs its default twin (identical forks,
##      op = default): block |t|, share significant, model R2, and spill-over
##      onto the other blocks' coefficients
## Partial r = t / sqrt(t^2 + df), df ~ n - terms - 1 (as in 45_summarize_run.R).
## Works on a partial run (uses whatever composite batches exist).
## Usage: Rscript R/multiverse/54_summarize_composites.R [base_dir]

source( "R/lib/00_setup.R" )
source( "R/lib/40_multiverse.R" )
source( "R/lib/41_composites.R" )
suppressPackageStartupMessages( library( arrow ) )
options( width = 220 )

args <- commandArgs( TRUE )
base_dir <- if ( length( args ) ) args[1] else file.path( OUT$spec, "run_principled_v3" )
cmp_dir  <- file.path( OUT$spec, "run_principled_cmp" )
KEYS <- c( "construct", "hu", "outcome", "ident", "dv_form", "pred_form", "rhs_logP", "unit",
           "est", "cs_year", "outlier", "drop", "op" )
PAIR <- setdiff( KEYS, "op" )
rd <- function( d, p ) rbindlist( lapply( list.files( d, pattern = p, full.names = TRUE ),
                                         function( f ) as.data.table( read_parquet( f ) ) ), fill = TRUE )
r3 <- function( dt ) dt[ , lapply( .SD, function( z ) if ( is.numeric( z ) ) round( z, 3 ) else z ) ]

## ---- data: full-model specs (drop = none) from base and composite runs --------------
bs <- as.data.table( read_parquet( file.path( base_dir, "specs.parquet" ) ) )[ drop == "none" ]
bc <- as.data.table( read_parquet( file.path( base_dir, "coefs.parquet" ) ) )[ spec_id %in% bs$spec_id ]
cg <- as.data.table( read_parquet( file.path( cmp_dir, "specs_grid.parquet" ) ) )
cc <- rd( file.path( cmp_dir, "batches" ), "^coefs_" ); cst <- rd( file.path( cmp_dir, "batches" ), "^status_" )
log_msg( "base full-model specs: ", nrow( bs ), "; composite specs run: ", nrow( cst ), " of ", nrow( cg ),
         " (", cst[ status == "ok", .N ], " ok)" )
OFF <- 1e7                                                   # keep the two id spaces apart
cc[ , spec_id := spec_id + OFF ]; cg[ , spec_id := spec_id + OFF ]; cst[ , spec_id := spec_id + OFF ]
sp <- rbind( bs[ , c( "spec_id", KEYS ), with = FALSE ][ , src := "base" ],
             cg[ spec_id %in% cst$spec_id, c( "spec_id", KEYS ), with = FALSE ][ , src := "cmp" ] )
co <- rbind( bc[ , .( spec_id, term, estimate, se, stat, p, p_state, r2, n ) ],
             cc[ , .( spec_id, term, estimate, se, stat, p, p_state, r2, n ) ] )
co[ , k := .N, by = spec_id ]
co[ , pr := stat / sqrt( stat^2 + pmax( n - k - 1, 1 ) ) ]
d <- merge( co[ term != "logP" & is.finite( pr ) ], sp, by = "spec_id" )
d[ , stratum := paste( construct, hu, ident, sep = "/" ) ]
d[ , op_block := ifelse( op == "default", NA_character_, sub( ":.*$", "", op ) ) ]

## block of each term: composites by their COMP_ALTS block, items by COVS
cmp_block <- unlist( lapply( names( COMP_ALTS ), function( b )
  setNames( rep( b, length( unique( unlist( COMP_ALTS[[ b ]] ) ) ) ), unique( unlist( COMP_ALTS[[ b ]] ) ) ) ) )
cmp_block <- cmp_block[ !duplicated( names( cmp_block ) ) ]
d[ , term_block := fifelse( term %in% names( cmp_block ), cmp_block[ term ], COVS$block[ match( term, COVS$var ) ] ) ]
## a block's own options: default op (its default terms) and its alternatives
d[ , own := ( op == "default" & term %in% unlist( lapply( BLOCKS, `[[`, "default" ) ) ) |
            ( !is.na( op_block ) & term %in% unlist( BLOCKS[[ op_block[1] ]]$alt[[ sub( "^[^:]+:", "", op[1] ) ]] ) ),
   by = op ]
d[ op == "default", term_block := fifelse( term %in% BLOCKS$need$default, "need", term_block ) ]
d[ , block := fifelse( op == "default", term_block, op_block ) ]
d[ , composite := op %in% COMP_OPS ]

## ---- A: stability by measure option ----------------------------------------------------
stab_of <- function( x ) {
  med <- median( x$pr )
  list( specs = nrow( x ), median_r = med, iqr_r = IQR( x$pr ),
        q25 = unname( quantile( x$pr, 0.25 ) ), q75 = unname( quantile( x$pr, 0.75 ) ),
     sign_stab = mean( sign( x$pr ) == sign( med ) ),
     sig05 = mean( x$p < 0.05, na.rm = TRUE ),
     sig05_same_sign = mean( x$p < 0.05 & sign( x$pr ) == sign( med ), na.rm = TRUE ),
     sig05_state = mean( x$p_state < 0.05, na.rm = TRUE ) )
}
A <- d[ own == TRUE, stab_of( .SD ), by = .( construct, ident, block, op, term, composite ) ]
A_all <- d[ own == TRUE, stab_of( .SD ), by = .( block, op, term, composite ) ]
fwrite( r3( A ), file.path( cmp_dir, "summary_option_stability.csv" ) )
fwrite( r3( A_all ), file.path( cmp_dir, "summary_option_stability_all.csv" ) )

## ---- B: paired composite vs default twin ------------------------------------------------
dd <- d[ op == "default" ]; dc <- d[ composite == TRUE ]
tw <- merge( unique( dc[ , c( "spec_id", PAIR, "op", "block" ), with = FALSE ] ),
             unique( dd[ , c( "spec_id", PAIR ), with = FALSE ] ), by = PAIR, suffixes = c( "", "_def" ) )
## block-level: strongest own term in each spec
blk_c <- dc[ own == TRUE, .( t_c = max( abs( stat ) ), sig_c = any( p < 0.05 ), r2_c = r2[1] ), by = spec_id ]
blk_d <- merge( tw[ , .( spec_id_def, block, op ) ], dd[ , .( spec_id, term_block, stat, p, r2 ) ],
                by.x = c( "spec_id_def", "block" ), by.y = c( "spec_id", "term_block" ), allow.cartesian = TRUE )[
  , .( t_d = max( abs( stat ) ), sig_d = any( p < 0.05 ), r2_d = r2[1] ), by = .( spec_id_def, op ) ]
B <- merge( merge( tw, blk_c, by = "spec_id" ), blk_d, by = c( "spec_id_def", "op" ) )
## spill-over: coefficients of the other blocks (terms present in both twins)
oth <- merge( dc[ own == FALSE, .( spec_id, term, pr_c = pr, p_c = p ) ], tw[ , .( spec_id, spec_id_def ) ], by = "spec_id" )
oth <- merge( oth, dd[ , .( spec_id_def = spec_id, term, pr_d = pr, p_d = p ) ], by = c( "spec_id_def", "term" ) )
oth <- merge( oth, unique( tw[ , .( spec_id, op, ident ) ] ), by = "spec_id" )
spill <- oth[ , .( pairs = .N, median_abs_dpr = median( abs( pr_c - pr_d ) ),
                   sign_change = mean( sign( pr_c ) != sign( pr_d ) ),
                   sig_lost = mean( p_d < 0.05 & p_c >= 0.05 ), sig_gained = mean( p_d >= 0.05 & p_c < 0.05 ) ),
              by = .( op, ident, term ) ]
fwrite( r3( spill ), file.path( cmp_dir, "summary_spillover.csv" ) )

pairsum <- B[ , .( pairs = .N, median_t_comp = median( t_c ), median_t_default = median( t_d ),
                   share_t_higher = mean( t_c > t_d ),
                   sig_comp = mean( sig_c ), sig_default = mean( sig_d ),
                   median_dR2 = median( r2_c - r2_d, na.rm = TRUE ), share_R2_higher = mean( r2_c > r2_d, na.rm = TRUE ) ),
              by = .( op, ident ) ][ order( op, ident ) ]
fwrite( r3( pairsum ), file.path( cmp_dir, "summary_paired.csv" ) )

## ---- console ------------------------------------------------------------------------------
cat( "\n== A. Measure options, whole universe (full model): composite rows marked TRUE ==\n" )
print( r3( A_all[ order( block, -composite, op, term ) ] ), nrows = 200 )
cat( "\n== A. By construct x identification (composites and defaults only) ==\n" )
print( r3( A[ composite == TRUE | op == "default" ][ order( block, construct, ident, op, term ) ]
            [ , .( construct, ident, block, op, term, specs, median_r, iqr_r, sign_stab, sig05, sig05_state ) ] ), nrows = 400 )
cat( "\n== B. Paired with the default twin: block |t|, share significant, R2 ==\n" )
print( r3( pairsum ), nrows = 100 )
cat( "\n== B. Spill-over onto other blocks (median over terms) ==\n" )
print( r3( spill[ , .( median_abs_dpr = median( median_abs_dpr ), max_sign_change = max( sign_change ),
                       sig_lost = mean( sig_lost ), sig_gained = mean( sig_gained ) ), by = .( op, ident ) ][ order( op, ident ) ] ), nrows = 100 )
cat( "\n== B. Largest spill-overs (term-level) ==\n" )
print( r3( spill[ order( -median_abs_dpr ) ][ 1:25 ] ) )
