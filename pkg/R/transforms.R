#' Transforms used by the multiverse models
#'
#' * `winsor()` caps `x` at its `p` and `1 - p` quantiles.
#' * `resid_logP()` is the SAMI first stage: the residual of `y` on log population,
#'   fitted year by year (years with three or fewer usable places give `NA`).
#' * `slog()` is `log(x + 1)` with negatives set to zero (kept for older scripts).
#' * `flog()` logs an extensive covariate with a per-capita floor: per-capita values
#'   below the `p`-th percentile of positive per-capita values are raised to it, then
#'   multiplied back by population and logged. Used from run v6 on, so that zeros
#'   (e.g. a metro with no foundation assets) do not sit many SDs below the mean.
#'
#' @param x Numeric vector.
#' @param p Quantile (winsor: each tail; flog: floor percentile).
#' @param y Outcome (log scale).
#' @param logP Log population.
#' @param year Year of each row.
#' @param pop Population.
#' @examples
#' winsor( c( 1:99, 1000 ) )[ 100 ]
#' flog( c( 0, 10, 20 ), pop = c( 100, 100, 100 ) )
#' @name transforms
NULL

#' @rdname transforms
#' @export
winsor <- function( x, p = 0.01 ) {
  q <- stats::quantile( x, c( p, 1 - p ), na.rm = TRUE )
  pmin( pmax( x, q[1] ), q[2] )
}

#' @rdname transforms
#' @export
resid_logP <- function( y, logP, year ) {
  out <- rep( NA_real_, length( y ) )
  for ( t in unique( year ) ) {
    i <- which( year == t & is.finite( y ) & is.finite( logP ) )
    if ( length( i ) > 3 ) {
      X <- cbind( 1, logP[ i ] )
      out[ i ] <- y[ i ] - X %*% qr.coef( qr( X ), y[ i ] )
    }
  }
  out
}

#' @rdname transforms
#' @export
slog <- function( x ) log( pmax( x, 0 ) + 1 )

#' @rdname transforms
#' @export
flog <- function( x, pop, p = 0.01 ) {
  pc <- x / pop
  f <- stats::quantile( pc[ is.finite( pc ) & pc > 0 ], p, na.rm = TRUE, names = FALSE )
  log( pmax( pc, f ) * pop )
}
