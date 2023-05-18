#' The Hypogeometric distribution
#'
#' Density, distribution function, quantile function and random gemeration for a
#' hypogeometric distribution.
#'
#' A hypogeometric random variable with parameter vector `probs` has the
#' distribution of a sum of independent geometric random variables with parameters
#' `probs[i]`.
#' 
#' @param x,q Vector of quantiles at which to evaluate the density or CDF
#' @param probs Vector of success probabilities of constituent geometric
#' distributions. Each entry must be in (0, 1]
#' @param n Number of observations
#' @param log,log.p Logical; if `TRUE`, probabilities are given on the log scale
#' @param lower.tail Logical; if `TRUE` gives CDF otherwise gives hazard function
#' @param ... Additional parameters to pass to the internal function`.dhypogeom`.
#'
#' @return A vector of the same length as `x` or `q`
#' 
#' @name Hypogeometric
NULL

#' @rdname Hypogeometric
#' @export
dhypogeom <- function(x, probs, log = FALSE, ...) {

  if (any(probs > 1 | probs <= 0)) {
    warning("NaNs produced")
    return(rep(NaN, length(x)))
  }
  
  # compute list of pmf values
  # pmf[k] = Pr(X = k-1)
  pmf <- .dhypogeom(probs, ...)
  
  # any non-integer x should produce a zero density and throw a warning
  # negative integer values should produce a zero but not throw a warning
  int_x_idxs <- ((x %% 1) == 0) & (x >= 0) & (x < length(pmf))
  for (bad_x in x[!int_x_idxs]) {
    if ((x %% 1) != 0) {
      warning(paste("non-integer x =", bad_x))
    }
  }
  
  d_vals <- numeric(length(x))
  d_vals[int_x_idxs] <- pmf[x[int_x_idxs] + 1L]
  
  if (log == TRUE) {
    log(d_vals)
  } else {
    d_vals
  }
  
}

#' @rdname Hypogeometric
#' @export
phypogeom <- function(q, probs, lower.tail = TRUE, log.p = FALSE, ...) {

  if (any(probs > 1 | probs <= 0)) {
    warning("NaNs produced")
    return(rep(NaN, length(q)))
  }
  
  cdf <- .chypogeom(probs, ...)
  
  if (!lower.tail) {
    cdf <- 1 - cdf
  }

  # Set negative and extremely large values to 0 and 1 respectively
  p_vals <- rep(0, length(q))
  p_vals[q >= length(cdf)] <- 1

  # Replace non-integer values with the corresponding floor
  pos_q_idxs <- (q >= 0) & (q < length(cdf))
  p_vals[pos_q_idxs] <- cdf[floor(q[pos_q_idxs]) + 1L]
  
  if (log.p == TRUE) {
    log(p_vals)
  } else {
    p_vals
  }
  
}

#' @rdname Hypogeometric
#' @export
qhypogeom <- function(p, probs, lower.tail = TRUE, log.p = FALSE, ...) {

  if (log.p == TRUE) {
    p <- exp(p)
  }

  if (lower.tail == FALSE) {
    p <- 1 - p
  }
  
  # probabilities outside [0, 1) should return NaNs, 1 should return Inf
  q_out <- rep(NaN, length(p))
  valid_p_idxs <- (p >= 0) & (p < 1)
  q_out[p == 1] <- Inf
  
  cdf <- .chypogeom(probs, ...)

  # for each valid p, find the smallest value in cdf that is greater than or equal to p
  q_out[valid_p_idxs] <- Vectorize(\(p) match(TRUE, cdf >= p))(p[valid_p_idxs]) - 1

  if (any(is.nan(q_out))) {
    warning("NaNs produced")
  }
  
  q_out
  
}

#' @rdname Hypogeometric
#' @export
rhypogeom <- function(n, prob) {

  geoms <- rgeom(n * length(prob), prob)

  unname(
    tapply(geoms, rep(1:n, each = length(prob)), sum)
  )
  
}
