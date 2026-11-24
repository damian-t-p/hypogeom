#' Length of the truncated support used by the FFT path
#'
#' @inheritParams .dhypogeom
#'
#' @return The length of the vector returned by `.dhypogeom`
.hypogeom_N <- function(probs, eps = .Machine$double.eps, max_N = Inf, d_override = NULL) {

  if (!is.null(d_override)) {
    return(length(d_override))
  }

  n <- length(probs)
  N <- min(qgeom(eps/n, min(probs), lower.tail = FALSE) * n, max_N)

  length(0L : (N - 1L))

}

#' Exact leading terms of the PMF of the hypogeometric distribution
#'
#' Exact Pr(X = 0..L-1) by sequential convolution with each geometric. All terms
#' are non-negative, so there is no cancellation.
#'
#' @param probs Vector of the probabilities of success of each geometric random
#' variable
#' @param L Number of leading terms
#'
#' @return A vector of length L; the `k`th entry is `Pr(X = k-1)`
.dhypogeom_head <- function(probs, L) {

  f <- c(1, numeric(L - 1L))
  for (p in probs) {
    q <- 1 - p
    f[1L] <- p * f[1L]
    if (L > 1L) for (k in 2:L) f[k] <- p * f[k] + q * f[k - 1L]
  }
  f

}

#' Compute a vector of PMF values of the hypogeometric distribution
#'
#' @param probs Vector of the probabilities of success of each geometric random
#' variable
#' @param eps Error tolerance for density
#' @param max_N Maximum N at which to truncate the densities
#' @param d_override Density vector that overwrites the function output. Use this option if you are
#' frequently re-running hypogeometric functions with the same parameters,
#'
#' @return A vector of numerics of length N, where N is a truncation that ensures
#' the desired precision. The `k`th entry of the return vector is `Pr(X = K-1)`
.dhypogeom <- function(probs, eps = .Machine$double.eps, max_N = Inf, d_override = NULL) {

  if (!is.null(d_override)) {
    return(d_override)
  }
  
  n   <- length(probs)
  N   <- min(qgeom(eps/n, min(probs), lower.tail = FALSE) * n, max_N)
  
  ts <- 0L : (N - 1L)
  dft <- Reduce(\(acc, y) acc * cfgeom(ts * (-2*pi / N), y), probs, init = 1)
  
  Re(fft(dft, inverse = TRUE) / N)
  
}

#' Compute a vector of CDF values of the hypogeometric distribution
#'
#' @param probs Vector of the probabilities of success of each geometric random
#' variable
#' @param ... Other parameters to pass to .dhypogeom
#' 
#' @return A vector of numerics of length N, where N is a truncation that ensures
#' the desired precision. The `k`th entry of the return vector is `Pr(X < )`
.chypogeom <- function(probs, ...) cumsum(.dhypogeom(probs, ...))
