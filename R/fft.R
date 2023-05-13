#' Compute a vector of PMF values of the hypogeometric distribution
#'
#' @param probs Vector of the probabilities of success of each geometric random
#' variable
#' @param eps Error tolerance for density
#' @param max_N Maximum N at which to truncate the densities
#'
#' @return A vector of numerics of length N, where N is a truncation that ensures
#' the desired precision. The `k`th entry of the return vector is `Pr(X = K-1)`
.dhypogeom <- function(probs, eps = .Machine$double.eps, max_N = Inf) {

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
