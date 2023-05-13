#' Characteristic function of a geometric distribution
#'
#' @param t Values at which to evaluate the characteristic function
#' @param prop Probability of success of each trial. 0 < p <= 1
#'
#' @return A complex number
cfgeom <- function(t, prob) {
  if(prob == 0) {
    rep(0 + 0i, length(t))
  } else {
    prob / (1 - (1 - prob) * exp(1i * t))
  }
}
