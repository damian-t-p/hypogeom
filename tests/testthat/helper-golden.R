# Shared by data-raw/make_golden.R and tests/testthat/test-golden.R so the
# recorded calls and the replayed calls cannot drift.

golden_capture <- function(expr) {
  w <- character()
  v <- withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        structure(
          list(class = class(e), message = conditionMessage(e)),
          class = "golden_error"
        )
      }
    ),
    warning = function(w0) {
      w <<- c(w, conditionMessage(w0))
      invokeRestart("muffleWarning")
    }
  )
  list(value = v, warnings = w)
}

golden_hcc <- c(0.07992709, 0.05544583, 0.34306511, 0.35207011, 0.16056321)

golden_probs <- function() {
  list(
    hcc_stage1 = golden_hcc[1:5],
    hcc_stage2 = golden_hcc[2:5],
    hcc_stage3 = golden_hcc[3:5],
    hcc_stage4 = golden_hcc[4:5],
    hcc_stage5 = golden_hcc[5:5],
    single_0.3 = 0.3,
    single_1 = 1,
    repeated = c(0.2, 0.2, 0.2),
    mixed_with_1 = c(0.5, 1, 0.25),
    small_p = c(1e-3, 0.05, 0.3, 0.3, 0.2),
    near_1 = c(1 - 1e-6, 0.9)
  )
}

# Returns a nested list [prob vector][group][call] of thunks. Group names are
# stable keys used in the fixture.
golden_cases <- function() {
  cases <- lapply(golden_probs(), function(probs) {
    N <- length(hypogeom:::.dhypogeom(probs))
    # Spec'd x includes values >= N, which hit the F5 warning-loop bug (error
    # for any vector x with an out-of-range entry); pinned under "dp_full_x".
    x_full <- unique(c(0:40, 200, N - 1, N, N + 5))
    # Valid x (all < N) so that the values themselves are pinned.
    x <- x_full[x_full < N]
    N50 <- length(hypogeom:::.dhypogeom(probs, max_N = 50))
    ov <- c(0.5, 0.25, 0.125)
    dh <- hypogeom::dhypogeom
    ph <- hypogeom::phypogeom
    qh <- hypogeom::qhypogeom
    p_q <- c(0, 0.01, 0.25, 0.5, 0.9, 0.999, 1, 1.2)
    list(
      internal = list(
        default = function() hypogeom:::.dhypogeom(probs),
        max_N_50 = function() hypogeom:::.dhypogeom(probs, max_N = 50)
      ),
      dhypogeom = list(
        log_FALSE = function() dh(x, probs, log = FALSE),
        log_TRUE = function() dh(x, probs, log = TRUE)
      ),
      phypogeom = list(
        lt_TRUE_lp_FALSE = function() ph(x, probs, lower.tail = TRUE, log.p = FALSE),
        lt_TRUE_lp_TRUE = function() ph(x, probs, lower.tail = TRUE, log.p = TRUE),
        lt_FALSE_lp_FALSE = function() ph(x, probs, lower.tail = FALSE, log.p = FALSE),
        lt_FALSE_lp_TRUE = function() ph(x, probs, lower.tail = FALSE, log.p = TRUE)
      ),
      dp_max_N = list(
        d = function() dh(0:(N50 - 1L), probs, max_N = 50),
        p = function() ph(0:(N50 - 1L), probs, max_N = 50)
      ),
      dp_full_x = list(
        d = function() dh(x_full, probs),
        d_log = function() dh(x_full, probs, log = TRUE),
        p = function() ph(x_full, probs),
        p_lt_FALSE_lp_TRUE = function() ph(x_full, probs, lower.tail = FALSE, log.p = TRUE),
        d_max_N_60 = function() dh(0:60, probs, max_N = 50),
        p_max_N_60 = function() ph(0:60, probs, max_N = 50)
      ),
      edge = list(
        d_neg1 = function() dh(-1, probs),
        d_2.5 = function() dh(2.5, probs),
        p_neg1 = function() ph(-1, probs),
        p_2.5 = function() ph(2.5, probs),
        d_vec_neg = function() dh(c(-1, 2), probs)
      ),
      quantile = list(
        lt_TRUE = function() qh(p_q, probs),
        lt_FALSE = function() qh(p_q, probs, lower.tail = FALSE)
      ),
      d_override = list(
        d = function() dh(0:5, probs, d_override = ov),
        p = function() ph(0:5, probs, d_override = ov)
      )
    )
  })
  cases$invalid <- list(
    invalid = list(
      d_0_0.5 = function() hypogeom::dhypogeom(0:3, c(0, 0.5)),
      d_1.2 = function() hypogeom::dhypogeom(0:3, 1.2),
      p_1.2 = function() hypogeom::phypogeom(0:3, 1.2)
    )
  )
  cases
}

golden_run <- function(cases = golden_cases()) {
  lapply(cases, function(by_group) {
    lapply(by_group, function(calls) lapply(calls, function(f) golden_capture(f())))
  })
}
