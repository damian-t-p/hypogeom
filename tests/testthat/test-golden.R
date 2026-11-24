# Golden regression tests pinned from the unmodified code (see data-raw/).
golden <- readRDS(test_path("fixtures", "golden.rds"))
reference <- readRDS(test_path("fixtures", "reference.rds"))
current <- golden_run()

expect_golden_group <- function(group, value_check = expect_identical) {
  for (pv in names(golden$results)) {
    old <- golden$results[[pv]][[group]]
    new <- current[[pv]][[group]]
    for (nm in names(old)) {
      label <- paste(pv, group, nm)
      expect_identical(new[[nm]]$warnings, old[[nm]]$warnings, label = label)
      value_check(new[[nm]]$value, old[[nm]]$value, label = label)
    }
  }
}

is_log_call <- function(nm) grepl("log_TRUE|lp_TRUE|d_log", nm)
is_upper_call <- function(nm) grepl("lt_FALSE", nm)

# Reference values aligned with the output of a call (x = 0, 1, ... first).
ref_for_call <- function(pv, group, nm) {
  r <- reference[[pv]]
  v <- if (group == "dhypogeom" || startsWith(nm, "d")) {
    r$pmf
  } else if (is_upper_call(nm)) {
    1 - r$cdf
  } else {
    r$cdf
  }
  v
}

# Value check on the natural scale (log outputs are compared as probabilities).
# Absolute floors, all due to the FFT path: its round-off is absolute (1e-15 for
# pmf/cdf, up to about 1e-13 for the upper tail 1 - cdf, which sums N terms).
# Negative FFT tail values: log() of them gave NaN in the old code. Where old is
# NaN and new is not, new must be finite and match the reference instead.
expect_golden_close <- function(new, old, label, log_scale = FALSE, abs_floor = 1e-15,
                                ref = NULL) {
  if (inherits(new, "golden_error") || inherits(old, "golden_error")) {
    return(expect_identical(new, old, label = label))
  }
  if (log_scale) {
    new <- exp(new)
    old <- exp(old)
  }
  expect_identical(length(new), length(old), label = paste(label, "length"))
  old_nan <- is.na(old)
  if (log_scale) {
    # new NaN where old was finite is never allowed
    expect_false(any(is.na(new) & !old_nan), label = paste(label, "new NaN"))
    if (any(old_nan)) {
      expect_true(all(is.finite(new[old_nan])), label = paste(label, "finite fix"))
      i <- which(old_nan)
      i <- i[i <= min(41L, length(ref))]
      if (!is.null(ref) && length(i) > 0) {
        expect_true(
          all(abs(new[i] - ref[i]) <= 1e-15), label = paste(label, "fix vs reference")
        )
      }
    }
  } else {
    expect_identical(is.na(new), old_nan, label = paste(label, "NA positions"))
  }
  ok <- !old_nan & !is.na(new)
  expect_true(
    all(abs(new[ok] - old[ok]) <= 1e-13 * abs(old[ok]) + abs_floor),
    label = paste(label, "within tolerance")
  )
}

expect_golden_group <- function(group, tolerance = FALSE, skip = character(),
                                skip_values = function(nm) FALSE) {
  for (pv in setdiff(names(golden$results), skip)) {
    old <- golden$results[[pv]][[group]]
    new <- current[[pv]][[group]]
    for (nm in names(old)) {
      label <- paste(pv, group, nm)
      logc <- tolerance && is_log_call(nm)
      # Old log() of negative FFT round-off gave a "NaNs produced" warning that
      # the exact path no longer triggers.
      old_w <- if (logc) setdiff(old[[nm]]$warnings, "NaNs produced") else
        old[[nm]]$warnings
      new_w <- if (logc) setdiff(new[[nm]]$warnings, "NaNs produced") else
        new[[nm]]$warnings
      expect_identical(new_w, old_w, label = label)
      if (!tolerance) {
        expect_identical(new[[nm]]$value, old[[nm]]$value, label = label)
      } else if (skip_values(nm) && !inherits(old[[nm]]$value, "golden_error")) {
        next
      } else {
        expect_golden_close(
          new[[nm]]$value, old[[nm]]$value, label,
          log_scale = is_log_call(nm),
          abs_floor = if (is_upper_call(nm)) 1e-13 else 1e-15,
          ref = ref_for_call(pv, group, nm)
        )
      }
    }
  }
}

# Exact-identity checks (FFT path, quantiles, overrides, errors).
test_that("golden: .dhypogeom is identical", expect_golden_group("internal"))
test_that("golden: qhypogeom is identical", expect_golden_group("quantile"))
test_that("golden: d_override is identical", expect_golden_group("d_override"))
test_that("golden: invalid probs are identical", expect_golden_group("invalid"))

# d/p values move by FFT round-off only. single_1 is excluded (see below).
test_that("golden: dhypogeom values", expect_golden_group("dhypogeom", TRUE, "single_1"))
test_that("golden: phypogeom values", expect_golden_group("phypogeom", TRUE, "single_1"))
test_that("golden: edge cases and errors", expect_golden_group("edge", TRUE, "single_1"))
test_that(
  "golden: full-x calls (incl. x >= N)",
  # max_N values are exact now (the old truncated FFT was aliased): only the
  # warnings/errors of the *_max_N_60 calls are pinned, see below for values.
  expect_golden_group("dp_full_x", TRUE, "single_1", \(nm) grepl("max_N_60", nm))
)
test_that(
  "golden: max_N d/p warnings are identical (values: see below)",
  expect_golden_group("dp_max_N", TRUE, "single_1", \(nm) TRUE)
)

# Aliasing under max_N: the old FFT wrapped around, the exact path does not, so
# the values on 0:(N-1) with max_N equal the untruncated ones.
test_that("max_N d/p equal the untruncated exact values on the retained support", {
  for (pv in names(golden_probs())) {
    probs <- golden_probs()[[pv]]
    N50 <- hypogeom:::.hypogeom_N(probs, max_N = 50)
    x <- 0:(N50 - 1L)
    expect_identical(dhypogeom(x, probs, max_N = 50), dhypogeom(x, probs), label = pv)
    expect_identical(phypogeom(x, probs, max_N = 50), phypogeom(x, probs), label = pv)
  }
})

# Guards the duplicated N expression in .hypogeom_N and .dhypogeom.
test_that(".hypogeom_N matches the length of .dhypogeom", {
  for (pv in names(golden_probs())) {
    probs <- golden_probs()[[pv]]
    expect_identical(hypogeom:::.hypogeom_N(probs), length(hypogeom:::.dhypogeom(probs)))
    expect_identical(
      hypogeom:::.hypogeom_N(probs, max_N = 50),
      length(hypogeom:::.dhypogeom(probs, max_N = 50))
    )
  }
})

# single_1: warnings/errors and the other groups still match the golden file.
test_that("golden: single_1 non-value groups are identical", {
  for (g in c("internal", "quantile", "d_override")) {
    expect_identical(current$single_1[[g]], golden$results$single_1[[g]])
  }
})

# accuracy fix: FFT path returned NaN for prob = 1
test_that("prob = 1 gives the exact point mass at 0", {
  expect_identical(dhypogeom(0:1, 1), c(1, 0))
  expect_identical(phypogeom(0:1, 1), c(1, 1))
})

# Validates the reference generator. The FFT has absolute (not relative) round-off
# error, so tiny tail values only agree to an absolute floor of 1e-15.
# single_1 is skipped: the FFT path returns NaN there (pinned above).
test_that("reference.rds agrees with golden d/p values (1e-12 rel + 1e-15 abs)", {
  close <- function(a, b) all(abs(a - b) <= 1e-12 * abs(b) + 1e-15)
  for (pv in setdiff(names(reference), "single_1")) {
    d <- golden$results[[pv]]$dhypogeom$log_FALSE$value
    p <- golden$results[[pv]]$phypogeom$lt_TRUE_lp_FALSE$value
    n <- min(41L, length(d))  # valid x are 0:(n-1)
    expect_true(close(d[1:n], reference[[pv]]$pmf[1:n]), label = paste(pv, "pmf"))
    expect_true(close(p[1:n], reference[[pv]]$cdf[1:n]), label = paste(pv, "cdf"))
  }
})

# Accuracy against the 256-bit reference (x = 0:40 restricted to x < N).
test_that("new d/p are within 1e-14 relative of the reference (exact zeros match)", {
  rel_ok <- function(v, r) {
    all(v[r == 0] == 0) && all(abs(v[r > 0] / r[r > 0] - 1) < 1e-14)
  }
  for (pv in names(reference)) {
    probs <- golden_probs()[[pv]]
    n <- min(41L, hypogeom:::.hypogeom_N(probs))
    x <- 0:(n - 1L)
    expect_true(rel_ok(dhypogeom(x, probs), reference[[pv]]$pmf[1:n]), label = pv)
    expect_true(rel_ok(phypogeom(x, probs), reference[[pv]]$cdf[1:n]), label = pv)
  }
})

# The upper tail 1 - cdf has absolute error only (cancellation near 1).
test_that("new upper tail is within 1e-15 absolute of 1 - reference cdf", {
  for (pv in names(reference)) {
    probs <- golden_probs()[[pv]]
    n <- min(41L, hypogeom:::.hypogeom_N(probs))
    x <- 0:(n - 1L)
    up <- phypogeom(x, probs, lower.tail = FALSE)
    expect_true(all(abs(up - (1 - reference[[pv]]$cdf[1:n])) <= 1e-15), label = pv)
  }
})

# Equal up to one eps: both are limited by double-precision accumulation.
test_that("new d/p are at least as accurate as the old FFT values (within eps)", {
  for (pv in setdiff(names(reference), "single_1")) {
    probs <- golden_probs()[[pv]]
    n <- min(41L, hypogeom:::.hypogeom_N(probs))
    x <- 0:(n - 1L)
    old_d <- golden$results[[pv]]$dhypogeom$log_FALSE$value[1:n]
    old_p <- golden$results[[pv]]$phypogeom$lt_TRUE_lp_FALSE$value[1:n]
    err <- function(v, ref) max(abs(v - ref[1:n]))
    expect_lte(
      err(dhypogeom(x, probs), reference[[pv]]$pmf),
      err(old_d, reference[[pv]]$pmf) + .Machine$double.eps, label = paste(pv, "pmf")
    )
    expect_lte(
      err(phypogeom(x, probs), reference[[pv]]$cdf),
      err(old_p, reference[[pv]]$cdf) + .Machine$double.eps, label = paste(pv, "cdf")
    )
  }
})
