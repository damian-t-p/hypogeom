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

# Exact-identity checks (stay identical after Step 2).
test_that("golden: .dhypogeom is identical", expect_golden_group("internal"))
test_that("golden: qhypogeom is identical", expect_golden_group("quantile"))
test_that("golden: edge cases and errors are identical", expect_golden_group("edge"))
test_that("golden: d_override is identical", expect_golden_group("d_override"))
test_that("golden: full-x calls (incl. x >= N) are identical", expect_golden_group("dp_full_x"))
test_that("golden: invalid probs are identical", expect_golden_group("invalid"))

# --- STEP 2: switch ONLY the value comparisons in the next three blocks to a
# tolerance (value_check = ...). Warnings stay identical. ---
test_that("golden: dhypogeom values", expect_golden_group("dhypogeom"))
test_that("golden: phypogeom values", expect_golden_group("phypogeom"))
test_that("golden: max_N d/p values", expect_golden_group("dp_max_N"))

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
