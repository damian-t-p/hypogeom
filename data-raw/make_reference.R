# Run from the package root:  Rscript data-raw/make_reference.R
# 256-bit Rmpfr reference PMF/CDF for x = 0:40. Rmpfr is installed into a temp
# library and is NOT a package dependency.
source("tests/testthat/helper-golden.R")
tmp <- file.path(tempdir(), "lib")
dir.create(tmp)
install.packages("Rmpfr", lib = tmp, repos = "https://cloud.r-project.org")
.libPaths(c(tmp, .libPaths()))
library(Rmpfr)

ref <- lapply(golden_probs(), function(probs) {
  L <- 41L
  f <- mpfr(c(1, numeric(L - 1L)), 256)
  pm <- mpfr(probs, 256)
  for (i in seq_along(pm)) {
    p <- pm[i]
    f[1L] <- p * f[1L]
    for (k in 2:L) f[k] <- p * f[k] + (1 - p) * f[k - 1L]
  }
  list(pmf = as.numeric(f), cdf = as.numeric(cumsum(f)))
})
saveRDS(ref, "tests/testthat/fixtures/reference.rds", version = 3)
