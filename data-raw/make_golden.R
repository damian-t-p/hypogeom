# Run from the package root, with the hypogeom code to pin installed:
#   Rscript data-raw/make_golden.R
# Writes tests/testthat/fixtures/golden.rds from the CURRENT code.
source("tests/testthat/helper-golden.R")
sha <- tryCatch(
  trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
  error = function(e) NA_character_
)
dirty <- tryCatch(
  length(system2("git", c("status", "--porcelain", "R"), stdout = TRUE)) > 0,
  error = function(e) NA
)
golden <- list(
  results = golden_run(),
  r_version = R.version.string,
  git_sha = sha,
  r_dir_dirty = dirty
)
dir.create("tests/testthat/fixtures", showWarnings = FALSE, recursive = TRUE)
saveRDS(golden, "tests/testthat/fixtures/golden.rds", version = 3)
