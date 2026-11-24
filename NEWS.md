# hypogeom 0.0.0.9001

* Performance: `dhypogeom()`/`phypogeom()` compute only Pr(X = 0..max(x)) exactly by sequential convolution (O(length(probs) * max(x))) instead of an FFT over N points (N up to ~1.9e5 when `min(probs) = 1e-3`). About 26x faster for `dhypogeom(33, probs)` + `phypogeom(33, probs)` with 5 probs; up to ~100x for small probs.
* Accuracy and behaviour changes (all are corrections of FFT artefacts):
  * Values change by FFT round-off only (at most ~1e-13 relative plus ~1e-13 absolute for upper tails); results are closer to a 256-bit reference (`tests/testthat/fixtures/reference.rds`). Where tiny probabilities were affected by FFT absolute round-off, the change is larger in relative terms (e.g. Pr(X = 33) for five geometrics with p = 1e-3 was off by 5e-7 relative; it now matches `dnbinom()` to 2e-15).
  * `prob = 1` now returns the exact distribution (point mass at 0) instead of `NaN`.
  * `log = TRUE` / `log.p = TRUE` no longer return `NaN` with a "NaNs produced" warning where the FFT produced slightly negative values.
  * With a finite `max_N`, d/p values are now exact for `x < max_N` instead of circularly aliased (the old values could be wrong by up to ~1 in the CDF). The support bound (0 or 1 at and beyond the truncation point) is unchanged.
* Unchanged: `qhypogeom()`, `rhypogeom()` and `.dhypogeom()` (bit-identical); argument handling, warnings and errors are otherwise unchanged.
* Known pre-existing issues, not changed: `dhypogeom()` errors ("the condition has length > 1") for a vector `x` with any entry that is negative, non-integer or >= the truncation point (the warning loop tests `x` instead of `bad_x`); `phypogeom(q, lower.tail = FALSE)` returns 1 instead of 0 for `q` at or beyond the truncation point.
* Tests: golden regression tests (`data-raw/` generators) pin the old outputs.
