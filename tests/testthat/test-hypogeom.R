test_that("One-parameter hypogeom agrees with geom", {
  x <- c(0, 1, 10, 100)
  p <- c(0.01, 0.5, 0.09)
  prob <- 0.3

  expect_equal(dhypogeom(x, prob), dgeom(x, prob))
  expect_equal(phypogeom(x, prob), pgeom(x, prob))
  expect_equal(qhypogeom(p, prob), qgeom(p, prob))
})
