
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hypogeom

<!-- badges: start -->
<!-- badges: end -->

`hypogeom` implements the basic distribution functions for the
hypogeometric distribution, which is the sum of independent geometric
distributions. It is the discrete analogue of the hypoexponential
distribution.

## Installation

You can install the development version of from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("damian-t-p/hypogeom")
```

## Example

This package contains functions that are typically associated with
probability distributions in R. That is, the probability mass function,
distribution function, quantile function, and random generator.

``` r
library(hypogeom)

# The hypogeometric distribution is parameterised by a vector of success probabilites,
# which are the parameters of the constituent geometric distributions
probs <- c(0.2, 0.5, 0.01)

dgeom(c(0, 1, 2, 10), probs)
#> [1] 0.20000000 0.25000000 0.00980100 0.02147484
```

``` r
pgeom(c(-1, 2, 5.5), probs)
#> [1] 0.00000000 0.87500000 0.05851985
```

``` r
qgeom(c(0.05, 0.5, 0.95), probs)
#> [1]   0   0 298
```

``` r
rgeom(10, probs)
#>  [1]   5   3   5   0   0 290  10   0  38  11
```
