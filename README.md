
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
probs <- c(0.2, 0.5, 0.1)

dhypogeom(c(0, 1, 2, 10), probs)
#> [1] 0.01000000 0.02200000 0.03270000 0.04772124
```

``` r
phypogeom(c(-1, 2, 5.5), probs)
#> [1] 0.0000000 0.0647000 0.2028012
```

``` r
qhypogeom(c(0.05, 0.5, 0.95), probs)
#> [1]  2 11 35
```

``` r
rhypogeom(10, probs)
#>  [1]  9 30 14 39 12 15 17 73 15  2
```
