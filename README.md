
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bench

[![Travis build
status](https://travis-ci.org/jimhester/bench.svg?branch=master)](https://travis-ci.org/jimhester/bench)

The goal of bench is to benchmark code.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jimhester/bench")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
set.seed(42)
dat <- data.frame(x = runif(10000, 1, 1000), y=runif(10000, 1, 1000))

# Throws an error if the results are not equivalent, so you don't accidentally
# benchmark against the wrong answer
results <- bench::mark(
  dat[dat$x > 500, ],
  dat[which(dat$x > 499), ],
  subset(dat, x > 500))
#> Error: results[[1]]$result not equal to results[[2]]$result.
#> Attributes: < Component "row.names": Numeric: lengths (5002, 5016) differ >
#> Component "x": Numeric: lengths (5002, 5016) differ
#> Component "y": Numeric: lengths (5002, 5016) differ

results <- bench::mark(
  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500))

results
#> # A tibble: 3 x 12
#>   name                      relative     n    mean     min  median    max `n/sec` allocated_memory memory result timing
#>   <chr>                        <dbl> <int>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl> <chr>            <list> <list> <list>
#> 1 dat[which(dat$x > 500), ]     1.54  1191 4.17e-4 2.52e-4 2.80e-4 0.0181   2397. 366.06 kB        <Rpro… <data… <dbl …
#> 2 dat[dat$x > 500, ]            1.22   943 5.28e-4 3.20e-4 3.53e-4 0.0186   1896. 426.10 kB        <Rpro… <data… <dbl …
#> 3 subset(dat, x > 500)          1.00   773 6.44e-4 4.11e-4 4.44e-4 0.0297   1553. 546.22 kB        <Rpro… <data… <dbl …
```

``` r
set.seed(42)
results <- bench::mark(
  setup = {
    dat <- data.frame(x = runif(num_x, 1, 1000), y=runif(num_y, 1, 1000))
  },
  parameters = list(num_x = 10 ^ seq(3, 5), num_y = c(1000, 10000)),

  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500)
)
```

``` r
library(tidyverse)
results %>%
  mutate(name = fct_reorder(name, relative)) %>%
  select(name, num_x, num_y, timing) %>%
  unnest() %>%
  ggplot(aes(x = name, y = timing)) +
    geom_jitter() +
    scale_y_log10() +
    coord_flip() +
    facet_grid(num_y ~ num_x, labeller = label_both)
```

<img src="man/figures/README-pressure-1.png" width="100%" />