
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
dat <- data.frame(x = runif(10000, 1, 1000), y=runif(10000, 1, 1000))
results <- bench::mark(
  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500))

results
#> # A tibble: 3 x 11
#>   name      relative     n    mean     min  median    max `n/sec` memory  
#>   <chr>        <dbl> <int>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl> <list>  
#> 1 dat[whic…     1.59  1159 4.28e-4 2.67e-4 2.79e-4 0.0301   2335. <Rprofm…
#> 2 dat[dat$…     1.33   972 5.12e-4 3.33e-4 3.55e-4 0.0327   1954. <Rprofm…
#> 3 subset(d…     1.00   729 6.74e-4 4.27e-4 4.50e-4 0.0317   1483. <Rprofm…
#> # ... with 2 more variables: result <list>, timing <list>
```

``` r
library(tidyverse)
results %>%
  mutate(name = fct_reorder(name, relative)) %>%
  select(name, timing) %>%
  unnest() %>%
  ggplot(aes(name, timing, color = name)) +
    geom_jitter() +
    scale_y_log10() +
    coord_flip()
```

<img src="man/figures/README-pressure-1.png" width="100%" />
