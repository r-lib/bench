
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bench

[![Travis build
status](https://travis-ci.org/jimhester/bench.svg?branch=master)](https://travis-ci.org/jimhester/bench)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jimhester/bench?branch=master&svg=true)](https://ci.appveyor.com/project/jimhester/bench)
[![Coverage
status](https://codecov.io/gh/jimhester/bench/branch/master/graph/badge.svg)](https://codecov.io/github/jimhester/bench?branch=master)

The goal of bench is to benchmark code.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jimhester/bench")
```

## Example

`bench::mark()` a function to easily benchmark a series of expressions
and evaluate relative performance.

``` r
set.seed(42)
dat <- data.frame(x = runif(10000, 1, 1000), y=runif(10000, 1, 1000))

# Throws an error if the results are not equivalent, so you don't accidentally
# benchmark against the wrong answer
results <- bench::mark(
  y = dat[dat$x > 500, ],
  x = dat[which(dat$x > 499), ],
  subset(dat, x > 500))
#> Error: All results must equal the first result:
#>   `dat[dat$x > 500, ]` does not equal `dat[which(dat$x > 499), ]`

results <- bench::mark(
  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500))

results
#> # A tibble: 3 x 10
#>   expression                     min     mean   median      max `itr/sec` mem_alloc total_time n_itr  n_gc
#>   <chr>                     <bch:tm> <bch:tm> <bch:tm> <bch:tm>     <dbl> <bch:byt>   <bch:tm> <int> <dbl>
#> 1 dat[dat$x > 500, ]           312µs    443µs    365µs   1.95ms     2255.      426K      425ms   958     5
#> 2 dat[which(dat$x > 500), ]    244µs    326µs    273µs   1.61ms     3066.      366K      405ms  1240     6
#> 3 subset(dat, x > 500)         397µs    530µs    439µs   2.29ms     1886.      546K      420ms   792     5
```

``` r
set.seed(42)

create_df <- function(rows, cols) {
  as.data.frame(setNames(
    replicate(cols, runif(rows, 1, 1000), simplify = FALSE),
    rep_len(c("x", letters), cols)))
}

results <- bench::mark(
  setup = dat <- create_df(rows, cols),
  parameters = list(rows = c(10000, 100000), cols = c(10, 100)),
  min_time = .5,
  min_iterations = 100,

  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500))
#> Running benchmark with:
#>     rows  cols
#> 1  10000    10
#> 2 100000    10
#> 3  10000   100
#> 4 100000   100
```

``` r
library(tidyverse)
results %>%
  select(expression, rows, cols, time, gc) %>%
  unnest() %>%
  mutate(gc =
    case_when(
      level2 > 0 ~ "level2",
      level1 > 0 ~ "level1",
      level0 > 0 ~ "level0",
      TRUE ~ "none")) %>%
  mutate(gc = factor(gc, c("none", "level0", "level1", "level2"))) %>%
  ggplot(aes(x = expression, y = time, color = gc)) +
    geom_jitter() +
    coord_flip() +
    scale_color_brewer(type = "qual", palette = 3) +
    facet_grid(rows ~ cols, labeller = label_both)
```

<img src="man/figures/README-pressure-1.png" width="100%" />

Also includes `system_time()`, a higher precision replacement for
`system.time()`

``` r
bench::system_time({ i <- 1; while(i < 1e7) i <- i + 1 })
#> process    real 
#>   344ms   347ms
bench::system_time(Sys.sleep(.5))
#> process    real 
#>    76µs   503ms
```
