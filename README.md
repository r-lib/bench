
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
#> # A tibble: 3 x 13
#>   expression                  rel min    mean   median  max     `itr/sec` mem_alloc num_gc time   result   memory gc   
#>   <chr>                     <dbl> <S3: > <S3: > <S3: b> <S3: b>     <dbl> <chr>      <int> <list> <list>   <list> <lis>
#> 1 subset(dat, x > 500)       1.50 422µs  560µs  487µs   2.38ms      1786. 561.10 kB     14 <S3: … <data.f… <Rpro… <chr…
#> 2 dat[dat$x > 500, ]         1.35 342µs  504µs  436µs   3.29ms      1985. 426.10 kB     12 <S3: … <data.f… <Rpro… <chr…
#> 3 dat[which(dat$x > 500), ]  1    271µs  373µs  324µs   2.17ms      2678. 366.06 kB     14 <S3: … <data.f… <Rpro… <chr…
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
#>    num_x num_y
#> 1   1000  1000
#> 2  10000  1000
#> 3 100000  1000
#> 4   1000 10000
#> 5  10000 10000
#> 6 100000 10000
```

``` r
library(tidyverse)
results %>%
  mutate(expression = fct_reorder(expression, rel)) %>%
  select(expression, num_x, num_y, time, gc) %>%
  unnest() %>%
  group_by(expression, num_x, num_y) %>%
  mutate(gc = sub(".*(level \\d+).*", "\\1", gc)) %>%
  ggplot(aes(x = expression, y = time, color = gc)) +
    geom_jitter() +
    scale_y_continuous(trans = bench::bench_time_trans()) +
    coord_flip() +
    facet_grid(num_y ~ num_x, labeller = label_both)
```

<img src="man/figures/README-pressure-1.png" width="100%" />
