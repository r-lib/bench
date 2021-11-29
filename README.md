
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bench

<!-- badges: start -->

[![R build
status](https://github.com/r-lib/bench/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/bench)
[![Coverage
status](https://codecov.io/gh/r-lib/bench/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/bench?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/bench)](https://cran.r-project.org/package=bench)
<!-- badges: end -->

The goal of bench is to benchmark code, tracking execution time, memory
allocations and garbage collections.

## Installation

You can install the release version from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("bench")
```

Or you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("r-lib/bench")
```

## Features

`bench::mark()` is used to benchmark one or a series of expressions, we
feel it has a number of advantages over [alternatives](#alternatives).

-   Always uses the highest precision APIs available for each operating
    system (often nanoseconds).
-   Tracks memory allocations for each expression.
-   Tracks the number and type of R garbage collections per expression
    iteration.
-   Verifies equality of expression results by default, to avoid
    accidentally benchmarking inequivalent code.
-   Has `bench::press()`, which allows you to easily perform and combine
    benchmarks across a large grid of values.
-   Uses adaptive stopping by default, running each expression for a set
    amount of time rather than for a specific number of iterations.
-   Expressions are run in batches and summary statistics are calculated
    after filtering out iterations with garbage collections. This allows
    you to isolate the performance and effects of garbage collection on
    running time (for more details see [Neal
    2014](https://radfordneal.wordpress.com/2014/02/02/inaccurate-results-from-microbenchmark/)).

The times and memory usage are returned as custom objects which have
human readable formatting for display (e.g. `104ns`) and comparisons
(e.g. `x$mem_alloc > "10MB"`).

There is also full support for plotting with
[ggplot2](http://ggplot2.tidyverse.org/) including custom scales and
formatting.

## Usage

### `bench::mark()`

Benchmarks can be run with `bench::mark()`, which takes one or more
expressions to benchmark against each other.

``` r
library(bench)
set.seed(42)
dat <- data.frame(x = runif(10000, 1, 1000), y=runif(10000, 1, 1000))
```

`bench::mark()` will throw an error if the results are not equivalent,
so you don’t accidentally benchmark inequivalent code.

``` r
bench::mark(
  dat[dat$x > 500, ],
  dat[which(dat$x > 499), ],
  subset(dat, x > 500))
#> Error: Each result must equal the first result:
#> `dat[dat$x > 500, ]` does not equal `dat[which(dat$x > 499), ]`
```

Results are easy to interpret, with human readable units.

``` r
bnch <- bench::mark(
  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500))
bnch
#> # A tibble: 3 × 6
#>   expression                     min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 dat[dat$x > 500, ]           258µs    383µs     2543.     377KB     17.1
#> 2 dat[which(dat$x > 500), ]    204µs    260µs     3803.     260KB     17.7
#> 3 subset(dat, x > 500)         332µs    426µs     2318.     510KB     20.7
```

By default the summary uses absolute measures, however relative results
can be obtained by using `relative = TRUE` in your call to
`bench::mark()` or calling `summary(relative = TRUE)` on the results.

``` r
summary(bnch, relative = TRUE)
#> # A tibble: 3 × 6
#>   expression                  min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
#> 1 dat[dat$x > 500, ]         1.26   1.47      1.10      1.45     1   
#> 2 dat[which(dat$x > 500), ]  1      1         1.64      1        1.03
#> 3 subset(dat, x > 500)       1.63   1.64      1         1.96     1.21
```

### `bench::press()`

`bench::press()` is used to run benchmarks against a grid of parameters.
Provide setup and benchmarking code as a single unnamed argument then
define sets of values as named arguments. The full combination of values
will be expanded and the benchmarks are then *pressed* together in the
result. This allows you to benchmark a set of expressions across a wide
variety of input sizes, perform replications and other useful tasks.

``` r
set.seed(42)

create_df <- function(rows, cols) {
  as.data.frame(setNames(
    replicate(cols, runif(rows, 1, 100), simplify = FALSE),
    rep_len(c("x", letters), cols)))
}

results <- bench::press(
  rows = c(1000, 10000),
  cols = c(2, 10),
  {
    dat <- create_df(rows, cols)
    bench::mark(
      min_iterations = 100,
      bracket = dat[dat$x > 500, ],
      which = dat[which(dat$x > 500), ],
      subset = subset(dat, x > 500)
    )
  }
)
#> Running with:
#>    rows  cols
#> 1  1000     2
#> 2 10000     2
#> 3  1000    10
#> 4 10000    10
results
#> # A tibble: 12 × 8
#>    expression  rows  cols      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr> <dbl> <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 bracket     1000     2   25.3µs     33µs    29008.   15.84KB    11.6 
#>  2 which       1000     2   25.5µs   32.5µs    29813.    7.91KB     8.95
#>  3 subset      1000     2   45.8µs   56.7µs    17164.    27.7KB     8.66
#>  4 bracket    10000     2   45.6µs     60µs    16286.  156.46KB    56.1 
#>  5 which      10000     2   40.4µs   42.9µs    20583.   78.23KB    31.7 
#>  6 subset     10000     2   94.6µs  117.5µs     8158.  273.79KB    49.1 
#>  7 bracket     1000    10   64.4µs     77µs    12601.   47.52KB    12.8 
#>  8 which       1000    10   58.9µs   63.3µs    14338.    7.91KB    12.4 
#>  9 subset      1000    10   85.1µs  104.7µs     8755.   59.38KB    10.7 
#> 10 bracket    10000    10  128.9µs  144.6µs     6752.   469.4KB    70.3 
#> 11 which      10000    10   89.8µs   97.3µs     9699.   78.23KB    14.8 
#> 12 subset     10000    10  189.5µs  232.9µs     4180.  586.73KB    56.8
```

## Plotting

`ggplot2::autoplot()` can be used to generate an informative default
plot. This plot is colored by gc level (0, 1, or 2) and faceted by
parameters (if any). By default it generates a
[beeswarm](https://github.com/eclarke/ggbeeswarm#geom_quasirandom) plot,
however you can also specify other plot types (`jitter`, `ridge`,
`boxplot`, `violin`). See `?autoplot.bench_mark` for full details.

``` r
ggplot2::autoplot(results)
```

<img src="man/figures/README-autoplot-1.png" width="100%" />

You can also produce fully custom plots by un-nesting the results and
working with the data directly.

## `system_time()`

**bench** also includes `system_time()`, a higher precision alternative
to
[system.time()](https://www.rdocumentation.org/packages/base/versions/3.5.0/topics/system.time).

``` r
bench::system_time({ i <- 1; while(i < 1e7) i <- i + 1 })
#> process    real 
#>   2.37s   2.37s
bench::system_time(Sys.sleep(.5))
#> process    real 
#>    91µs   500ms
```

## Alternatives

-   [rbenchmark](https://cran.r-project.org/package=rbenchmark)
-   [microbenchmark](https://cran.r-project.org/package=microbenchmark)
-   [tictoc](https://cran.r-project.org/package=tictoc)
-   [system.time()](https://www.rdocumentation.org/packages/base/versions/3.5.0/topics/system.time)
