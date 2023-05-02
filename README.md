
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bench

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bench)](https://cran.r-project.org/package=bench)
[![R-CMD-check](https://github.com/r-lib/bench/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/bench/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/bench/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/bench?branch=main)
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

- Always uses the highest precision APIs available for each operating
  system (often nanoseconds).
- Tracks memory allocations for each expression.
- Tracks the number and type of R garbage collections per expression
  iteration.
- Verifies equality of expression results by default, to avoid
  accidentally benchmarking inequivalent code.
- Has `bench::press()`, which allows you to easily perform and combine
  benchmarks across a large grid of values.
- Uses adaptive stopping by default, running each expression for a set
  amount of time rather than for a specific number of iterations.
- Expressions are run in batches and summary statistics are calculated
  after filtering out iterations with garbage collections. This allows
  you to isolate the performance and effects of garbage collection on
  running time (for more details see [Neal
  2014](https://radfordneal.wordpress.com/2014/02/02/inaccurate-results-from-microbenchmark/)).

The times and memory usage are returned as custom objects which have
human readable formatting for display (e.g. `104ns`) and comparisons
(e.g. `x$mem_alloc > "10MB"`).

There is also full support for plotting with
[ggplot2](https://ggplot2.tidyverse.org/) including custom scales and
formatting.

## Continuous benchmarking

*This feature is still in early and active development, but the brave
can test it out.*

You can setup continuous benchmarking for an R package by adding `.R`
scripts containing one or more calls to `bench::mark()` in the `bench/`
directory of an R package. Then from any CI service you can then fetch
previous results, run the benchmarks and push the results back to the
repository with the following.

``` r
bench::cb_fetch()
bench::cb_run()
bench::cb_push()
```

To retrieve the full dataset of benchmark results locally use the
following.

``` r
bench::cb_fetch()
results <- bench::cb_read()
```

And to plot the benchmark times per commit

``` r
bench::cb_plot_time(results)
```

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
#> 1 dat[dat$x > 500, ]           278µs    392µs     2481.     377KB     16.6
#> 2 dat[which(dat$x > 500), ]    205µs    266µs     3756.     260KB     17.7
#> 3 subset(dat, x > 500)         355µs    462µs     2162.     510KB     17.3
```

By default the summary uses absolute measures, however relative results
can be obtained by using `relative = TRUE` in your call to
`bench::mark()` or calling `summary(relative = TRUE)` on the results.

``` r
summary(bnch, relative = TRUE)
#> # A tibble: 3 × 6
#>   expression                  min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
#> 1 dat[dat$x > 500, ]         1.36   1.48      1.15      1.45     1   
#> 2 dat[which(dat$x > 500), ]  1      1         1.74      1        1.06
#> 3 subset(dat, x > 500)       1.73   1.74      1         1.96     1.04
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
#>  1 bracket     1000     2     30µs   35.8µs    25614.   15.84KB     12.8
#>  2 which       1000     2   26.3µs   34.6µs    25452.    7.91KB     12.7
#>  3 subset      1000     2   52.6µs   62.1µs    13154.    27.7KB     10.4
#>  4 bracket    10000     2   62.1µs   69.3µs    13811.  156.46KB     41.1
#>  5 which      10000     2   47.9µs   53.8µs    17927.   78.23KB     24.9
#>  6 subset     10000     2  111.7µs  131.3µs     7309.  273.79KB     39.4
#>  7 bracket     1000    10   65.3µs   82.6µs    11875.   47.52KB     15.4
#>  8 which       1000    10   60.1µs   73.5µs    13446.    7.91KB     18.8
#>  9 subset      1000    10   90.5µs  110.4µs     8852.   59.38KB     15.1
#> 10 bracket    10000    10    148µs    168µs     5733.   469.4KB     49.2
#> 11 which      10000    10   69.9µs   88.7µs    11382.   78.23KB     18.7
#> 12 subset     10000    10  207.6µs  236.8µs     4088.  586.73KB     47.4
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
#>   218ms   219ms
bench::system_time(Sys.sleep(.5))
#> process    real 
#>   105µs   504ms
```

## Alternatives

- [rbenchmark](https://cran.r-project.org/package=rbenchmark)
- [microbenchmark](https://cran.r-project.org/package=microbenchmark)
- [tictoc](https://cran.r-project.org/package=tictoc)
- [system.time()](https://www.rdocumentation.org/packages/base/versions/3.5.0/topics/system.time)
