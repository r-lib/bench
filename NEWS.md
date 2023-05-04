# bench 1.1.3

* Long unnamed `bench_expr` expressions are now truncated correctly when used as
  columns of a tibble (#94).

* `bench_mark` tibbles now respect the knitr paged df option (#103).

* Fixed an issue where macOS specific C code paths were accidentally being used
  on GNU Hurd (#118).

* Fixed `-Wstrict-prototypes` warnings, as requested by CRAN (#124).

* R >=3.5.0 is now required, which is aligned with tidyverse standards.

* bench now uses testthat 3e (#129).

* bench no longer Suggests mockery.

# bench 1.1.2

* Davis Vaughan is now the maintainer.

* `autoplot.bench_mark()` again supports factor levels for `expression`, as intended (#82)

* `bench::mark()` and `bench::workout()` no longer support unquote
  and splice operators. This fixes inconsistencies in performance
  results with functions like `rlang::list2()` (#61).

* bench has been re-licensed as MIT (#101).

# bench 1.1.1

* `mark()` columns memory, result and mem_alloc columns are now always
  included, even if they are unused.

# bench 1.1.0

## New features

* New `bench_process_memory()` function, to return the current and maximum
  memory used by the current process. This uses system functions to track
  memory, so can measure memory outside of R's GC heap.

* New `workout_expressions()` function, a low-level function to workout a list
  of expressions, like those obtained via `parse()` from a file.

* `mark()` gains a `memory` argument to control if it records memory
  allocations, set `memory = FALSE` to disable recording memory allocations,
  which can be helpful when trying to benchmark long pieces of code with many
  allocations (#62).

## Minor improvements and fixes

* `mark()` now permits empty arguments, e.g. accidental trailing commas (#61).

* `mark()` now errors correctly when the expressions deparsed length is
  different.

* `bench_expr` objects now work better with the upcoming versions of tibble and
  vctrs (@romainfrancois, #64)

* `autoplot.bench_mark()` provides a more informative error if the `ggbeeswarm` package is not installed (@coatless, #69).

* Update documentation of `bench_mark` columns (@jdblischak, #67).

# bench 1.0.4

* `bench_memory()` examples no longer fail if they are run with R that does not
  have memory profiling capability (#56).

* `bench_expr` now has a class of `c("bench_expr", "list")` rather than
  `c("bench_expr", "expression")`, as it is really a list of calls rather than
  a true expression object. (https://github.com/r-lib/vctrs/issues/559)

# bench 1.0.3

* `summary.bench_mark()` gains a `time_unit` argument, so you can report all
  times in a consistent scale if desired (#18, #26).

* `bench_mark()` now checks for user interrupts, to allow you to stop benchmarking 
  if it takes longer than you were expecting (#49).

* New `bench_memory()` to capture just the memory allocated by an expression.

* `bench_time()` is now an alias for `system_time()`.

* `unnest.bench_mark()` is now compatible with the upcoming tidyr 1.0.0 (#48, #51)

* New `hires_time()` allows you to explicitly capture high resolution time
  points.

# bench 1.0.2

* `workout()` a new function which makes timing multiple expressions in turn
  simpler.

* `mark()` now internally uses a tempfile rather than a
  textConnection, as the latter has a 100,000 character limit on
  some platforms (#27)

* `mark()` no longer returns the mean or max values and the column order has
  been tweaked to try and put the most interesting columns first (#37)

* Errors during evaluation now halt execution (#28, #43)

* `scale_bench_time()` and `scale_bench_bytes()` now allow you to use a non-logarithmic scale.

# bench 1.0.1

* Add support for macOS versions prior to 10.12
* Disable load sensitive tests on CRAN, to avoid failures

# bench 1.0.0

* Added a `NEWS.md` file to track changes to the package.
