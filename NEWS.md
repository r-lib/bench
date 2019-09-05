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
