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
