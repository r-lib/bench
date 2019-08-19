library(testthat)
library(bench)

if (requireNamespace("xml2")) {
  test_check("bench", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("bench")
}
