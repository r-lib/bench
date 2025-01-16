test_that("autoplot works", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("ggbeeswarm")
  y <- mark(x = 1:1000)
  expect_s3_class(ggplot2::autoplot(y), "ggplot")
  expect_s3_class(plot(y), "ggplot")
})
