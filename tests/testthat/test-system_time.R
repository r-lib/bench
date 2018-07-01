context("test-system_time.R")

describe("system_time", {
  skip_on_cran()

  res <- system_time(1 + 1:1e7)
  it("returns process and real time", {
    expect_equal(names(res), c("process", "real"))
  })
  it("returns times that are reasonable, system and real time are relatively
    close for process bound expressions", {
    epsilon <- abs(res[[1]] - res[[2]])
    expect_true((epsilon / res[[1]]) < 1)
  })
  it("returns times that are reasonable, system and real time are far apart
    for non-process bound expressions", {
    res <- system_time(Sys.sleep(.5))
    epsilon <- abs(res[[1]] - res[[2]])
    expect_true((epsilon / res[[1]]) > 100)
  })
})
