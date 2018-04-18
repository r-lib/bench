context("test-mark.R")

describe("mark_", {
  it("If min_time is Inf, runs for max_iterations", {
    res <- .Call(mark_, quote(1), new.env(), Inf, as.integer(0), as.integer(10), tempfile())
    expect_equal(length(res[[1]]), 10)
    expect_equal(length(res[[2]]), 10)

    res <- .Call(mark_, quote(1), new.env(), Inf, as.integer(0), as.integer(20), tempfile())
    expect_equal(length(res[[1]]), 20)
    expect_equal(length(res[[2]]), 20)
  })

  it("If min_time is 0, runs for min_iterations", {
    res <- .Call(mark_, quote(1), new.env(), 0, as.integer(1), as.integer(10), tempfile())
    expect_equal(length(res[[1]]), 1)
    expect_equal(length(res[[2]]), 1)

    res <- .Call(mark_, quote(1), new.env(), 0, as.integer(5), as.integer(10), tempfile())
    expect_equal(length(res[[1]]), 5)
    expect_equal(length(res[[2]]), 5)
  })

  it("If min_time is 0, runs for min_iterations", {
    res <- .Call(mark_, quote({i <- 1; while(i < 10000) i <- i + 1}), new.env(), .1, as.integer(1), as.integer(1000), tempfile())
    expect_equal(length(res[[1]]), length(res[[2]]))

    expect_gt(length(res[[1]]), 1)
    expect_lt(length(res[[1]]), 1000)
  })

  it("Evaluates code in the environment", {
    e <- new.env(parent = baseenv())
    res <- .Call(mark_, quote({a <- 42}), e, Inf, as.integer(1), as.integer(1), tempfile())
    expect_equal(e[["a"]], 42)
  })

  it("captures stderr in results[[2]]", {
    res <- .Call(mark_, quote(message("hi")), new.env(), Inf, as.integer(1), as.integer(1), tempfile())
    expect_equal(e[["a"]], "hi\n")
  })
})
