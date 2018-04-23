context("test-mark.R")

describe("mark_", {
  it("If min_time is Inf, runs for max_iterations", {
    res <- .Call(mark_, quote(1), new.env(), Inf, as.integer(0), as.integer(10))
    expect_equal(length(res), 10)

    res <- .Call(mark_, quote(1), new.env(), Inf, as.integer(0), as.integer(20))
    expect_equal(length(res), 20)
  })

  it("If min_time is 0, runs for min_iterations", {
    res <- .Call(mark_, quote(1), new.env(), 0, as.integer(1), as.integer(10))
    expect_equal(length(res), 1)

    res <- .Call(mark_, quote(1), new.env(), 0, as.integer(5), as.integer(10))
    expect_equal(length(res), 5)
  })

  it("If min_time is 0, runs for min_iterations", {
    res <- .Call(mark_, quote({i <- 1; while(i < 10000) i <- i + 1}), new.env(), .1, as.integer(1), as.integer(1000))

    expect_gte(length(res), 1)
    expect_lte(length(res), 1000)
  })

  it("Evaluates code in the environment", {
    e <- new.env(parent = baseenv())
    res <- .Call(mark_, quote({a <- 42}), e, Inf, as.integer(1), as.integer(1))
    expect_equal(e[["a"]], 42)
  })
})

describe("mark_internal", {
  it("Uses all.equal to check results by default", {
    res <- mark_internal(1 + 1, 1L + 1L, check = NULL,
      setup = NULL, env = new.env(), min_time = Inf,
      min_iterations = as.integer(1), max_iterations = as.integer(1))

    expect_is(res$result, "list")
    expect_true(all.equal(res$result[[1]], res$result[[2]]))
  })
  it("Can use other functions to check results like identical to check results", {

    # numerics and integers not identical
    expect_error(regexp = "All results must equal the first result",
      mark_internal(1 + 1, 1L + 1L, check = identical,
        setup = NULL, env = new.env(), min_time = Inf,
        min_iterations = as.integer(1), max_iterations = as.integer(1)))

    # Function that always returns false
    expect_error(regexp = "All results must equal the first result",
      mark_internal(1 + 1, 1 + 1, check = function(x, y) FALSE,
        setup = NULL, env = new.env(), min_time = Inf,
        min_iterations = as.integer(1), max_iterations = as.integer(1)))

    # Function that always returns true
    res <- mark_internal(1 + 1, 1 + 2, check = function(x, y) TRUE,
      setup = NULL, env = new.env(), min_time = Inf,
      min_iterations = as.integer(1), max_iterations = as.integer(1))

    expect_is(res$result, "list")
    expect_equal(res$result[[1]], 2)
    expect_equal(res$result[[2]], 3)

    # Using check = FALSE is equivalent
    res2 <- mark_internal(1 + 1, 1 + 2, check = FALSE,
      setup = NULL, env = new.env(), min_time = Inf,
      min_iterations = as.integer(1), max_iterations = as.integer(1))

    expect_is(res2$result, "list")
    expect_equal(res2$result[[1]], 2)
    expect_equal(res2$result[[2]], 3)
  })

  it("works with capabilities('profmem')", {
    skip_if_not(capabilities("profmem")[[1]])

    res <- mark_internal(1, 2, check = NULL,
      setup = NULL, env = new.env(), min_time = Inf,
      min_iterations = as.integer(1), max_iterations = as.integer(1))

    expect_equal(length(res$memory), 2)

    expect_is(res$memory[[1]], "Rprofmem")
    expect_equal(ncol(res$memory[[1]]), 3)
    expect_gte(nrow(res$memory[[1]]), 1)
  })

  it("works without capabilities('profmem')", {
    mockery::stub(mark_internal, "capabilities", FALSE)

    res <- mark_internal(1, 2, check = NULL,
      setup = NULL, env = new.env(), min_time = Inf,
      min_iterations = as.integer(1), max_iterations = as.integer(1))

    expect_equal(length(res$memory), 2)

    expect_is(res$memory[[1]], "Rprofmem")
    expect_equal(ncol(res$memory[[1]]), 3)
    expect_equal(nrow(res$memory[[1]]), 0)
  })
})
