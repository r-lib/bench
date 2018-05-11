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

describe("mark", {
  it("Uses all.equal to check results by default", {
    res <- mark(1 + 1, 1L + 1L, check = NULL, iterations = 1)

    expect_is(res$result, "list")
    expect_true(all.equal(res$result[[1]], res$result[[2]]))
  })
  it("Can use other functions to check results like identical to check results", {

    # numerics and integers not identical
    expect_error(regexp = "All results must equal the first result",
      mark(1 + 1, 1L + 1L, check = identical, iterations = 1))

    # Function that always returns false
    expect_error(regexp = "All results must equal the first result",
      mark(1 + 1, 1 + 1, check = function(x, y) FALSE, iterations = 1))

    # Function that always returns true
    res <- mark(1 + 1, 1 + 2, check = function(x, y) TRUE, iterations = 1)

    expect_is(res$result, "list")
    expect_equal(res$result[[1]], 2)
    expect_equal(res$result[[2]], 3)

    # Using check = FALSE is equivalent
    res2 <- mark(1 + 1, 1 + 2, check = FALSE, iterations = 1)

    expect_is(res2$result, "list")
    expect_equal(res2$result[[1]], 2)
    expect_equal(res2$result[[2]], 3)
  })

  it("works with capabilities('profmem')", {
    skip_if_not(capabilities("profmem")[[1]])

    res <- mark(1, 2, check = NULL, iterations = 1)

    expect_equal(length(res$memory), 2)

    expect_is(res$memory[[1]], "Rprofmem")
    expect_equal(ncol(res$memory[[1]]), 3)
    expect_gte(nrow(res$memory[[1]]), 0)
  })

  it("works without capabilities('profmem')", {
    mockery::stub(mark, "capabilities", FALSE)

    res <- mark(1, 2, check = NULL, iterations = 1)

    expect_equal(length(res$memory), 2)

    expect_is(res$memory[[1]], "Rprofmem")
    expect_equal(ncol(res$memory[[1]]), 3)
    expect_equal(nrow(res$memory[[1]]), 0)
  })
})

describe("mark", {
  it("just runs mark if there are no parameters", {
    res <- mark(1, max_iterations = 10)
    expect_equal(colnames(res), c("expression", summary_cols, data_cols))
    expect_equal(nrow(res), 1)
  })
  it("Can handle `NULL` results", {
    res <- mark(if (FALSE) 1, max_iterations = 10)
    expect_equal(res$result[[1]], NULL)
  })
})

describe("summary.bench_mark", {
  it("computes relative summaries if called with relative = TRUE", {
    res <- mark(1+1, 2+0, max_iterations = 10)

    # remove memory columns, as there likely are no allocations or gc in these
    # benchmarks
    for (col in setdiff(summary_cols, c("mem_alloc", "n_gc"))) {

      # Absolute values should always be positive
      expect_true(all(res[[!!col]] >= 0))
    }

    # Relative values should always be greater than or equal to 1
    res2 <- summary(res, relative = TRUE)
    for (col in setdiff(summary_cols, c("mem_alloc", "n_gc"))) {
      expect_true(all(res2[[!!col]] >= 1))
    }
  })
  it("does not filter gc is `filter_gc` is FALSE", {
    # This should be enough allocations to trigger at least a few GCs
    res <- mark(1 + 1:1e6, iterations = 100)
    res2 <- summary(res, filter_gc = FALSE)

    expect_gt(res$n_gc, 0)
    expect_equal(res$n_gc, res2$n_gc)

    # The max should be higher with gc included
    expect_gt(res2$max, res$max)
  })
})

describe("unnest.bench_mark", {
  it("does not contain result or memory columns", {
    skip_if_not_installed("tidyr")
    bnch <- mark(1+1, 2+0)
    res <- tidyr::unnest(bnch)

    gc_cols <- colnames(bnch$gc[[1]])

    expected_cols <- c(setdiff(
        c("expression", summary_cols, data_cols, gc_cols),
        c("result", "memory", "gc")),
      "gc")
    expect_equal(colnames(res), expected_cols)

    expect_equal(nrow(res), length(bnch$time[[1]]) + length(bnch$time[[2]]))
  })
})
