dat <- data.frame(x = runif(100, 1, 1000), y=runif(10, 1, 1000))
bench::mark(
  min_time = .1,
  dat[dat$x > 500, ],
  dat[which(dat$x > 500), ],
  subset(dat, x > 500)
)
