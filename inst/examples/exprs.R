x <- 1:1000
evens <- x %% 2 == 0
y <- x[evens]
length(y)
length(which(evens))
sum(evens)
