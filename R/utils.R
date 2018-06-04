
fast_mean <- function(x) {
    sum(x) / length(x)
    # x <- rnorm(1e6)
    # library(microbenchmark)
    # microbenchmark::microbenchmark(mean(x),
    #                                sum(x)/length(x))
}

