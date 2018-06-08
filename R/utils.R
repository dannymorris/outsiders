
fast_mean <- function(x) {
    sum(x) / length(x)
    # x <- rnorm(1e6)
    # library(microbenchmark)
    # microbenchmark::microbenchmark(mean(x),
    #                                sum(x)/length(x))
}

square_errors <- function(model_object, newdata, actual) {
    predictions <- predict(model_object, newdata = as.data.frame(newdata))
    squared_errors <- (predictions - actual)^2
    return(squared_errors)
}
