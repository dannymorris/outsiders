
fast_mean <- function(x) {
    sum(x) / length(x)
    # x <- rnorm(1e6)
    # library(microbenchmark)
    # microbenchmark::microbenchmark(mean(x),
    #                                sum(x)/length(x))
}

square_errors <- function(predictions, actual) {
    
    # inputs must be vectors
    # if (!is.vector(predictions) && !is.vector(actual)) {
    #     stop("arguments need to be vectors")
    # }
    
    if ("character" %in% sapply(list(predictions, actual), class))
        stop("arguments should be of class numeric or factor")
    
    # if 
    if (class(predictions) == "factor" | class(actual) == "factor") {
        squared_errors <- (as.numeric(predictions) - as.numeric(actual))^2
    } else {
        squared_errors <- (predictions - actual)^2
    }
}


count_spaces <- function(string) { 
    sapply(gregexpr(" ", string), function(x) { sum(x >= 0) } ) 
}

clear_colname_spaces <- function(data, substitute = "") {
    colnames(data) <- gsub(" ", substitute, colnames(data))
    data
}

make_tibble <- function(data) {
    
    if ("tbl" %in% class(data)) {
        data
    } else {
        dplyr::as_tibble(data)
    }
}

char_to_factor <- function(data) {
    
    # covert to tibble
    data_tbl <- make_tibble(data) %>%
        dplyr::mutate_if(is.character, as.factor)

    return(data_tbl)
}

