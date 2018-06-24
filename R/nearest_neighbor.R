#' Unsupervised K-Nearest Neighbors
#' @param distance_matrix A distance matrix
#' @param k Value for k. Either an integer or sequence of integers
#' @return A data.frame
#' @details A distance matrix (either a full square matrix or a "dist" object) is
#' required as input.
#' @examples my_dist <- dist(scale(state.x77))
#' nearest_neighbors(distance_matrix = my_dist, k = 1:3)
#' @export
#' 
nearest_neighbors <- function(distance_matrix,  k) {
    
    if (missing(distance_matrix)) 
        stop("Distance matrix required.")
    
    if (class(distance_matrix) != "dist" && nrow(distance_matrix) != ncol(distance_matrix))
        stop("Distance matrix not a square matrix.")
    
    if (missing(k)) {
        k <- 1
        warning('k not specified. Defaults to 1.')
    }
    
    dmat <- make_matrix(distance_matrix)
    
    # pairwise distance
    pairwise_distances <- tibble(N1 = as.integer(rownames(dmat)[col(dmat)]),
                                 N2 = as.integer(colnames(dmat)[row(dmat)]),
                                 distance = c(dmat)) %>%
        filter(N1 != N2) 
        
    
    output <- pairwise_distances %>%
        group_by(N1) %>%
        arrange(N1, distance) %>%
        mutate(knn = row_number()) %>%
        filter(knn %in% k) %>%
        ungroup()
    
    return(output)
}


#' Exact K-NN
#' @param distance_matrix A matrix or data frame or distance matrix
#' @param k An integer specifying value for K
#' @return A numeric vector
#' @examples exact_knn(iris[, -5], 3) # 3-NN
#' @export
#' 
exact_knn <- function(data, k = 1) {
    
    if (class(data) == "dist") {
        score <- nearest_neighbors(data, k = k) %>%
            pull(distance)
    } else {
        score <- nearest_neighbors(dist(data), k = k) %>%
            pull(distance)
    }

    return(score)
}

#' Exact K-NN
#' @param distance_matrix A matrix or data frame or distance matrix
#' @param fun A function to aggregate distances
#' @param k An integer specifying value for K
#' @return A numeric vector
#' @examples aggregate_knn(iris[, -5], mean, 3) # average 3-NN
#' aggregate_knn(dist(iris[, -5]), median, 5)
#' @export
#' 
aggregate_knn <- function(data, fun, k) {
    
    fun_call <- function(vector, ...) {
        do.call(fun, list(vector))
    }
    
    if (class(data) == "dist") {
        nn <- nearest_neighbors(data, k = 1:k)
    } else {
        nn <- nearest_neighbors(dist(data), k = 1:k)
    }
    
    score <- nn %>%
        group_by(N1) %>%
        summarise(
            agg_d = fun_call(distance)
        ) %>%
        pull(agg_d)
    
    return(score)
}


