#' Return point-wise k nearest neighbor
#' @param distance_matrix A distance matrix
#' @param k An integer or sequence of integers specifying the desired nearest neighbors
#' @return An N x k tibble with ordered pairwise distances
#' @details A distance matrix (either a full square matrix or a "dist" object) is
#' required as input.
#' @examples my_dist <- dist(scale(state.x77))
#' nearest_neighbors(distance_matrix = my_dist, k = 1:3)
#' @export
#' 
nearest_neighbors <- function(distance_matrix, k) {
    
    if (!class(distance_matrix) %in% c("dist", "matrix", "data.frame")) {
        stop("distance_matrix must be a dist object or a square matrix or data.frame")
    }
    
    if (class(distance_matrix) != "dist" && nrow(distance_matrix) != ncol(distance_matrix))
        stop("distance_matrix not a square matrix.")
    
    if (missing(k)) {
        k <- 1
        warning('k not specified. Defaults to 1.')
    }
    
    dmat <- make_matrix(distance_matrix)
    
    pairwise_distances <- tibble(N1 = as.integer(rownames(dmat)[col(dmat)]),
                                 N2 = as.integer(colnames(dmat)[row(dmat)]),
                                 distance = c(dmat)) %>%
        filter(N1 != N2)

    output_tbl <- pairwise_distances %>%
        group_by(N1) %>%
        arrange(N1, distance) %>%
        mutate(knn = row_number()) %>%
        filter(knn %in% k) %>%
        ungroup()
    
    return(output_tbl)
}


#' Compute exact k nearest neighbor distance
#' @param data A numeric matrix, data frame, or distance matrix
#' @param k An integer specifying the k nearest neighbor
#' @return A numeric vector
#' @examples exact_knn(iris[, -5], 3) # 3-nn
#' @export
#' 
exact_knn <- function(data, k = 1) {
    
    if ("dist" %in% class(data)) {
        score <- nearest_neighbors(data, k = k) %>%
            dplyr::pull(distance)
    } else {
        warning("Data not a distance matrix. Euclidean distance matrix computed by default.")
        score <- nearest_neighbors(dist(data), k = k) %>%
            dplyr::pull(distance)
    }

    return(score)
}

#' Aggregate point-wise distances over k nearest neighbors
#' @param data A numeric matrix, data frame or distance matrix
#' @param fun A function to aggregate point-wise distances
#' @param k An integer specifying number of nearest neighbors used to compute aggregation
#' @return A numeric vector
#' @examples aggregate_knn(iris[, -5], mean, 3) # average 3-nn
#' aggregate_knn(dist(iris[, -5]), max, 5) # equivalent to exact 5-nn
#' @export
#' 
aggregate_knn <- function(data, fun, k) {
    
    fun_call <- function(vector, ...) {
        do.call(fun, list(vector))
    }
    
    if ("dist" %in% class(data)) {
        nn <- nearest_neighbors(data, k = 1:k)
    } else {
        warning("Data not a distance matrix. Euclidean distance matrix computed by default.")
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

