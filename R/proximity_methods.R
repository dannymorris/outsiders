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
    
    dmat <- if (class(distance_matrix) != "matrix") {
        as.matrix(distance_matrix)
    } 
    
    #dimnames(dmat) <- list(id_var, id_var)
    pairwise_distances <- data_frame(N1 = rownames(dmat)[col(dmat)],
                                     N2 = colnames(dmat)[row(dmat)],
                                     distance = c(dmat)) %>%
        filter(N1 != N2)

    output <- pairwise_distances %>%
        group_by(N1) %>%
        arrange(distance) %>%
        mutate(knn = row_number()) %>%
        filter(knn %in% k) %>%
        arrange(N1, knn) %>%
        ungroup()
    
    return(output)
}
