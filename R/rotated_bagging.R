#' PCA Rotated Bagging
#' 
#' @param data a matrix or data.frame.
#' @return 
#' @examples
#' @export
#' 
pca_bag <- function(data, n_features = floor(sqrt(ncol(data))),
                    n_iterations = 50, outlier_fun, scores_fun = sum,
                    scores_only = TRUE) {
    
    # PCA rotated feature bagging is an ensemble-centric, unsupervised outlier detection
    # technique for locating outliers in feature subspaces in high dimensional data. The
    # algorithm is provided a defined number of iterations, a random subset of features
    # is obtained, rotated via principal components analysis with all components retained, 
    # and final outlier scores are combined by summing scores from individual iterations.
    
    # Subspace outlier detection methods work by locating outliers in locally relevant
    # feature sets in high dimensional data.
    
    if (missing(outlier_fun)) {
        stop("A base detector method must be specified. For example, try
             method = function(x) mahalanobis(x, center=colMeans(x), cov=cov(x))
             for computing the Mahalanobis distances in each iteration")
    }
    
    if (missing(n_iterations)) {
        warning("n_iterations not specified. Defaults to 50")
    }
    
    if (missing(n_features)) {
        warnings("n_features not specified. Defaults to the square root of the number of 
                  original features rounded down to the nearest integer")
    }
    
    data_tbl <- make_tibble(data)
    
    score_matrix <- matrix(nrow = nrow(data_tbl), 
                           ncol = n_iterations)
    
    # iterate over the features m times and subset r subsets of features
    # rotate each subsample with PCA, retaining all components
    # score the data points with a method applied to pca rotated subsample
    # insert subsamples scores into score matrix
    for (i in 1:n_iterations) {
        features_index <- sample(1:ncol(data_tbl), n_features, F)
        data_subset <- data_tbl[, features_index]
        pca_scores <- princomp(data_subset)$scores
        outlier_score <- do.call(outlier_fun, list(pca_scores))
        score_matrix[, i] <- outlier_score
    }
    
    scores <- apply(score_matrix, 1, scores_fun, na.rm = T)
    
    if (scores_only == TRUE) {
        return(scores)
    } else {
        output_ls <- list(score_matrix = score_matrix, 
                          scores = scores)
        return(output_ls)
    }
}
