#' PCA Rotated Bagging
#' 
#' @param data a matrix or data.frame.
#' @return 
#' @examples
#' @export
#' 
pca_bag <- function(data, scale=T, n_features=floor(sqrt(ncol(data))),
                    n_iterations=50, method) {
    
    # PCA rotated feature bagging is an ensemble-centric, unsupervised outlier detection
    # technique for locating outliers in feature subspaces in high dimensional data. The
    # algorithm is provided a defined number of iterations, a random subset of features
    # is obtained, rotated via principal components analysis and retaining all components, 
    # and final outlier scores are combined by summing scores from individual iterations.
    
    # Subspace outlier detection methods work by locating outliers in locally relevant
    # feature sets in high dimensional data.
    
    if (missing(method)) {
        stop("A base detector method must be specified. For example, try
             method = function(x) mahalanobis(x, center=colMeans(x), cov=cov(x))
             for computing the Mahalanobis distances in each iteration")
    }
    
    if (missing(n_iterations)) {
        message("Message: Number of iterations defaults to 50")
    }
    
    if (missing(n_subsets)) {
        message("Message: Number of features in each subsample defaults to the
                square root of the number of columns rounded down to the nearest integer")
    }
    
    data_df <- if (!('data.frame' %in% class(data))) {
        data.frame(data)
    } else data
    
    r <- n_subsets
    m <- n_iterations
    
    score_matrix <- matrix(nrow=nrow(data_df), ncol=m)
    
    # iterate over the features m times and subset r subsets of features
    # rotate each subsample with PCA, retaining all components
    # score the data points with a method applied to pca rotated subsample
    # insert subsamples scores into score matrix
    for (i in 1:m) {
        features_index <- sample(1:ncol(data_df), r, F)
        data_subset <- data_df[,features_index]
        data_subset_pca <- princomp(data_subset)$scores
        score <- method(data_subset_pca)
        score_matrix[,i] <- score
    }
    
    if (scale == TRUE) {
        score_matrix <- scale(score_matrix)
    } else {
        score_matrix
    }
    
    combined_errors <- apply(score_matrix, 1, sum)
    
    return(combined_errors)
    
    }