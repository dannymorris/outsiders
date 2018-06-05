#' Chi-Squared Coordinates for Multivariate Normality
#' 
#' @param data a matrix or data.frame.
#' @return 
#' @examples
#' @export
#' 
chisq_mvn <- function(data) {
    
    if (!is.matrix(data) && !is.data.frame(data)) 
        stop("Data must be a matrix or data frame.")
    
    data_mat <- if (!is.matrix(data)) {
        as.matrix(data)
    } else data
    
    n_rows <- nrow(data_mat)
    n_cols <- ncol(data_mat)
    xbar <- apply(data_mat, 2, fast_mean)
    
    covariance_matrix <- var(data_mat)
    lapack <- solve(covariance_matrix)
    
    index <- (1:n_rows) / (n_rows + 1)
    centered_data_mat <- t(t(data_mat) - xbar)
    
    di <- apply(centered_data_mat, 1, function(x,lapack) x %*% lapack %*% x,lapack)
    quant <- qchisq(index, n_cols)
    
    quantiles <- data_frame(observed = sort(di), 
                            theoretical = quant)
    return(quantiles)
}
