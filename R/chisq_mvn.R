#' Compute multivariate Mahalanobis distances and corresponding chi-squared quantiles
#' to assess multivariate normality
#' 
#' @param data a numeric matrix or data.frame
#' @return a Nx2 tibble with observed Mahalanobis distances and ordered chi-sq
#' quantiles 
#' @details This function is inspired by a discussion of multivariate normality in
#' "An R and S-Plus Companion to Multivariate Analysis" (Everitt, 2005)
#' @examples
#' mvn_coords <- chisq_mvn(iris[, -5])
#' plot(distances ~ chisq, data = mvn_coords)
#' abline(lm(distances ~ chisq, data = mvn_coords))
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
    chisq_quant <- qchisq(index, n_cols)
    
    output <- data_frame(distances = sort(di), 
                         chisq = chisq_quant)
    return(output)
}
