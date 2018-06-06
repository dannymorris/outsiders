#' Regression Ensemble
#' 
#' @param formula a formula interface specifying the model (see help("lm") for more detail)
#' @param data a matrix or data.frame containing variables in model
#' @param method a model function (e.g. "lm", "randomForest"). Functions in packages 
#' outside of the base R distribution are accessed by loading the necessary package
#' @param n_components an integer specifying the number of components in the ensemble
#' @param n_train_points an integer or numeric value specifying the number of rows
#' used in the training phase of each ensemble
#' @param error_agg_fun a function for combining the squared prediction errors 
#' (.e.g. "mean", mean, median)
#' @param scores_only logical, if TRUE return a vector of outlier scores. If FALSE,
#' return the error matrix and outlier scores
#' @examples
#' ensemble_lm(formula = Sepal.Length ~ ., data = iris[,-5],
#' n_components=100, n_train_points=100, error_agg_fun = median, 
#' scores_only = T)#' 
#' @export
regression_ensemble <- function(formula, data, method = 'lm', n_components, n_train_points, 
                                error_agg_fun, scores_only = TRUE, ...) {

    # ensure that data is either data.frame or matrix
    if (!(any(c("data.frame", "matrix") %in% class(data)))) 
        stop("Data must be a data frame, or matrix")
    
    # work with a matrix for speed
    data_mat <- if (!("matrix" %in% class(data))) {
        as.matrix(data)
    } else data
    
    #formula <- as.formula(formula)
    
    error_matrix <- matrix(ncol = n_components,
                          nrow = nrow(data_mat))
    
    for (i in 1:n_components) {
        train_index <- sample(1:nrow(data_mat), n_train_points, FALSE)
        training_set <- data_mat[train_index, ]
        #component_lm <- lm(Sepal.Width ~ Sepal.Length, data = as.data.frame(training_set))
        component_lm <- do.call(method, 
                                args = list(formula = formula, 
                                            data = as.data.frame(training_set)))
        predictions <- predict(component_lm, newdata = as.data.frame(data_mat))
        squared_errors <- (predictions - data_mat[, as.character(formula[2])])^2
        error_matrix[, i] <- squared_errors
    }
    
    scores <- apply(error_matrix, 1, error_agg_fun, na.action = na.omit)
    
    #return(scores)
    # 
    if (scores_only == TRUE) {
        return(scores)
    } else {
        output <- list(error_matrix = error_matrix,
                       scores = scores)
        return(output)
    }
}

