#' Dependent Variable Regression Ensemble
#' 
#' @param formula a formula interface specifying the model (see help("lm") for more detail)
#' @param data a matrix or data.frame containing variables in model
#' @param method a model function (e.g. "lm", "randomForest")
#' @param n_predictions an integer specifying the number of components in the ensemble.
#' If score_set = "test", set this high enough to ensure all points are predicted
#' a sufficient number of times
#' @param n_train_points an integer or numeric value specifying the number of rows
#' used in the training phase of each ensemble
#' @param error_agg_fun a function for combining the squared prediction errors.
#' Defaults to mean.
#' @param score_set one of "all" or "test". If "all", scores all N points in each
#' training iteration. If "test", score out of sample points in each iteration.
#' @param scores_only logical, if TRUE return a vector of outlier scores. If FALSE,
#' return the error matrix and outlier scores
#' @return if scores_only = TRUE, a vector of outlier scores. If FALSE, a list
#' with outlier scores and the ensemble error matrix
#' @references section 3.2.1 of "Outlier Analysis" (C. C. Aggarwal. Outlier Analyis. 
#' Springer, 2017.)
#' @examples
#' ensemble_lm(formula = Sepal.Length ~ ., data = iris[,-5],
#' n_predictions=100, n_train_points=100, error_agg_fun = median, 
#' scores_only = T)#' 
#' @export
dvr_ensemble <- function(formula, data, method = 'lm', 
                         n_predictions, n_train_points, 
                         score_set = c("all", "test"), error_agg_fun = mean,
                         scores_only = TRUE, ...) {
    
    

    # ensure that data is either data.frame or matrix
    if (!(any(c("data.frame", "matrix") %in% class(data)))) 
        stop("Data must be a data frame, or matrix")
    
    # default behavior is to score all N points from training model
    if (length(score_set) == 2) {
        warning("Score set not specified. Defaults to all")
    }
    
    # work with a matrix for speed
    data_mat <- if (!("matrix" %in% class(data))) {
        as.matrix(data)
    } else data
    
    # prepare N x n_predictions error matrix
    error_matrix <- matrix(ncol = n_predictions,
                           nrow = nrow(data_mat))
    
    # ensemble
    for (i in 1:n_predictions) {
        
        # define a random sample of training points
        # n_train_points can be an integer or a vector from sample()
        random_train_index <- sample(1:nrow(data_mat), n_train_points, FALSE)
        training_set <- data_mat[random_train_index, ]
        
        # prepare ith error vector
        # if score_set = "test", entries corresponding to the train index remain NA
        error_vector <- rep(NA, nrow(data))
        
        # train model
        fit <- do.call(method, 
                       args = list(formula = formula,
                                   data = as.data.frame(training_set)))
        
        # score all N points if score_set = "all" or if unspecified
        # score only test points if score_set = "test"
        if (score_set == "all" || length(score_set) == 2) {
            # square_errors() in utils.R
            # populate error vector with squared errors
            predictions <- predict(fit, as.data.frame(data_mat))
            error_vector <- square_errors(predictions = predictions,
                                          actual = data_mat[, as.character(formula[2])])
        } else {
            # prepare test set
            testing_set <- data_mat[-random_train_index, ]
            predictions <- predict(fit, as.data.frame(testing_set))
            error_vector[-random_train_index] <- 
                square_errors(predictions = predictions,
                              actual = testing_set[, as.character(formula[2])])
            
        } 
        
        # overwrite ith column of error matrix with ith error vector
        error_matrix[, i] <- error_vector
    }    
    
    # aggregate rows of error matrix
    scores <- apply(error_matrix, 1, error_agg_fun, na.rm = TRUE)
    
    # define output
    if (scores_only == TRUE) {
        return(scores)
    } else {
        output <- list(error_matrix = error_matrix,
                       scores = scores)
        return(output)
    }
}

