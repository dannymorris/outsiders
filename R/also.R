#' Attribute-wise Learning for Scoring Outliers.
#' 
#' @param data a matrix or data.frame.
#' @param method a function name in the form of a character string which is 
#' passed to formula (e.g. 'lm')
#' @param cv logical indicating the use of cross validation for scoring
#' @param folds an integer specifying the number of folds if cv = TRUE
#' @param return_list logical, if TRUE return list of output (see details). If 
#' FALSE return outlier scores only
#' @param ... additional arguments passed to method formula
#' @return If return_list = TRUE, the function returns a list containing outlier
#' scores, the error matrix, the rmse_matrix, and feature weights. If return_list
#' = FALSE, outlier scores only are returned.
#' @examples
#' # Build ncol(data) linear regression models. In each iteration, a new feature
#' # is treated as the target and the remaining features are the predictors. With 
#' # cv == TRUE, 10-fold cross validation is applied in each iteration. All points are 
#' # therefore given an out of sample score.
#' 
#' also(data = scale(state.x77), method = 'lm', cv = TRUE, folds = 10, 
#'      return_list = TRUE)
#' 
#' # Outlier scores from ncol(data) random forests
#'  
#' also(data = scale(state.x77), method = 'randomForest', cv = FALSE, 
#'      return_list = FALSE)
#' @export
also <- function(data, method, cv = FALSE, folds = NULL, 
                 return_list = TRUE, ...) {
    
    # Attribute-wise Learning for Scores Outliers (ALSO) is an unsupervised
    # outlier detection technique for locating outliers among a set of correlated
    # observations. The technique assumes that correlated features can be used to
    # predict one another in an attempt to locate points that deviate from the correlation
    # structure. The predictability of a feature determines its weight in the scoring process.
    # Features that can be predicted with high accuracy are weighted more heavily than
    # features with low predictability. Weights can be defined as 1 - min(1, RMSE).
    # If the root mean squared error (RMSE) of a feature model exceeds 1, the weight of the
    # dependent feature defaults to 0 since the RMSE for a predictive model which only
    # predicts the mean of the dependent variable for all data points is always 1.
    # Otherwise, the feature weight is the RMSE of its own model. Final scoring is
    # accomplished by summing the weighed scores. Extreme-value analysis can then
    # be applied to locate outliers that deviate from the correlation structure of
    # the data set.

    # make data a data.frame
    data_df <- dplyr::as_tibble(data)
    original_colnames <- colnames(data_df)
    colnames(data_df) <- gsub(" ", "", colnames(data_df))
    
    # characters to factors
    character_vars <- lapply(data_df, class) == "character"
    data_df[, character_vars] <- lapply(data_df[character_vars], as.factor)
    
    # prepare loop over all q variables
    n_cols <- ncol(data_df)
    
    # initialize empty N x q error matrix
    error_matrix <- matrix(ncol = ncol(data_df),
                           nrow = nrow(data_df))
    
    # init empty 1 x q rmse matrix
    rmse_mat <- matrix(ncol = ncol(data_df), nrow = 1)
    
    # create variable-wise k-fold cv prediction error matrix
    if (cv == TRUE) {
        cv_error_mat <- matrix(ncol = 1, nrow = nrow(data_df))
        if (is.null(folds)) {
            message("k_folds not supplided. default to 5")
            k_folds <- caret::createFolds(1:nrow(data_df), k = 5)
        } else {
            k_folds <- caret::createFolds(1:nrow(data_df), k = folds)
        }
    }
    
    # loop through each col and predict from the rest
    for (i in 1:n_cols) {
        
        # if no CV
        if (cv == FALSE) {
            # prepare model df
            #X <- ifelse(n_cols == 2, data.frame(X=data_df[, -i]), data_df[,-i])
            X <- data_df[, -i]
            Y <- data_df[, i]
            model_df <- dplyr::bind_cols(X, Y)
            # prepare formula
            f <- as.formula(paste(colnames(Y),
                                  "~",
                                  paste(colnames(X), collapse="+")))
            # call method with formula and data_df args
            fit <- do.call(method, list(formula = f, data = model_df))
            # get prediction errors
            fit_predict <- predict(fit)
            if (class(pull(Y)) == 'factor') {
                sq_error <- (as.numeric(oos_predict) - as.numeric(dplyr::pull(Y)))^2
            } else {
            sq_error <- (fit_predict - dplyr::pull(Y))^2
            }
            # population error matrix and rmse matrix
            error_matrix[, i] <- sq_error
            rmse_mat[, i] <- mean(sq_error, na.rm = TRUE)
            
        } else {
            
            # CV
            # each observation gets out of sample prediction
            for (j in 1:length(k_folds)) {
                # prepare train and test splits
                # if (n_cols == 2) {
                X_train <- data_df[-k_folds[[j]], -i]
                Y_train <- data_df[-k_folds[[j]], i]
                X_test <- data_df[k_folds[[j]], -i]
                Y_test <- data_df[k_folds[[j]], i]
                
                # prepare rain/test dfs
                model_df <- dplyr::bind_cols(X_train, Y_train)
                test_df <- dplyr::bind_cols(X_test, Y_test)
                
                # prepare formula
                f <- as.formula(paste(colnames(Y_train),
                                      "~",
                                      paste(colnames(X_train), collapse="+"))                    )
                # call method with formula and data arguments
                fit <- do.call(method, list(formula = f, data = model_df))
                # get out of sample predictions
                oos_predict <- predict(fit, newdata = test_df)
                if (class(pull(Y_train)) == 'factor') {
                    oos_sq_error <- (as.numeric(oos_predict) - 
                                         as.numeric(dplyr::pull(Y_test)))^2
                } else {
                    oos_sq_error <- (oos_predict - dplyr::pull(Y_test))^2
                }
                # populate cv prediction error
                cv_error_mat[k_folds[[j]], ] <- oos_sq_error # get errors for jth variable
            }
            
            # population error matrix and rmse matrix
            error_matrix[, i] <- cv_error_mat
            rmse_mat[, i] <- fast_mean(cv_error_mat)
        }
    }
    
    # adjusted rmse if > 1 then 1 else rmse
    bounded_rmse <- ifelse(rmse_mat > 1, 1, rmse_mat)
    # compute feature weights as 1-adjusted rmse
    feature_weights <- 1 - bounded_rmse
    # compute outlier scores
    scores <- apply(error_matrix, 1, function(x) {
        sum(x * feature_weights, na.rm = TRUE)
    })
    
    if (return_list == TRUE) {
        # list output
        return(
            list(scores = scores,
                 error_matrix = error_matrix,
                 rmse_mat = rmse_mat,
                 weights = feature_weights)
        )
    } else {
        return(scores)
    }
}
