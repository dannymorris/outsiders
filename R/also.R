#' Attribute-wise Learning for Scoring Outliers.
#' 
#' @param data a matrix or data.frame.
#' @param scale_numerics logical, if TRUE then center and scale numeric variables.
#' If FALSE then ignore scaling
#' @param method a function name in the form of a character string which is 
#' passed to formula (e.g. 'lm')
#' @param cross_validate logical indicating the use of cross validation for scoring
#' @param n_folds an integer specifying the number of folds if cross validating.
#' Defaults to 5
#' @param scores_only logical, if TRUE return outlier scores only. If FALSE
#' return a list with outlier scores and the error matrix
#' @param ... additional arguments passed to method 
#' @return If scores_only = TRUE, only outlier scores are returned. If FALSE, 
#' the function returns a list containing outlier scores, feature RMSE, feature weights,
#' raw prediction error matrix, squared prediction error matrix, and feature weighted squared
#' prediction error matrix
#' @references see "Outlier Analysis" (C.C Aggarwal. Springer, 2017) section 7.7
#' @examples
#' # OLS with cross validation for out of sample scoring
#' also(data = state.x77, scale_numerics = TRUE, method = 'lm', cross_validate = TRUE, 
#' n_folds = 10, scores_only = TRUE)
#' 
#' # random forest without cross validation#'  
#' rf_also <- also(data = scale(state.x77), scale_numerics = FALSE, method = 'randomForest', 
#' cross_validate = FALSE, scores_only = FALSE)
#' rf_also$scores; rf_also$feature_weights; rf_also$raw_error_matrix
#' @export
also <- function(data, scale_numerics = TRUE, method, cross_validate = TRUE, 
                 n_folds = 5, scores_only = TRUE, ...) {
    
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
    
    if (missing(method))
        stop("Please supply a base regressor/classifier e.g. 'lm' or 'randomForest'")

    # make data a tibble and convert characters to factor
    data_tbl <- make_tibble(data) %>%
        char_to_factor() %>%
        clear_colname_spaces() %>%
        mutate_if(is.numeric, 
                  funs(scale(., center = scale_numerics, scale = scale_numerics)))

    # prepare loop over all q variables
    n_cols <- ncol(data_tbl)
    
    # initialize empty N x q error matrix for squared errors
    raw_error_matrix <- matrix(ncol = ncol(data_tbl),
                              nrow = nrow(data_tbl))
    
    sq_error_matrix <- raw_error_matrix
    
    # init empty 1 x q rmse matrix
    rmse_mat <- matrix(ncol = ncol(data_tbl), nrow = 1)
    
    # create variable-wise k-fold cross_validate prediction error matrix
    if (cross_validate == TRUE) {
        cv_raw_error_mat <- matrix(ncol = 1, nrow = nrow(data_tbl))
        cv_sq_error_mat <- cv_raw_error_mat
        if (is.null(n_folds)) {
            message("n_folds not supplided. default to 5")
            folds <- caret::createFolds(1:nrow(data_tbl), k = 5)
        } else {
            folds <- caret::createFolds(1:nrow(data_tbl), k = n_folds)
        }
    }
    
    # loop through each col and predict from the rest
    for (i in 1:n_cols) {
        
        # if no cross_validate
        if (cross_validate == FALSE) {
            # prepare model df
            #X <- ifelse(n_cols == 2, data.frame(X=data_tbl[, -i]), data_tbl[,-i])
            X <- data_tbl[, -i]
            Y <- data_tbl[, i]
            model_tbl <- dplyr::bind_cols(X, Y)
            # prepare formula
            f <- as.formula(paste(colnames(Y),
                                  "~",
                                  paste(colnames(X), collapse="+")))
            # call method with formula and data_tbl args
            fit <- do.call(method, list(formula = f, data = model_tbl))
            
            # get predictions from fit
            fit_predict <- predict(fit)
            
            # raw prediction errors
            raw_error <- dplyr::pull(Y) - fit_predict
            
            # square fit prediction errors
            sq_error <- square_errors(predictions = fit_predict,
                                      actual = dplyr::pull(Y))
            
            # populate error matrix and rmse matrix
            sq_error_matrix[, i] <- sq_error
            raw_error_matrix[, i] <- raw_error
            rmse_mat[, i] <- mean(sq_error, na.rm = TRUE)
            
        } else {
            
            # cross_validate
            # each observation gets out of sample prediction
            for (j in 1:length(folds)) {
                # prepare train and test splits
                # if (n_cols == 2) {
                X_train <- data_tbl[-folds[[j]], -i]
                Y_train <- data_tbl[-folds[[j]], i]
                X_test <- data_tbl[folds[[j]], -i]
                Y_test <- data_tbl[folds[[j]], i]
                
                # prepare rain/test dfs
                train_tbl <- dplyr::bind_cols(X_train, Y_train)
                test_tbl <- dplyr::bind_cols(X_test, Y_test)
                
                # prepare formula
                f <- as.formula(paste(colnames(Y_train),
                                      "~",
                                      paste(colnames(X_train), collapse="+")))
                # call method with formula and data arguments
                fit <- do.call(method, list(formula = f, data = train_tbl))
                # get out of sample predictions
                # oos_predict <- predict(fit, newdata = test_tbl)
                # if (class(pull(Y_train)) == 'factor') {
                #     oos_sq_error <- (as.numeric(oos_predict) - 
                #                          as.numeric(dplyr::pull(Y_test)))^2
                # } else {
                #     oos_sq_error <- (oos_predict - dplyr::pull(Y_test))^2
                # }
                # 
                oos_predict <- predict(fit, newdata = test_tbl)
                oos_raw_error <- dplyr::pull(Y_test) - oos_predict
                oos_sq_error <- square_errors(predictions = oos_predict, 
                                              actual = dplyr::pull(Y_test))
                
                cv_raw_error_mat[folds[[j]], ] <- oos_raw_error
                # populate cross_validate squared prediction error
                cv_sq_error_mat[folds[[j]], ] <- oos_sq_error # get errors for jth variable
            }
            
            # populatie error matrix and rmse matrix
            raw_error_matrix[, i] <- cv_raw_error_mat
            sq_error_matrix[, i] <- cv_sq_error_mat
            rmse_mat[, i] <- fast_mean(cv_sq_error_mat)
        }
    }
    
    # adjusted rmse if > 1 then 1 else rmse
    bounded_rmse <- ifelse(rmse_mat > 1, 1, rmse_mat)
    # compute feature weights as 1-adjusted rmse
    feature_weights <- 1 - bounded_rmse
    # multiple rows in error matrix by feature weights
    feature_weighted_sq_error_matrix <- sweep(sq_error_matrix, 2, feature_weights, "*") 
    # compute outlier scores
    scores <- rowSums(feature_weighted_sq_error_matrix)
    
    if (scores_only == TRUE) {
        return(scores)
        # list output
    } else {
        return(list(scores = scores,
                    feature_rmse = rmse_mat,
                    feature_weights = feature_weights,
                    raw_error_matrix = raw_error_matrix,
                    sq_error_matrix = sq_error_matrix,
                    feature_weighted_sq_error_matrix = feature_weighted_sq_error_matrix))    
        }
}
