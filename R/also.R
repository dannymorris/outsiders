ALSO <- function(data, method, cv=FALSE, folds=NULL, return_list=T, ...) {
    
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
    
    require(caret)
    
    # make data a data.frame
    data_df <- if (!('data.frame' %in% class(data))) {
        data.frame(data)
    } else data
    
    # characters to factors
    character_vars <- lapply(data_df, class) == "character"
    iris[, character_vars] <- lapply(iris[character_vars], as.factor)
    
    # prepare loop over all q variables
    n_cols <- ncol(data_df)
    
    # N x q error matrix
    error_matrix <- matrix(ncol = ncol(data_df),
                           nrow = nrow(data_df))
    
    # 1 x q rmse matrix
    rmse_mat <- matrix(ncol=ncol(data_df), nrow=1)
    
    # create variable-wise k-fold cv prediction error matrix
    if (cv==TRUE) {
        cv_error_mat <- matrix(ncol=1, nrow=nrow(data_df))
        if(is.null(folds)) {
            message("k_folds not supplided. default to 5")
            k_folds <- createFolds(1:nrow(data_df), k=5)
        } else {
            k_folds <- createFolds(1:nrow(data_df), k=folds)
        }
    }
    
    # loop through each col and predict from the rest
    for (i in 1:n_cols) {
        
        # if no CV
        if (cv==FALSE) {
            # prepare model df
            #X <- ifelse(n_cols == 2, data.frame(X=data_df[, -i]), data_df[,-i])
            if (n_cols == 2) {
                X <- data.frame(X=data_df[,-i])
                Y <- data.frame(Y=data_df[, i])
            } else {
                X <- data_df[,-i]
                Y <- data_df[, i]
            }
            #Y <- ifelse(n_cols == 2, data.frame(X=data_df[, i]), data_df[,i])
            model_df <- data.frame(X,Y)
            # prepare formula
            f <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
            # call method with formula and data_df args
            fit <- do.call(method, list(formula=f, data=model_df, ...))
            # get prediction errors
            if (class(Y) == 'factor') {
                sq_error <- (as.numeric(fit$predicted) - as.numeric(Y))^2
            } else {
                sq_error <- (predict(fit) - Y)^2
            }
            # population error matrix and rmse matrix
            error_matrix[,i] <- sq_error
            rmse_mat[,i] <- mean(sq_error, na.rm=T)
            
        } else {
            
            # CV
            # each observation gets out of sample prediction
            for (j in 1:length(k_folds)) {
                # prepare train and test splits
                if (n_cols == 2) {
                    X_train <- data.frame(X_train = data_df[-k_folds[[j]], -i])
                    Y_train <- data.frame(Y_train = data_df[-k_folds[[j]], i])
                    X_test <- data.frame(X_test = data_df[k_folds[[j]], -i])
                    Y_test <- data.frame(Y_test = data_df[k_folds[[j]], i])
                } else {
                    X_train <- data_df[-k_folds[[j]], -i]
                    Y_train <- data_df[-k_folds[[j]], i]
                    X_test <- data_df[k_folds[[j]], -i]
                    Y_test <- data_df[k_folds[[j]], i]
                }
                # X_train <- data.frame(data_df[-k_folds[[j]], -i])
                # Y_train <- data.frame(data_df[-k_folds[[j]], i])
                # X_test <- data.frame(data_df[k_folds[[j]], -i])
                # Y_test <- data.frame(data_df[k_folds[[j]], i])
                # prepare model df
                model_df <- data.frame(X_train, Y_train)
                # prepare formula
                f <- as.formula(paste("Y_train ~", paste(colnames(X_train), collapse="+")))
                # call method with formula and data arguments
                fit <- do.call(method, list(formula=f, data=model_df))
                # get out of sample predictions
                oos_predict <- predict(fit, newdata = X_test)
                if (class(Y_train) == 'factor') {
                    oos_sq_error <- (as.numeric(oos_predict) - as.numeric(Y_test))^2
                } else {
                    oos_sq_error <- (oos_predict - Y_test)^2
                }
                # populate cv prediction error
                cv_error_mat[k_folds[[j]],] <- oos_sq_error # get errors for jth variable
                
            }
            # population error matrix and rmse matrix
            error_matrix[,i] <- cv_error_mat
            rmse_mat[,i] <- mean(cv_error_mat)
        }
    }
    # adjusted rmse if > 1 then 1 else rmse
    bounded_rmse <- ifelse(rmse_mat > 1, 1, rmse_mat)
    # compute feature weights as 1-adjusted rmse
    feature_weights <- 1-bounded_rmse
    # compute outlier scores
    scores <- apply(error_matrix, 1, function(x) {
        sum(x*feature_weights, na.rm=T)
    })
    
    
    if (return_list == TRUE) {
        # list output
        list(scores=scores,
             error_matrix=error_matrix,
             rmse_mat=rmse_mat,
             weights=feature_weights)
    } else {
        scores
    }
}