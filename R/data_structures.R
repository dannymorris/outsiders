#' Categorical Data Matrix to One-Hot Binary Matrix
#' 
#' @param data a matrix or data.frame of categorical variables as character 
#' strings or factors.
#' @param normalize logical, if FALSE then binary matrix is returned. If TRUE, then
#' normalization (see details) is applied to each binary transformed variable.
#' @return A transformed matrix is returned. 
#' @details The normalization technique is taken from Outlier Analysis (Aggarwal, 2017),
#' section 8.3. For each column j in the binary transformed matrix, a normalization 
#' factor is defined as sqrt(ni \* pj \* (1-pj)), where ni is the number of distinct
#' categories in the reference variable from the raw data set and pj is the proportion
#' of records taking the value of 1 for the jth variable 
#' @examples
#' x <- data.frame(gender = sample(c("male", "female"), 15, T),
#'                 age_cat = sample(c("young", "old", "unknown"), 15, T)) 
#' one_hot(data = x, normalize = TRUE)
one_hot <- function(data, normalize = FALSE) {
    
    # ensure that data is either data.frame or matrix
    if (!(any(c("data.frame", "matrix") %in% class(data)))) 
        stop("Data must be a data.frame or matrix")
    
    # ensure no numeric column classes in data
    if (any(c("numeric", "integer") %in% apply(data, 2, class)))
        stop("All variables must be character strings or factors")
    
    # work with a matrix for speed
    data_mat <- if (class(data) != "matrix") {
        as.matrix(data)
    } else data
    
    # set default column names if missing
    # column names appear in output as prefix to column name (e.g X1_male, X1_female, ...)
    if (is.null(colnames(data))) {
        warning("Column names missing from data. Defaults to X[i]")
        colnames(data_mat) <- paste0("X", 1:ncol(data_mat))
    }
    
    # if (is.null(colnames(data_mat))) {
    #     colnames(data_mat) <- paste0("X", 1:ncol(data_mat))
    # }
    
    # each attribute binary representation will be added to list
    # binary reps are column bound at end of function
    init_list <- list()
    
    # loop through each column in data and get binary representation
    for (i in 1:ncol(data_mat)) {
        # get column i
        ith_var <- data_mat[, i]
        # prepare a binary matrix for i with number of columns equal to
        # number of distinct categories for i
        ith_var_binary_mat <- matrix(ncol = length(unique(ith_var)),
                                     nrow = nrow(data_mat))
        
        # set arbitrary column names for ith binary matrix
        # these are eventually replaced by names of categories
        colnames(ith_var_binary_mat) <- paste0("V", 1:ncol(ith_var_binary_mat))
        
        # loop through each column in the ith binary representation
        # compare values in ith variable to column name of ith binary rep
        ### if matching then 1 else 0
        for (j in 1:ncol(ith_var_binary_mat)) {
            # prepare a binary vector for jth column in ith binary matrix
            binary_vec <- numeric()
            # match ith variable to jth category of i
            # if match then 1 else 0 
            for (k in 1:length(ith_var)) {
                binary_vec[k] <- ifelse(ith_var[k] == unique(ith_var)[j], 1, 0)
            }
            # populate jth column of binary matrix with jth binary vector
            ith_var_binary_mat[, j] <- binary_vec
            
            # set column name for jth column of binary matrix
            jth_column_name <- paste0(colnames(data_mat)[i], "_", unique(ith_var)[j])
            colnames(ith_var_binary_mat)[j] <- jth_column_name
        }
        
        # update list with ith binary representation of j categories
        init_list[[i]] <- ith_var_binary_mat
    }
    
    init_list <- if (normalize == TRUE) {
        fij <- function(x) {
            sum(x) / length(x)
        }
        
        lapply(init_list, function(x) {
            # ni
            ni <- ncol(x)
            # fij of each column in the ith binary matrix
            col_means <- apply(x, 2, function(y) fij(y))
            # normalization factors
            norm <- sqrt(ni * col_means * (1 - col_means))
            # divide each column in ith binary matrix by corresponding norm factor
            x / norm
        })
    } else init_list
    
    # column bind list elements
    output <- do.call("cbind", init_list)
    return(output)
}

data <- data.frame(gender = sample(c("male", "female", "unknown"), 150, T),
                   race = sample(c("black", "white"), 150, T))

data 

i <- 1
ni <- length(unique(data[, i]))
one_hot_veci <- one_hot(data)[,i]
fij <- sum(one_hot_veci)/length(one_hot_veci)
sqrt(ni * fij * (1-fij))
