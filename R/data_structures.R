one_hot <- function(data) {
    
    if (!(TRUE %in% c(c("data.frame", "matrix") %in% class(data))))
        stop("Data must be a data.frame or matrix")
    
    if (TRUE %in% c(c("numeric", "integer") %in% lapply(data, class)))
        stop("All variables must be character strings or factors")
    
    if (is.null(colnames(data))) {
        message("Column names missing from data. Defaults to X[i]")
    }
    
    data_df <- if (!('data.frame' %in% class(data))) {
        data.frame(data)
    } else data
    
    distinct_categories <- sum(apply(data_df, 2, function(x) {
        length(unique(x))
    }))
    
    init_df <- data.frame(row_id = 1:nrow(data_df))
    
    for (i in 1:ncol(data_df)) {
        ith_var <- data_df[, i]
        ith_var_binary_df <- as.data.frame(
            matrix(ncol = length(unique(ith_var)),
                   nrow = nrow(data_df)
            )
        )
        
        for (j in 1:ncol(ith_var_binary_df)) {
            binary_vec <- numeric()
            for (k in 1:length(ith_var)) {
                binary_vec[k] <- ifelse(ith_var[k] == unique(ith_var)[j], 1, 0)
            }
            #vals <- ifelse(ith_var == unique(ith_var)[i], 1, 0)
            ith_var_binary_df[, j] <- binary_vec
            
            column_name <- paste0(colnames(data_df)[i], "_", unique(ith_var)[j])
            colnames(ith_var_binary_df)[j] <- column_name
        }
        
        init_df <- bind_cols(init_df, ith_var_binary_df)
    }
    
    init_df$row_id <- NULL
    return(init_df)
}