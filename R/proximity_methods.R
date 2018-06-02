nearest_neighbors <- function(data, d=NULL, ids, k) {
    
    require(dplyr)
    
    if (missing(ids))
        stop('"ids" is missing. provide a string of row labels')
    
    if (missing(k)) {
        k <- 1
        warning('"k" is missing. defaults to 1')
    }
    
    if (!is.null(d)) {
        data <- NULL
        dmat <- as.matrix(d)
    }
    
    if (!is.null(data)) {
        if (!all(apply(data, 2, class) == 'numeric'))
            stop("all columns of the data set must be numeric")
        
        d <- dist(data)
        dmat <- as.matrix(d)
    }
    
    dimnames(dmat) <- list(ids, ids)
    pairs_key <- t(combn(colnames(dmat), 2))
    pairwise_d <- data.frame(col=colnames(dmat)[col(dmat)],
                             row=rownames(dmat)[row(dmat)],
                             distance=c(dmat))
    pairwise_d %>%
        tbl_df() %>%
        filter(col != row) %>%
        mutate_at(vars(1,2), as.character) %>%
        group_by(col) %>%
        arrange(col, distance) %>%
        mutate(knn = row_number()) %>%
        filter(knn %in% k)
}
