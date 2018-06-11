
dist_to_centers <- function(data, labels, centers) {
    
    # append labels to data
    if (!is.null(missing)) {
        data_tbl <- make_tibble(data) %>% mutate(labels = labels)
    } else {
        data_tbl <- make_tibble(data) %>% mutate(labels = as.integer(1))
    }

    # check that centers is a data frame or matrix
    check_df_mat(data = centers,
                 error_msg = "centers must be a data frame or matrix with a number 
                 of columns equal to ncol(data)")
    
    if (length(unique(data_tbl$labels)) != nrow(centers))
        tidy_error_message("number of distinct labels should match number of
                           rows in centers")

    #data_tbl <- make_tibble(data)
    x2c_dist_mat <- matrix(nrow = nrow(data_tbl), 
                           ncol = nrow(centers))
    
    # for reach cluster i
    for (i in 1:nrow(centers)) {
        # get data for cluster i
        data_i <- data_tbl %>%
            filter(labels == i) %>%
            select(-labels)
        
        # convert cluster i mean vector to numeric
        center_i <- centers[i, ] %>% as.numeric()
        
        # get covariance matrix of cluster i data
        cov_i <- cov(data_i)
        
        # mahalanobis distance of Ni to center_i
        mahal <- mahalanobis(data, center = center_i, cov = cov_i)
        
        x2c_dist_mat[, i] <- mahal
    }
    
    return(x2c_dist_mat)
    
}

## Mahalanobis distance of observation to cluster centroid (e.g. mean) ##

## Working ##

# data <- scale(state.x77) %>%
#     as_tibble()
# 
# # get distance matrix and k clusters
# dmat <- dist(data)
# hclust_ward <- hclust(dmat, method = 'ward.D2')
# k_clusters <- cutree(hclust_ward, k=3)
# 
# # add cluster labels and state names
# data_labeled <- data %>%
#     mutate(cluster = k_clusters) %>%
#     mutate(state = rownames(state.x77))
# 
# # cluster mean vecotrs
# clus_mean_vecs <- data_labeled %>%
#     group_by(cluster) %>%
#     select(-state) %>%
#     summarise_all(mean)
# 
# x2c_dist_mat <- matrix(nrow = nrow(data), ncol = nrow(clus_mean_vecs))
# 
# # for reach cluster i
# for (i in 1:nrow(clus_mean_vecs)) {
#     # get data for cluster i
#     data_i <- data_labeled %>%
#         filter(cluster == i) %>%
#         select(-cluster, -state)
#     
#     # convert cluster i mean vector to numeric
#     center_i <- clus_mean_vecs[i, -1] %>% as.numeric()
#     
#     # get covariance matrix of cluster i data
#     cov_i <- cov(data_i)
#     
#     # mahalanobis distance of Ni to center_i
#     mahal <- mahalanobis(data, center = center_i, cov = cov_i)
#     
#     # squared distance of nij to mean vector i
#     # to_center_i <- sweep(data, 2, center_i, "-")^2
#     # 
#     # # normalize nij by dividing by std dev of data for cluster i
#     # norm_dist_ij <- sweep(to_center_i, 2, apply(data_i, 2, sd) %>% as.numeric(), "/")
#     # 
#     # # sum
#     # sumrow_i <- rowSums(norm_dist_ij)
#     
#     
#     x2c_dist_mat[, i] <- mahal
# }
# 
# apply(x2c_dist_mat, 1, min)
