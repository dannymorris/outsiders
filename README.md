# outsiders

`outsiders` is an R package for multidimensional outlier analysis and anomaly detection. Many of the techniques available in this package are inspired by *Outlier Analysis* (C. C. Aggarwal. Springer, 2017.). 

# Installation

`devtools::install_github("dannymorris/outsiders")` for quick and easy installation

# Featured Methods

## `ALSO()`

Attribute-wise Learning for Scores Outliers (ALSO) combines supervised and unsupervised outlier detection to locate outliers among a set of correlated features. The technique assumes correlated features can be used to predict one another, and data points are considered outliers if they deviate significantly from the correlation structure. The predictability of a feature determines its weight in the scoring process. Features that can be predicted with high accuracy are weighted more heavily than unpredictable features. Weights are defined as 1 - min(1, RMSE). If the root mean squared error (RMSE) of a feature model exceeds 1, the weight of the dependent feature defaults to 0 since the RMSE of a predictive model which only predicts the mean of the dependent variable for all data points is always 1. Otherwise, the feature weight is the RMSE of its own model. Final scoring is accomplished by summing the weighed scores. Extreme-value analysis can then be applied to locate outliers that deviate from the correlation structure of the data set. 

**Example Usage**

`outsiders::also(data = state.x77, scale_numerics = TRUE, method = lm, cross_validate = TRUE, n_folds = 10, scores_only = TRUE)`
