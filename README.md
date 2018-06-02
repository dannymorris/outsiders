# outsiders

`outsiders` is an R package for outlier analysis and anomaly detection. Most of the current functions are intended for use with multidimensional data sets.

## `ALSO()`

Attribute-wise Learning for Scores Outliers (ALSO) is an unsupervised outlier detection technique for locating outliers among a set of correlated numeric observations. The technique assumes that correlated features can be used to predict one another, and data points are considered outliers if they deviate significantly from the correlation structure. The predictability of a feature determines its weight in the scoring process. Features that can be predicted with high accuracy are weighted more heavily than unpredictable features. Weights are defined as 1 - min(1, RMSE). If the root mean squared error (RMSE) of a feature model exceeds 1, the weight of the dependent feature defaults to 0 since the RMSE of a predictive model which only predicts the mean of the dependent variable for all data points is always 1. Otherwise, the feature weight is the RMSE of its own model. Final scoring is accomplished by summing the weighed scores. Extreme-value analysis can then be applied to locate outliers that deviate from the correlation structure of the data set. 
