# Style Guide

The style guide is only slightly modified from Hadley Wickham's ![style guide](http://adv-r.had.co.nz/Style.html) in his Advanced R book. The modifications are presented here.

## Naming

### Variable and Function Names

Hadley's guide says to make all function and variable names lowercase, and that variable names should be nouns and function names verbs. The only modification is that function names can be nouns (e.g. `also()`) or verbs (e.g. `bag_features()`). Function names as nouns are acceptable under the following conditions:

- function name attempts for formalize a technique (e.g. `also()` for Attribute-wise Learning for Scoring Outliers)

- function output contains a list of results (e.g. outlier scores, error matrix, feature weights, etc.)

```
# Good
get_features()
also()
iso_tree()

# Bad
getFeatures()
ALSO()
```
