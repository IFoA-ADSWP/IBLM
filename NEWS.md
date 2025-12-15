# IBLM 1.0.2

## Major changes

* `iblm` class model objects now require factor variables for categorical features. Users should explicitly convert character columns to factors before training.

## New features

* Added "IBLM" vignette providing a comprehensive walkthrough of package functionality.

## Bug fixes

* Fixed bug where all variable types were converted to numeric before training and predicting of booster model, which could lead to improper handling of categorical features.

## Internal changes

* Updated to maintain compatibility with xgboost v3.1.2.1. Package now requires xgboost >= 3.1.2.1.

# IBLM 1.0.0

* Initial CRAN submission.
