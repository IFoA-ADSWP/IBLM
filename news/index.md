# Changelog

## IBLM 2.0.1

### Major changes

- train_iblm_xgb() methodology changed. The booster model component is
  now trained differently, using glm predictions as an offset for
  xgb.train() instead of the ratios method employed in versions ≤ 1.0.3.
- Structure of data object freMTPLmini changed. Output of
  load_freMTPL2freq() also changed accordingly.

### New features

- The IBLM model may now be trained using a weighting variable and/or an
  offset variable. More information is available at
  <https://ifoa-adswp.github.io/IBLM/articles/IBLM.html#offsetting-and-weighting>.

## IBLM 1.0.2

CRAN release: 2025-12-16

### Major changes

- `iblm` class model objects now require factor variables for
  categorical features. Users should explicitly convert character
  columns to factors before training.

### New features

- Added “IBLM” vignette providing a comprehensive walkthrough of package
  functionality.

### Bug fixes

- Fixed bug where all variable types were converted to numeric before
  training and predicting of booster model, which could lead to improper
  handling of categorical features.

### Internal changes

- Updated to maintain compatibility with xgboost v3.1.2.1. Package now
  requires xgboost \>= 3.1.2.1.

## IBLM 1.0.0

- Initial CRAN submission.
