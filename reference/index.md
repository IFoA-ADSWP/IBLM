# Package index

## Train

### Train iblm

- [`train_iblm_xgb()`](https://ifoa-adswp.github.io/IBLM/reference/train_iblm_xgb.md)
  : Train IBLM Model on XGBoost

### Train xgboost

- [`train_xgb_as_per_iblm()`](https://ifoa-adswp.github.io/IBLM/reference/train_xgb_as_per_iblm.md)
  : Train XGBoost Model Using the IBLM Model Parameters

## Explain

### Main Explanation

- [`explain_iblm()`](https://ifoa-adswp.github.io/IBLM/reference/explain_iblm.md)
  : Explain GLM Model Predictions Using SHAP Values

### Plotting Functions

- [`beta_corrected_density()`](https://ifoa-adswp.github.io/IBLM/reference/beta_corrected_density.md)
  : Density Plot of Beta Corrections for a Variable
- [`beta_corrected_scatter()`](https://ifoa-adswp.github.io/IBLM/reference/beta_corrected_scatter.md)
  : Scatter Plot of Beta Corrections for a Variable
- [`bias_density()`](https://ifoa-adswp.github.io/IBLM/reference/bias_density.md)
  : Density Plot of Bias Corrections from SHAP values
- [`overall_correction()`](https://ifoa-adswp.github.io/IBLM/reference/overall_correction.md)
  : Plot Overall Corrections from Booster Component
- [`correction_corridor()`](https://ifoa-adswp.github.io/IBLM/reference/correction_corridor.md)
  : Plot GLM vs IBLM Predictions with Different Corridors

### Create Plotting Functions

- [`create_beta_corrected_density()`](https://ifoa-adswp.github.io/IBLM/reference/create_beta_corrected_density.md)
  : Create Pre-Configured Beta Corrected Density Plot Function
- [`create_beta_corrected_scatter()`](https://ifoa-adswp.github.io/IBLM/reference/create_beta_corrected_scatter.md)
  : Create Pre-Configured Beta Corrected Scatter Plot Function
- [`create_bias_density()`](https://ifoa-adswp.github.io/IBLM/reference/create_bias_density.md)
  : Create Pre-Configured Bias Density Plot Function
- [`create_overall_correction()`](https://ifoa-adswp.github.io/IBLM/reference/create_overall_correction.md)
  : Create Pre-Configured Overall Correction Plot Function

### Data Transformation

- [`extract_booster_shap()`](https://ifoa-adswp.github.io/IBLM/reference/extract_booster_shap.md)
  : Extract SHAP values from an xgboost Booster model
- [`data_to_onehot()`](https://ifoa-adswp.github.io/IBLM/reference/data_to_onehot.md)
  : Convert Data Frame to Wide One-Hot Encoded Format
- [`shap_to_onehot()`](https://ifoa-adswp.github.io/IBLM/reference/shap_to_onehot.md)
  : Convert Shap values to Wide One-Hot Encoded Format
- [`beta_corrections_derive()`](https://ifoa-adswp.github.io/IBLM/reference/beta_corrections_derive.md)
  : Compute Beta Corrections based on SHAP values
- [`data_beta_coeff_booster()`](https://ifoa-adswp.github.io/IBLM/reference/data_beta_coeff_booster.md)
  : Obtain Booster Model Beta Corrections for tabular data
- [`data_beta_coeff_glm()`](https://ifoa-adswp.github.io/IBLM/reference/data_beta_coeff_glm.md)
  : Obtain GLM Beta Coefficients for tabular data

## Predict

- [`predict(`*`<iblm>`*`)`](https://ifoa-adswp.github.io/IBLM/reference/predict.iblm.md)
  : Predict Method for IBLM
- [`get_pinball_scores()`](https://ifoa-adswp.github.io/IBLM/reference/get_pinball_scores.md)
  : Calculate Pinball Scores for IBLM and Additional Models

## Data

- [`freMTPLmini`](https://ifoa-adswp.github.io/IBLM/reference/freMTPLmini.md)
  : French Motor Insurance Claims Dataset
- [`load_freMTPL2freq()`](https://ifoa-adswp.github.io/IBLM/reference/load_freMTPL2freq.md)
  : Load French Motor Third-Party Liability Frequency Dataset

## Other

### Prepare

- [`split_into_train_validate_test()`](https://ifoa-adswp.github.io/IBLM/reference/split_into_train_validate_test.md)
  : Split Dataframe into: 'train', 'validate', 'test'

### Theme

- [`theme_iblm()`](https://ifoa-adswp.github.io/IBLM/reference/theme_iblm.md)
  : Custom ggplot2 Theme for IBLM

### Check

- [`check_iblm_model()`](https://ifoa-adswp.github.io/IBLM/reference/check_iblm_model.md)
  : Check Object of Class \`iblm\`
