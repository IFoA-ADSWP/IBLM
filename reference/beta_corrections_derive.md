# Compute Beta Corrections based on SHAP values

Processes SHAP values in one-hot (wide) format to create beta
coefficient corrections.

This includes:

- scaling shap values of continuous variables by the predictor value for
  that row

- migrating shap values to the bias for continuous variables where the
  predictor value was zero

- migrating shap values to the bias for categorical variables where the
  predictor value was reference level

## Usage

``` r
beta_corrections_derive(
  shap_wide,
  wide_input_frame,
  iblm_model,
  migrate_reference_to_bias = TRUE
)
```

## Arguments

- shap_wide:

  Data frame containing SHAP values from XGBoost that have been
  converted to wide format by \[shap_to_onehot()\]

- wide_input_frame:

  Wide format input data frame (one-hot encoded).

- iblm_model:

  Object of class 'iblm'

- migrate_reference_to_bias:

  Logical, migrate the beta corrections to the bias for reference
  levels? This applied to categorical vars only. It is recommended to
  leave this setting on TRUE

## Value

A data frame with the booster model beta corrections in one-hot (wide)
format

## Examples

``` r
df_list <- freMTPLmini |>
  dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
  split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimNb",
  offset_var = "LogExposure",
  family = "poisson"
)

shap <- extract_booster_shap(iblm_model$booster_model, df_list$test)

wide_input_frame <- data_to_onehot(df_list$test, iblm_model)

shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)

beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model)

beta_corrections |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 17
#> $ bias        <dbl> -0.03010190, -0.03010190, -0.03010190, -0.03010190, -0.030…
#> $ BonusMalus  <dbl> -1.425127e-04, 9.072397e-04, -8.565751e-04, -1.274934e-03,…
#> $ DrivAge     <dbl> -7.341999e-03, 3.301016e-03, 2.897860e-03, 3.001865e-03, 3…
#> $ VehAge      <dbl> 0.011503229, -0.270234734, 0.014044377, 0.009927072, 0.006…
#> $ VehPower    <dbl> -0.0023957209, -0.0059312492, 0.0037696779, 0.0034830798, …
#> $ AreaA       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ AreaB       <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000…
#> $ AreaC       <dbl> 0.000000000, 0.143781751, 0.000000000, 0.000000000, 0.0000…
#> $ AreaD       <dbl> -0.0044879219, 0.0000000000, 0.0000000000, -0.0023310741, …
#> $ AreaE       <dbl> 0.000000e+00, 0.000000e+00, 1.612203e-02, 0.000000e+00, 1.…
#> $ VehBrandB1  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ VehBrandB12 <dbl> 0.000000000, -0.153895795, 0.000000000, 0.000000000, -0.02…
#> $ VehBrandB2  <dbl> 0.00000000, 0.00000000, 0.01185244, -0.03065922, 0.0000000…
#> $ VehBrandB3  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000…
#> $ VehBrandB4  <dbl> -0.07181698, 0.00000000, 0.00000000, 0.00000000, 0.0000000…
#> $ VehBrandB5  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000…
#> $ VehBrandB6  <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0…
```
