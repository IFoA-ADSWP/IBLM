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
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

shap <- stats::predict(
  iblm_model$booster_model,
  newdata = xgboost::xgb.DMatrix(
    data.matrix(
      dplyr::select(df_list$test, -dplyr::all_of(iblm_model$response_var))
    )
  ),
  predcontrib = TRUE
) |> data.frame()

wide_input_frame <- data_to_onehot(df_list$test, iblm_model)

shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)

beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model)

beta_corrections |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 24
#> $ bias          <dbl> -0.04750277, -0.10246549, -0.03115642, -0.05418567, -0.0…
#> $ VehPower      <dbl> -0.0024568909, -0.0016454663, 0.0026447953, 0.0018707892…
#> $ VehAge        <dbl> -0.2105023563, -0.0411032960, 0.0093703484, 0.0057610178…
#> $ DrivAge       <dbl> 5.773106e-05, 1.018574e-02, -7.892709e-04, 1.405477e-03,…
#> $ BonusMalus    <dbl> -0.0004004939, 0.0011334786, -0.0011458378, -0.000680937…
#> $ AreaA         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ AreaB         <dbl> 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.00…
#> $ AreaC         <dbl> 0.0000000000, 0.0000000000, 0.0000000000, 0.0050815106, …
#> $ AreaD         <dbl> 0.000000000, 0.000000000, 0.003577256, 0.000000000, 0.00…
#> $ AreaE         <dbl> -0.01119005, 0.00000000, 0.00000000, 0.00000000, 0.00000…
#> $ AreaF         <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.000000…
#> $ VehBrandB1    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB10   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB11   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB12   <dbl> -0.095233344, 0.000000000, 0.000000000, 0.000000000, 0.0…
#> $ VehBrandB13   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB14   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB2    <dbl> 0.000000000, 0.000000000, 0.000000000, 0.000000000, -0.0…
#> $ VehBrandB3    <dbl> 0.00000000, 0.00000000, -0.01702617, 0.00000000, 0.00000…
#> $ VehBrandB4    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB5    <dbl> 0.000000, 0.000000, 0.000000, -0.018466, 0.000000, 0.000…
#> $ VehBrandB6    <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.000000…
#> $ VehGasDiesel  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehGasRegular <dbl> 0.0000000000, 0.0000000000, 0.0000000000, -0.0055609480,…
```
