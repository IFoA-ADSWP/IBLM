# Convert Shap values to Wide One-Hot Encoded Format

Transforms categorical variables in a data frame into one-hot encoded
format. Renames "BIAS" to lowercase.

## Usage

``` r
shap_to_onehot(shap, wide_input_frame, iblm_model)
```

## Arguments

- shap:

  Data frame containing raw SHAP values from XGBoost.

- wide_input_frame:

  Wide format input data frame (one-hot encoded).

- iblm_model:

  Object of class 'iblm'

## Value

A data frame where SHAP values are in wide format for categorical
variables. Column "bias" is moved to start.

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

shap_wide |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 24
#> $ bias          <dbl> -0.05418567, -0.05418567, -0.05418567, -0.05418567, -0.0…
#> $ VehPower      <dbl> -0.0270258002, -0.0082273316, 0.0158687718, 0.0093539460…
#> $ VehAge        <dbl> -0.210502356, -0.041103296, 0.074962787, 0.080654249, 0.…
#> $ DrivAge       <dbl> 0.003348402, 0.254643559, -0.033149377, 0.101194337, -0.…
#> $ BonusMalus    <dbl> -0.020024695, 0.107680462, -0.057291888, -0.034046866, -…
#> $ AreaA         <dbl> 0.0000000000, 0.0001318646, 0.0000000000, 0.0000000000, …
#> $ AreaB         <dbl> 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.00…
#> $ AreaC         <dbl> 0.0000000000, 0.0000000000, 0.0000000000, 0.0050815106, …
#> $ AreaD         <dbl> 0.000000000, 0.000000000, 0.003577256, 0.000000000, 0.00…
#> $ AreaE         <dbl> -0.01119005, 0.00000000, 0.00000000, 0.00000000, 0.00000…
#> $ AreaF         <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.000000…
#> $ VehBrandB1    <dbl> 0.000000000, -0.106390789, 0.000000000, 0.000000000, 0.0…
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
#> $ VehGasDiesel  <dbl> 0.0066829044, 0.0579791106, 0.0230292529, 0.0000000000, …
#> $ VehGasRegular <dbl> 0.0000000000, 0.0000000000, 0.0000000000, -0.0055609480,…
```
