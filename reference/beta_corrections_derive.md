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

shap <- extract_booster_shap(iblm_model$booster_model, df_list$test)

wide_input_frame <- data_to_onehot(df_list$test, iblm_model)

shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)

beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model)

beta_corrections |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 24
#> $ bias          <dbl> -0.037607307, -0.093988436, -0.016267627, -0.039208010, …
#> $ VehPower      <dbl> -8.648532e-04, -3.418528e-03, 2.084949e-03, 1.190031e-03…
#> $ VehAge        <dbl> -1.181081e-01, -1.903213e-01, 7.902823e-03, 6.329534e-03…
#> $ DrivAge       <dbl> -1.840783e-04, 7.133882e-04, -4.240216e-04, 1.399568e-03…
#> $ BonusMalus    <dbl> -1.981097e-04, 3.789446e-04, -5.019076e-04, 1.458261e-04…
#> $ AreaA         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ AreaB         <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.000000…
#> $ AreaC         <dbl> 0.000000000, 0.000000000, 0.000000000, -0.009702625, -0.…
#> $ AreaD         <dbl> 0.000000000, 0.000000000, 0.003410609, 0.000000000, 0.00…
#> $ AreaE         <dbl> -0.017667860, 0.000000000, 0.000000000, 0.000000000, 0.0…
#> $ AreaF         <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0…
#> $ VehBrandB1    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB10   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB11   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB12   <dbl> -0.06775799, 0.00000000, 0.00000000, 0.00000000, 0.00000…
#> $ VehBrandB13   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB14   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB2    <dbl> 0.000000e+00, 0.000000e+00, 0.000000e+00, 0.000000e+00, …
#> $ VehBrandB3    <dbl> 0.0000000, 0.0000000, -0.0293063, 0.0000000, 0.0000000, …
#> $ VehBrandB4    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB5    <dbl> 0.00000000, 0.00000000, 0.00000000, 0.04647116, 0.000000…
#> $ VehBrandB6    <dbl> 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.00…
#> $ VehGasDiesel  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehGasRegular <dbl> 0.000000000, 0.000000000, 0.000000000, -0.006539818, 0.0…
```
