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

shap_wide |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 17
#> $ bias        <dbl> -0.0301019, -0.0301019, -0.0301019, -0.0301019, -0.0301019…
#> $ BonusMalus  <dbl> -0.009690866, 0.072579175, -0.042828754, -0.063746683, 0.0…
#> $ DrivAge     <dbl> -0.227601975, 0.191458955, 0.113016523, 0.114070870, 0.160…
#> $ VehAge      <dbl> 0.11503229, -0.27023473, 0.05617751, 0.11912487, 0.0260591…
#> $ VehPower    <dbl> -0.016770046, -0.041518744, 0.018848389, 0.013932319, 0.02…
#> $ AreaA       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ AreaB       <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000…
#> $ AreaC       <dbl> 0.000000000, 0.143781751, 0.000000000, 0.000000000, 0.0000…
#> $ AreaD       <dbl> -0.0044879219, 0.0000000000, 0.0000000000, -0.0023310741, …
#> $ AreaE       <dbl> 0.000000e+00, 0.000000e+00, 1.612203e-02, 0.000000e+00, 1.…
#> $ VehBrandB1  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000…
#> $ VehBrandB12 <dbl> 0.000000000, -0.153895795, 0.000000000, 0.000000000, -0.02…
#> $ VehBrandB2  <dbl> 0.00000000, 0.00000000, 0.01185244, -0.03065922, 0.0000000…
#> $ VehBrandB3  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000…
#> $ VehBrandB4  <dbl> -0.07181698, 0.00000000, 0.00000000, 0.00000000, 0.0000000…
#> $ VehBrandB5  <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000…
#> $ VehBrandB6  <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0…
```
