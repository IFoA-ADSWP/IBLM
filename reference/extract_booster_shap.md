# Extract SHAP values from an xgboost Booster model

A function to extract SHAP (SHapley Additive exPlanations) values from
fitted booster model

## Usage

``` r
extract_booster_shap(booster_model, data, ...)

# S3 method for class 'xgb.Booster'
extract_booster_shap(booster_model, data, ...)
```

## Arguments

- booster_model:

  A model object. In the IBLM context it will be the "booster_model"
  item from an object of class "iblm"

- data:

  A data frame containing the predictor variables. Note anything extra
  will be quietly dropped.

- ...:

  Additional arguments passed to methods.

## Value

A data frame of SHAP values, where each column corresponds to a feature
and each row corresponds to an observation.

## Details

Currently only a booster_model of class \`xgb.Booster\` is supported

## Methods (by class)

- `extract_booster_shap(xgb.Booster)`: Extract SHAP values from an
  \`xgb.Booster\` model

## See also

\[xgboost::predict.xgb.Booster()\]

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

booster_shap <- extract_booster_shap(iblm_model$booster_model, df_list$test)

booster_shap |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 7
#> $ Area       <dbl> -4.487922e-03, 1.437818e-01, 1.612203e-02, -2.331074e-03, 1…
#> $ BonusMalus <dbl> -0.009690866, 0.072579175, -0.042828754, -0.063746683, 0.03…
#> $ DrivAge    <dbl> -0.227601975, 0.191458955, 0.113016523, 0.114070870, 0.1605…
#> $ VehAge     <dbl> 0.11503229, -0.27023473, 0.05617751, 0.11912487, 0.02605914…
#> $ VehBrand   <dbl> -0.071816981, -0.153895795, 0.011852440, -0.030659221, -0.0…
#> $ VehPower   <dbl> -0.016770046, -0.041518744, 0.018848389, 0.013932319, 0.024…
#> $ BIAS       <dbl> -0.0301019, -0.0301019, -0.0301019, -0.0301019, -0.0301019,…
```
