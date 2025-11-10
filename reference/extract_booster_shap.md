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
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

booster_shap <- extract_booster_shap(iblm_model$booster_model, df_list$test)

booster_shap |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 8
#> $ Area       <dbl> -0.0111900549, 0.0001318646, 0.0035772556, 0.0050815106, -0…
#> $ VehPower   <dbl> -0.0270258002, -0.0082273316, 0.0158687718, 0.0093539460, 0…
#> $ VehAge     <dbl> -0.210502356, -0.041103296, 0.074962787, 0.080654249, 0.071…
#> $ DrivAge    <dbl> 0.003348402, 0.254643559, -0.033149377, 0.101194337, -0.040…
#> $ BonusMalus <dbl> -0.020024695, 0.107680462, -0.057291888, -0.034046866, -0.0…
#> $ VehBrand   <dbl> -0.095233344, -0.106390789, -0.017026167, -0.018466003, -0.…
#> $ VehGas     <dbl> 0.0066829044, 0.0579791106, 0.0230292529, -0.0055609480, 0.…
#> $ BIAS       <dbl> -0.05418567, -0.05418567, -0.05418567, -0.05418567, -0.0541…
```
