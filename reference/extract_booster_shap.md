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
#> $ Area       <dbl> -0.017667860, 0.016070237, 0.003410609, -0.009702625, -0.02…
#> $ VehPower   <dbl> -9.513386e-03, -1.709264e-02, 1.250970e-02, 5.950155e-03, -…
#> $ VehAge     <dbl> -0.1181081235, -0.1903213263, 0.0632225871, 0.0886134803, 0…
#> $ DrivAge    <dbl> -0.0106765414, 0.0178347062, -0.0178089067, 0.1007688642, -…
#> $ BonusMalus <dbl> -0.009905483, 0.035999734, -0.025095381, 0.007291305, -0.03…
#> $ VehBrand   <dbl> -6.775799e-02, -7.489327e-02, -2.930630e-02, 4.647116e-02, …
#> $ VehGas     <dbl> 0.0016007025, 0.0040426101, 0.0229403824, -0.0065398184, 0.…
#> $ BIAS       <dbl> -0.03920801, -0.03920801, -0.03920801, -0.03920801, -0.0392…
```
