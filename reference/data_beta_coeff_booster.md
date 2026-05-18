# Obtain Booster Model Beta Corrections for tabular data

Creates dataframe of Shap beta corrections for each row and predictor
variable of \`data\`

## Usage

``` r
data_beta_coeff_booster(data, beta_corrections, iblm_model)
```

## Arguments

- data:

  A data frame containing the dataset for analysis

- beta_corrections:

  A data frame or matrix containing beta correction values for all
  variables and bias

- iblm_model:

  Object of class 'iblm'

## Value

A data frame with beta coefficient corrections. The structure will be
the same dimension as \`data\` except for a "bias" column at the start.

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

explainer_outputs <- explain_iblm(iblm_model, df_list$test)

data_booster <- data_beta_coeff_booster(
  df_list$test,
  explainer_outputs$beta_corrections,
  iblm_model
)

data_booster |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 7
#> $ bias       <dbl> -0.03010190, -0.03010190, -0.03010190, -0.03010190, -0.0301…
#> $ Area       <dbl> -4.487922e-03, 1.437818e-01, 1.612203e-02, -2.331074e-03, 1…
#> $ BonusMalus <dbl> -1.425127e-04, 9.072397e-04, -8.565751e-04, -1.274934e-03, …
#> $ DrivAge    <dbl> -7.341999e-03, 3.301016e-03, 2.897860e-03, 3.001865e-03, 3.…
#> $ VehAge     <dbl> 0.011503229, -0.270234734, 0.014044377, 0.009927072, 0.0065…
#> $ VehBrand   <dbl> -0.071816981, -0.153895795, 0.011852440, -0.030659221, -0.0…
#> $ VehPower   <dbl> -0.0023957209, -0.0059312492, 0.0037696779, 0.0034830798, 0…
```
