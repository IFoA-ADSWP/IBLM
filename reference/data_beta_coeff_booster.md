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
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
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
#> Columns: 8
#> $ bias       <dbl> -0.04750277, -0.10246549, -0.03115642, -0.05418567, -0.0303…
#> $ Area       <dbl> -0.0111900549, 0.0000000000, 0.0035772556, 0.0050815106, -0…
#> $ VehPower   <dbl> -0.0024568909, -0.0016454663, 0.0026447953, 0.0018707892, 0…
#> $ VehAge     <dbl> -0.2105023563, -0.0411032960, 0.0093703484, 0.0057610178, 0…
#> $ DrivAge    <dbl> 5.773106e-05, 1.018574e-02, -7.892709e-04, 1.405477e-03, -8…
#> $ BonusMalus <dbl> -0.0004004939, 0.0011334786, -0.0011458378, -0.0006809373, …
#> $ VehBrand   <dbl> -0.095233344, 0.000000000, -0.017026167, -0.018466003, -0.0…
#> $ VehGas     <dbl> 0.0000000000, 0.0000000000, 0.0000000000, -0.0055609480, 0.…
```
