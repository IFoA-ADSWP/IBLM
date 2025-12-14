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
#> $ bias       <dbl> -0.037607307, -0.093988436, -0.016267627, -0.039208010, -0.…
#> $ Area       <dbl> -0.017667860, 0.000000000, 0.003410609, -0.009702625, -0.02…
#> $ VehPower   <dbl> -8.648532e-04, -3.418528e-03, 2.084949e-03, 1.190031e-03, -…
#> $ VehAge     <dbl> -1.181081e-01, -1.903213e-01, 7.902823e-03, 6.329534e-03, 4…
#> $ DrivAge    <dbl> -1.840783e-04, 7.133882e-04, -4.240216e-04, 1.399568e-03, -…
#> $ BonusMalus <dbl> -1.981097e-04, 3.789446e-04, -5.019076e-04, 1.458261e-04, -…
#> $ VehBrand   <dbl> -6.775799e-02, 0.000000e+00, -2.930630e-02, 4.647116e-02, 6…
#> $ VehGas     <dbl> 0.000000000, 0.000000000, 0.000000000, -0.006539818, 0.0000…
```
