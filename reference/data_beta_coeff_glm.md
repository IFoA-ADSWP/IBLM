# Obtain GLM Beta Coefficients for tabular data

Creates dataframe of GLM beta coefficients for each row and predictor
variable of \`data\`

## Usage

``` r
data_beta_coeff_glm(data, iblm_model)
```

## Arguments

- data:

  Data frame with predictor variables

- iblm_model:

  Object of class 'iblm'

## Value

A data frame with beta coefficients. The structure will be the same
dimension as \`data\` except for a "bias" column at the start.

## Examples

``` r
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

data_glm <- data_beta_coeff_glm(df_list$train, iblm_model)

data_glm |> dplyr::glimpse()
#> Rows: 17,423
#> Columns: 8
#> $ bias       <dbl> -2.365821, -2.365821, -2.365821, -2.365821, -2.365821, -2.3…
#> $ Area       <dbl> -0.004269464, -0.004269464, -0.137876072, 0.000000000, 0.28…
#> $ VehPower   <dbl> -0.005447975, -0.005447975, -0.005447975, -0.005447975, -0.…
#> $ VehAge     <dbl> -0.04132123, -0.04132123, -0.04132123, -0.04132123, -0.0413…
#> $ DrivAge    <dbl> -0.003435393, -0.003435393, -0.003435393, -0.003435393, -0.…
#> $ BonusMalus <dbl> 0.01192857, 0.01192857, 0.01192857, 0.01192857, 0.01192857,…
#> $ VehBrand   <dbl> 0.01820581, 0.00000000, 0.02804685, 0.02804685, 0.72133504,…
#> $ VehGas     <dbl> 0.2654272, 0.2654272, 0.2654272, 0.0000000, 0.2654272, 0.00…
```
