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
df_list <- freMTPLmini |>
  dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
  split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimNb",
  offset_var = "LogExposure",
  family = "poisson"
)

data_glm <- data_beta_coeff_glm(df_list$train, iblm_model)

data_glm |> dplyr::glimpse()
#> Rows: 17,423
#> Columns: 7
#> $ bias       <dbl> -3.811027, -3.811027, -3.811027, -3.811027, -3.811027, -3.8…
#> $ Area       <dbl> -0.18102641, 0.05738965, 0.05738965, -0.21138346, -0.211383…
#> $ BonusMalus <dbl> 0.01843112, 0.01843112, 0.01843112, 0.01843112, 0.01843112,…
#> $ DrivAge    <dbl> 0.005883557, 0.005883557, 0.005883557, 0.005883557, 0.00588…
#> $ VehAge     <dbl> -0.03139225, -0.03139225, -0.03139225, -0.03139225, -0.0313…
#> $ VehBrand   <dbl> 0.10403080, 0.10403080, -0.04224723, 0.00000000, 0.10403080…
#> $ VehPower   <dbl> 0.06012024, 0.06012024, 0.06012024, 0.06012024, 0.06012024,…
```
