# Convert Data Frame to Wide One-Hot Encoded Format

Transforms categorical variables in a data frame into one-hot encoded
format

## Usage

``` r
data_to_onehot(data, iblm_model, remove_target = TRUE)
```

## Arguments

- data:

  Input data frame to be transformed. This will typically be the "train"
  data subset

- iblm_model:

  Object of class 'iblm'

- remove_target:

  Logical, whether to remove the response_var variable from the output
  (default TRUE).

## Value

A data frame in wide format with one-hot encoded categorical variables,
an intercept column, and all variables ordered according to
"coeff_names\$all" from \`iblm_model\`

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

wide_input_frame <- data_to_onehot(df_list$test, iblm_model)

wide_input_frame |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 17
#> $ `(Intercept)` <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ BonusMalus    <int> 68, 80, 50, 50, 85, 90, 76, 50, 72, 68, 95, 66, 57, 68, …
#> $ DrivAge       <int> 31, 58, 39, 38, 48, 25, 25, 54, 37, 33, 29, 48, 39, 32, …
#> $ VehAge        <dbl> 10, 1, 4, 12, 4, 1, 9, 0, 10, 12, 5, 10, 2, 6, 8, 1, 6, …
#> $ VehPower      <int> 7, 7, 5, 4, 5, 6, 7, 7, 5, 5, 6, 7, 7, 6, 7, 5, 11, 5, 5…
#> $ AreaA         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ AreaB         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ AreaC         <int> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0,…
#> $ AreaD         <int> 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0,…
#> $ AreaE         <int> 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1,…
#> $ VehBrandB1    <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB12   <int> 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB2    <int> 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0,…
#> $ VehBrandB3    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1,…
#> $ VehBrandB4    <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB5    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0,…
#> $ VehBrandB6    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
```
