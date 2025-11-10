# Check Object of Class \`iblm\`

Validates an iblm model object has required structure and features

## Usage

``` r
check_iblm_model(model, booster_models_supported = c("xgb.Booster"))
```

## Arguments

- model:

  Model object to validate, expected class "iblm"

- booster_models_supported:

  Booster model classes currently supported in the iblm package

## Value

Invisible TRUE if all checks pass

## Examples

``` r
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

check_iblm_model(iblm_model)
```
