# Train XGBoost Model Using the IBLM Model Parameters

Trains an XGBoost model using parameters extracted from the booster
residual component of the iblm model. This is a convenient way to fit an
XGBoost model for direct comparison with a fitted iblm model

## Usage

``` r
train_xgb_as_per_iblm(iblm_model, ...)
```

## Arguments

- iblm_model:

  Ensemble model object of class "iblm" containing GLM and XGBoost model
  components. Also contains data that was used to train it.

- ...:

  optional arguments to insert into xgb.train(). Note this will cause
  deviation from the settings used for training \`iblm_model\`

## Value

Trained XGBoost model object (class "xgb.Booster").

## See also

[xgb.train](https://rdrr.io/pkg/xgboost/man/xgb.train.html)

## Examples

``` r
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

# training with plenty of rounds allowed
iblm_model1 <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson",
  max_depth = 6,
  nrounds = 1000
)

xgb1 <- train_xgb_as_per_iblm(iblm_model1)

# training with severe restrictions (expected poorer results)
iblm_model2 <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson",
  max_depth = 1,
  nrounds = 5
)

xgb2 <- train_xgb_as_per_iblm(iblm_model2)

# comparison shows the poor training mirrored in second set:
get_pinball_scores(
  df_list$test,
  iblm_model1,
  trim = NA_real_,
  additional_models = list(iblm2 = iblm_model2, xgb1 = xgb1, xgb2 = xgb2)
)
#>   model poisson_deviance pinball_score
#> 1 homog         1.313877    0.00000000
#> 2   glm         1.232683    0.06179691
#> 3  iblm         1.187413    0.09625226
#> 4 iblm2         1.211137    0.07819591
#> 5  xgb1         1.216096    0.07442165
#> 6  xgb2         1.660000   -0.26343662
```
