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
df_list <- freMTPLmini |>
  dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
  split_into_train_validate_test(seed = 9000)

# training with plenty of rounds allowed
iblm_model1 <- train_iblm_xgb(
  df_list,
  response_var = "ClaimNb",
  offset_var = "LogExposure",
  family = "poisson",
  params = list(max_depth = 6),
  nrounds = 1000
)

xgb1 <- train_xgb_as_per_iblm(iblm_model1)

# training with severe restrictions (expected poorer results)
iblm_model2 <- train_iblm_xgb(
  df_list,
  response_var = "ClaimNb",
  offset_var = "LogExposure",
  family = "poisson",
  params = list(max_depth = 1),
  nrounds = 2
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
#> 1 homog        0.2716294    0.00000000
#> 2   glm        0.2647943    0.02516329
#> 3  iblm        0.2567511    0.05477428
#> 4 iblm2        0.2631261    0.03130501
#> 5  xgb1        0.2591308    0.04601350
#> 6  xgb2        0.6606099   -1.43202634
```
