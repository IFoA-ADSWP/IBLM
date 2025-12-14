# Predict Method for IBLM

This function generates predictions from an ensemble model consisting of
a GLM and an XGBoost model.

## Usage

``` r
# S3 method for class 'iblm'
predict(object, newdata, trim = NA_real_, type = "response", ...)
```

## Arguments

- object:

  An object of class 'iblm'. This should be output by
  \`train_iblm_xgb()\`

- newdata:

  A data frame or matrix containing the predictor variables for which
  predictions are desired. Must have the same structure as the training
  data used to fit the 'iblm' model.

- trim:

  Numeric value for post-hoc truncating of XGBoost predictions. If `NA`
  (default) then no trimming is applied.

- type:

  string, defines the type argument used in GLM/Booster Currently only
  "response" is supported

- ...:

  additional arguments affecting the predictions produced.

## Value

A numeric vector of ensemble predictions computed as the element-wise
product of GLM response probabilities and (optionally trimmed) XGBoost
predictions.

## Details

The prediction process involves the following steps:

1.  Generate GLM predictions

2.  Generate Booster predictions

3.  If trimming is specified, apply to booster predictions

4.  Combine GLM and Booster predictions as per "relationship" described
    within iblm model object

At this point, only an iblm model with a "booster_model" object of class
\`xgb.Booster\` is supported

## See also

[predict.glm](https://rdrr.io/r/stats/predict.glm.html),
[predict.xgb.Booster](https://rdrr.io/pkg/xgboost/man/predict.xgb.Booster.html)

## Examples

``` r
data <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  data,
  response_var = "ClaimRate",
  family = "poisson"
)

predictions <- predict(iblm_model, data$test)

predictions |> dplyr::glimpse()
#>  num [1:3764] 0.196 0.195 0.112 0.206 0.071 ...
```
