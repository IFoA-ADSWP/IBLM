# Calculate Pinball Scores for IBLM and Additional Models

Computes Poisson deviance and pinball scores for an IBLM model alongside
homogeneous, GLM, and optional additional models.

## Usage

``` r
get_pinball_scores(
  data,
  iblm_model,
  trim = NA_real_,
  additional_models = list()
)
```

## Arguments

- data:

  Data frame. If you have used \`split_into_train_validate_test()\` this
  will be the "test" portion of your data.

- iblm_model:

  Fitted IBLM model object of class "iblm"

- trim:

  Numeric trimming parameter for IBLM predictions. Default is
  \`NA_real\_\`.

- additional_models:

  (Named) list of fitted models for comparison. These models MUST be
  fitted on the same data as \`iblm_model\` for sensible results. If
  unnamed, models are labeled by their class.

## Value

Data frame with 3 columns:

- "model" - will be homog, glm, iblm and any other models specified in
  \`additional_models\`

- "\`family\`\_deviance" - the value from the loss function based on the
  family of the glm function

- "pinball_score" - The more positive the score, the better the model
  than a basic homog model (i.e. all predictions are mean value). A
  negative score indicates worse than homog model.

## Details

Pinball scores are calculated relative to a homogeneous model (i.e. a
simple mean prediction of training data). Higher scores indicate better
predictive performance.

## Examples

``` r
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

get_pinball_scores(data = df_list$test, iblm_model = iblm_model)
#>   model poisson_deviance pinball_score
#> 1 homog         1.313877    0.00000000
#> 2   glm         1.232683    0.06179691
#> 3  iblm         1.187413    0.09625226
```
