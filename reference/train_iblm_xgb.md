# Train IBLM Model on XGBoost

This function trains an interpretable boosted linear model.

The function combines a Generalized Linear Model (GLM) with a booster
model of XGBoost

The "booster" model is trained on: - actual responses / GLM predictions,
when the link function is log - actual responses - GLM predictions, when
the link function is identity

## Usage

``` r
train_iblm_xgb(
  df_list,
  response_var,
  family = "poisson",
  params = list(),
  nrounds = 1000,
  obj = NULL,
  feval = NULL,
  verbose = 0,
  print_every_n = 1L,
  early_stopping_rounds = 25,
  maximize = NULL,
  save_period = NULL,
  save_name = "xgboost.model",
  xgb_model = NULL,
  callbacks = list(),
  ...,
  strip_glm = TRUE
)
```

## Arguments

- df_list:

  A named list containing training and validation datasets. Must have
  elements named "train" and "validate", each containing df_list frames
  with the same structure. This item is naturally output from the
  function \[split_into_train_validate_test()\]

- response_var:

  Character string specifying the name of the response variable column
  in the datasets. The string MUST appear in both \`df_list\$train\` and
  \`df_list\$validate\`.

- family:

  Character string specifying the distributional family for the model.
  Currently only "poisson", "gamma", "tweedie" and "gaussian" is fully
  supported. See details for how this impacts fitting.

- params:

  Named list of additional parameters to pass to
  [xgb.train](https://rdrr.io/pkg/xgboost/man/xgb.train.html). Note that
  train_iblm_xgb will select "objective" and "base_score" for you
  depending on \`family\` (see details section). However you may
  overwrite these (do so with caution)

- nrounds, obj, feval, verbose, print_every_n, early_stopping_rounds,
  maximize, save_period, save_name, xgb_model, callbacks, ...:

  These are passed directly to
  [xgb.train](https://rdrr.io/pkg/xgboost/man/xgb.train.html)

- strip_glm:

  TRUE/FALSE, whether to strip superfluous data from the \`glm_model\`
  object saved within \`iblm\` class that is output. Only serves to
  reduce memory constraints.

## Value

An object of class "iblm" containing:

- glm_model:

  The GLM model object, fitted on the \`df_list\$train\` data that was
  provided

- booster_model:

  The booster model object, trained on the residuals leftover from the
  glm_model

- data:

  A list containing the data that was used to train and validate this
  iblm model

- relationship:

  String that explains how to combine the \`glm_model\` and
  \`booster_model\`. Currently only either "Additive" or
  "Multiplicative"

- response_var:

  A string describing the response variable used for this iblm model

- predictor_vars:

  A list describing the predictor variables used for this iblm model

- cat_levels:

  A list describing the categorical levels for the predictor vars

- coeff_names:

  A list describing the coefficient names

## Details

The \`family\` argument will be fed into the GLM fitting. Default values
for the XGBoost fitting are also selected based on family.

Note: Any xgboost configuration below will be overwritten by any
explicit arguments input via \`params\`

For "poisson" family the link function is 'log' and XGBoost is
configured with:

- objective: "count:poisson"

- base_score: 1

For "gamma" family the link function is 'log' and XGBoost is configured
with:

- objective: "reg:gamma"

- base_score: 1

For "tweedie" family the link function is 'log' (with a var.power = 1.5)
and XGBoost is configured with:

- objective: "reg:tweedie"

- base_score: 1

- tweedie_variance_power = 1.5

For "gaussian" family the link function is 'identity' and XGBoost is
configured with:

- objective: "reg:squarederror"

- base_score: 0

## See also

[glm](https://rdrr.io/r/stats/glm.html),
[xgb.train](https://rdrr.io/pkg/xgboost/man/xgb.train.html)

## Examples

``` r
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)
```
