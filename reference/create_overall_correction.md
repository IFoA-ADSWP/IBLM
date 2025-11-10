# Create Pre-Configured Overall Correction Plot Function

Factory function that returns a plotting function with data
pre-configured.

## Usage

``` r
create_overall_correction(shap, iblm_model)
```

## Arguments

- shap:

  Dataframe. Contains raw SHAP values.

- iblm_model:

  Object of class 'iblm'.

## Value

Function with signature `function(transform_x_scale_by_link = TRUE)`.

## See also

\[overall_correction()\]

## Examples

``` r
# ------- prepare iblm objects required -------

df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

test_data <- df_list$test
shap <- extract_booster_shap(iblm_model$booster_model, test_data)

# ------- demonstration of functionality -------

# create_overall_correction() can create function of type 'overall_correction'
my_overall_correction <- create_overall_correction(shap, iblm_model)

# this custom function then acts as per overall_correction()
my_overall_correction()

```
