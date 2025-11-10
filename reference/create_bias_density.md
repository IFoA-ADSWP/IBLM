# Create Pre-Configured Bias Density Plot Function

Factory function that returns a plotting function with data
pre-configured.

## Usage

``` r
create_bias_density(shap, data, iblm_model, migrate_reference_to_bias = TRUE)
```

## Arguments

- shap:

  Dataframe. Contains raw SHAP values.

- data:

  Dataframe. The testing data.

- iblm_model:

  Object of class 'iblm'.

- migrate_reference_to_bias:

  TRUE/FALSE determines whether the shap values of categorical reference
  levels be migrated to the bias? Default is TRUE

## Value

Function with signature `function(q = 0, type = "hist")`.

## See also

\[bias_density()\]

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

# create_bias_density() can create function of type 'bias_density'
my_bias_density <- create_bias_density(shap, test_data, iblm_model)

# this custom function then acts as per bias_density()
my_bias_density()
#> $bias_correction_var

#> 
#> $bias_correction_total

#> 

```
