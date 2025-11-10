# Create Pre-Configured Beta Corrected Density Plot Function

Factory function that returns a plotting function with data
pre-configured.

## Usage

``` r
create_beta_corrected_density(
  wide_input_frame,
  beta_corrections,
  data,
  iblm_model
)
```

## Arguments

- wide_input_frame:

  Dataframe. Wide format input data (one-hot encoded).

- beta_corrections:

  Dataframe. Output from
  [`beta_corrections_derive`](https://ifoa-adswp.github.io/IBLM/reference/beta_corrections_derive.md).

- data:

  Dataframe. The testing data.

- iblm_model:

  Object of class 'iblm'.

## Value

Function with signature `function(varname, q = 0.05, type = "kde")`.

## See also

\[beta_corrected_density()\]

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
wide_input_frame <- data_to_onehot(test_data, iblm_model)
shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)
beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model)

# ------- demonstration of functionality -------

# create_beta_corrected_density() can create function of type 'beta_corrected_density'
my_beta_corrected_density <- create_beta_corrected_density(
  wide_input_frame,
  beta_corrections,
  test_data,
  iblm_model
)

# this custom function then acts as per beta_corrected_density()
my_beta_corrected_density(varname = "VehAge")

```
