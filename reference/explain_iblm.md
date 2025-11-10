# Explain GLM Model Predictions Using SHAP Values

Creates a list that explains the beta values, and their corrections, of
the ensemble IBLM model

## Usage

``` r
explain_iblm(iblm_model, data, migrate_reference_to_bias = TRUE)
```

## Arguments

- iblm_model:

  An object of class 'iblm'. This should be output by
  \`train_iblm_xgb()\`

- data:

  Data frame.

  If you have used \`split_into_train_validate_test()\` this will be the
  "test" portion of your data.

- migrate_reference_to_bias:

  Logical, migrate the beta corrections to the bias for reference
  levels? This applied to categorical vars only. It is recommended to
  leave this setting on TRUE

## Value

A list containing:

- beta_corrected_scatter:

  Function to create scatter plots showing SHAP corrections vs variable
  values (see
  [`beta_corrected_scatter`](https://ifoa-adswp.github.io/IBLM/reference/beta_corrected_scatter.md))

- beta_corrected_density:

  Function to create density plots of SHAP corrections for variables
  (see
  [`beta_corrected_density`](https://ifoa-adswp.github.io/IBLM/reference/beta_corrected_density.md))

- bias_density:

  Function to create density plots of SHAP corrections migrated to bias
  (see
  [`bias_density`](https://ifoa-adswp.github.io/IBLM/reference/bias_density.md))

- overall_correction:

  Function to show global correction distributions (see
  [`overall_correction`](https://ifoa-adswp.github.io/IBLM/reference/overall_correction.md))

- shap:

  Dataframe showing raw SHAP values of data records

- beta_corrections:

  Dataframe showing beta corrections (in wide/one-hot format) of data
  records

- data_beta_coeff:

  Dataframe showing beta coefficients of data records

## Details

The following outputs are functions that can be called to create plots:

- beta_corrected_scatter

- beta_corrected_density

- bias_density

- overall_correction

For each of these, the key data arguments (e.g. data, shap, iblm_model)
are already populated by \`explain_iblm()\`. When calling these
functions output from \`explain_iblm()\` only key settings like variable
names, colours...etc need populating.

## Examples

``` r
df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

ex <- explain_iblm(iblm_model, df_list$test)

# the output contains functions that can be called to visualise iblm
ex$beta_corrected_scatter("DrivAge")
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

ex$beta_corrected_density("DrivAge")

ex$overall_correction()

ex$bias_density()
#> $bias_correction_var

#> 
#> $bias_correction_total

#> 

# the output contains also dataframes
ex$shap |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 8
#> $ Area       <dbl> -0.0111900549, 0.0001318646, 0.0035772556, 0.0050815106, -0…
#> $ VehPower   <dbl> -0.0270258002, -0.0082273316, 0.0158687718, 0.0093539460, 0…
#> $ VehAge     <dbl> -0.210502356, -0.041103296, 0.074962787, 0.080654249, 0.071…
#> $ DrivAge    <dbl> 0.003348402, 0.254643559, -0.033149377, 0.101194337, -0.040…
#> $ BonusMalus <dbl> -0.020024695, 0.107680462, -0.057291888, -0.034046866, -0.0…
#> $ VehBrand   <dbl> -0.095233344, -0.106390789, -0.017026167, -0.018466003, -0.…
#> $ VehGas     <dbl> 0.0066829044, 0.0579791106, 0.0230292529, -0.0055609480, 0.…
#> $ BIAS       <dbl> -0.05418567, -0.05418567, -0.05418567, -0.05418567, -0.0541…
ex$beta_corrections |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 24
#> $ bias          <dbl> -0.04750277, -0.10246549, -0.03115642, -0.05418567, -0.0…
#> $ VehPower      <dbl> -0.0024568909, -0.0016454663, 0.0026447953, 0.0018707892…
#> $ VehAge        <dbl> -0.2105023563, -0.0411032960, 0.0093703484, 0.0057610178…
#> $ DrivAge       <dbl> 5.773106e-05, 1.018574e-02, -7.892709e-04, 1.405477e-03,…
#> $ BonusMalus    <dbl> -0.0004004939, 0.0011334786, -0.0011458378, -0.000680937…
#> $ AreaA         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ AreaB         <dbl> 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.00…
#> $ AreaC         <dbl> 0.0000000000, 0.0000000000, 0.0000000000, 0.0050815106, …
#> $ AreaD         <dbl> 0.000000000, 0.000000000, 0.003577256, 0.000000000, 0.00…
#> $ AreaE         <dbl> -0.01119005, 0.00000000, 0.00000000, 0.00000000, 0.00000…
#> $ AreaF         <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.000000…
#> $ VehBrandB1    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB10   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB11   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB12   <dbl> -0.095233344, 0.000000000, 0.000000000, 0.000000000, 0.0…
#> $ VehBrandB13   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB14   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB2    <dbl> 0.000000000, 0.000000000, 0.000000000, 0.000000000, -0.0…
#> $ VehBrandB3    <dbl> 0.00000000, 0.00000000, -0.01702617, 0.00000000, 0.00000…
#> $ VehBrandB4    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehBrandB5    <dbl> 0.000000, 0.000000, 0.000000, -0.018466, 0.000000, 0.000…
#> $ VehBrandB6    <dbl> 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.000000…
#> $ VehGasDiesel  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ VehGasRegular <dbl> 0.0000000000, 0.0000000000, 0.0000000000, -0.0055609480,…
ex$data_beta_coeff |> dplyr::glimpse()
#> Rows: 3,764
#> Columns: 8
#> $ bias       <dbl> -2.413323, -2.468286, -2.396977, -2.420006, -2.396167, -2.4…
#> $ Area       <dbl> -0.015459519, 0.000000000, 0.069547486, -0.132794561, -0.15…
#> $ VehPower   <dbl> -7.904865e-03, -7.093441e-03, -2.803179e-03, -3.577185e-03,…
#> $ VehAge     <dbl> -0.25182359, -0.08242453, -0.03195088, -0.03556021, -0.0353…
#> $ DrivAge    <dbl> -0.0033776616, 0.0067503497, -0.0042246636, -0.0020299158, …
#> $ BonusMalus <dbl> 0.011528078, 0.013062051, 0.010782734, 0.011247635, 0.01066…
#> $ VehBrand   <dbl> 6.204195e-01, 0.000000e+00, 1.102069e-02, 7.028690e-01, -8.…
#> $ VehGas     <dbl> 0.0000000, 0.0000000, 0.0000000, 0.2598662, 0.0000000, 0.25…
```
