# Plot Overall Corrections from Booster Component

Creates a visualization showing for each record the overall booster
component (either multiplicative or additive)

**NOTE** This function signature documents the interface of functions
created by
[`create_overall_correction`](https://ifoa-adswp.github.io/IBLM/reference/create_overall_correction.md).

## Usage

``` r
overall_correction(transform_x_scale_by_link = TRUE)
```

## Arguments

- transform_x_scale_by_link:

  TRUE/FALSE, whether to transform the x axis by the link function

## Value

A ggplot2 object.

## See also

[`create_overall_correction`](https://ifoa-adswp.github.io/IBLM/reference/create_overall_correction.md),
[`explain_iblm`](https://ifoa-adswp.github.io/IBLM/reference/explain_iblm.md)

## Examples

``` r
# This function is created inside explain_iblm() and is output as an item

df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

iblm_model <- train_iblm_xgb(
  df_list,
  response_var = "ClaimRate",
  family = "poisson"
)

explain_objects <- explain_iblm(iblm_model, df_list$test)

explain_objects$overall_correction()



# This function must be created, and cannot be called directly from the package
try(
overall_correction()
)
#> Error in overall_correction() : 
#>   This function documents the interface only and cannot be called
#> directly. Instead, try one of the following
#> ℹ Use explain_iblm()$overall_correction()
#> ℹ Call a function output from create_overall_correction()
```
