# Check Data Variability for Modeling

Validates that the response variable and all predictor variables have
more than one unique value.

## Usage

``` r
check_data_variability(data, response_var)
```

## Arguments

- data:

  A data frame containing the variables to check.

- response_var:

  Character string naming the response variable in \`data\`.

## Value

Invisibly returns \`TRUE\` if all checks pass, otherwise throws an
error.
