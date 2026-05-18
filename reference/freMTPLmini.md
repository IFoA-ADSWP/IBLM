# French Motor Insurance Claims Dataset

A dataset containing information about French motor insurance policies
and claims, commonly used for actuarial modeling and risk assessment
studies.

This is a "mini" subset of the CASdatasets \`freMTPL2freq\` data, with
some manipulation (see details) so that it is ready to plug into the
IBLM functions

## Usage

``` r
freMTPLmini
```

## Format

A data frame with 25,000 rows and 8 variables:

- Area:

  Area classification where the policy holder resides (factor with
  levels A through F)

- BonusMalus:

  Bonus-malus coefficient, a rating factor used in French insurance
  where lower values indicate better driving records (integer)

- DrivAge:

  Age of the driver in years (integer)

- VehAge:

  Age of the vehicle in years (integer)

- VehBrand:

  Vehicle brand/manufacturer code (factor with levels like B6, B12,
  etc.)

- VehPower:

  Vehicle power rating or engine horsepower category (integer)

- ClaimNb:

  Number of claims made, at an annualised rate (double)

- Exposure:

  Length of Exposure in years (double)

## Source

\['https://github.com/dutangc/CASdatasets/raw/c49cbbb37235fc49616cac8ccac32e1491cdc619/data/freMTPL2freq.rda'\]

## Details

The dataset is a sample of 25,000 records from \`freMTPL2freq\` from the
\`CASdatasets\` package. Other modifications applied are:

- `ClaimRate`: Converted to ClaimNb per Exposure, winsorized at the
  99.9th percentile, and rounded.

- `VehAge`: Ceiling of 50 years applied

- Dropped columns: VehGas, Region, Density, ClaimNb, IDpol

## Examples

``` r
head(freMTPLmini)
#> # A tibble: 6 × 8
#>   Area  BonusMalus DrivAge VehAge VehBrand VehPower ClaimNb Exposure
#>   <fct>      <int>   <int>  <dbl> <fct>       <int>   <dbl>    <dbl>
#> 1 D             50      53      2 B12            12       0     0.24
#> 2 E             90      30      1 B12             4       0     0.05
#> 3 E             76      42      7 B2              4       0     0.08
#> 4 D             68      31     10 B4              7       0     0.96
#> 5 D             57      35      9 B2              5       0     0.29
#> 6 C             57      32     10 B1              9       0     0.3 
```
