# French Motor Insurance Claims Dataset

A dataset containing information about French motor insurance policies
and claims, commonly used for actuarial modeling and risk assessment
studies.

This is a "mini" subset of the CASdatasets \`freMTPL2freq\` data, with
some manipulation (see details)

## Usage

``` r
freMTPLmini
```

## Format

A data frame with 25,000 rows and 8 variables:

- ClaimRate:

  Number of claims made, at an annualised rate, rounded (integer)

- VehPower:

  Vehicle power rating or engine horsepower category (integer)

- VehAge:

  Age of the vehicle in years (integer)

- DrivAge:

  Age of the driver in years (integer)

- BonusMalus:

  Bonus-malus coefficient, a rating factor used in French insurance
  where lower values indicate better driving records (integer)

- VehBrand:

  Vehicle brand/manufacturer code (factor with levels like B6, B12,
  etc.)

- VehGas:

  Type of fuel used by the vehicle (factor with levels: Regular, Diesel)

- Area:

  Area classification where the policy holder resides (factor with
  levels A through F)

## Source

\['https://github.com/dutangc/CASdatasets/raw/c49cbbb37235fc49616cac8ccac32e1491cdc619/data/freMTPL2freq.rda'\]

## Details

The dataset is a random sample of 50,000 records from \`freMTPL2freq\`
from the \`CASdatasets\` pacakge. Other modifications applied are:

- `ClaimRate`: Converted to ClaimNb per Exposure, winsorized at the
  99.9th percentile, and rounded.

- `VehAge`: Ceiling of 50 years applied

- Dropped columns: Region, Density, Exposure, ClaimNb, IDpol

## Examples

``` r
head(freMTPLmini)
#> # A tibble: 6 × 8
#>   Area  VehPower VehAge DrivAge BonusMalus VehBrand VehGas  ClaimRate
#>   <fct>    <int>  <dbl>   <int>      <int> <fct>    <fct>       <int>
#> 1 E           10     14      60         50 B2       Regular         0
#> 2 E            6      4      50         53 B1       Regular         0
#> 3 C            7      2      37         50 B3       Regular         0
#> 4 E           11      1      58         50 B12      Diesel          0
#> 5 B            8      1      29         64 B12      Regular         0
#> 6 A            7      1      28         57 B3       Diesel          0
```
