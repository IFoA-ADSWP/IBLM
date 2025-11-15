# Load French Motor Third-Party Liability Frequency Dataset

Downloads the French Motor Third-Party Liability (freMTPL2freq) dataset
from the CASdatasets GitHub repository, and apply minor transformations.

## Usage

``` r
load_freMTPL2freq()
```

## Value

A data frame containing the following columns:

- ClaimNb:

  Number of claims per year.

- VehPower:

  The power of the car (ordered categorical).

- VehAge:

  The vehicle age in years, capped at 50.

- DrivAge:

  The driver age in years (minimum 18, the legal driving age in France).

- BonusMalus:

  Bonus/malus coefficient, ranging from 50 to 350. Values below 100
  indicate a bonus (discount), while values above 100 indicate a malus
  (surcharge) in the French insurance system.

- VehBrand:

  The car brand (categorical, with unknown/proprietary category labels).

- VehGas:

  The car fuel type: "Diesel" or "Regular".

- Area:

  The density classification of the city community where the driver
  lives, ranging from "A" (rural area) to "F" (urban centre).

- Density:

  The population density (inhabitants per square kilometer) of the city
  where the driver lives.

- Region:

  The policy region in France, based on the 1970-2015 administrative
  classification.

## Details

The function performs the following modifications to the sourced:

- `ClaimNb`: Converted to ClaimNb per Exposure, and winsorized at the
  99.9th percentile

- `VehAge`: Ceiling of 50 years applied

- All factor variables converted to character type

## Note

This function requires an internet connection to download the data.

## References

Dutang, C. CASdatasets: Insurance datasets.
<https://github.com/dutangc/CASdatasets/raw/c49cbbb37235fc49616cac8ccac32e1491cdc619/data/freMTPL2freq.rda>

## Examples

``` r
# \donttest{
# Load the preprocessed dataset
freMTPL2freq <- load_freMTPL2freq()

freMTPL2freq |> dplyr::glimpse()
#> Rows: 678,013
#> Columns: 10
#> $ ClaimNb    <dbl> 10.000000, 1.298701, 1.333333, 11.111111, 1.190476, 1.92307…
#> $ Area       <chr> "D", "D", "B", "B", "B", "E", "E", "C", "C", "B", "B", "C",…
#> $ VehPower   <int> 5, 5, 6, 7, 7, 6, 6, 7, 7, 7, 7, 7, 4, 4, 4, 9, 6, 6, 6, 6,…
#> $ VehAge     <dbl> 0, 0, 2, 0, 0, 2, 2, 0, 0, 0, 0, 0, 1, 0, 9, 0, 2, 2, 2, 2,…
#> $ DrivAge    <int> 55, 55, 52, 46, 46, 38, 38, 33, 33, 41, 41, 56, 27, 27, 23,…
#> $ BonusMalus <int> 50, 50, 50, 50, 50, 50, 50, 68, 68, 50, 50, 50, 90, 90, 100…
#> $ VehBrand   <chr> "B12", "B12", "B12", "B12", "B12", "B12", "B12", "B12", "B1…
#> $ VehGas     <chr> "Regular", "Regular", "Diesel", "Diesel", "Diesel", "Regula…
#> $ Density    <int> 1217, 1217, 54, 76, 76, 3003, 3003, 137, 137, 60, 60, 173, …
#> $ Region     <chr> "R82", "R82", "R22", "R72", "R72", "R31", "R31", "R91", "R9…
# }
```
