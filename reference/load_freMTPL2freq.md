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
if (FALSE) { # \dontrun{
# Load the preprocessed dataset
freMTPL2freq <- load_freMTPL2freq()

freMTPL2freq |> dplyr::glimpse()
} # }
```
