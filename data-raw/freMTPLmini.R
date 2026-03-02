# code pulls the `freMTPL2freq` data object from GitHub and saves as compressed data object for our package
library(magrittr)

# commit <- "master"  # <- use this commit for the latest `CASdatasets` variant
commit <- "c49cbbb37235fc49616cac8ccac32e1491cdc619" # <- use this commit for the kaggle variant
seed_no <- 9000

url <- paste0("https://github.com/dutangc/CASdatasets/raw/", commit, "/data/freMTPL2freq.rda")

temp <- tempfile()

download.file(url, temp)

load(temp)

freMTPLmini <- freMTPL2freq |>
  dplyr::mutate(
    ClaimRate = as.numeric(ClaimNb) / Exposure,
    ClaimRate = pmin(ClaimRate, quantile(ClaimRate, 0.999)), # <-- kept in to help rec with original paper
    VehAge = pmin(VehAge,50) # <-- kept in to help rec with original paper
  ) |>
  # turn any character fields into factors, should help keep package memory lower
  dplyr::mutate(dplyr::across(dplyr::where(is.character), function(field) factor(field))) |>
  # filter some variables out as dataset smaller... targetted selection so still enough data available in remaining groups
  dplyr::filter(Region == "R31") |>
  dplyr::filter(!VehBrand %in% c("B10", "B11", "B13", "B14")) |>
  dplyr::select(dplyr::all_of(c(
    "Area",
    "BonusMalus",
    "DrivAge",
    "VehAge",
    "VehBrand",
    "VehPower",
    "ClaimRate",
    "Exposure"
  ))) |>
  dplyr::slice_sample(n = 25000) %>%
  withr::with_seed(seed_no, .) |>
  tibble::as_tibble()



usethis::use_data(freMTPLmini, overwrite = TRUE)
