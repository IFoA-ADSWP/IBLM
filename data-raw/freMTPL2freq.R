# code pulls the `freMTPL2freq` data object from GitHub and saves as compressed data object for our package

commit <- "c49cbbb37235fc49616cac8ccac32e1491cdc619" # <- use this commit for the kaggle variant
commit <- "master"  # <- use this commit for the latest `CASdatasets` variant

url <- paste0("https://github.com/dutangc/CASdatasets/raw/", commit, "/data/freMTPL2freq.rda")

temp <- tempfile()

download.file(url, temp)

load(temp)

freMTPL2freq <- freMTPL2freq |>
  dplyr::mutate(
    ClaimRate = ClaimNb / Exposure,
    ClaimRate = pmin(ClaimRate, quantile(ClaimRate, 0.999))
  ) |>
  dplyr::select(-dplyr::all_of(c("IDpol", "Exposure", "ClaimNb")))

usethis::use_data(freMTPL2freq, overwrite = TRUE, compress = "xz")
