# code pulls the `freMTPL2freq` data object from GitHub and saves as compressed data object for our package

commit <- "master"  # <- use this commit for the latest `CASdatasets` variant
commit <- "c49cbbb37235fc49616cac8ccac32e1491cdc619" # <- use this commit for the kaggle variant

url <- paste0("https://github.com/dutangc/CASdatasets/raw/", commit, "/data/freMTPL2freq.rda")

temp <- tempfile()

download.file(url, temp)

load(temp)

freMTPL2freq <- freMTPL2freq |>
  dplyr::mutate(
    ClaimRate = ClaimNb / Exposure,
    ClaimRate = pmin(ClaimRate, quantile(ClaimRate, 0.999)), # <-- kept in to help rec with original paper
    VehAge = pmin(VehAge,50) # <-- kept in to help rec with original paper
  ) |>
  # turn any character fields into factors, should help keep package memory lower
  dplyr::mutate(dplyr::across(dplyr::where(is.character), function(field) factor(field))) |>
  dplyr::select(-dplyr::all_of(c("IDpol", "Exposure", "ClaimNb")))

usethis::use_data(freMTPL2freq, overwrite = TRUE, compress = "xz")
