# code pulls the `freMTPL2freq` data object from GitHub and saves as compressed data object for our package

url <- "https://github.com/dutangc/CASdatasets/raw/master/data/freMTPL2freq.rda"

temp <- tempfile()

download.file(url, temp)

load(temp)

usethis::use_data(freMTPL2freq, overwrite = TRUE, compress = "xz")
