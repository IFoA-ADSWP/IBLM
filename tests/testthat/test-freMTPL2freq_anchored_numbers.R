testthat::test_that("test against anchored values", {

  # test takes too long for CRAN
  testthat::skip_on_cran()


  # ============================ Download data =====================

  # download the correct version of freMTPL2freq dataset to complete the rec

  commit <- "2a718359896bee4edf852721364ac5eaae442fc1"  # <- use this commit

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

  # ============================ Input data =====================

  data <- freMTPL2freq |> split_into_train_validate_test(seed = 1)


  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    family = "quasipoisson",
    params = list(seed=0, tree_method = "auto")
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = FALSE)


  # ============================ Karol (og) process =====================

  # IMPORTANT: this test was originally to reconcile objects taken from Karol original script, using the same seed, input and settings
  # ...however movement to use of base_margin means this is not possible.
  # ...instead these are tested against a snapshot in time (i.e. IBLM v1.0.3 outputs)

  explainer_og <- list()

  explainer_og$shap_wide_colsums <-
    c(
      bias = -1460.0089587446455,
      VehPower = 11.952861946920704,
      VehAge = -181.10350105278917,
      DrivAge = -31.317626704898323,
      BonusMalus = -11.029937179061974,
      Density = 17.130473734266022,
      VehBrandB1 = 53.97203129208174,
      VehBrandB10 = 29.012124345961638,
      VehBrandB11 = -33.22955208989151,
      VehBrandB12 = 381.33587243314105,
      VehBrandB13 = -16.640272930620995,
      VehBrandB14 = 2.9008418445155257,
      VehBrandB2 = -111.69312065418308,
      VehBrandB3 = -43.18245471134833,
      VehBrandB4 = -48.15570740340263,
      VehBrandB5 = -78.06832866355035,
      VehBrandB6 = -80.62585830023454,
      VehGasDiesel = 43.530368736326636,
      VehGasRegular = -44.02244237555351,
      AreaA = -28.75134131041341,
      AreaB = 77.21440274520319,
      AreaC = -103.53582653789454,
      AreaD = 92.34398221131778,
      AreaE = -87.91036703403188,
      AreaF = 30.681130975703127,
      RegionAlsace = 1.106194727588445,
      RegionAquitaine = 5.619225856731646,
      RegionAuvergne = -7.606002277811058,
      `RegionBasse-Normandie` = -1.9639979107305408,
      RegionBourgogne = 2.2023375268327072,
      RegionBretagne = -123.42455152017646,
      RegionCentre = -270.68972353148274,
      `RegionChampagne-Ardenne` = 3.4111120691522956,
      RegionCorse = -2.730551877291873,
      `RegionFranche-Comte` = 1.200985164497979,
      `RegionHaute-Normandie` = -12.773443870566553,
      `RegionIle-de-France` = 234.1845492721186,
      `RegionLanguedoc-Roussillon` = -55.225505495094694,
      RegionLimousin = -29.18673318857327,
      RegionLorraine = 8.68670308450237,
      `RegionMidi-Pyrenees` = 26.830272579973098,
      `RegionNord-Pas-de-Calais` = 145.21133474679664,
      `RegionPays-de-la-Loire` = 22.021929057038506,
      RegionPicardie = 15.651259544538334,
      `RegionPoitou-Charentes` = -23.789477064739913,
      `RegionProvence-Alpes-Cotes-D'Azur` = 97.2441464230069,
      `RegionRhone-Alpes` = -138.7825649927836
    )

  explainer_og$raw_shap_colsums <-
    c(
      VehPower = 105.03363245885339,
      VehAge = -511.70221551092345,
      DrivAge = -460.8299511852383,
      BonusMalus = -496.29030748519654,
      VehBrand = 55.62557516246852,
      VehGas = -0.49207363922687364,
      Area = -19.95801895011573,
      Density = 104.64678279646887,
      Region = -102.80250167647318,
      BIAS = -817.9418300371617
    )

  # ============================ comparisons =====================

  # increase tolerance slightly for non-Windows
  # The absolute figures we are reconciling against are ran on windows and show tiny differences
  is_windows <- (Sys.info()["sysname"] == "windows")

  os_tolerance <- if (!is_windows) 1e-6 else testthat::testthat_tolerance()

  testthat::expect_equal(
    explainer_nu$beta_corrections |> colSums(),
    explainer_og$shap_wide_colsums,
    tolerance = os_tolerance
  )

  testthat::expect_equal(
    explainer_nu$shap |> colSums(),
    explainer_og$raw_shap_colsums,
    tolerance = os_tolerance
  )
})
