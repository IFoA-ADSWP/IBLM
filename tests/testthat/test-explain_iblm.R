testthat::test_that("test against Karol original script", {

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
    family = "poisson",
    params = list(seed=0, tree_method = "auto")
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = FALSE)


  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM_testing` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_explain_test

  explainer_og <- list()

  explainer_og$shap_wide_colsums <-
    c(
      bias = -1708.6684105788445,
      VehPower = 26.01553420041986,
      VehAge = -256.571148711141,
      DrivAge = -50.06147676303107,
      BonusMalus = -18.42787296280043,
      Density = 22.072232441378393,
      VehBrandB1 = -273.386326685757,
      VehBrandB10 = 39.751871885357104,
      VehBrandB11 = -23.553031662544527,
      VehBrandB12 = 1089.8126656933327,
      VehBrandB13 = -48.09883122530846,
      VehBrandB14 = -5.5525740051489265,
      VehBrandB2 = -495.90280463883755,
      VehBrandB3 = -224.97964470623992,
      VehBrandB4 = 125.75223000903497,
      VehBrandB5 = -129.36582155104406,
      VehBrandB6 = -28.16889056122818,
      VehGasDiesel = 59.906505742664194,
      VehGasRegular = -56.34294857816951,
      AreaA = 24.2141983361862,
      AreaB = 6.580454605180421,
      AreaC = 9.029201175868138,
      AreaD = -60.96857544378054,
      AreaE = -66.20219113786334,
      AreaF = 35.95275043609945,
      RegionAlsace = 14.868020770605654,
      RegionAquitaine = 200.02629335300298,
      RegionAuvergne = -19.55482835287694,
      `RegionBasse-Normandie` = 10.147629502112977,
      RegionBourgogne = -28.502676848816918,
      RegionBretagne = -243.14726781326317,
      RegionCentre = -1012.1001042925345,
      `RegionChampagne-Ardenne` = 42.88122421933804,
      RegionCorse = -9.18966015346814,
      `RegionFranche-Comte` = 9.929858500952832,
      `RegionHaute-Normandie` = 42.67572305968497,
      `RegionIle-de-France` = 445.5563439260586,
      `RegionLanguedoc-Roussillon` = 207.8120885383687,
      RegionLimousin = -29.94007034938113,
      RegionLorraine = -28.336859274364542,
      `RegionMidi-Pyrenees` = 163.82334529876243,
      `RegionNord-Pas-de-Calais` = 154.3834550518077,
      `RegionPays-de-la-Loire` = -74.87333380343625,
      RegionPicardie = 3.6783490418456495,
      `RegionPoitou-Charentes` = -2.8664685743424343,
      `RegionProvence-Alpes-Cotes-D'Azur` = 350.3612418755074,
      `RegionRhone-Alpes` = -585.5019154318434
    )
    # c(
    #   bias = -2067.634628662665,
    #   VehPower = 77.15579762084639,
    #   VehAge = -23.215015305266128,
    #   DrivAge = -61.24309050987545,
    #   BonusMalus = -22.445602931956973,
    #   Density = 25.378615547426733,
    #   VehBrandB1 = 0.4929784910491435,
    #   VehBrandB10 = 20.502513993749744,
    #   VehBrandB11 = 47.836586910823826,
    #   VehBrandB12 = 586.8557514288768,
    #   VehBrandB13 = -16.759563245454046,
    #   VehBrandB14 = -10.66495745186694,
    #   VehBrandB2 = -354.99009596340693,
    #   VehBrandB3 = -169.61300241362187,
    #   VehBrandB4 = -47.662840547491214,
    #   VehBrandB5 = -57.07242563375621,
    #   VehBrandB6 = -4.931066558579914,
    #   VehGasDiesel = 262.6049230671415,
    #   VehGasRegular = -260.8031225083723,
    #   AreaA = -7.057203395199394,
    #   AreaB = 8.401192414939942,
    #   AreaC = 6.267889281658995,
    #   AreaD = 7.026916613533103,
    #   AreaE = 7.206054686117568,
    #   AreaF = 0.7287656644657545,
    #   RegionAlsace = 29.41269837069558,
    #   RegionAquitaine = 219.19630470527773,
    #   RegionAuvergne = 0.03033583141223062,
    #   `RegionBasse-Normandie` = 9.1936749826491,
    #   RegionBourgogne = -52.224121282157284,
    #   RegionBretagne = -227.96657285522087,
    #   RegionCentre = -1098.584975179394,
    #   `RegionChampagne-Ardenne` = 32.18406923182192,
    #   RegionCorse = 32.32750511944323,
    #   `RegionFranche-Comte` = 7.144027646740142,
    #   `RegionHaute-Normandie` = 47.25576676859055,
    #   `RegionIle-de-France` = 304.6266946428623,
    #   `RegionLanguedoc-Roussillon` = 222.1023564936586,
    #   RegionLimousin = 23.28911739943578,
    #   RegionLorraine = 58.126529967139504,
    #   `RegionMidi-Pyrenees` = 91.25889614265907,
    #   `RegionNord-Pas-de-Calais` = 89.35858378249577,
    #   `RegionPays-de-la-Loire` = 83.542801530637,
    #   RegionPicardie = 14.302402913701371,
    #   `RegionPoitou-Charentes` = 19.301416881062323,
    #   `RegionProvence-Alpes-Cotes-D'Azur` = 143.6244444957265,
    #   `RegionRhone-Alpes` = -398.0069321255505
    # )

  explainer_og$raw_shap_colsums <-
    c(
      VehPower = 64.9431847836795,
      VehAge = -498.18680342239327,
      DrivAge = -836.9150417760684,
      BonusMalus = -674.3150080790183,
      VehBrand = 26.30884255161618,
      VehGas = 3.563557164494682,
      Area = -51.394162028309665,
      Density = 243.26412361955158,
      Region = -387.8696117562795,
      BIAS = -823.2791528105736
    )
    # c(
    #   VehPower = 120.16935467716758,
    #   VehAge = -550.591719013908,
    #   DrivAge = -1125.4035902854048,
    #   BonusMalus = -946.9910313908222,
    #   VehBrand = -6.006120989677584,
    #   VehGas = 1.8018005587691732,
    #   Area = 22.57361526551597,
    #   Density = 97.71078889113824,
    #   Region = -350.50497453631397,
    #   BIAS = -1023.7343763839453
    # )


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



testthat::test_that("test explain completes when one categorical and one continuous", {
  vars <- c("VehBrand", "VehPower", "ClaimRate")

    splits <- freMTPLmini  |>
      dplyr::select(dplyr::all_of(vars)) |>
      split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[1])
      ex$overall_correction()
      ex$bias_density()
    }
  )
})

testthat::test_that("test explain completes when categorical only", {
  vars <- c("VehBrand", "VehGas", "Area", "ClaimRate")


    splits <- freMTPLmini  |>
      dplyr::select(dplyr::all_of(vars)) |>
      split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[1])
      ex$overall_correction()
      ex$bias_density()
    }
  )
})

testthat::test_that("test explain completes when continuous only", {

  vars <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", "ClaimRate")

  splits <- freMTPLmini  |>
    dplyr::select(dplyr::all_of(vars)) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[1])
      ex$overall_correction()
      ex$bias_density()
    }
  )
})


testthat::test_that("test explain completes when logical field", {

  vars <- names(freMTPLmini)


  splits <- freMTPLmini  |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(dummy = sample(c(TRUE, FALSE), size = nrow(freMTPLmini), replace = TRUE)) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[1])
      ex$overall_correction()
      ex$bias_density()
    }
  )
})



testthat::test_that("test explain completes when no reference/zero levels", {

  vars <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", "ClaimRate")

  splits <- freMTPLmini  |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(dplyr::across(-dplyr::all_of("ClaimRate"), \(x) pmax(x, 1))) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[1])
      ex$overall_correction()
      suppressMessages({ex$bias_density()}) # expect message here to let user no there are no plots produced
      }
  )
})











testthat::test_that("test migrate-to-bias vs non-migrate-to-bias options", {

  # A note on this test...

  # This test compares the predictions with 'migrate_reference_to_bias' as TRUE or FALSE.
  # They should lead to the same predictions

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  explainer_w_migrate <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = TRUE)

  explainer_wout_migrate <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = FALSE)

  coeff_multiplier <- splits$test |>
    dplyr::select(-dplyr::all_of("ClaimRate")) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(IBLM$predictor_vars$categorical),
        ~1
      )
    ) |>
    dplyr::mutate(bias = 1, .before = 1)

  predict_w_migrate <- rowSums(explainer_w_migrate$data_beta_coeff * coeff_multiplier) |>
    exp() |>
    unname()

  predict_wout_migrate <- rowSums(explainer_wout_migrate$data_beta_coeff * coeff_multiplier) |>
    exp() |>
    unname()

  prediction_max_difference <- max(abs(predict_w_migrate / predict_wout_migrate - 1))

  testthat::expect_equal(prediction_max_difference, 0)

})










testthat::test_that("test gaussian can run", {

  # note this is just a crude test that it will run. should probably expand with numerical reconciliations

  vars <- names(freMTPLmini)

  splits <- freMTPLmini |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "gaussian"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[2])
      ex$overall_correction()
      ex$bias_density()
    }
  )

  get_pinball_scores(splits$test, IBLM)

})


testthat::test_that("test gamma can run", {

  # note this is just a crude test that it will run. should probably expand with numerical reconciliations

  vars <- names(freMTPLmini)

  splits <- freMTPLmini |>
    dplyr::mutate(ClaimRate = round(ClaimRate) |> pmax(0.1)) |> # set min. ClaimRate to 0.1 as pre-requisite for gamma dist is x>0
    split_into_train_validate_test(seed = 1)


  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "gamma"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[2])
      ex$overall_correction()
      ex$bias_density()
      get_pinball_scores(splits$test, IBLM)
    }
  )

})



testthat::test_that("test tweedie can run", {

  # note this is just a crude test that it will run. should probably expand with numerical reconciliations

  vars <- names(freMTPLmini)

  splits <- freMTPLmini |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "tweedie"
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[2])
      ex$overall_correction()
      ex$bias_density()
      get_pinball_scores(splits$test, IBLM)
    }
  )

})



testthat::test_that("test can change objective function", {

  # note this is just a crude test that it will run. should probably expand with numerical reconciliations

  vars <- names(freMTPLmini)

  splits <- freMTPLmini |>
    split_into_train_validate_test(seed = 1)

  testthat::expect_message(
  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson",
    params = list(objective = "reg:squarederror")
  )
  )

  testthat::expect_no_error(
    {
      ex <- explain_iblm(iblm_model = IBLM, data = splits$test)
      ex$beta_corrected_scatter(vars[1])
      ex$beta_corrected_density(vars[2])
      ex$overall_correction()
      ex$bias_density()
      get_pinball_scores(splits$test, IBLM)
    }
  )

})


