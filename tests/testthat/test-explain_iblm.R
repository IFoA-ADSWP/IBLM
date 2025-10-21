testthat::test_that("test against Karol original script", {

  # ============================ Input data =====================

  withr::with_seed(1, {
    data <- freMTPL2freq |> split_into_train_validate_test()
  })

  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), function(field) as.character(field)))) |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # ============================ IBLM package process =====================

  IBLM <- train_iblm(
    splits,
    response_var = "ClaimNb",
    family = "poisson"
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = FALSE)


  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_explain_test

  explainer_og <- list()

  explainer_og$shap_wide_colsums <-
    c(
      bias = -2067.634628662665,
      VehPower = 77.15579762084639,
      VehAge = -23.215015305266128,
      DrivAge = -61.24309050987545,
      BonusMalus = -22.445602931956973,
      Density = 25.378615547426733,
      VehBrandB1 = 0.4929784910491435,
      VehBrandB10 = 20.502513993749744,
      VehBrandB11 = 47.836586910823826,
      VehBrandB12 = 586.8557514288768,
      VehBrandB13 = -16.759563245454046,
      VehBrandB14 = -10.66495745186694,
      VehBrandB2 = -354.99009596340693,
      VehBrandB3 = -169.61300241362187,
      VehBrandB4 = -47.662840547491214,
      VehBrandB5 = -57.07242563375621,
      VehBrandB6 = -4.931066558579914,
      VehGasDiesel = 262.6049230671415,
      VehGasRegular = -260.8031225083723,
      AreaA = -7.057203395199394,
      AreaB = 8.401192414939942,
      AreaC = 6.267889281658995,
      AreaD = 7.026916613533103,
      AreaE = 7.206054686117568,
      AreaF = 0.7287656644657545,
      RegionAlsace = 29.41269837069558,
      RegionAquitaine = 219.19630470527773,
      RegionAuvergne = 0.03033583141223062,
      `RegionBasse-Normandie` = 9.1936749826491,
      RegionBourgogne = -52.224121282157284,
      RegionBretagne = -227.96657285522087,
      RegionCentre = -1098.584975179394,
      `RegionChampagne-Ardenne` = 32.18406923182192,
      RegionCorse = 32.32750511944323,
      `RegionFranche-Comte` = 7.144027646740142,
      `RegionHaute-Normandie` = 47.25576676859055,
      `RegionIle-de-France` = 304.6266946428623,
      `RegionLanguedoc-Roussillon` = 222.1023564936586,
      RegionLimousin = 23.28911739943578,
      RegionLorraine = 58.126529967139504,
      `RegionMidi-Pyrenees` = 91.25889614265907,
      `RegionNord-Pas-de-Calais` = 89.35858378249577,
      `RegionPays-de-la-Loire` = 83.542801530637,
      RegionPicardie = 14.302402913701371,
      `RegionPoitou-Charentes` = 19.301416881062323,
      `RegionProvence-Alpes-Cotes-D'Azur` = 143.6244444957265,
      `RegionRhone-Alpes` = -398.0069321255505
    )

  explainer_og$raw_shap_colsums <-
    c(
      VehPower = 120.16935467716758,
      VehAge = -550.591719013908,
      DrivAge = -1125.4035902854048,
      BonusMalus = -946.9910313908222,
      VehBrand = -6.006120989677584,
      VehGas = 1.8018005587691732,
      Area = 22.57361526551597,
      Density = 97.71078889113824,
      Region = -350.50497453631397,
      BIAS = -1023.7343763839453
    )

  explainer_og$betas <-
    c(
      `(Intercept)` = -4.242066337203175,
      VehPower = 0.020919022901048436,
      VehAge = -0.0015729129207102523,
      DrivAge = 0.003678738724085157,
      BonusMalus = 0.022271569195154908,
      VehBrandB10 = -0.022050779654071543,
      VehBrandB11 = 0.12258915754554618,
      VehBrandB12 = -0.33937589303248894,
      VehBrandB13 = -0.024222623095271373,
      VehBrandB14 = -0.15209238438395092,
      VehBrandB2 = 0.018380080148412886,
      VehBrandB3 = 0.06854296197332409,
      VehBrandB4 = 0.069628410141503,
      VehBrandB5 = 0.16393122240390984,
      VehBrandB6 = 0.07708115361227003,
      VehGasRegular = -0.11690985440726433,
      AreaB = 0.09498747735199077,
      AreaC = 0.187048079826102,
      AreaD = 0.3047512946817673,
      AreaE = 0.37852879400458916,
      AreaF = 0.7534667692151087,
      Density = -8.754411353089703e-06,
      RegionAquitaine = -0.05006972584714555,
      RegionAuvergne = -0.2685436498310038,
      `RegionBasse-Normandie` = 0.03663198601389714,
      RegionBourgogne = -0.05270900448597896,
      RegionBretagne = 0.007879177787769805,
      RegionCentre = 0.04112781448054476,
      `RegionChampagne-Ardenne` = -0.17751644960877921,
      RegionCorse = 0.004739475283265479,
      `RegionFranche-Comte` = 0.13791137553603086,
      `RegionHaute-Normandie` = -0.09172377067765793,
      `RegionIle-de-France` = -0.11793808470968409,
      `RegionLanguedoc-Roussillon` = 0.026517531294650443,
      RegionLimousin = 0.185707738525705,
      RegionLorraine = -0.1444734080684801,
      `RegionMidi-Pyrenees` = -0.3722734844103491,
      `RegionNord-Pas-de-Calais` = -0.14617441627015104,
      `RegionPays-de-la-Loire` = 0.07216540567490749,
      RegionPicardie = 0.16669065844932673,
      `RegionPoitou-Charentes` = 0.11191692361170916,
      `RegionProvence-Alpes-Cotes-D'Azur` = -0.028409322255906543,
      `RegionRhone-Alpes` = 0.2464175176544479
    )

  # ============================ comparisons =====================

  testthat::expect_equal(
    explainer_nu$beta_corrections |> colSums(),
    explainer_og$shap_wide_colsums
  )

  testthat::expect_equal(
    explainer_nu$shap |> colSums(),
    explainer_og$raw_shap_colsums
  )
})



testthat::test_that("test explain completes when one categorical and one continuous", {
  vars <- c("VehBrand", "VehPower", "ClaimRate")

  withr::with_seed(1, {
    splits <- freMTPL2freq |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::slice_sample(n = 10000) |>
      dplyr::mutate(ClaimRate = round(ClaimRate)) |>
      split_into_train_validate_test()
  })

  IBLM <- train_iblm(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    explain_iblm(iblm_model = IBLM, data = splits$test)
  )
})

testthat::test_that("test explain completes when categorical only", {
  vars <- c("VehBrand", "VehGas", "Area", "Region", "ClaimRate")

  withr::with_seed(1, {
    splits <- freMTPL2freq |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::slice_sample(n = 10000) |>
      dplyr::mutate(ClaimRate = round(ClaimRate)) |>
      split_into_train_validate_test()
  })

  IBLM <- train_iblm(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    explain_iblm(iblm_model = IBLM, data = splits$test)
  )
})

testthat::test_that("test explain completes when continuous only", {

  vars <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", "Density", "ClaimRate")

  withr::with_seed(1, {
    splits <- freMTPL2freq |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::slice_sample(n = 10000) |>
      dplyr::mutate(ClaimRate = round(ClaimRate)) |>
      split_into_train_validate_test()
  })

  IBLM <- train_iblm(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  testthat::expect_no_error(
    explain_iblm(iblm_model = IBLM, data = splits$test)
  )
})






testthat::test_that("test migrate-to-bias vs non-migrate-to-bias options", {

  # A note on this test...

  # This test compares the predictions with 'migrate_reference_to_bias' as TRUE or FALSE.
  # They should lead to the same predictions

  # ============================ Input data =====================

  withr::with_seed(1, {
    data <- freMTPL2freq |> dplyr::slice_sample(n=50000) |>  split_into_train_validate_test()
  })

  # changing "ClaimRate" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimRate = round(ClaimRate)))

  # ============================ IBLM package process =====================

  IBLM <- train_iblm(
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

