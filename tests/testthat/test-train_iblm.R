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

  IBLM_nu <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    family = "poisson",
    verbose = 2,
    params = list(seed = 0, tree_method = "auto")
  )

  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM_testing` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_iblm_model_test

  IBLM_og <- list()
  IBLM_og$glm_model$coefficients <-
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


  IBLM_og$booster_model$evaluation_log <-
    data.frame(
      iter = 1:38,
      validation_poisson_nloglik = c(
        0.37732617223974435, 0.3766871263275117, 0.3763515186732858,
        0.37619019884452237, 0.37591618598946297, 0.3757923072341154,
        0.3756108509228318, 0.3754724350143468, 0.37540847506100317,
        0.3752146694676997, 0.37506592729499677, 0.3750464936017373,
        0.3750213544499583, 0.37514673648859787, 0.37519092208946003,
        0.37521570496045276, 0.3752069478692766, 0.3752163517283868,
        0.37523039005023606, 0.3752278997088973, 0.37524900648250803,
        0.37525102372277835, 0.3752353030906068, 0.37529852642468386,
        0.3751987001975535, 0.37533833070415457, 0.37530829248301883,
        0.3753192423780824, 0.3752910237008038, 0.3753075558498672, 0.375267226453425,
        0.37528822999066846, 0.3752240001965543, 0.375259934091958,
        0.37516692799224804, 0.3751715016138348, 0.37532090713778243,
        0.37537006858646355
      )
    )


  # ============================ comparisons =====================

  # was GLM fitted the same coefficients?
  testthat::expect_equal(
    IBLM_nu$glm_model$coefficients,
    IBLM_og$glm_model$coefficients
  )

  # was XGBoost fitted with the same log?
  testthat::expect_equal(
    IBLM_nu$booster_model |> attr("evaluation_log") |> as.data.frame(),
    IBLM_og$booster_model$evaluation_log
  )
})


testthat::test_that("glm_model input is optional and reusable", {
  df_list <- freMTPLmini |>
    dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
    split_into_train_validate_test(seed = 123)

  iblm_default <- train_iblm_xgb(
    df_list,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    family = "poisson",
    nrounds = 5,
    early_stopping_rounds = NULL,
    params = list(seed = 99)
  )

  testthat::expect_s3_class(iblm_default$glm_model, "glm")

  external_glm <- stats::glm(
    ClaimNb ~ . + offset(LogExposure),
    data = df_list$train |>
      dplyr::select(-dplyr::any_of(c("weight"))),
    family = stats::poisson()
  )

  iblm_external <- train_iblm_xgb(
    df_list,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    glm_model = external_glm,
    family = "poisson",
    nrounds = 5,
    early_stopping_rounds = NULL,
    params = list(seed = 99)
  )

  testthat::expect_equal(
    unname(iblm_external$glm_model$coefficients),
    unname(external_glm$coefficients)
  )
})





