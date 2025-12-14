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

  # commented out:
  # previous test result using xgboost v3.1.2.1 but not including data.matrix() fix

  # IBLM_og$booster_model$evaluation_log <-
  # data.frame(
  #   iter = 1:51,
  #   validation_poisson_nloglik = c(
  #     4.056412669623219, 4.049391971186827, 4.04593601499781, 4.040796560592584,
  #     4.038147681033262, 4.037023673948217, 4.035670705689747, 4.03558868150948,
  #     4.034461414394595, 4.0350597042646505, 4.033899296287862, 4.034118170740442,
  #     4.0336632400412, 4.033490046582557, 4.033011656338575, 4.033969343580477,
  #     4.033498235629004, 4.032802075205225, 4.032345918316999, 4.031624212365903,
  #     4.03118653271876, 4.030500840845004, 4.030124733061418, 4.030014329129201,
  #     4.028939285980656, 4.028043152320707, 4.029473892921388, 4.029125284354635,
  #     4.028950720739686, 4.028619772708841, 4.028666405145946, 4.029250004327979,
  #     4.02853665670761, 4.030008083076863, 4.029501758779282, 4.029577520962253,
  #     4.029540839021276, 4.029386050326882, 4.02949880839609, 4.0289731192691045,
  #     4.0294204893222005, 4.029389321784211, 4.029361198219802, 4.028683057475174,
  #     4.028373390836119, 4.0284907752229975, 4.02853633067712, 4.030251662234247,
  #     4.030247865018032, 4.0301320481799445, 4.029976138293727
  #   )
  # )

  IBLM_og$booster_model$evaluation_log <-
    data.frame(
      iter = 1:42,
      validation_poisson_nloglik = c(
        4.055663071821976, 4.0511376224278965, 4.047429231854068, 4.043382042312346,
        4.040398398015043, 4.038383840658861, 4.035323472723095, 4.033685188183917,
        4.033753625098215, 4.033098768753775, 4.0323387971392535, 4.031543764158868,
        4.031739708292138, 4.032990946799782, 4.030436560262982, 4.029935233654651,
        4.029625360265585, 4.031376165789747, 4.032221250794373, 4.032896658696438,
        4.03057581403106, 4.0309315416143505, 4.029629209625946, 4.0305141145064445,
        4.030329241380248, 4.029850974831948, 4.030353241269721, 4.030224678768604,
        4.030504354251887, 4.029639271228043, 4.030380707268085, 4.030557012210286,
        4.031261938765065, 4.030918106713254, 4.030792515377258, 4.030857810165818,
        4.031318963031328, 4.031009757468136, 4.0310946117681485, 4.031450972544412,
        4.031814775847612, 4.03165021015748
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

