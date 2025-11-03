
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

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    family = "poisson"
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores(splits$test, IBLM)


  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM_testing` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_pinball_score_test

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(0.6821738013621665, 0.6614369861117713, 0.6561672090472287),
    pinball_score = c(0, 0.03039814078023506, 0.03812311798402079)
  )

  testthat::expect_equal(
    ps_nu, ps_og, tolerance = 1E-6 # tolerance lowered because calcs were changed in package function that give slightly diff answer
  )

})





testthat::test_that("test against Karol paper", {


  # test takes too long for CRAN
  testthat::skip_on_cran()


  # ============================ Download data =====================

  # download the correct version of freMTPL2freq dataset to complete the rec

  commit <- "c49cbbb37235fc49616cac8ccac32e1491cdc619"  # <- use this commit

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
    dplyr::select(-dplyr::all_of(c("IDpol", "Exposure", "ClaimNb")))


  # ============================ Input data =====================

  data <- freMTPL2freq |> split_into_train_validate_test(seed = 1)


  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) x |> dplyr::select(-dplyr::any_of(c("IDpol", "Exposure")))) |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), function(field) as.character(field)))) |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate"))
    #purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # ============================ IBLM package process =====================

  # warning are given because of non-integer response vars and a poisson predictor...
  # ...just have to suppress for this test as we cannot change data...
  suppressWarnings(
  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    family = "poisson",
    # additional param settings required for rec...
      params = list(
        base_score = 0.5,
        objective = "count:poisson"
        ),
      nrounds = 1000,
      verbose = 0,
      early_stopping_rounds = 25
  )
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores(splits$test, IBLM)

  ps_nu <- ps_nu |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("poisson_deviance", "pinball_score")),
          function(x) round(x, 4)
        )
      )



  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM_testing` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_pinball_score_test

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(1.4195,1.3606, 1.2483),
    pinball_score = c(0.00,4.15,12.06)/100
  )


  testthat::expect_equal(ps_nu, ps_og)

})





testthat::test_that("test results are same for character or factor fields", {

  # ============================ Input data =====================

  data <- freMTPLmini |> head(25000) |> split_into_train_validate_test(seed = 1)

  # get data where categoricals are factors
  splits_fct <- data |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.character), function(field) as.factor(field)))) |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # get identical data where categoricals are strings
  splits_chr <- splits_fct |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), function(field) as.character(field))))

  # ============================ IBLM package process =====================

  IBLM_fct <- train_iblm_xgb(
    splits_fct,
    response_var = "ClaimNb",
    family = "poisson"
  )

  IBLM_chr <- train_iblm_xgb(
    splits_chr,
    response_var = "ClaimNb",
    family = "poisson"
  )

  ps_fct <- get_pinball_scores(splits_fct$test, IBLM_fct)

  ps_chr <- get_pinball_scores(splits_chr$test, IBLM_chr)

  testthat::expect_equal(ps_fct, ps_chr)

})






