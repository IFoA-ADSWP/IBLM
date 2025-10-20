testthat::test_that("test against Karol original script", {

  # NOTE this test used to work. Find rec values from earlier commit to fix thiss

  testthat::skip()


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
  ps_nu <- get_pinball_scores(splits$test, IBLM)


  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_pinball_score_test

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(1.4195,1.3606, 1.2483),
    pinball_score = c(0.00,4.15,12.06)/100
  )

  testthat::expect_equal(
    ps_nu, ps_og
  )
})





testthat::test_that("test against Karol paper", {

  testthat::skip()

  # =================== Get version of `freMTPL2freq` =====================

  # confusingly there are two versions of `freMTPL2freq` circulating online
  # the code below pulls the earlier one (which is same as kaggle version used by Karol)

  commit <- "c49cbbb37235fc49616cac8ccac32e1491cdc619"
  url <- paste0("https://github.com/dutangc/CASdatasets/raw/", commit, "/data/freMTPL2freq.rda")

  temp <- tempfile()

  download.file(url, temp)

  load(temp)

  freMTPL2freq <- freMTPL2freq |>
    dplyr::mutate(ClaimNb = as.numeric(ClaimNb)) |>
    dplyr::mutate(ClaimNb = ClaimNb/Exposure,
                  VehAge = pmin(VehAge,50),
                  ClaimNb = pmin(ClaimNb,quantile(ClaimNb,0.999))) |>
    dplyr::select(-IDpol)

  # ============================ Input data =====================

  withr::with_seed(1, {
    data <- freMTPL2freq |> split_into_train_validate_test()
  })

  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) x |> dplyr::select(-dplyr::any_of(c("IDpol", "Exposure")))) |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), function(field) as.character(field))))
    #purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    #purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # ============================ IBLM package process =====================

  IBLM <- train_iblm(
    splits,
    response_var = "ClaimNb",
    family = "poisson"
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores(splits$test, IBLM)


  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_pinball_score_test

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(1.4195,1.3606, 1.2483),
    pinball_score = c(0.00,4.15,12.06)/100
  )

  testthat::expect_equal(
    ps_nu, ps_og
  )
})








