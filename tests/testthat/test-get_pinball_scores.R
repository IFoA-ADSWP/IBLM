
testthat::test_that("test against Karol original script", {

  testthat::skip_on_cran() # code too long for CRAN, but useful test


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
    poisson_deviance = c(0.6821738013621665, 0.6614369861117713, 0.6561672090472287),
    pinball_score = c(0, 0.03039814078023506, 0.03812311798402079)
  )

  testthat::expect_equal(
    ps_nu, ps_og
  )

})





testthat::test_that("test against Karol paper", {

  testthat::skip("The results of this test are **close** but not identical. Decide on whether to investigate")

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

  ps_nu <- ps_nu |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("poisson_deviance", "pinball_score")),
          function(x) round(x, 4)
        )
      )



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


  testthat::expect_equal(ps_nu, ps_og)

})





testthat::test_that("test results are same for character or factor fields", {

  # ============================ Input data =====================

  withr::with_seed(1, {
    data <- freMTPL2freq |> dplyr::slice_sample(n=50000) |> split_into_train_validate_test() # partial data to speed up test
  })

  # get data where categoricals are factors
  splits_fct <- data |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.character), function(field) as.factor(field)))) |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # get identical data where categoricals are strings
  splits_chr <- splits_fct |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), function(field) as.character(field))))

  # ============================ IBLM package process =====================

  IBLM_fct <- train_iblm(
    splits_fct,
    response_var = "ClaimNb",
    family = "poisson"
  )

  IBLM_chr <- train_iblm(
    splits_chr,
    response_var = "ClaimNb",
    family = "poisson"
  )

  ps_fct <- get_pinball_scores(splits_fct$test, IBLM_fct)

  ps_chr <- get_pinball_scores(splits_chr$test, IBLM_chr)

  testthat::expect_equal(ps_fct, ps_chr)

})


