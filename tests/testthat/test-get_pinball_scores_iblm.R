testthat::test_that("test against Karol original script", {

  # ============================ Input data =====================

  withr::with_seed(1,
                   {data <- freMTPL2freq |> split_into_train_validate_test()}
  )

  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), function(field) as.character(field)))) |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # ============================ IBLM package process =====================

  IBLM <- train_glm_xgb(
    splits,
    response_var = "ClaimNb",
    family= "poisson"
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores_iblm(splits$test, IBLM)


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
