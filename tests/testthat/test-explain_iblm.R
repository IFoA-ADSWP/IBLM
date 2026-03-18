
testthat::test_that("test explain completes when one categorical and one continuous", {
  vars <- c("VehBrand", "VehPower", "ClaimNb")

  splits <- freMTPLmini  |>
    dplyr::select(dplyr::all_of(vars)) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
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
  vars <- c("VehBrand", "Area", "ClaimNb")


  splits <- freMTPLmini  |>
    dplyr::select(dplyr::all_of(vars)) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
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

  vars <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", "ClaimNb")

  splits <- freMTPLmini  |>
    dplyr::select(dplyr::all_of(vars)) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
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
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    weight_var = "Exposure",
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

  vars <- c("VehPower", "VehAge", "DrivAge", "BonusMalus", "ClaimNb")

  splits <- freMTPLmini  |>
    dplyr::select(dplyr::all_of(vars)) |>
    dplyr::mutate(dplyr::across(-dplyr::all_of("ClaimNb"), \(x) pmax(x, 1))) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
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
    response_var = "ClaimNb",
    weight_var = "Exposure",
    family = "poisson"
  )

  explainer_w_migrate <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = TRUE)

  explainer_wout_migrate <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = FALSE)

  coeff_multiplier <- splits$test |>
    dplyr::select(-dplyr::all_of(c("ClaimNb", "Exposure"))) |>
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
    response_var = "ClaimNb",
    weight_var = "Exposure",
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
    dplyr::mutate(ClaimNb = round(ClaimNb) |> pmax(0.1)) |> # set min. ClaimNb to 0.1 as pre-requisite for gamma dist is x>0
    split_into_train_validate_test(seed = 1)


  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    weight_var = "Exposure",
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
    response_var = "ClaimNb",
    weight_var = "Exposure",
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
    response_var = "ClaimNb",
    family = "poisson",
    weight_var = "Exposure",
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


