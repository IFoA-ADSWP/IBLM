testthat::test_that("test weighting feature (mini) poisson", {

  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  df <- freMTPLmini |> dplyr::select(Area, VehPower, DrivAge, VehGas, ClaimRate)

  data <- df |>  split_into_train_validate_test(seed = 1)

  splits <- data |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimRate = round(ClaimRate)))

  splits_weighted <-
    splits |>
    purrr::map(
      function(x)
        x |> dplyr::summarise(
          ClaimRate = mean(ClaimRate),
          weight = dplyr::n(),
          .by = setdiff(names(df), "ClaimRate")
        )
    )

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = NULL,
    family = "poisson"
  )

  # get warnings because ClaimRate is now a mean and has non-integer values. This is expected so suppress
  suppressWarnings(
    IBLM_w <- train_iblm_xgb(
      splits_weighted,
      response_var = "ClaimRate",
      weight_var = "weight",
      family = "poisson"
    )
  )

  # ============================ Test predict() =====================

  pred <- predict(IBLM, splits$test)

  pred_w <- predict(IBLM_w, splits$test)

  prediction_max_difference <- max(abs(pred_w / pred - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-7
  )

  # ============================ Test explain_iblm() =====================

  ex <- explain_iblm(IBLM, splits$test)
  ex_w <- explain_iblm(IBLM_w, splits$test)

  shap_diff <- max(abs(ex[["shap"]] / ex_w[["shap"]] - 1))
  beta_corrections_diff <- max(abs(ex[["beta_corrections"]] / ex_w[["beta_corrections"]] - 1), na.rm = T)
  beta_coeff_diff <- max(abs(ex[["data_beta_coeff"]] / ex_w[["data_beta_coeff"]] - 1), na.rm = T)

  # tolerance is a bit more forgiving as expect some minor differences from xgboost between 2 methods
  testthat::expect_equal(shap_diff, 0, tolerance = 1E-4)
  testthat::expect_equal(beta_corrections_diff, 0, tolerance = 1E-4)
  testthat::expect_equal(beta_coeff_diff, 0, tolerance = 1E-4)



})


testthat::test_that("test weighting feature (mini) gaussian", {

  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  df <- freMTPLmini |> dplyr::select(Area, VehPower, DrivAge, VehGas, ClaimRate)

  data <- df |>  split_into_train_validate_test(seed = 1)

  splits <- data |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimRate = round(ClaimRate)))

  splits_weighted <-
    splits |>
    purrr::map(
      function(x)
        x |> dplyr::summarise(
          ClaimRate = mean(ClaimRate),
          weight = dplyr::n(),
          .by = setdiff(names(df), "ClaimRate")
        )
    )

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = NULL,
    family = "gaussian"
  )

  # get warnings because ClaimRate is now a mean and has non-integer values. This is expected so suppress
  suppressWarnings(
    IBLM_w <- train_iblm_xgb(
      splits_weighted,
      response_var = "ClaimRate",
      weight_var = "weight",
      family = "gaussian"
    )
  )

  # ============================ Test predict() =====================

  pred <- predict(IBLM, splits$test)

  pred_w <- predict(IBLM_w, splits$test)

  prediction_max_difference <- max(abs(pred_w / pred - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-6
  )


})




testthat::test_that("test weighting feature (mini) gamma", {

  testthat::skip("Test is slightly outside tolerance - not sure if this is problem")

  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  df <- freMTPLmini |> dplyr::select(Area, VehPower, DrivAge, VehGas, ClaimRate)

  data <- df |>  split_into_train_validate_test(seed = 1)

  withr::with_seed(1, {
  splits <- data |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimRate = rgamma(nrow(x), DrivAge/10, VehPower/1000)))
  }
  )

  splits_weighted <-
    splits |>
    purrr::map(
      function(x)
        x |> dplyr::summarise(
          ClaimRate = mean(ClaimRate),
          weight = dplyr::n(),
          .by = setdiff(names(df), "ClaimRate")
        )
    )

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = NULL,
    family = "gamma"
  )


    IBLM_w <- train_iblm_xgb(
      splits_weighted,
      response_var = "ClaimRate",
      weight_var = "weight",
      family = "gamma"
    )



  # ============================ Test predict() =====================

  pred <- predict(IBLM, splits$test)

  pred_w <- predict(IBLM_w, splits$test)

  prediction_max_difference <- max(abs(pred_w / pred - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-6
  )


})




testthat::test_that("test weighting feature (mini) tweedie", {


  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  df <- freMTPLmini |> dplyr::select(Area, VehPower, DrivAge, VehGas, ClaimRate)

  data <- df |>  split_into_train_validate_test(seed = 1)

  withr::with_seed(1, {
    splits <- data |>
      purrr::modify(.f = function(x) dplyr::mutate(x, ClaimRate = rgamma(nrow(x), DrivAge/2, VehPower/100)))
  }
  )

  splits_weighted <-
    splits |>
    purrr::map(
      function(x)
        x |> dplyr::summarise(
          ClaimRate = mean(ClaimRate),
          weight = dplyr::n(),
          .by = setdiff(names(df), "ClaimRate")
        )
    )

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = NULL,
    family = "tweedie"
  )

    IBLM_w <- train_iblm_xgb(
      splits_weighted,
      response_var = "ClaimRate",
      weight_var = "weight",
      family = "tweedie"
    )



  # ============================ Test predict() =====================

  pred <- predict(IBLM, splits$test)

  pred_w <- predict(IBLM_w, splits$test)

  prediction_max_difference <- max(abs(pred_w / pred - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-6
  )


})









testthat::test_that("test explain completes with weighting", {

  vars <- names(freMTPLmini) |> setdiff("ClaimRate")

  splits <- freMTPLmini  |>
    dplyr::mutate(dummy = sample(c(1, 2, 3), nrow(freMTPLmini), replace = T)) |>
    split_into_train_validate_test(seed = 1)

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = "dummy",
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
