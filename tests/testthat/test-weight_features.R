testthat::test_that("test weighting feature (mini) poisson", {

  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  df <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimNb / Exposure) |>
    dplyr::select(Area, VehPower, DrivAge, ClaimRate)

  splits <- df |>  split_into_train_validate_test(seed = 1)


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
    family = "quasipoisson"
  )

  IBLM_w <- train_iblm_xgb(
    splits_weighted,
    response_var = "ClaimRate",
    weight_var = "weight",
    family = "quasipoisson"
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


testthat::test_that("test weighting feature (mini) gaussian", {

  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  df <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimNb / Exposure) |>
    dplyr::select(Area, VehPower, DrivAge, ClaimRate)

  splits <- df |>  split_into_train_validate_test(seed = 1)

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

  IBLM_w <- train_iblm_xgb(
    splits_weighted,
    response_var = "ClaimRate",
    weight_var = "weight",
    family = "gaussian"
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

  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  withr::with_seed(1, {
  df <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimNb / Exposure) |>
    dplyr::select(Area, VehPower, DrivAge, ClaimRate) |>
    dplyr::mutate(ClaimRate = rgamma(nrow(freMTPLmini), DrivAge/10, VehPower/1000))
  }
  )

  splits <- df |>  split_into_train_validate_test(seed = 1)

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
    tolerance = 1E-5
  )


})




testthat::test_that("test weighting feature (mini) tweedie", {


  # A note on this test...

  # This test compares the IBLM trained model predictions from:
  # an ungrouped dataset
  # the same dataset, but grouped, with a "weight" col.

  # We expect same outcome if weighting feature is applied correctly.


  # ============================ Input data =====================

  withr::with_seed(1, {
    df <- freMTPLmini |>
      dplyr::mutate(ClaimRate = ClaimNb / Exposure) |>
      dplyr::select(Area, VehPower, DrivAge, ClaimRate) |>
      dplyr::mutate(ClaimRate = rgamma(nrow(freMTPLmini), DrivAge/10, VehPower/1000))
  }
  )

  splits <- df |>  split_into_train_validate_test(seed = 1)


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
    tolerance = 1E-5
  )


})









testthat::test_that("test explain completes with weighting", {

  # ============================ Input data =====================

  df <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimNb / Exposure) |>
    dplyr::select(Area, VehPower, DrivAge, ClaimRate)

  splits <- df |>  split_into_train_validate_test(seed = 1)


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
    family = "quasipoisson"
  )

  IBLM_w <- train_iblm_xgb(
    splits_weighted,
    response_var = "ClaimRate",
    weight_var = "weight",
    family = "quasipoisson"
  )


  # ============================ Test explain_iblm() =====================

  ex <- explain_iblm(IBLM, splits$test)
  ex_w <- explain_iblm(IBLM_w, splits$test)

  # max. difference
  beta_corrections_diff <- max(abs(ex[["beta_corrections"]] / ex_w[["beta_corrections"]] - 1), na.rm = T)
  beta_coeff_diff <- max(abs(ex[["data_beta_coeff"]] / ex_w[["data_beta_coeff"]] - 1), na.rm = T)

  testthat::expect_equal(beta_corrections_diff, 0, tolerance = 1E-4)
  testthat::expect_equal(beta_coeff_diff, 0, tolerance = 1E-4)

  # mean difference
  beta_corrections_mean_diff <- mean(
    (ex[["beta_corrections"]] / ex_w[["beta_corrections"]]) |> unlist(),
    na.rm = T
  ) - 1
  beta_coeff_mean_diff <- mean(
    (ex[["data_beta_coeff"]] / ex_w[["data_beta_coeff"]]) |> unlist(),
    na.rm = T
  ) - 1

  testthat::expect_equal(beta_corrections_mean_diff, 0, tolerance = 1E-8)
  testthat::expect_equal(beta_coeff_mean_diff, 0, tolerance = 1E-8)


  testthat::expect_no_error(
    {
      ex_w$beta_corrected_scatter(names(df)[1])
      ex_w$beta_corrected_density(names(df)[1])
      ex_w$overall_correction()
      ex_w$bias_density()
    }
  )
})

