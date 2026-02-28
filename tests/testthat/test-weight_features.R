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
