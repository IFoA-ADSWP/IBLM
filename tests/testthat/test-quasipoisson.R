
testthat::test_that("test quasipoisson vs poisson", {

  # A note on this test...

  # This test compares quasipoisson outputs vs poisson across various functions

  # expect almost identical results


  # ============================ Input data =====================

  splits <- freMTPLmini |>  split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  IBLM_q <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = "Exposure",
    family = "quasipoisson"
  )

  # get warnings because ClaimRate is now a mean and has non-integer values. This is expected so suppress
  suppressWarnings(
    IBLM_p <- train_iblm_xgb(
      splits,
      response_var = "ClaimRate",
      weight_var = "Exposure",
      family = "poisson"
    )
  )

  # ============================ Test predict() =====================

  pred_p <- predict(IBLM_p, splits$test)

  pred_q <- predict(IBLM_q, splits$test)

  prediction_max_difference <- max(abs(pred_p / pred_q - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-7
  )

  # ============================ Test get_pinball_scors() =====================

  pb_p <- get_pinball_scores(splits$test, IBLM_p)

  pb_q <- get_pinball_scores(splits$test, IBLM_q)

  testthat::expect_equal(
    pb_p,
    pb_q,
    tolerance = 1E-7
  )



})
