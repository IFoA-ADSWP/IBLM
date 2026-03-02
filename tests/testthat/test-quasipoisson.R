
testthat::test_that("test weighting feature (mini) quasipoisson", {

  # A note on this test...

  # This test compares quasipoisson outputs vs poisson across various functions

  # expect almost identical results


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

  IBLM_q <- train_iblm_xgb(
    splits_weighted,
    response_var = "ClaimRate",
    weight_var = "weight",
    family = "quasipoisson"
  )

  # get warnings because ClaimRate is now a mean and has non-integer values. This is expected so suppress
  suppressWarnings(
    IBLM_p <- train_iblm_xgb(
      splits_weighted,
      response_var = "ClaimRate",
      weight_var = "weight",
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

  pb_p <- get_pinball_scores(splits_weighted$test, IBLM_p)

  pb_q <- get_pinball_scores(splits_weighted$test, IBLM_p)

  testthat::expect_equal(
    pb_p,
    pb_q,
    tolerance = 1E-7
  )



})
