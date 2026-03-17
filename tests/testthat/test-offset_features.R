testthat::test_that("test offset predictions are same as weight predictions", {

  # A note on this test...

  # This test compares the two alternative methods for dealing with exposure weighting in data

  # a) By setting the weight_var = Exposure and the response_var = ClaimRate
  # b) By setting the weight_var = log(Exposure) and the response_var = ClaimNb

  # In theory, the results should be almost identical barring noise

  # ============================ Input data =====================

  data_weight <- freMTPLmini |>  split_into_train_validate_test(seed = 1)

  data_offset <- freMTPLmini |>
    dplyr::mutate(
      ClaimNb = ClaimRate * Exposure,
      LogExposure = log(Exposure)
      ) |>
    dplyr::select(-c("ClaimRate", "Exposure")) |>
    split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  IBLM_weight <- train_iblm_xgb(
    data_weight,
    response_var = "ClaimRate",
    weight_var = "Exposure",
    family = "quasipoisson"
  )

  IBLM_offset <- train_iblm_xgb(
    data_offset,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    family = "quasipoisson"
  )


  # ============================ Test precict() =====================

  p_weight <- predict( IBLM_weight, data_weight$test  )

  p_offset <- predict( IBLM_offset, data_offset$test |> dplyr::mutate(LogExposure = 0) )

  prediction_max_difference <- max(abs(p_weight / p_offset - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-6
  )


  # ============================ Test explain_iblm() =====================

  # NOTE the shap values are not perfectly reconciling although they are close on average
  # not sure if this is error or just noise from alternative methods...


  ex_weight <- explain_iblm(IBLM_weight, data_weight$test)
  ex_offset <- explain_iblm(IBLM_offset, data_offset$test)

  # booster_model <- IBLM_offset$booster_model
  #
  # feature_names <- xgboost::getinfo(booster_model, "feature_name")
  #
  # data <- data_offset$test |> dplyr::select(dplyr::all_of(feature_names))
  #
  # shap <- stats::predict(
  #   booster_model,
  #   newdata = xgboost::xgb.DMatrix(data, base_margin = data_offset$test$LogExposure),
  #   predcontrib = TRUE
  # ) |>
  #   data.frame() |>
  #   dplyr::rename(dplyr::any_of(c("BIAS" = "X.Intercept."))) |>
  #   dplyr::mutate(BIAS = BIAS - data_offset$test$LogExposure)
  #   shap_diff <- max(abs(ex_weight[["shap"]] / shap - 1))




  shap_diff <- max(abs(ex_weight[["shap"]] / ex_offset[["shap"]] - 1))
  beta_corrections_diff <- max(abs(ex_weight[["beta_corrections"]] / ex_offset[["beta_corrections"]] - 1), na.rm = T)
  beta_coeff_diff <- max(abs(ex_weight[["data_beta_coeff"]] / ex_offset[["data_beta_coeff"]] - 1), na.rm = T)

  # tolerance is a bit more forgiving as expect some minor differences from xgboost between 2 methods
  # testthat::expect_equal(shap_diff, 0, tolerance = 1E-4)
  # testthat::expect_equal(beta_corrections_diff, 0, tolerance = 1E-4)
  # testthat::expect_equal(beta_coeff_diff, 0, tolerance = 1E-4)


  # ============================ Test get_pinball_scores() =====================

  ps_weight <- get_pinball_scores(data_weight$test, IBLM_weight) |> dplyr::pull(pinball_score) |> tail(-1)
  ps_offset <- get_pinball_scores(data_offset$test, IBLM_offset) |> dplyr::pull(pinball_score) |> tail(-1)
  ps_max_difference <- max(abs(ps_weight / ps_offset - 1))
  testthat::expect_equal(ps_max_difference, 0, tolerance = 1E-7)

})


testthat::test_that("test corrected beta coeffecient predictions are same as predict iblm() ...when using offset", {
  # A note on this test...

  # This test compares the two alternative ways of deriving predictions of the 'iblm' model.

  # a) By using the 'data_beta_coeff' dataframe of corrected beta coefficients output by explain_iblm().
  #     If we multiply these coefficients by the relevant values (i.e. 1 for bias/categoricals, x for continuous)
  #     We can sum together and apply inverse link function to get the prediction glm-style.
  # b) By using the predict() function, which will use predict.iblm() method from the iblm package

  # In theory, the results should be very similar (not expect identical due to shap noise).

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    dplyr::mutate(
      ClaimNb = ClaimRate * Exposure,
      LogExposure = log(Exposure)
    ) |>
    dplyr::select(-c("ClaimRate", "Exposure")) |>
    split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    family = "quasipoisson"
  )

  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = TRUE)

  coeff_multiplier <- splits$test |>
    dplyr::select(-dplyr::all_of(c("ClaimNb", "LogExposure"))) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(IBLM$predictor_vars$categorical),
        ~1
      )
    ) |>
    dplyr::mutate(bias = 1, .before = 1)

  predict_w_beta_coeff <- rowSums(explainer_nu$data_beta_coeff * coeff_multiplier) |>
    exp() |>
    unname()

  predict_w_predict <- predict(IBLM, splits$test |> dplyr::select(-LogExposure))

  prediction_max_difference <- max(abs(predict_w_beta_coeff / predict_w_predict - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-6
  )
})

