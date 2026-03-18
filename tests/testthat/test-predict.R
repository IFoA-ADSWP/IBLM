testthat::test_that("test corrected beta coeffecient predictions are same as predict iblm()", {
  # A note on this test...

  # This test compares the two alternative ways of deriving predictions of the 'iblm' model.

  # a) By using the 'data_beta_coeff' dataframe of corrected beta coefficients output by explain_iblm().
  #     If we multiply these coefficients by the relevant values (i.e. 1 for bias/categoricals, x for continuous)
  #     We can sum together and apply inverse link function to get the prediction glm-style.
  # b) By using the predict() function, which will use predict.iblm() method from the iblm package

  # In theory, the results should be very similar (not expect identical due to shap noise).

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimNb / Exposure) |>
    dplyr::select(-ClaimNb) |>
    dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
    dplyr::mutate(ClaimRate = round(ClaimRate)) |>
    split_into_train_validate_test(seed = 1)


  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    family = "poisson"
  )

  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test, migrate_reference_to_bias = TRUE)

  coeff_multiplier <- splits$test |>
    dplyr::select(-dplyr::all_of("ClaimRate")) |>
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

  predict_w_predict <- predict(IBLM, splits$test)

  prediction_max_difference <- max(abs(predict_w_beta_coeff / predict_w_predict - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-6
    # the tolerance is a bit higher for this test because...
    # ...shap values are estimates and so there is expected noise between two methods
  )
})




testthat::test_that("test multi/add predict() method gives same answer as base_margin method", {
  # A note on this test...

  # This test compares the two alternative ways of deriving predictions of the 'iblm' model.

  # a) By using the 'data_beta_coeff' dataframe of corrected beta coefficients output by explain_iblm().
  #     If we multiply these coefficients by the relevant values (i.e. 1 for bias/categoricals, x for continuous)
  #     We can sum together and apply inverse link function to get the prediction glm-style.
  # b) By using the predict() function, which will use predict.iblm() method from the iblm package

  # In theory, the results should be very similar (not expect identical due to shap noise).

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimNb / Exposure) |>
    dplyr::select(-ClaimNb) |>
    split_into_train_validate_test(seed = 1)

  withr::with_seed(1, {
    splits_gamma <- splits |>
      purrr::modify(.f = function(x) dplyr::mutate(x, ClaimRate = rgamma(nrow(x), DrivAge/10, VehPower/1000)))
  }
  )

  # ============================ IBLM package process =====================

  IBLM_poisson <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = "Exposure",
    family = "quasipoisson"
  )

  IBLM_gaussian <- train_iblm_xgb(
    splits,
    response_var = "ClaimRate",
    weight_var = "Exposure",
    family = "gaussian"
  )

  IBLM_gamma <- train_iblm_xgb(
    splits_gamma,
    response_var = "ClaimRate",
    weight_var = "Exposure",
    family = "gamma"
  )

  # ================== Base Margin version of predict() function ================

  predict_base_margin_method <- function(object, newdata, type = "response") {
    response_var <- object$response_var
    weight_var <- object$weight_var
    data <- newdata |> dplyr::select(-dplyr::any_of(c(response_var, weight_var)))
    glm_links <- unname(stats::predict(object$glm_model, data, type = "link"))
    toreturn <- stats::predict(object$booster_model, xgboost::xgb.DMatrix(data, base_margin = glm_links), type = type)
    return(toreturn)
  }


  # ============================ Check the two predict() outcomes =====================

  predict_vs_predict <- function(IBLM_model, test_data) {

    predict_w_predict <- predict(IBLM_model, test_data)
    predict_w_base_margin <- predict_base_margin_method(IBLM_model, test_data)
    prediction_max_difference <- max(abs(predict_w_base_margin / predict_w_predict - 1))
    prediction_mean_difference <- mean(predict_w_base_margin / predict_w_predict - 1)
    testthat::expect_equal(prediction_max_difference, 0, tolerance = 1E-4)
    testthat::expect_equal(prediction_mean_difference, 0, tolerance = 1E-6)

  }

  # poisson

  predict_vs_predict(IBLM_poisson, splits$test)

  predict_vs_predict(IBLM_gaussian, splits$test)

  predict_vs_predict(IBLM_gamma, splits$test)


})



testthat::test_that("test multi/add predict() method gives same answer as base_margin method when offset", {
  # A note on this test...

  # This test compares the two alternative ways of deriving predictions of the 'iblm' model.

  # a) By using the 'data_beta_coeff' dataframe of corrected beta coefficients output by explain_iblm().
  #     If we multiply these coefficients by the relevant values (i.e. 1 for bias/categoricals, x for continuous)
  #     We can sum together and apply inverse link function to get the prediction glm-style.
  # b) By using the predict() function, which will use predict.iblm() method from the iblm package

  # In theory, the results should be very similar (not expect identical due to shap noise).

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
    split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  IBLM_model <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    family = "quasipoisson"
  )

  # ================== Base Margin version of predict() function ================

  predict_base_margin_method <- function(object, newdata, type = "response") {
    response_var <- object$response_var
    weight_var <- object$weight_var
    offset_var <- object$offset_var
    data <- newdata |> dplyr::select(-dplyr::any_of(c(response_var, weight_var)))
    glm_links <- unname(stats::predict(object$glm_model, data, type = "link"))
    toreturn <- stats::predict(
      object$booster_model,
      xgboost::xgb.DMatrix(
        data |> dplyr::select(-dplyr::any_of(offset_var)),
        base_margin = glm_links),
      type = type)
    return(toreturn)
  }


  # ============================ Check the two predict() outcomes =====================

  test_data <- splits$test
    predict_w_predict <- predict(IBLM_model, test_data)
    predict_w_base_margin <- predict_base_margin_method(IBLM_model, test_data)
    prediction_max_difference <- max(abs(predict_w_base_margin / predict_w_predict - 1))
    prediction_mean_difference <- mean(predict_w_base_margin / predict_w_predict - 1)
    testthat::expect_equal(prediction_max_difference, 0, tolerance = 1E-4)
    testthat::expect_equal(prediction_mean_difference, 0, tolerance = 1E-6)


})





testthat::test_that("test predict.iblm() link versus response", {

  df_list <- freMTPLmini |>
    dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
    split_into_train_validate_test(seed = 9000)

  iblm_model <- train_iblm_xgb(
    df_list,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    family = "poisson"
  )

  predictions <- predict(iblm_model, df_list$test, type = "response")

  links <- predict(iblm_model, df_list$test, type = "link")

  prediction_max_difference <- max(abs(predictions / exp(links)  - 1))

  testthat::expect_equal(
    prediction_max_difference,
    0,
    tolerance = 1E-8
  )
})

