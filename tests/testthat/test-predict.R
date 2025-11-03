testthat::test_that("test corrected beta coeffecient predictions are same as predict iblm()", {
  # A note on this test...

  # This test compares the two alternative ways of deriving predictions of the 'iblm' model.

  # a) By using the 'data_beta_coeff' dataframe of corrected beta coefficients output by explain_iblm().
  #     If we multiply these coefficients by the relevant values (i.e. 1 for bias/categoricals, x for continuous)
  #     We can sum together and apply inverse link function to get the prediction glm-style.
  # b) By using the predict() function, which will use predict.iblm() method from the iblm package

  # In theory, the results should be very similar (not expect identical due to shap noise).

  # ============================ Input data =====================

  data <- freMTPLmini |>  split_into_train_validate_test(seed = 1)


  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimRate = round(ClaimRate)))

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

