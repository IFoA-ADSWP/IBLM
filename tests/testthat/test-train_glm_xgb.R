testthat::test_that("test against Karol original script", {


  # ============================ Input data =====================

  data <- freMTPL2freq |> split_into_train_validate_test()

  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  data <- data |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(tidyselect::where(is.factor), function(field) as.character(field)))) |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # the input data for KG script is called `splits`
  splits <- data

  # ============================ IBLM package process =====================

  IBLM2 <- train_glm_xgb(
    data,
    response_var = "ClaimNb",
    family= "poisson"
  )

  # ============================ OG Karol process =====================

  withr::local_package("tidyverse")
  withr::local_package("xgboost")

  response = "ClaimNb"
  predictors = colnames(splits$train) |> setdiff(response)

  formula = as.formula(paste(response, "~", paste(predictors, collapse = " + ")))

  base_glm = glm(formula, data = splits$train, family = poisson())

  train_multipl_GLM_XGB = function(glm_model,
                                   x,
                                   y,
                                   vdt,
                                   params = list(
                                     objective = "count:poisson",
                                     eval_metric = "poisson-nloglik"),
                                   use_glm=F,
                                   p = NULL){

    glm_preds_in = unname(predict(glm_model,x,type="response"))
    glm_preds_out = unname(predict(glm_model,vdt$x_val,type="response"))

    #training set
    y_train = y
    X_train = data.matrix(x)

    train_targets = y_train/glm_preds_in
    valid_targets = vdt$y_val/glm_preds_out

    dtrain = xgb.DMatrix(X_train, label = train_targets)
    vtrain = xgb.DMatrix(data.matrix(vdt$x_val), label = valid_targets)

    # Initialize with GLM predictions if use_glm is TRUE
    if (use_glm && !is.null(glm_model)) {

      glm_predictions_train = predict(glm_model, x,type="link")
      setinfo(dtrain, "base_margin", unname(glm_predictions_train))

      glm_predictions_val = predict(glm_model, vdt$x_val,type="link")
      setinfo(vtrain, "base_margin", unname(glm_predictions_val))

    }

    # Fit final, tuned model
    xgb_model = xgb.train(
      params = params,
      data = dtrain,
      nrounds = 1000,
      verbose = 1,
      watchlist = list(validation = vtrain),
      early_stopping_rounds = 25

    )

    toreturn = list(glm_model = glm_model,
                    xgb_model = xgb_model)

    class(toreturn) = "ens"

    return(toreturn)

  }

  IBLM = train_multipl_GLM_XGB(
    glm_model = base_glm,
    params = list(
      base_score = 1,
      objective = "count:poisson",
      eval_metric = "poisson-nloglik"),
    x = splits$train %>% dplyr::select(-ClaimNb),
    y = splits$train$ClaimNb,
    vdt = list(x_val = splits$validate %>% dplyr::select(-ClaimNb),
               y_val = splits$validate$ClaimNb))


  # ============================ comparisons =====================

  # was GLM fitted the same coefficients?
  testthat::expect_equal(
    IBLM$glm_model$coefficients,
    IBLM2$glm_model$coefficients
  )

  # was XGBoost fitted with the same log?
  testthat::expect_equal(
    IBLM$xgb_model$evaluation_log,
    IBLM2$xgb_model$evaluation_log
  )

  # was XGBoost fitted with the same trees?
  testthat::expect_equal(
    IBLM$xgb_model |> xgboost::xgb.dump(),
    IBLM2$xgb_model |> xgboost::xgb.dump()
  )

})
