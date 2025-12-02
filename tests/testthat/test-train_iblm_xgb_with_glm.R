testthat::test_that("test train_iblm_xgb_with_glm same as train_iblm_xgb - poisson", {

  family <- "poisson"

  df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

  formula <- stats::as.formula(paste("ClaimRate", "~ ."))

  glm_model <- stats::glm(formula, data = df_list[["train"]], family = family)

  iblm_model <- train_iblm_xgb_with_glm(
    df_list,
    glm_model
  )

  iblm_model2 <- train_iblm_xgb(
    df_list,
    response_var = "ClaimRate",
    family = family
  )

  testthat::expect_equal(
    predict(iblm_model, df_list[["test"]]),
    predict(iblm_model2, df_list[["test"]])
  )

})


testthat::test_that("test train_iblm_xgb_with_glm same as train_iblm_xgb - gaussian", {

  family <- "gaussian"

  df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)

  formula <- stats::as.formula(paste("ClaimRate", "~ ."))

  glm_model <- stats::glm(formula, data = df_list[["train"]], family = family)

  iblm_model <- train_iblm_xgb_with_glm(
    df_list,
    glm_model
  )

  iblm_model2 <- train_iblm_xgb(
    df_list,
    response_var = "ClaimRate",
    family = family
  )

  testthat::expect_equal(
    predict(iblm_model, df_list[["test"]]),
    predict(iblm_model2, df_list[["test"]])
  )

})

testthat::test_that("test train_iblm_xgb_with_glm same as train_iblm_xgb - gamma", {

  family <- "gamma"

  df_list <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimRate + 1) |>
    split_into_train_validate_test(seed = 9000)

  formula <- stats::as.formula(paste("ClaimRate", "~ ."))

  glm_model <- stats::glm(formula, data = df_list[["train"]], family = stats::Gamma(link = "log"))

  iblm_model <- train_iblm_xgb_with_glm(
    df_list,
    glm_model
  )

  iblm_model2 <- train_iblm_xgb(
    df_list,
    response_var = "ClaimRate",
    family = family
  )

  testthat::expect_equal(
    predict(iblm_model, df_list[["test"]]),
    predict(iblm_model2, df_list[["test"]])
  )

})


testthat::test_that("test train_iblm_xgb_with_glm same as train_iblm_xgb - tweeds", {

  family <- "tweedie"

  df_list <- freMTPLmini |>
    dplyr::mutate(ClaimRate = ClaimRate + 1) |>
    split_into_train_validate_test(seed = 9000)

  formula <- stats::as.formula(paste("ClaimRate", "~ ."))

  glm_model <- stats::glm(formula, data = df_list[["train"]], family = statmod::tweedie(var.power = 1.5, link.power = 0))

  iblm_model <- train_iblm_xgb_with_glm(
    df_list,
    glm_model
  )

  iblm_model2 <- train_iblm_xgb(
    df_list,
    response_var = "ClaimRate",
    family = family
  )

  testthat::expect_equal(
    predict(iblm_model, df_list[["test"]]),
    predict(iblm_model2, df_list[["test"]])
  )

})
