#' Calculate Pinball Scores for IBLM and Additional Models
#'
#' Computes Poisson deviance and pinball scores for an IBLM model alongside
#' homogeneous, GLM, and optional additional models.
#'
#' @param data Data frame.
#' If you have used `split_into_train_validate_test()` this will be the "test" portion of your data.
#' @param iblm_model Fitted IBLM model object of class "iblm"
#' @param trim Numeric trimming parameter for IBLM predictions. Default is `NA_real_`.
#' @param additional_models (Named) list of fitted models for comparison. These models MUST be fitted on the same data as `iblm_model` for sensible results.
#' If unnamed, models are labeled by their class.
#'
#' @return Data frame with 3 columns:
#' \itemize{
#'   \item "model" - will be homog, glm, iblm and any other models specified in `additional_models`
#'   \item "`family`_deviance" - the value from the loss function based on the family of the glm function
#'   \item "pinball_score" - The more positive the score, the better the model than a basic homog model (i.e. all predictions are mean value). A negative score indicates worse than homog model.
#' }
#'
#' @details
#' Pinball scores are calculated relative to a homogeneous model (i.e. a simple mean prediction of training data).
#' Higher scores indicate better predictive performance.
#'
#' @examples
#' df_list <- freMTPLmini |>
#'   dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>
#'   split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimNb",
#'   offset_var = "LogExposure",
#'   family = "poisson"
#' )
#'
#' get_pinball_scores(data = df_list$test, iblm_model = iblm_model)
#'
#' @export
get_pinball_scores <- function(data,
                               iblm_model,
                               trim = NA_real_,
                               additional_models = list()) {
  check_iblm_model(iblm_model)

  response_var <- iblm_model$response_var
  weight_var   <- iblm_model$weight_var
  offset_var   <- iblm_model$offset_var

  linkinv      <- iblm_model$glm_model$family$linkinv
  vars_for_model <- iblm_model$predictor_vars$all

  actual <- data[[response_var]]

  # ------- Derive test-set offsets and predictors -------

  if (is.null(offset_var)) {
    data_predictors <- data |> dplyr::select(dplyr::all_of(vars_for_model))
    data_offsets    <- 0
  } else if (offset_var %in% names(data)) {
    data_predictors <- data |> dplyr::select(dplyr::all_of(c(vars_for_model, offset_var)))
    data_offsets    <- data[[offset_var]]
  } else {
    data_predictors <- data |> dplyr::select(dplyr::all_of(vars_for_model))
    data_predictors[[offset_var]] <- 0
    cli::cli_inform("Column {.field {offset_var}} not found in {.arg data}. Offset of 0 assumed.")
    data_offsets <- 0
  }

  # ------- Derive test-set weights for deviance calculation -------

  if (is.null(weight_var)) {
    weight <- NULL
  } else if (weight_var %in% names(data)) {
    weight <- data[[weight_var]]
  } else {
    weight <- NULL
    cli::cli_inform("Column {.field {weight_var}} not found in {.arg data}. Weight of 1 assumed.")
  }

  # ------- Derive homogeneous baseline via intercept-only GLM on training data -------
  # Fitting a proper intercept-only GLM solves the MLE score equations correctly
  # for any family, link function, offset and weight combination. This avoids the
  # errors that arise from manually computing a weighted mean on the link scale.

  train_data <- iblm_model$data$train

  homog_glm_args <- list(
    formula = stats::as.formula(paste(response_var, "~ 1")),
    family  = iblm_model$glm_model$family,
    data    = train_data
  )

  if (!is.null(weight_var)) {
    homog_glm_args$weights <- train_data[[weight_var]]
  }

  if (!is.null(offset_var)) {
    homog_glm_args$offset <- train_data[[offset_var]]
  }

  homog_glm <- withCallingHandlers(
    do.call(stats::glm, homog_glm_args),
    warning = function(w) {
      if (grepl("non-integer", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  beta0 <- stats::coef(homog_glm)[[1]]

  # Apply the test-set offsets to the single intercept
  homog <- linkinv(beta0 + data_offsets)

  # ------- Get predictions for homogeneous, GLM and IBLM -------

  model_predictions <- data.frame(
    homog = homog,
    glm   = stats::predict(iblm_model$glm_model, data_predictors, type = "response") |> as.vector(),
    iblm  = stats::predict(iblm_model, data_predictors, trim)
  )

  # ------- Append predictions for any additional models -------

  if (length(additional_models) > 0) {
    if (is.null(names(additional_models))) {
      names(additional_models) <- purrr::map_chr(additional_models, function(x) class(x)[1])
    }

    predict_dispatch <- function(model, data) {
      if (inherits(model, "xgb.Booster")) {
        stats::predict(model, xgboost::xgb.DMatrix(data))
      } else {
        stats::predict(model, data, type = "response")
      }
    }

    additional_model_predictions <- purrr::map(
      additional_models,
      .f = function(x) predict_dispatch(x, data_predictors)
    ) |>
      stats::setNames(names(additional_models)) |>
      dplyr::bind_cols()

    model_predictions <- dplyr::bind_cols(model_predictions, additional_model_predictions)
  }

  model_names <- names(model_predictions)

  family <- iblm_model$glm_model$family$family
  if (family == "quasipoisson") {family <- "poisson"}

  pds <- purrr::map_dbl(
    model_names,
    function(x) {
      calculate_deviance(
        y_true = actual,
        y_pred = model_predictions[[x]],
        family = family,
        weight = weight
      )
    }
  ) |> stats::setNames(model_names)

  devcol <- paste0(tolower(family), "_deviance")

  result <- data.frame(model = model_names, deviance = unname(pds))
  names(result)[names(result) == "deviance"] <- devcol

  result <- result |>
    dplyr::mutate(
      pinball_score = 1 - .data[[devcol]] / pds[["homog"]]
    )

  return(result)
}

#' Calculate Mean Deviance
#'
#' Calculates the mean deviance between observed and predicted values for
#' various GLM families.
#'
#' @param y_true Numeric vector of observed values.
#' @param y_pred Numeric vector of predicted values.
#' @param family Character string specifying the distribution family. One of
#'   "gaussian", "poisson", "gamma", or "tweedie" (with p=1.5).
#' @param weight Numeric vector of weight for each observation. If NULL,
#'   all observations are given equal weight. Default is NULL.
#' @param correction Numeric value added to both y_true and y_pred to avoid
#'   log(0) and division by zero errors. Default is 1e-10.
#'
#' @return Numeric value of the mean deviance.
#'
#' @examples
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 2.2, 2.8, 4.1, 4.9)
#' calculate_deviance(y_true, y_pred, "gaussian")
#' calculate_deviance(y_true, y_pred, "poisson")
#' # With weight
#' weight <- c(1, 1, 2, 1, 1)
#' calculate_deviance(y_true, y_pred, "gaussian", weight = weight)
#'
#' @noRd
calculate_deviance <- function(y_true,
                               y_pred,
                               family = "gaussian",
                               weight = NULL,
                               correction = 1e-10) {
  family <- tolower(family)

  # Handle weight
  if (is.null(weight)) {
    weight <- rep(1, length(y_true))
  }

  # Apply correction to avoid log(0) and division by zero
  y_true <- y_true + correction
  y_pred <- y_pred + correction

  mean_deviance <- switch(family,
    "gaussian" = {
      sum(weight * (y_true - y_pred)^2) / sum(weight)
    },
    "poisson" = {
      2 * sum(weight * (y_pred - y_true - y_true * log(y_pred / y_true))) / sum(weight)
    },
    "gamma" = {
      2 * sum(weight * (-log(y_true / y_pred) + (y_true - y_pred) / y_pred)) / sum(weight)
    },
    "tweedie" = {
      # Tweedie with p=1.5 (common default)
      p <- 1.5
      2 * sum(weight * ((y_true^(2 - p)) / ((1 - p) * (2 - p)) -
        (y_true * y_pred^(1 - p)) / (1 - p) +
        (y_pred^(2 - p)) / (2 - p))) / sum(weight)
    },
    cli::cli_abort("family must be one of: gaussian, poisson, gamma, tweedie")
  )
  return(mean_deviance)
}
