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
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
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

  data_predictors <- data |> dplyr::select(dplyr::all_of(iblm_model$predictor_vars$all))

  actual <- data[[response_var]]

  # get predictions for homogenous, glm and iblm

  model_predictions <-
    data.frame(
      homog = iblm_model$data$train[[response_var]] |> mean(),
      glm = stats::predict(iblm_model$glm_model, data_predictors, type = "response") |> as.vector(),
      iblm = stats::predict(
        iblm_model,
        data_predictors,
        trim
      )
    )

  # get predictions for any additional models passed in and append to model_predictions df
  if (length(additional_models) > 0) {
    if (is.null(names(additional_models))) {
      names(additional_models) <- purrr::map_chr(additional_models, function(x) class(x)[1])
    }

    # Create a safe predict function that tries multiple approaches
    safe_predict <- function(model, data) {
      # Try methods in order of preference
      methods <- list(
        function() stats::predict(model, data, type = "response"),
        function() stats::predict(model, as.matrix(data)),
        function() stats::predict(model, data),
        function() stats::predict(model, xgboost::xgb.DMatrix(data))
      )

      for (method in methods) {
        result <- tryCatch(method(), error = function(e) NULL)
        if (!is.null(result)) {
          return(result)
        }
      }

      stop("Could not generate predictions for model: ", class(model)[1])
    }

    additional_model_predictions <- purrr::map(
      additional_models,
      .f = ~ safe_predict(.x, data_predictors)
    ) |>
      stats::setNames(names(additional_models)) |>
      dplyr::bind_cols()

    model_predictions <- dplyr::bind_cols(model_predictions, additional_model_predictions)
  }

  model_names <- names(model_predictions)

  family <- iblm_model$glm_model$family$family

  pds <- purrr::map_dbl(
    model_names,
    function(x) calculate_deviance(y_true = actual, y_pred = model_predictions[[x]], family = family)
  ) |> stats::setNames(model_names)

  result <- data.frame(
    model = model_names,
    deviance = unname(pds)
  )

  devcol <- paste0(family, "_deviance") |> tolower()

  names(result)[names(result) == "deviance"] <- devcol

  result <- result |>
    dplyr::mutate(
      pinball_score = 1 - result[[devcol]] / pds["homog"]
    )

  return(result)
}


#' Calculate Poisson Deviance
#'
#' Computes the Poisson deviance between true and predicted values, commonly
#' used as a loss function for Poisson regression models.
#'
#' @param y_true Numeric vector of true/observed values
#' @param y_pred Numeric vector of predicted values
#' @param correction Numeric value added to avoid log(0) issues. Default is 1e-7
#'
#' @return Numeric value representing twice the mean Poisson deviance
#'
#' @noRd
poisson_deviance <- function(y_true, y_pred, correction = +10^-7) {
  pd <- mean((y_pred - y_true - y_true * log((y_pred + correction) / (y_true + correction))))
  return(2 * pd)
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
#' @param correction Numeric value added to both y_true and y_pred to avoid
#'   log(0) and division by zero errors. Default is 1e-7.
#'
#' @return Numeric value of the mean deviance.
#'
#' @examples
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 2.2, 2.8, 4.1, 4.9)
#' calculate_deviance(y_true, y_pred, "gaussian")
#' calculate_deviance(y_true, y_pred, "poisson")
#'
#' @noRd
calculate_deviance <- function(y_true, y_pred, family = "gaussian", correction = 1e-10) {

  family <- tolower(family)

  # Apply correction to avoid log(0) and division by zero
  y_true <- y_true + correction
  y_pred <- y_pred + correction

  mean_deviance <- switch(family,
                          "gaussian" = {
                            mean((y_true - y_pred)^2)
                          },
                          "poisson" = {
                            2 * mean(y_pred - y_true - y_true * log(y_pred / y_true))
                          },
                          "gamma" = {
                            2 * mean(-log(y_true / y_pred) + (y_true - y_pred) / y_pred)
                          },
                          "tweedie" = {
                            # Tweedie with p=1.5 (common default)
                            p <- 1.5
                            2 * mean((y_true^(2-p)) / ((1-p)*(2-p)) -
                                       (y_true * y_pred^(1-p)) / (1-p) +
                                       (y_pred^(2-p)) / (2-p))
                          },
                          cli::cli_abort("family must be one of: gaussian, poisson, gamma, tweedie")
  )

  return(mean_deviance)
}

