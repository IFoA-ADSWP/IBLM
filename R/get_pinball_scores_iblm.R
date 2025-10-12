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
#' @return Data frame with columns for "poission_deviance" and "pinball_score" for each model.
#'
#' @details
#' Pinball scores are calculated relative to a homogeneous model (i.e. a simple mean prediction of training data).
#' Higher scores indicate better predictive performance.
#'
#'
#' @export
get_pinball_scores_iblm <- function(data,
                                    iblm_model,
                                    trim = NA_real_,
                                    additional_models = list()
                                    ) {

  check_iblm_model(iblm_model)

  response_var <- all.vars(iblm_model$glm_model$formula)[1]

  data_predictors <- data |> dplyr::select(-dplyr::all_of(response_var))

  actual <- data[[response_var]]

  # get predictions for homogenous, glm and iblm

  model_predictions <-
    data.frame(
      homog = iblm_model$glm_model$data[[response_var]] |> mean(),
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
        function() stats::predict(model, xgboost::xgb.DMatrix(data.matrix(data)))
      )

      for (method in methods) {
        result <- tryCatch(method(), error = function(e) NULL)
        if (!is.null(result)) return(result)
      }

      stop("Could not generate predictions for model: ", class(model)[1])
    }

    additional_model_predictions <- purrr::map(
      additional_models,
      .f = ~safe_predict(.x, data_predictors)
    ) |>
      stats::setNames(names(additional_models)) |>
      dplyr::bind_cols()

    model_predictions <- dplyr::bind_cols(model_predictions, additional_model_predictions)
  }

  model_names <- names(model_predictions)

  pds <- purrr::map_dbl(
    model_names,
    function(x) poisson_deviance(y_true = actual, y_pred = model_predictions[[x]])
    ) |> stats::setNames(model_names)

  data.frame(
    model = model_names,
    poisson_deviance = unname(pds)
  ) |>
    dplyr::mutate(
    pinball_score = 1 - .data$poisson_deviance / pds["homog"]
  )

}
