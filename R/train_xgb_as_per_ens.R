#' Train XGBoost Model Using the Ensemble Booster Model Parameters
#'
#' Trains an XGBoost model using parameters extracted from the booster residual component of the ensemble model.
#' This is a convenient way to fit an XGBoost model for comparison with the ensemble.
#'
#' @param data List with at least \code{train} and \code{validate} data frames. Training
#'   data MUST match the `data` input that was used to create the ensemble model.
#' If you have used `split_into_train_validate_test()` this will be the "train" portion of your data.
#' @param iblm_model Ensemble model object of class "ens" containing GLM and
#'   XGBoost model components.
#' @param xgb_additional_params Named list of XGBoost parameters. Defaults:
#'   \code{nrounds = 1000}, \code{verbose = 0}, \code{early_stopping_rounds = 25}.
#'
#' @return Trained XGBoost model object (class "xgb.Booster").
#'
#' @export
#'
#' @seealso
#' \link[xgboost]{xgb.train}
#'
#' @export
train_xgb_as_per_ens <- function(
    data,
    iblm_model,
    xgb_additional_params = list(
      nrounds = 1000,
      verbose = 0,
      early_stopping_rounds = 25
    )) {

  # ==================== checks ====================

  check_required_names(data, c("train", "validate"))

  check_iblm_model(iblm_model)

  # Check if residual model is xgb.Booster
  if (!("xgb.Booster" %in% class(iblm_model$xgb_model))) {
    cli::cli_abort(c(
      "Residual model must be of class {.cls xgb.Booster}.",
      "x" = "You supplied a residual model of class {.cls {class(iblm_model$xgb_model)}}.",
      "i" = "The ensemble model must use XGBoost for this function to work."
    ))
  }

  # Check if training data matches
  if (!dplyr::setequal(data[["train"]], iblm_model$glm_model$data)) {
    cli::cli_abort(c(
      "{.arg data$train} must match the training data used for {.arg iblm_model}.",
      "x" = "The supplied training data is not equivalent to the model's training data.",
      "i" = "Ensure you're using the same data that was used to train the model."
    ))
  }

  # ==================== input generation ====================


  response_var <- all.vars(iblm_model$glm_model$formula)[1]

  xgb_family_params <- iblm_model$xgb_model$params

  train <- list()
  validate <- list()

  train$targets <- data[["train"]] |> dplyr::pull(response_var)
  validate$targets <- data[["validate"]] |> dplyr::pull(response_var)

  train$features <- data[["train"]] |> dplyr::select(-dplyr::all_of(response_var))
  validate$features <- data[["validate"]] |> dplyr::select(-dplyr::all_of(response_var))


  # ==================== Preparing for XGB  ====================

  train$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(train$features), label = train$targets)
  validate$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(validate$features), label = validate$targets)


  # ==================== Fitting XGB  ====================

  xgb_core_params <- list(
    params = xgb_family_params,
    data = train$xgb_matrix,
    watchlist = list(validation = validate$xgb_matrix)
  )

  xgb_all_params <- utils::modifyList(xgb_core_params, xgb_additional_params)

  xgb_model <- do.call(xgboost::xgb.train, xgb_all_params)

  return(xgb_model)
}
