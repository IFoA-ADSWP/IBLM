#' Train XGBoost Model Using the IBLM Model Parameters
#'
#' Trains an XGBoost model using parameters extracted from the booster residual component of the iblm model.
#' This is a convenient way to fit an XGBoost model for direct comparison with a fitted iblm model
#'
#' @param iblm_model Ensemble model object of class "iblm" containing GLM and
#'   XGBoost model components. Also contains data that was used to train it.
#' @param ... optional arguments to insert into xgb.train().
#' Note this will cause deviation from the settings used for training `iblm_model`
#'
#' @return Trained XGBoost model object (class "xgb.Booster").
#'
#' @examples
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' # training with plenty of rounds allowed
#' iblm_model1 <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson",
#'   params = list(max_depth = 6),
#'   nrounds = 1000
#' )
#'
#' xgb1 <- train_xgb_as_per_iblm(iblm_model1)
#'
#' # training with severe restrictions (expected poorer results)
#' iblm_model2 <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson",
#'   params = list(max_depth = 1),
#'   nrounds = 5
#' )
#'
#' xgb2 <- train_xgb_as_per_iblm(iblm_model2)
#'
#' # comparison shows the poor training mirrored in second set:
#' get_pinball_scores(
#'   df_list$test,
#'   iblm_model1,
#'   trim = NA_real_,
#'   additional_models = list(iblm2 = iblm_model2, xgb1 = xgb1, xgb2 = xgb2)
#' )
#'
#' @seealso
#' \link[xgboost]{xgb.train}
#'
#' @export
train_xgb_as_per_iblm <- function(iblm_model, ...) {

  # ==================== checks ====================

  check_iblm_model(iblm_model)

  # Check if residual model is xgb.Booster
  if (!("xgb.Booster" %in% class(iblm_model$booster_model))) {
    cli::cli_abort(c(
      "Residual model must be of class {.cls xgb.Booster}.",
      "x" = "You supplied a residual model of class {.cls {class(iblm_model$booster_model)}}.",
      "i" = "The ensemble model must use XGBoost for this function to work."
    ))
  }

  # ==================== input generation ====================


  response_var <- iblm_model$response_var

  train <- list()
  validate <- list()

  train$targets <- iblm_model$data$train |> dplyr::pull(response_var)
  validate$targets <- iblm_model$data$validate |> dplyr::pull(response_var)

  train$features <- iblm_model$data$train |> dplyr::select(-dplyr::all_of(response_var))
  validate$features <- iblm_model$data$validate |> dplyr::select(-dplyr::all_of(response_var))


  # ==================== Preparing for XGB  ====================

  train$xgb_matrix <- xgboost::xgb.DMatrix(train$features, label = train$targets)
  validate$xgb_matrix <- xgboost::xgb.DMatrix(validate$features, label = validate$targets)


  # ==================== Fitting XGB  ====================

  xgb_params <- iblm_model$xgb_params

  if (is.null(xgb_params[["evals"]])) {
    xgb_params[["evals"]] <- list()
  }

  xgb_params <- utils::modifyList(xgb_params, list(...))

  xgb_params <- utils::modifyList(xgb_params, list(data = train$xgb_matrix))

  xgb_params[["evals"]] <- utils::modifyList(xgb_params[["evals"]], list(validation = validate$xgb_matrix))

  booster_model <- do.call(xgboost::xgb.train, xgb_params)

  return(booster_model)
}
