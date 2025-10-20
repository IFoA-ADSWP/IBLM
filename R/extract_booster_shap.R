


#' Extract SHAP values from an xgboost Booster model
#'
#' A function to extract SHAP (SHapley Additive exPlanations) values from
#' fitted booster model
#'
#' @param booster_model A model object. In the IBLM context it will be the "booster_model" item
#' from an object of class "iblm"
#' @param data A data frame containing the data that SHAP values are desired for.
#' @param ... Additional arguments passed to methods.
#'
#' @return A data frame of SHAP values, where each column corresponds to a feature
#'   and each row corresponds to an observation.
#'
#' @details
#' Currently only a booster_model of class `xgb.Booster` is supported
#'
#'
#' @examples
#' df_list <- freMTPL2freq |>
#'   head(10000) |>
#'   dplyr::mutate(ClaimRate = round(ClaimRate)) |>
#'   split_into_train_validate_test()
#'
#' iblm_model <- train_iblm(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' extract_booster_shap(iblm_model$booster_model, df_list$test)
#'
#' @export
extract_booster_shap <- function(booster_model, data, ...) {
  UseMethod("extract_booster_shap", booster_model)
}

#' @describeIn extract_booster_shap Extract SHAP values from an `xgb.Booster` model
#'
#' @param data A data frame containing the predictor variables ONLY
#'
#' @seealso [xgboost::predict.xgb.Booster()]
#'
#' @export
extract_booster_shap.xgb.Booster <- function(booster_model, data, ...) {
  stats::predict(
    booster_model,
    newdata = xgboost::xgb.DMatrix(
      data.matrix(data)
    ),
    predcontrib = TRUE
  ) |>
    data.frame()
}

