#' Predict Method for IBLM
#'
#' @description
#' This function generates predictions from an ensemble model consisting of a GLM
#' and an XGBoost model.
#'
#' @param object An object of class 'iblm'. This should be output by `train_iblm_xgb()`
#' @param newdata A data frame or matrix containing the predictor variables for
#'   which predictions are desired. Must have the same structure as the
#'   training data used to fit the 'iblm' model.
#' @param trim Numeric value for post-hoc truncating of XGBoost predictions. If \code{NA} (default) then no trimming is applied.
#' @param type string, defines the type argument used in GLM/Booster Currently only "response" is supported
#' @param ... additional arguments affecting the predictions produced.
#'
#'
#' @return A numeric vector of ensemble predictions computed as the element-wise
#'   product of GLM response probabilities and (optionally trimmed) XGBoost
#'   predictions.
#'
#' @details
#' The prediction process involves the following steps:
#' \enumerate{
#'   \item Generate GLM predictions
#'   \item Generate Booster predictions
#'   \item If trimming is specified, apply to booster predictions
#'   \item Combine GLM and Booster predictions as per "relationship" described within iblm model object
#' }
#'
#' At this point, only an iblm model with a "booster_model" object of class `xgb.Booster` is supported
#'
#' @examples
#' data <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   data,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' predictions <- predict(iblm_model, data$test)
#'
#' predictions |> dplyr::glimpse()
#'
#' @seealso \link[stats]{predict.glm}, \link[xgboost]{predict.xgb.Booster}
#'
#' @export
#'
predict.iblm <- function(object, newdata, trim = NA_real_, type = "response", ...) {

  check_iblm_model(object)

  if (type != "response") {
    cli::cli_abort(c(
      "x" = "Only supported type currently is {.val response}",
      "i" = "You supplied {.val {type}}"
    ))
  }

  response_var <- all.vars(object$glm_model$formula)[1]
  data <- newdata |> dplyr::select(-dplyr::any_of(response_var))
  relationship <- object["relationship"]
  glm <- unname(stats::predict(object$glm_model, data, type = type))
  booster <- stats::predict(object$booster_model, xgboost::xgb.DMatrix(data), type = type)

  if (!is.na(trim)) {
    truncate <- function(x) {
      return(
        pmax(
          pmin(booster, 1 + trim),
          max(1 - trim, 0)
        )
      )
    }
    booster <- truncate(booster)
    booster <- booster * 1 / mean(booster)
  }

  if (relationship == "multiplicative") {
    toreturn <- glm * booster
  } else if (relationship == "additive") {
    toreturn <- glm + booster
  } else {
    cli::cli_abort(c(
      "x" = "Invalid relationship attribute: {.val {relationship}}",
      "i" = "Relationship must be either {.val multiplicative} or {.val additive}"
    ))
  }

  return(toreturn)
}
