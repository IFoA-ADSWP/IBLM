#' Predict method for ensemble models
#'
#' @description
#' This function generates predictions from an ensemble model consisting of a GLM
#' and an XGBoost model.
#'
#' @param model An object of class 'iblm'. This should be output by `train_iblm()`
#' @param data A data frame or matrix containing the predictor variables for
#'   which predictions are desired. Must have the same structure as the
#'   training data used to fit the ensemble model.
#' @param trim Numeric value for post-hoc truncating of XGBoost predictions. If \code{NA} (default) then no trimming is applied.
#' @param type string, defines the type argument used in GLM/Booster Currently only "response" is supported
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
#' data <- freMTPL2freq |> head(10000) |> split_into_train_validate_test()
#'
#' iblm_model <- train_iblm(
#'   data,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' predictions <- predict(iblm_model, data$test)
#'
#' predictions
#'
#' @seealso \link[stats]{predict.glm}, \link[xgboost]{predict.xgb.Booster}
#'
#' @export
#'
predict.iblm <- function(model, data, trim = NA_real_, type = "response") {

  check_iblm_model(model)

  if (type != "response") {

    cli::cli_abort(c(
      "x" = "Only supported type currently is {.val response}",
      "i" = "You supplied {.val {type}}"
    ))

  }

  response_var <- all.vars(model$glm_model$formula)[1]
  data <- data |> dplyr::select(-dplyr::any_of(response_var))
  relationship <- model["relationship"]
  glm <- unname(stats::predict(model$glm_model, data, type = type))
  booster <- stats::predict(model$booster_model, xgboost::xgb.DMatrix(data.matrix(data)), type = type)

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
