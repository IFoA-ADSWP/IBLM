#' Predict method for ensemble models
#'
#' @description
#' This function generates predictions from an ensemble model consisting of a GLM
#' and an XGBoost model.
#'
#' @param model An object of class "ens" containing:
#'   \itemize{
#'     \item{glm_model}{ A fitted GLM model object}
#'     \item{xgb_model}{ A fitted XGBoost model object}
#'   }
#'   The object must also contain the attribute "relationship" which describes how the ensemble predictions are combined
#' @param data A data frame or matrix containing the predictor variables for
#'   which predictions are desired. Must have the same structure as the
#'   training data used to fit the ensemble model.
#' @param trim Numeric value for post-hoc truncating of XGBoost predictions.
#'   If \code{NA} (default), no trimming is applied.
#'
#' @return A numeric vector of ensemble predictions computed as the element-wise
#'   product of GLM response probabilities and (optionally trimmed) XGBoost
#'   predictions.
#'
#' @details
#' The prediction process involves the following steps:
#' \enumerate{
#'   \item Generate GLM predictions using \code{type="response"}
#'   \item Generate XGBoost predictions on a DMatrix conversion of the input data
#'   \item If trimming is specified, apply to XGBoost predictions
#'   \item Multiply **or** Add the GLM and XGBoost predictions to get the ensemble predictions
#' }
#'
#' @examples
#' \dontrun{
#' data <- freMTPL2freq |> split_into_train_validate_test()
#'
#' IBLM <- train_glm_xgb(
#'   data,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' predictions <- predict(IBLM, data$test)
#'
#' predictions
#' }
#'
#' @seealso \code{\link[stats]{predict.glm()}}, \code{\link[xgboost]{predict.xgb.Booster()}}
#'
#' @export
#'
predict.ens <- function(model, data, trim = NA, type = "response") {
  if (type != "response") {
    stop("only supported type currently is 'response'")
  }

  relationship <- attr(model, "relationship")

  glm <- unname(stats::predict(model$glm_model, data, type = type))
  xgb <- stats::predict(model$xgb_model, xgboost::xgb.DMatrix(data.matrix(data)), type = type)

  if (!is.na(trim)) {
    # Post hoc trimming based on learned min/max

    # squish_affine does not appear to be used...
    squish_affine <- function(x) {
      min_x <- model$min_res
      max_x <- model$max_res

      transformed <- ifelse(
        x < 1,
        1 + (x - 1) * trim * (x - min_x) / (1 - min_x),
        1 + (x - 1) * trim * (x - 1) / (max_x - 1)
      )

      return(transformed)
    }

    truncate <- function(x) {
      return(
        pmax(
          pmin(xgb, 1 + trim),
          max(1 - trim, 0)
        )
      )
    }

    xgb <- truncate(xgb)
    xgb <- xgb * 1 / mean(xgb)
  }

  if (relationship == "multiplicative") {

    toreturn <- glm * xgb

  } else if (relationship == "additive") {

    toreturn <- glm + xgb

  } else {

    stop(paste0("relationship attribute was not 'multiplicative' or 'additive' - unable to combine glm and xgb"))

  }

  if (any(is.na(toreturn) | is.nan(toreturn) | toreturn < 0)) {
    browser()
  }

  return(toreturn)
}
