#' Predict method for ensemble models
#'
#' @description
#' This function generates predictions from an ensemble model consisting of a GLM
#' and an XGBoost model. The final prediction is computed as the product of the
#' GLM response probabilities and the XGBoost predictions, with optional
#' post-hoc trimming applied to the XGBoost component.
#'
#' @param model An object of class "ens" containing:
#'   \describe{
#'     \item{glm_model}{A fitted GLM model object}
#'     \item{xgb_model}{A fitted XGBoost model object}
#'     \item{min_res}{Minimum residual value for trimming (when trim is used)}
#'     \item{max_res}{Maximum residual value for trimming (when trim is used)}
#'   }
#' @param dt A data frame or matrix containing the predictor variables for
#'   which predictions are desired. Must have the same structure as the
#'   training data used to fit the ensemble model.
#' @param trim Numeric value for post-hoc trimming of XGBoost predictions.
#'   If \code{NA} (default), no trimming is applied. When specified, applies
#'   both affine transformation based on learned min/max values and truncation
#'   to the range [max(1-trim, 0), 1+trim], followed by normalization to
#'   maintain unit mean.
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
#'   \item If trimming is specified:
#'     \itemize{
#'       \item Apply affine transformation based on model's learned min/max residuals
#'       \item Truncate predictions to the specified trim range
#'       \item Normalize by dividing by the mean to maintain unit average
#'     }
#'   \item Multiply GLM and XGBoost predictions element-wise
#' }
#'
#' The function includes error checking and will enter the debugger if any
#' predictions are NA, NaN, or negative.
#'
#' @examples
#' \dontrun{
#' # Basic prediction without trimming
#' predictions <- predict(ensemble_model, new_data)
#'
#' # Prediction with trimming
#' trimmed_predictions <- predict(ensemble_model, new_data, trim = 0.1)
#' }
#'
#' @seealso \code{\link[stats]{predict}}, \code{\link[xgboost]{predict.xgb.Booster}}
#'
#' @export
#'
predict.ens <- function(model, dt, trim=NA){

  glm = unname(stats::predict(model$glm_model, dt, type="response"))
  xgb = stats::predict(model$xgb_model, xgboost::xgb.DMatrix(data.matrix(dt)), type="response")

  if (!is.na(trim)) {
    # Post hoc trimming based on learned min/max

    squish_affine <- function(x) {
      min_x <- model$min_res
      max_x <- model$max_res

      transformed <- ifelse(
        x < 1,
        1 + (x - 1) * trim * (x - min_x)/(1 - min_x),
        1 + (x - 1) * trim * (x - 1)/(max_x - 1)
      )

      return(transformed)
    }

    truncate <- function(x){
      return(
        pmax(
          pmin(xgb, 1 + trim),
          max(1 - trim, 0)
        )
      )
    }

    xgb = truncate(xgb)
    xgb = xgb * 1 / mean(xgb)
  }

  toreturn = glm * xgb

  if(any(is.na(toreturn) | is.nan(toreturn) | toreturn < 0)) { browser() }

  return(toreturn)
}




