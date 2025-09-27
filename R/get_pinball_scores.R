
#' Calculate Pinball Scores for Model Comparison
#'
#' Computes Poisson deviance and relative pinball scores for multiple models
#' compared to a homogeneous baseline model.
#'
#' @param res Data frame containing model predictions and target variable
#' @param models Character vector of model names (column names in res).
#'   Default is c("homog", "GLM", "XGB", "IBLM")
#' @param target Character string specifying the target variable column name.
#'   Default is "ClaimNb"
#' @param return_both Logical indicating whether to return both Poisson deviance
#'   and pinball scores. Default is FALSE (returns only pinball scores)
#'
#' @return Data frame with model performance metrics. If return_both is TRUE,
#'   returns both Poisson deviance and pinball scores with a 'stat' column.
#'   If FALSE, returns only pinball scores as percentages
#'
#' @details
#' Pinball scores are calculated as 1 - (model_deviance / homog_deviance),
#' representing the relative improvement over the homogeneous model.
#'
#' @examples
#' # Assuming 'results' is a data frame with model predictions
#' # get_pinball_scores(results, models = c("homog", "GLM"), target = "ClaimNb")
#'
#' @export
get_pinball_scores <- function(res,
                               models = c("homog", "GLM", "XGB", "IBLM"),
                               target = "ClaimNb",
                               return_both = FALSE) {
  x <- lapply(models, FUN = function(x) poisson_deviance(y_true = res[[target]], y_pred = res[[x]])) |>
    stats::setNames(models) |>
    as.data.frame()
  if (return_both) {
    dplyr::bind_rows(
      x |> dplyr::mutate_all(.funs = round, 4),
      data.frame(1 - x / x$homog) # |> dplyr::mutate_all(.funs = scales::percent, 0.01)
    ) |>
      dplyr::mutate(stat = c("Posson Deviance", "Pinball Score"),
                    .before = dplyr::everything())
  } else {
    return(data.frame(1 - x / x$homog) |>
             dplyr::mutate_all(.funs = scales::percent, 0.01))
  }
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
#' @examples
#' \dontrun{
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 1.9, 3.2, 3.8, 5.1)
#' poisson_deviance(y_true, y_pred)
#' }
poisson_deviance <- function(y_true, y_pred, correction = +10^-7) {
  pd <- mean((y_pred - y_true - y_true * log((y_pred + correction) / (y_true + correction))))
  return(2 * pd)
}
