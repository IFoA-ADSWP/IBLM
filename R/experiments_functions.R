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
#' y_true <- c(1, 2, 3, 4, 5)
#' y_pred <- c(1.1, 1.9, 3.2, 3.8, 5.1)
#' poisson_deviance(y_true, y_pred)
poisson_deviance <- function(y_true, y_pred, correction = +10^-7) {
  pd <- mean((y_pred - y_true - y_true * log((y_pred + correction) / (y_true + correction))))
  return(2 * pd)
}

#' Detect Outliers in Numeric Vector
#'
#' Identifies outliers in a numeric vector using either quantile-based method
#' or isolation forest algorithm.
#'
#' @param x Numeric vector to analyze for outliers
#' @param method Character string specifying the method to use. Options are
#'   "quantile" (default) or "isoforest"
#' @param q Numeric value between 0 and 0.5 specifying the quantile threshold
#'   for outlier detection. Default is 0.01
#'
#' @return Logical vector of same length as x, where TRUE indicates values
#'   to keep (non-outliers) and FALSE indicates outliers
#'
#' @details
#' For "quantile" method: values below the q-th quantile or above the
#' (1-q)-th quantile are considered outliers.
#'
#' For "isoforest" method: uses isolation forest algorithm from the isotree
#' package to detect anomalies.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100)  # 100 is an outlier
#' detect_outliers(x, method = "quantile", q = 0.1)
#'
#' @export
detect_outliers <- function(x, method = c("quantile", "isoforest"), q = 0.01) {
  method <- match.arg(method)
  if (!is.numeric(x)) stop("Input vector 'x' must be numeric.")
  if (!is.numeric(q) || q <= 0 || q >= 0.5) stop("Parameter 'q' must be between 0 and 0.5.")
  keep <- rep(TRUE, length(x))  # default: keep all
  if (method == "quantile") {
    lower <- stats::quantile(x, probs = q, na.rm = TRUE)
    upper <- stats::quantile(x, probs = 1 - q, na.rm = TRUE)
    keep <- x >= lower & x <= upper
  } else if (method == "isoforest") {
    if (!requireNamespace("isotree", quietly = TRUE)) {
      stop("Package 'isotree' is required for 'isoforest' method. Install it with install.packages('isotree').")
    }
    model <- isotree::isolation.forest(matrix(x, ncol = 1))
    scores <- stats::predict(model, matrix(x, ncol = 1), type = "score")
    threshold <- stats::quantile(scores, probs = 1 - q, na.rm = TRUE)
    keep <- scores < threshold
  }
  return(keep)
}

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

#' Create Actual vs Expected Chart
#'
#' Generates a ggplot2 chart comparing actual values against multiple model
#' predictions across different levels of a grouping variable.
#'
#' @param df Data frame containing the data to plot
#' @param x Character string specifying the column name for the x-axis variable
#'   (grouping variable)
#' @param models Character vector of model names (column names in df) to include
#'   in the comparison
#' @param actual Character string specifying the column name containing actual
#'   values
#'
#' @return A ggplot2 object showing actual vs expected values by group
#'
#' @details
#' The function groups data by the x variable, calculates means for actual and
#' predicted values, and creates a scatter plot with different colors and shapes
#' for each model. The actual values are shown in black, while model predictions
#' use different colors from the default ggplot2 palette.
#'
#' @examples
#' # Assuming 'data' has columns: age_group, actual_claims, model1_pred, model2_pred
#' # ave_chart(data, x = "age_group", models = c("model1_pred", "model2_pred"),
#' #           actual = "actual_claims")
#'
#' @export
ave_chart <- function(df, x, models, actual) {
  # Build vector of all selected columns
  selected_cols <- c(x, actual, models)
  # Rename columns for internal use
  df_plot <- df |>
    dplyr::select(dplyr::all_of(selected_cols)) |>
    stats::setNames(c("xvar", "actual", models))
  # Group by xvar and calculate means
  df_long <- df_plot |>
    dplyr::group_by(xvar) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_longer(cols = -xvar, names_to = "name", values_to = "value")
  # Set up color palette
  color_vals <- stats::setNames(
    c("black", scales::hue_pal()(length(models))),
    c("actual", models)
  )
  # Plot
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = xvar, y = value, group = name, color = name, shape = name)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_vals) +
    ggplot2::ggtitle(paste(x, "- Actual vs Expected")) +
    ggplot2::theme_minimal()
  return(p)
}
