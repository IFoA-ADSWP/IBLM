#' Create Correction Corridor Visualization
#'
#' Generates a faceted scatter plot comparing GLM predictions against ensemble
#' predictions across different trim values. This visualization helps assess
#' how ensemble corrections vary with different trimming parameters and
#' identifies the "correction corridor" where the ensemble modifies GLM predictions.
#'
#' @param ensemble An ensemble model object (typically of class "ens") containing
#'   both GLM and XGBoost components, with a predict method
#' @param explainer An explainer object containing an `input_frame` element with
#'   the data to be used for predictions
#' @param trim_vals Numeric vector of trim values to test. Default is
#'   `c(NA_real_, 4, 1, 0.2, 0.15, 0.1, 0.05, 0)`. `NA_real_` represents no trimming
#' @param sample_perc Numeric value between 0 and 1 specifying the fraction
#'   of data to sample for visualization. Default is 0.2 (20%)
#' @param var Character string specifying an optional variable name from the
#'   data to use for coloring points. Default is `NULL` (no coloring variable)
#'
#' @return A ggplot2 object.
#'
#' The plot contains:
#' \itemize{
#'   \item X-axis: GLM predictions
#'   \item Y-axis: Ensemble predictions
#'   \item Facets: Different trim values
#'   \item Reference line: y = x (perfect agreement line)
#'   \item Optional: Points colored by specified variable
#' }
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Sampling a subset of data from the explainer's input_frame
#'   \item Computing GLM predictions once for efficiency
#'   \item Generating ensemble predictions for each trim value
#'   \item Creating a faceted visualization to compare prediction patterns
#' }
#'
#' The "correction corridor" refers to the deviation patterns between GLM and
#' ensemble predictions. Points near the diagonal line (y = x) indicate minimal
#' correction, while points further from the line show where the ensemble
#' significantly adjusts GLM predictions.
#'
#' Different trim values control the magnitude of ensemble corrections, and
#' this visualization helps identify optimal trimming parameters by showing
#' how corrections behave across the prediction range.
#'
#' @examples
#' # Basic usage with default trim values
#' \donttest{
#' correction_corridor(my_ensemble, my_explainer)
#' }
#'
#' # With custom trim values and coloring by a variable
#' \dontrun{
#' correction_corridor(
#'   ensemble = my_ensemble,
#'   explainer = my_explainer,
#'   trim_vals = c(NA_real_, 1, 0.5, 0.1),
#'   sample_perc = 0.3,
#'   var = "age_group"
#' )
#'
#' # High-resolution plot with more data
#' correction_corridor(
#'   ensemble = my_ensemble,
#'   explainer = my_explainer,
#'   sample_perc = 0.5
#' )
#' }
#'
#' @export
correction_corridor <- function(ensemble,
                                explainer,
                                trim_vals = c(NA_real_, 4, 1, 0.2, 0.15, 0.1, 0.05, 0),
                                sample_perc = 0.2,
                                var = NA) {

  # Sample data
  df <- explainer$input_frame |>
    dplyr::sample_frac(sample_perc)

  # Store optional variable if given
  var_vals <- if (!is.na(var)) df[[var]] else NULL

  # Compute GLM predictions once
  glm_pred <- stats::predict(ensemble$glm_model, df, type = "response") |>
    as.vector()

  # Generate predictions for each trim value
  df_list <- lapply(trim_vals, function(trim_val) {
    ens_pred <- predict(
      model = ensemble,
      dt = df |>
        dplyr::select(-ClaimNb),
      trim = trim_val
    )

    out <- data.frame(
      glm = glm_pred,
      ens = ens_pred,
      trim = ifelse(is.na(trim_val), "NA", as.character(trim_val))
    )

    # Add var if provided
    if (!is.null(var_vals)) {
      out[[var]] <- var_vals
    }

    return(out)
  })

  # Combine all predictions
  df_all <- dplyr::bind_rows(df_list)

  # Start ggplot
  p <- ggplot2::ggplot(df_all, ggplot2::aes(x = glm, y = ens)) +
    {
      if (!is.na(var)) {
        ggplot2::geom_point(ggplot2::aes(color = .data[[var]]), alpha = 0.4)
      } else {
        ggplot2::geom_point(alpha = 0.4)
      }
    } +
    ggplot2::facet_wrap(~ trim, ncol = min(4, length(trim_vals))) +
    ggplot2::labs(
      x = "GLM Prediction",
      y = "Ensemble Prediction",
      title = "Correction Corridor by Trim Value",
      color = if (!is.na(var)) var else NULL
    ) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::theme_minimal()

  return(p)
}
