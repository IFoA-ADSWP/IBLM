#' Plot Correction Corridor for Ensemble Predictions
#'
#' Generates a diagnostic plot comparing predictions from a GLM model and an ensemble
#' across a range of trimming values. This helps visualize how the ensemble predictions
#' deviate from the GLM baseline under different trim levels.
#'
#' @param ensemble An ensemble model object containing at least a fitted `glm_model`.
#' @param explainer A DALEX explainer or similar object with an `input_frame` data frame.
#' @param response_var Character string specifying the name of the response variable
#' @param trim_vals Numeric vector of trimming values to test. Defaults to
#'   `c(NA_real_, 4, 1, 0.2, 0.15, 0.1, 0.05, 0)`.
#' @param sample_perc Numeric scalar in (0,1]; fraction of data to sample for plotting.
#'   Defaults to `0.2`.
#' @param var Optional string giving the name of a variable to color points by.
#'
#' @return A `ggplot2` object showing ensemble vs. GLM predictions for each trim value.
#'
#' @examples
#' \dontrun{
#' p <- correction_corridor(ensemble = my_ensemble,
#'                          explainer = my_explainer,
#'                          var = "Age")
#' p
#' }
#'
#' @import ggplot2
#'
#' @export
correction_corridor <- function(ensemble,
                                explainer,
                                response_var,
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
    ens_pred <- stats::predict(
      model = ensemble,
      dt = df |>
        dplyr::select(-dplyr::all_of(response_var)),
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
  p <- ggplot(df_all, aes(x = .data$glm, y = .data$ens)) +
    {
      if (!is.na(var)) {
        geom_point(aes(color = .data[[var]]), alpha = 0.4)
      } else {
        geom_point(alpha = 0.4)
      }
    } +
    facet_wrap(~ trim, ncol = min(4, length(trim_vals))) +
    labs(
      x = "GLM Prediction",
      y = "Ensemble Prediction",
      title = "Correction Corridor by Trim Value",
      color = if (!is.na(var)) var else NULL
    ) +
    geom_abline(slope = 1, intercept = 0) +
    theme_minimal()

  return(p)
}
