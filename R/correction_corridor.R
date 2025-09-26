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
    ens_pred <- stats::predict(
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
