#' Plot GLM vs IBLM Predictions with Different Corridors
#'
#' Creates a faceted scatter plot comparing GLM predictions to ensemble predictions
#' across different trim values, showing how the ensemble corrects the base GLM model.
#'
#' @param iblm_model An IBLM model object of class "iblm".
#' @param data Data frame.
#' If you have used `split_into_train_validate_test()` this will usually be the "test" portion of your data.
#' @param trim_vals Numeric vector of trim values to compare.
#' The length of this vector will dictate the no. of facets shown in plot output
#' @param sample_perc Proportion of data to randomly sample for plotting (0 to 1).
#'   Default is 0.2 to improve performance with large datasets
#' @param color Optional. Name of a variable in `data` to color points by
#' @param ... Additional arguments passed to `geom_point()`
#'
#' @return A ggplot object showing GLM vs IBLM predictions faceted by trim value.
#'   The diagonal line (y = x) represents perfect agreement between models
#'
#' @examples
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' correction_corridor(iblm_model = iblm_model, data = df_list$test, color = "DrivAge")
#'
#' @export
correction_corridor <- function(iblm_model,
                                data,
                                trim_vals = c(NA_real_, 4, 1, 0.2, 0.1, 0),
                                sample_perc = 0.2,
                                color = NA,
                                ...) {

  check_iblm_model(iblm_model)

  response_var <- all.vars(iblm_model$glm_model$formula)[1]

  # Sample data
  df <- data |> dplyr::sample_frac(sample_perc)

  # Store optional variable if given
  var_vals <- if (!is.na(color)) df[[color]] else NULL

  # Compute GLM predictions once
  glm_pred <- stats::predict(iblm_model$glm_model, df, type = "response") |>
    as.vector()

  # Generate predictions for each trim value
  df_list <- lapply(trim_vals, function(trim_val) {
    iblm_pred <- stats::predict(
      object = iblm_model,
      newdata = df,
      trim = trim_val
    )

    out <- data.frame(
      glm = glm_pred,
      iblm = iblm_pred,
      trim = ifelse(is.na(trim_val), "NA", as.character(trim_val))
    )

    # Add color if provided
    if (!is.null(var_vals)) {
      out[[color]] <- var_vals
    }

    return(out)
  })

  # Combine all predictions
  df_all <- dplyr::bind_rows(df_list)

  # Start ggplot
  p <- ggplot(df_all, aes(x = .data$glm, y = .data$iblm)) +
    {
      if (!is.na(color)) {
        geom_point(aes(color = .data[[color]]), ...)
      } else {
        geom_point(...)
      }
    } +
    facet_wrap(~trim) +
    labs(
      x = "GLM Prediction",
      y = "IBLM Prediction",
      title = "Correction Corridor by Trim Value",
      color = if (!is.na(color)) color else NULL
    ) +
    geom_abline(slope = 1, intercept = 0) +
    theme_iblm() +
    coord_equal()

  return(p)
}
