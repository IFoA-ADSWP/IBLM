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
