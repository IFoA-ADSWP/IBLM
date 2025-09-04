poisson_deviance <- function(y_true, y_pred, correction = +10^-7) {
  pd <- mean((y_pred - y_true - y_true * log((y_pred + correction) / (y_true + correction))))
  return(2 * pd)
}

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
