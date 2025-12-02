

#' Check Data Variability for Modeling
#'
#' Validates that the response variable and all predictor variables have more
#' than one unique value.
#'
#' @param data A data frame containing the variables to check.
#' @param response_var Character string naming the response variable in `data`.
#'
#' @return Invisibly returns `TRUE` if all checks pass, otherwise throws an error.
#'
#' @keywords internal
check_data_variability <- function(data, response_var) {

  unique_resp_vals <- length(unique(data[[response_var]]))

  if(unique_resp_vals <= 1) {
    cli::cli_abort(
      c("Response variable must have more than one unique value.",
        "x" = "The following variables have only one unique value: {.field {response_var}}.")
    )
  }

  unique_pred_vals <- purrr::map_dbl(data, function(x) length(unique(x)))
  unvaried_fields <- names(data)[unique_pred_vals <= 1]

  if(length(unvaried_fields) > 0) {
    cli::cli_abort(
      c("Predictor variables must have more than one unique value.",
        "x" = "The following variables have only one unique value: {.field {unvaried_fields}}.")
    )
  }

  invisible(TRUE)

}




#' Check if Two Data Frames are Equal
#'
#' Compares two data frames for equality, ignoring row order, column order,
#' and data type differences.
#'
#' @param df1 A data frame.
#' @param df2 A data frame to compare with \code{df1}.
#'
#' @return Logical. \code{TRUE} if data frames have matching dimensions,
#'   column names, and values (after type conversion); \code{FALSE} otherwise.
#'
#' @examples
#' df1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
#' df2 <- data.frame(b = c("z", "y", "x"), a = c(3, 2, 1))
#' check_dfs_equal(df1, df2)  # TRUE
#'
#' @noRd
check_dfs_equal <- function(df1, df2) {
  # Check dimensions
  if (nrow(df1) != nrow(df2) || ncol(df1) != ncol(df2)) {
    return(FALSE)
  }
  # Check column names
  if (!setequal(names(df1), names(df2))) {
    return(FALSE)
  }
  # Reorder columns to match
  df2 <- df2[, names(df1)]
  # Convert factors to character, keep numeric as numeric
  convert_col <- function(x) {
    if (is.factor(x)) as.character(x) else x
  }
  df1_conv <- as.data.frame(lapply(df1, convert_col), stringsAsFactors = FALSE)
  df2_conv <- as.data.frame(lapply(df2, convert_col), stringsAsFactors = FALSE)
  # Sort rows
  df1_sorted <- df1_conv[do.call(order, df1_conv), , drop = FALSE]
  df2_sorted <- df2_conv[do.call(order, df2_conv), , drop = FALSE]
  # Reset row names and compare
  rownames(df1_sorted) <- NULL
  rownames(df2_sorted) <- NULL
  isTRUE(all.equal(df1_sorted, df2_sorted))
}
