#' Check Required Names in a List or Data Frame
#'
#' Verifies that all required names are present in a list or data frame.
#' Throws an informative error if any required names are missing.
#'
#' @param df A data frame (or list) to check.
#' @param required_names A character vector of names that must be present in `df`.
#'
#' @return Returns \code{TRUE} if all required names are present. Throws an error otherwise.
#'
#' @examples
#' check_required_names(mtcars, c("mpg", "cyl"))
#' \dontrun{
#' check_required_names(mtcars, c("mpg", "cyl", "idonotexist"))
#' }
check_required_names <- function(df, required_names) {
  # Check input type
  if (!is.list(df)) {
    stop("Input must be a list or data.frame.")
  }

  # Find missing names
  missing <- setdiff(required_names, names(df))

  # Throw error if any are missing
  if (length(missing) > 0) {
    stop("Missing required names: ", paste(missing, collapse = ", "))
  }

  # Return TRUE if all checks pass
  invisible(TRUE)
}
