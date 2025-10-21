#' Check Required Names in a List or Data Frame
#'
#' Verifies that all required names are present in a list or data frame.
#' Throws an informative error if any required names are missing.
#'
#' @param x A list (including dataframes) to check.
#' @param required_names A character vector of names that must be present in `x`.
#'
#' @return Returns \code{TRUE} if all required names are present. Throws an error otherwise.
#'
#' @noRd
check_required_names <- function(x, required_names) {

  # Check input type
  if (!is.list(x)) {
    cli::cli_abort("Input must be a list or data.frame.")
  }
  # Find missing names
  missing <- setdiff(required_names, names(x))
  # Throw error if any are missing
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required names:",
      stats::setNames(missing, rep("*", length(missing)))
    ))
  }
  # Return TRUE if all checks pass
  invisible(TRUE)
}


#' Assign a variable type based on membership in predefined lists
#'
#' This function checks whether a given variable is listed in
#' `vars_continuous` or `vars_categorical` and assigns it the
#' corresponding type (`"numerical"` or `"categorical"`). If the
#' variable is not found in either list, an error is thrown.
#'
#' @param var A variable name or symbol to check.
#' @param vars_continuous A character vector of variable names
#'   considered continuous.
#' @param vars_categorical A character vector of variable names
#'   considered categorical.
#'
#' @return A character string: either `"numerical"` or `"categorical"`.
#'
#' @noRd
assign_variable_type <- function(
    var,
    vars_continuous,
    vars_categorical) {

  varname <- substitute(var) # captures the expression passed in
  varname <- as.character(varname)

  if (var %in% vars_continuous) {
    vartype <- "numerical"
  } else if (var %in% vars_categorical) {
    vartype <- "categorical"
  } else {
    valid_vars <- base::union(vars_continuous, vars_categorical)
    cli::cli_abort(c(
      "!" = paste0("'", varname, "' is not a recognized variable."),
      "x" = "{.var varname} should be one of the following:",
      " " = paste(valid_vars, collapse = ", ")
    ))
  }


  return(vartype)
}
