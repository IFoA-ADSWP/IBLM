

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
