


drop_xgb_data_params <- function(xgb_all_params) {
  # Create a copy to avoid modifying the original
  cleaned_params <- xgb_all_params

  # Remove the 'data' item
  cleaned_params$data <- NULL

  # Remove 'validation' from evals if it exists
  if (!is.null(cleaned_params$evals)) {
    cleaned_params$evals$validation <- NULL

    # If evals is now empty, remove it entirely
    if (length(cleaned_params$evals) == 0) {
      cleaned_params$evals <- NULL
    }
  }

  return(cleaned_params)
}

