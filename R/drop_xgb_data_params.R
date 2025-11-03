


drop_xgb_data_params <- function(xgb_all_params) {
  # Create a copy to avoid modifying the original
  cleaned_params <- xgb_all_params

  # Remove the 'data' item
  cleaned_params$data <- NULL

  # Remove 'validation' from watchlist if it exists
  if (!is.null(cleaned_params$watchlist)) {
    cleaned_params$watchlist$validation <- NULL

    # If watchlist is now empty, remove it entirely
    if (length(cleaned_params$watchlist) == 0) {
      cleaned_params$watchlist <- NULL
    }
  }

  return(cleaned_params)
}

