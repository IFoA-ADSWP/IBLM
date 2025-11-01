#' Detect Outliers in Numeric Vector
#'
#' Identifies outliers in a numeric vector using either quantiles
#'
#' @param x Numeric vector to analyze for outliers
#' @param q Numeric value between 0 and 0.5 specifying the quantile threshold
#'   for outlier detection. Default is 0.01
#'
#' @return Logical vector of same length as x, where TRUE indicates values
#'   to keep (non-outliers) and FALSE indicates outliers
#'
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100) # 100 is an outlier
#' detect_outliers(x, method = "quantile", q = 0.1)
#'
#' @noRd
detect_outliers <- function(x, q = 0.01) {

  if (!is.numeric(x)) stop("Input vector 'x' must be numeric.")
  if (!is.numeric(q) || q <= 0 || q >= 0.5) stop("Parameter 'q' must be between 0 and 0.5.")
  keep <- rep(TRUE, length(x)) # default: keep all

    lower <- stats::quantile(x, probs = q, na.rm = TRUE)
    upper <- stats::quantile(x, probs = 1 - q, na.rm = TRUE)
    keep <- x >= lower & x <= upper

  return(keep)
}
