#' Detect Outliers in Numeric Vector
#'
#' Identifies outliers in a numeric vector using either quantile-based method
#' or isolation forest algorithm.
#'
#' @param x Numeric vector to analyze for outliers
#' @param method Character string specifying the method to use. Options are
#'   "quantile" (default) or "isoforest"
#' @param q Numeric value between 0 and 0.5 specifying the quantile threshold
#'   for outlier detection. Default is 0.01
#'
#' @return Logical vector of same length as x, where TRUE indicates values
#'   to keep (non-outliers) and FALSE indicates outliers
#'
#' @details
#' For "quantile" method: values below the q-th quantile or above the
#' (1-q)-th quantile are considered outliers.
#'
#' For "isoforest" method: uses isolation forest algorithm from the isotree
#' package to detect anomalies.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100)  # 100 is an outlier
#' detect_outliers(x, method = "quantile", q = 0.1)
#'
#' @noRd
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

