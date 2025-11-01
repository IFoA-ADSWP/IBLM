#' Split Dataframe into: 'train', 'validate', 'test'
#'
#' This function randomly splits a data frame into three subsets for machine
#' learning workflows: training, validation, and test sets. The proportions
#' can be customized and must sum to 1.
#'
#' @param df A data frame to be split into subsets.
#' @param train_prop A numeric value between 0 and 1 specifying the proportion
#'   of data to allocate to the training set.
#' @param validate_prop A numeric value between 0 and 1 specifying the proportion
#'   of data to allocate to the validation set.
#' @param test_prop A numeric value between 0 and 1 specifying the proportion
#'   of data to allocate to the test set.
#' @param seed (optional) a numeric value to set the random no. seed within function environment.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{train}{A data frame containing the training subset}
#'   \item{validate}{A data frame containing the validation subset}
#'   \item{test}{A data frame containing the test subset}
#' }
#'
#' @details The function assigns each row to either "train", "validate" or "test" with
#' the probability defined in the function.
#'
#' Because each row is assigned a bucket independently, for very small datasets the proportions may not
#' be as desired. This should not be an issue as data used for `iblm` must be reasonably large.
#'
#' @examples
#' # Using 'mtcars'
#' split_into_train_validate_test(
#'   mtcars,
#'   train_prop = 0.6,
#'   validate_prop = 0.2,
#'   test_prop = 0.2,
#'   seed = 9000
#' )
#'
#' @export
split_into_train_validate_test <- function(
    df,
    train_prop = 0.7,
    validate_prop = 0.15,
    test_prop = 0.15,
    seed = NULL) {

  stopifnot(
    is.data.frame(df),
    dplyr::near(sum(train_prop, validate_prop, test_prop), 1)
  )

  if (!is.null(seed)) {
    withr::local_seed(seed)
  }


  split <- sample(
    c("train", "validate", "test"),
    size = nrow(df),
    replace = TRUE,
    prob = c(train_prop, validate_prop, test_prop)
  )

  dfs <- lapply(
    c("train", "validate", "test"),
    FUN = function(train_features) {
      df[split == train_features, ]
    }
  ) |>
    stats::setNames(c("train", "validate", "test"))

  return(dfs)
}
