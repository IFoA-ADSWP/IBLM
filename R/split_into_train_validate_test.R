



#' Split a data frame into training, validation, and test sets
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
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{train}{A data frame containing the training subset}
#'   \item{validate}{A data frame containing the validation subset}
#'   \item{test}{A data frame containing the test subset}
#' }
#'
#' @details The function uses random sampling with replacement to assign each
#'   row to one of the three sets according to the specified proportions. The
#'   proportions must sum to 1 (with some tolerance for floating point precision).
#'
#' @examples
#' # Using 'mtcars'
#' split_into_train_validate_test(
#'   mtcars,
#'   train_prop = 0.8,
#'   validate_prop = 0.1,
#'   test_prop = 0.1
#' )
#'
#' # Using 'freMTPL2freq'
#' freMTPL2freq |> split_into_train_validate_test()
#'
#' @export
split_into_train_validate_test <- function(
    df,
    train_prop = 0.7,
    validate_prop = 0.15,
    test_prop = 0.15
) {

  stopifnot(
    is.data.frame(df),
    dplyr::near(sum(train_prop, validate_prop, test_prop), 1)
  )

  split <- sample(
    c("train", "validate", "test"),
    size = nrow(df),
    replace = TRUE,
    prob = c(train_prop, validate_prop, test_prop)
  )

  dfs <- lapply(
    c("train", "validate", "test"),
    FUN = function(train_features){df[split==train_features,]}
  ) |>
    stats::setNames(c("train", "validate", "test"))

  return(dfs)

}
