
#' Train GLM-XGBoost Ensemble Model
#'
#' This function trains an ensemble model combining a Generalized Linear Model (GLM)
#' with an XGBoost model. The XGBoost model is trained on the ratio of actual responses
#' to GLM predictions, effectively learning the residual patterns that the GLM couldn't
#' capture. Optionally, GLM predictions can be used as base margins for XGBoost training.
#'
#' @param data A named list containing training and validation datasets. Must have
#'   elements named "train" and "validate", each containing data frames with the
#'   same structure.
#' @param glm_model A fitted GLM model object (from \code{\link[stats]{glm}}) that
#'   will be used to generate baseline predictions for the ensemble.
#' @param response_var Character string specifying the name of the response variable
#'   column in the datasets. Default is "ClaimRate".
#' @param family Character string specifying the distributional family for the model.
#'   Currently only "poisson" is fully supported. Default is "poisson".
#' @param use_glm Logical indicating whether to use GLM predictions as base margins
#'   in XGBoost training. When TRUE, XGBoost starts from GLM link predictions rather
#'   than zero. Default is FALSE.
#' @param xbg_train_additional_params Named list of additional parameters to pass to
#'   \code{\link[xgboost]{xgb.train}}. Default includes nrounds = 1000, verbose = 1,
#'   and early_stopping_rounds = 25.
#'
#' @return An object of class "ens" containing:
#'   \item{glm_model}{The input GLM model object}
#'   \item{xgb_model}{The trained XGBoost model object}
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Using the provided GLM model to generate predictions on training and validation sets
#'   \item Computing target ratios by dividing actual responses by GLM predictions
#'   \item Training an XGBoost model to predict these ratios
#'   \item Optionally using GLM link predictions as base margins if \code{use_glm = TRUE}
#' }
#'
#' The ensemble prediction would typically be: GLM_prediction * XGBoost_prediction
#'
#' For Poisson family, XGBoost is configured with:
#' \itemize{
#'   \item objective: "count:poisson"
#'   \item eval_metric: "poisson-nloglik"
#'   \item base_score: 1
#' }
#'
#' @note
#' \itemize{
#'   \item The function expects both training and validation datasets to have identical structure
#'   \item GLM predictions should not be zero to avoid division by zero errors
#'   \item Currently only supports Poisson family; other families will use default XGBoost settings
#'   \item The parameter name \code{xbg_train_additional_params} appears to have a typo (should be "xgb")
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' train_data <- data.frame(
#'   ClaimRate = rpois(1000, 2),
#'   feature1 = rnorm(1000),
#'   feature2 = runif(1000)
#' )
#' validate_data <- data.frame(
#'   ClaimRate = rpois(500, 2),
#'   feature1 = rnorm(500),
#'   feature2 = runif(500)
#' )
#' data_list <- list(train = train_data, validate = validate_data)
#'
#' # Fit GLM model
#' glm_fit <- glm(ClaimRate ~ feature1 + feature2,
#'                data = train_data,
#'                family = poisson())
#'
#' # Train ensemble
#' ensemble_model <- train_glm_xgb(
#'   data = data_list,
#'   glm_model = glm_fit,
#'   use_glm = TRUE,
#'   xbg_train_additional_params = list(
#'     nrounds = 500,
#'     eta = 0.1,
#'     max_depth = 6
#'   )
#' )
#' }
#'
#' @seealso
#' \code{\link[stats]{glm}}, \code{\link[xgboost]{xgb.train}}, \code{\link[xgboost]{xgb.DMatrix}}
#'
#' @export
train_glm_xgb <- function(data,
                          glm_model,
                          response_var = "ClaimRate",
                          family= "poisson",
                          use_glm = FALSE,
                          xbg_train_additional_params = list(
                            nrounds = 1000,
                            verbose = 1,
                            early_stopping_rounds = 25
                          )
){

  check_required_names(data, c("train", "validate"))
  check_required_names(data[['train']], response_var)
  check_required_names(data[['validate']], response_var)

  predictor_vars <- setdiff(names(data[['train']]), response_var)

  train_responses <- data[['train']] |>  dplyr::select(dplyr::all_of(response_var))
  train_features <- data[['train']] |>  dplyr::select(-dplyr::all_of(response_var))
  validate_responses <- data[['validate']] |>  dplyr::select(dplyr::all_of(response_var))
  validate_features <- data[['validate']] |>  dplyr::select(-dplyr::all_of(response_var))

  if(family == "poisson") {
  params <- list(
    base_score = 1,
    objective = "count:poisson",
    eval_metric = "poisson-nloglik")
  }

  # GLM fitting


  # Preparing data for XBG

  train_glm_preds <- unname(predict(glm_model, train_features, type="response"))
  validate_glm_preds <- unname(predict(glm_model, validate_features, type="response"))

  train_targets <- train_responses / train_glm_preds
  validate_targets <- validate_responses / validate_glm_preds

  train_xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(train_features), label = train_targets)
  validate_xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(validate_features), label = validate_targets)

  # Initialize with GLM predictions if use_glm is TRUE
  if (use_glm && !is.null(glm_model)) {

    glm_predictions_train <- predict(glm_model, train_features, type="link")
    xgboost::setinfo(train_xgb_matrix, "base_margin", unname(glm_predictions_train))

    glm_predictions_val <- predict(glm_model, validate_features, type="link")
    xgboost::setinfo(validate_xgb_matrix, "base_margin", unname(glm_predictions_val))

  }

  xbg_train_core_params <- list(
    params = params,
    data = train_xgb_matrix,
    watchlist = list(validation = validate_xgb_matrix)
  )
  xbg_train_all_params <- modifyList(xbg_train_core_params, xbg_train_additional_params)

  xgb_model <- do.call(xgboost::xgb.train, xbg_train_all_params)

  toreturn = list(glm_model = glm_model,
                  xgb_model = xgb_model)

  class(toreturn) <- "ens"

  return(toreturn)
}






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
#' # Custom proportions
#' splits <- split_into_train_validate_test(
#'   mtcars,
#'   train_prop = 0.8,
#'   validate_prop = 0.1,
#'   test_prop = 0.1
#' )
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

  splits <- lapply(
    c("train", "validate", "test"),
    FUN = function(train_features){df[split==train_features,]}
  ) |>
    stats::setNames(c("train", "validate", "test"))

  return(splits)

}
