
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
#' @param response_var Character string specifying the name of the response variable
#'   column in the datasets. The string MUST appear in both `data$train` and `data$validate`.
#' @param family Character string specifying the distributional family for the model.
#'   Currently only "poisson" is fully supported. Default is "poisson".
#' @param use_glm Logical indicating whether to use GLM predictions as base margins
#'   in XGBoost training. When TRUE, XGBoost starts from GLM link predictions rather
#'   than zero. Default is FALSE.
#' @param xgb_additional_params Named list of additional parameters to pass to
#'   \code{\link[xgboost]{xgb.train}}. Default includes nrounds = 1000, verbose = 1,
#'   and early_stopping_rounds = 25.
#'
#' @return An object of class "ens" containing:
#'   \item{glm_model}{The fitted GLM model object}
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
#'   \item The function expects `data$train` and `data$validate` to have identical column structures
#'   \item GLM predictions should not be zero to avoid division by zero errors
#'   \item Currently only supports Poisson family; other families will use default XGBoost settings
#' }
#'
#' @examples
#' library(IBLM)
#'
#' data <- split_into_train_validate_test(freMTPL2freq)
#'
#' ensemble_model <- train_glm_xgb(data, response_var = "ClaimRate")
#'
#' @seealso
#' \code{\link[stats]{glm}}, \code{\link[xgboost]{xgb.train}}
#'
#' @export
train_glm_xgb <- function(data,
                          response_var,
                          family= "poisson",
                          use_glm = FALSE,
                          xgb_additional_params = list(
                            nrounds = 1000,
                            verbose = 1,
                            early_stopping_rounds = 25
                          )
){

  # ==================== checks ====================

  check_required_names(data, c("train", "validate"))
  check_required_names(data[['train']], response_var)
  check_required_names(data[['validate']], response_var)
  stopifnot(
    length(response_var) == 1,
    names(data[['train']]) == names(data[['validate']])
  )

  # ==================== input generation ====================

  train <- list()
  validate <- list()

  predictor_vars <- setdiff(names(data[['train']]), response_var)

  train$responses <- data[['train']] |>  dplyr::pull(response_var)
  validate$responses <- data[['validate']] |>  dplyr::pull(response_var)

  train$features <- data[['train']] |>  dplyr::select(-dplyr::all_of(response_var))
  validate$features <- data[['validate']] |>  dplyr::select(-dplyr::all_of(response_var))

  if(family == "poisson") {

  xgb_family_params <- list(
    base_score = 1,
    objective = "count:poisson",
    eval_metric = "poisson-nloglik")

  glm_family <- poisson()

  }

  # ==================== GLM fitting ====================

  predictor_vars <- setdiff(names(data[['train']]), response_var)

  formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " + ")))

  glm_model <- glm(formula, data = data[['train']], family = glm_family)

  # ==================== Preparing for XGB  ====================

  train$glm_preds <- unname(predict(glm_model, train$features, type="response"))
  validate$glm_preds <- unname(predict(glm_model, validate$features, type="response"))

  train$targets <- train$responses / train$glm_preds
  validate$targets <- validate$responses / validate$glm_preds

  train$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(train$features), label = train$targets)
  validate$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(validate$features), label = validate$targets)

  # Initialize with GLM predictions if use_glm is TRUE
  if (use_glm && !is.null(glm_model)) {

    glm_predictions_train <- predict(glm_model, train$features, type="link")
    xgboost::setinfo(train$xgb_matrix, "base_margin", unname(glm_predictions_train))

    glm_predictions_val <- predict(glm_model, validate$features, type="link")
    xgboost::setinfo(validate$xgb_matrix, "base_margin", unname(glm_predictions_val))

  }

  # ==================== Fitting XGB  ====================

  xgb_core_params <- list(
    params = xgb_family_params,
    data = train$xgb_matrix,
    watchlist = list(validation = validate$xgb_matrix)
  )
  xgb_all_params <- modifyList(xgb_core_params, xgb_additional_params)

  xgb_model <- do.call(xgboost::xgb.train, xgb_all_params)

  # ==================== Collating Output  ====================

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
