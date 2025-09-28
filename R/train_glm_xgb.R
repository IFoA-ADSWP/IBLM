
#' Train GLM-XGBoost Ensemble Model
#'
#' @description
#' This function trains an ensemble model combining a Generalized Linear Model (GLM)
#' with an XGBoost model.
#'
#' The XGBoost model is trained on:
#' - actual responses / GLM predictions, when the link function is log
#' - actual responses - GLM predictions, when the link function is identity
#'
#' This gets XGBoost to effectively learn the residual patterns that the GLM couldn't
#' capture. Optionally, GLM predictions can be used as base margins for XGBoost training.
#'
#' @param data A named list containing training and validation datasets. Must have
#'   elements named "train" and "validate", each containing data frames with the
#'   same structure. This item is naturally output from the function [split_into_train_validate_test()]
#' @param response_var Character string specifying the name of the response variable
#'   column in the datasets. The string MUST appear in both `data$train` and `data$validate`.
#' @param family Character string specifying the distributional family for the model.
#'   Currently only "poisson", "gamma", "tweedie" and "gaussian" is fully supported. See details for how this impacts fitting.
#' @param xgb_additional_params Named list of additional parameters to pass to \link[xgboost]{xgb.train}
#'
#' @return An object of class "ens" containing:
#'   \item{glm_model}{The fitted GLM model object}
#'   \item{xgb_model}{The trained XGBoost model object}
#'
#' @details
#' The `family` argument will be fed into the GLM fitting. Default values for the XGBoost fitting are also selected based on family.
#'
#' Note: Any xgboost configuration below will be overwritten by any explicit arguments input via `xgb_additional_params`
#'
#' For "poisson" family the link function is 'log' and XGBoost is configured with:
#' \itemize{
#'   \item objective: "count:poisson"
#'   \item base_score: 1
#' }
#'
#' For "gamma" family the link function is 'log' and XGBoost is configured with:
#' \itemize{
#'   \item objective: "reg:gamma"
#'   \item base_score: 1
#' }
#'
#' For "tweedie" family the link function is 'log' (with a var.power = 1.5) and XGBoost is configured with:
#' \itemize{
#'   \item objective: "reg:tweedie"
#'   \item base_score: 1
#'   \item tweedie_variance_power = 1.5
#' }
#'
#' For "gaussian" family the link function is 'identity' and XGBoost is configured with:
#' \itemize{
#'   \item objective: "reg:squarederror"
#'   \item base_score: 0
#' }
#' @examples
#' \dontrun{
#' library(IBLM)
#'
#' data <- split_into_train_validate_test(freMTPL2freq)
#'
#' ensemble_model <- train_glm_xgb(data, response_var = "ClaimRate")
#' }
#'
#' @seealso
#' \link[stats]{glm}, \link[xgboost]{xgb.train}
#'
#' @export
train_glm_xgb <- function(data,
                          response_var,
                          family= "poisson",
                          xgb_additional_params = list(
                            nrounds = 1000,
                            verbose = 0,
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

  # ==================== glm/xgb distribution and link choices ====================

  if(family == "poisson") {

  xgb_family_params <- list(
    base_score = 1,
    objective = "count:poisson"
    )

  glm_family <- stats::poisson()

  } else if(family == "gamma") {

    xgb_family_params <- list(
      base_score = 1,
      objective = "reg:gamma"
    )

    glm_family <- stats::Gamma(link = "log")

  } else if(family == "tweedie") {

    xgb_family_params <- list(
      base_score = 1,
      objective = "reg:tweedie",
      tweedie_variance_power = 1.5
    )

    glm_family <- statmod::tweedie(var.power = 1.5, link.power = 0)
    glm_family$link <- "log"

  } else if(family == "gaussian") {

    xgb_family_params <- list(
      base_score = 0,
      objective = "reg:squarederror"
    )

    glm_family <- stats::gaussian()

  } else {

    stop(paste0("family was ", family, " but should be one of: poisson, gamma, tweedie, gaussian"))

         }

  # ==================== GLM fitting ====================

  predictor_vars <- setdiff(names(data[['train']]), response_var)

  formula <- stats::as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " + ")))

  glm_model <- stats::glm(formula, data = data[['train']], family = glm_family)

  # ==================== Preparing for XGB  ====================

  link <- glm_family$link

  train$glm_preds <- unname(stats::predict(glm_model, train$features, type="response"))
  validate$glm_preds <- unname(stats::predict(glm_model, validate$features, type="response"))

  if(link=="log") {

    train$targets <- train$responses / train$glm_preds
    validate$targets <- validate$responses / validate$glm_preds
    relationship <- "multiplicative"

  } else if(link == "identity") {

    train$targets <- train$responses - train$glm_preds
    validate$targets <- validate$responses - validate$glm_preds
    relationship <- "additive"

  } else {

    stop(paste0("link function was ",link," but should be one of: log, identity"))

  }

  train$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(train$features), label = train$targets)
  validate$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(validate$features), label = validate$targets)


  # ==================== Fitting XGB  ====================

  xgb_core_params <- list(
    params = xgb_family_params,
    data = train$xgb_matrix,
    watchlist = list(validation = validate$xgb_matrix)
  )
  xgb_all_params <- utils::modifyList(xgb_core_params, xgb_additional_params)

  xgb_model <- do.call(xgboost::xgb.train, xgb_all_params)

  # ==================== Collating Output  ====================

  toreturn = list(glm_model = glm_model,
                  xgb_model = xgb_model)

  attr(toreturn, "relationship") <- relationship

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
