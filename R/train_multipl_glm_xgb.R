#' Train Multiplicative GLM-XGBoost Ensemble Model
#'
#' Trains an ensemble model combining GLM and XGBoost where XGBoost learns
#' multiplicative corrections to GLM predictions. The XGBoost model is trained
#' on the ratio of actual values to GLM predictions, creating a multiplicative
#' ensemble approach.
#'
#' @param glm_model A fitted GLM model object that can be used with predict()
#' @param x Data frame or matrix containing features for the training data
#' @param y Numeric vector of actual target values for the training data
#' @param vdt List containing validation data with elements:
#'   \itemize{
#'     \item x_val: Features for validation data
#'     \item y_val: Actual target values for validation data
#'   }
#' @param params List of XGBoost parameters. Default includes:
#'   \itemize{
#'     \item objective = "count:poisson": Loss function for Poisson regression
#'     \item eval_metric = "poisson-nloglik": Evaluation metric
#'   }
#' @param use_glm Logical indicating whether to initialize XGBoost with GLM
#'   predictions using base_margin. Default is FALSE
#' @param p Numeric parameter (currently unused in the function)
#'
#' @return An object of class "ens" containing:
#'   \itemize{
#'     \item glm_model: The original fitted GLM model
#'     \item xgb_model: The fitted XGBoost model trained on multiplicative corrections
#'   }
#'
#' @details
#' This function implements a multiplicative ensemble approach where:
#' \enumerate{
#'   \item GLM predictions are generated for both training and validation sets
#'   \item Target ratios are calculated as actual_values / glm_predictions
#'   \item XGBoost is trained to predict these ratios (multiplicative corrections)
#'   \item Final predictions are obtained by multiplying GLM predictions with XGBoost ratios
#' }
#'
#' When use_glm = TRUE, the XGBoost model is initialized with GLM link predictions
#' as base margins, which can improve convergence and performance.
#'
#' The function uses early stopping with 25 rounds and trains for a maximum of
#' 1000 rounds, monitoring performance on the validation set.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fitted GLM model and prepared data
#' glm_fit <- glm(claims ~ age + gender, family = poisson(), data = train_data)
#'
#' # Prepare validation data
#' val_data <- list(x_val = test_features, y_val = test_targets)
#'
#' # Train ensemble model
#' ensemble_model <- train_multipl_glm_xgb(
#'   glm_model = glm_fit,
#'   x = train_features,
#'   y = train_targets,
#'   vdt = val_data,
#'   use_glm = TRUE
#' )
#' }
#'
#' @export
train_multipl_glm_xgb <- function(glm_model,
                                  x, # features of training data
                                  y, # actuals of training data
                                  vdt, # list containing x and y for validation data instead
                                  params = list(
                                    objective = "count:poisson",
                                    eval_metric = "poisson-nloglik"),
                                  use_glm=F,
                                  p = NULL # <-- not used
){

  glm_preds_in = unname(predict(glm_model, x, type="response")) # glm predictions for training subset
  glm_preds_out = unname(predict(glm_model, vdt$x_val, type="response")) # glm predictions for validation subset

  # Training set
  y_train = y # actual values
  X_train = data.matrix(x) # features dataframe in numeric matrix format

  train_targets = y_train / glm_preds_in # scalings/multipliers required to get glm predictions to match actual values (training subset)
  valid_targets = vdt$y_val / glm_preds_out # scalings/multipliers required to get glm predictions to match actual values (validation subset)

  # NOTE the naming conventions here get confusing, however:
  # 'dtrain' is the matrix for training data
  # 'vtrain' is the matrix for validation data
  dtrain = xgboost::xgb.DMatrix(X_train, label = train_targets)
  vtrain = xgboost::xgb.DMatrix(data.matrix(vdt$x_val), label = valid_targets)

  # Initialize with GLM predictions if use_glm is TRUE
  if (use_glm && !is.null(glm_model)) {

    glm_predictions_train = predict(glm_model, x, type="link")
    xgboost::setinfo(dtrain, "base_margin", unname(glm_predictions_train))

    glm_predictions_val = predict(glm_model, vdt$x_val, type="link")
    xgboost::setinfo(vtrain, "base_margin", unname(glm_predictions_val))

  }

  # Fit final, tuned model
  xgb_model = xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = 1000,
    verbose = 1,
    watchlist = list(validation = vtrain),
    early_stopping_rounds = 25
  )

  toreturn = list(glm_model = glm_model,
                  xgb_model = xgb_model)

  class(toreturn) = "ens"

  return(toreturn)
}
