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


# NOTE `predict.ens` is locked in to the glm x xgb combo under this code. Will need tweaks to expand flexibility.
# NOTE need to import `xgboost` to get correct method for predict(). Will need to add this in roxygen code

#' Predict method for ensemble models
#'
#' @description
#' This function generates predictions from an ensemble model consisting of a GLM
#' and an XGBoost model. The final prediction is computed as the product of the
#' GLM response probabilities and the XGBoost predictions, with optional
#' post-hoc trimming applied to the XGBoost component.
#'
#' @param model An object of class "ens" containing:
#'   \describe{
#'     \item{glm_model}{A fitted GLM model object}
#'     \item{xgb_model}{A fitted XGBoost model object}
#'     \item{min_res}{Minimum residual value for trimming (when trim is used)}
#'     \item{max_res}{Maximum residual value for trimming (when trim is used)}
#'   }
#' @param dt A data frame or matrix containing the predictor variables for
#'   which predictions are desired. Must have the same structure as the
#'   training data used to fit the ensemble model.
#' @param trim Numeric value for post-hoc trimming of XGBoost predictions.
#'   If \code{NA} (default), no trimming is applied. When specified, applies
#'   both affine transformation based on learned min/max values and truncation
#'   to the range [max(1-trim, 0), 1+trim], followed by normalization to
#'   maintain unit mean.
#'
#' @return A numeric vector of ensemble predictions computed as the element-wise
#'   product of GLM response probabilities and (optionally trimmed) XGBoost
#'   predictions.
#'
#' @details
#' The prediction process involves the following steps:
#' \enumerate{
#'   \item Generate GLM predictions using \code{type="response"}
#'   \item Generate XGBoost predictions on a DMatrix conversion of the input data
#'   \item If trimming is specified:
#'     \itemize{
#'       \item Apply affine transformation based on model's learned min/max residuals
#'       \item Truncate predictions to the specified trim range
#'       \item Normalize by dividing by the mean to maintain unit average
#'     }
#'   \item Multiply GLM and XGBoost predictions element-wise
#' }
#'
#' The function includes error checking and will enter the debugger if any
#' predictions are NA, NaN, or negative.
#'
#' @examples
#' \dontrun{
#' # Basic prediction without trimming
#' predictions <- predict(ensemble_model, new_data)
#'
#' # Prediction with trimming
#' trimmed_predictions <- predict(ensemble_model, new_data, trim = 0.1)
#' }
#'
#' @seealso \code{\link[stats]{predict}}, \code{\link[xgboost]{predict.xgb.Booster}}
#'
#' @export
#'
predict.ens <- function(model, dt, trim=NA){

  glm = unname(stats::predict(model$glm_model, dt, type="response"))
  xgb = stats::predict(model$xgb_model, xgboost::xgb.DMatrix(data.matrix(dt)), type="response")

  if (!is.na(trim)) {
    # Post hoc trimming based on learned min/max

    squish_affine <- function(x) {
      min_x <- model$min_res
      max_x <- model$max_res

      transformed <- ifelse(
        x < 1,
        1 + (x - 1) * trim * (x - min_x)/(1 - min_x),
        1 + (x - 1) * trim * (x - 1)/(max_x - 1)
      )

      return(transformed)
    }

    truncate <- function(x){
      return(
        pmax(
          pmin(xgb, 1 + trim),
          max(1 - trim, 0)
        )
      )
    }

    xgb = truncate(xgb)
    xgb = xgb * 1 / mean(xgb)
  }

  toreturn = glm * xgb

  if(any(is.na(toreturn) | is.nan(toreturn) | toreturn < 0)) { browser() }

  return(toreturn)
}



#' Create Correction Corridor Visualization
#'
#' Generates a faceted scatter plot comparing GLM predictions against ensemble
#' predictions across different trim values. This visualization helps assess
#' how ensemble corrections vary with different trimming parameters and
#' identifies the "correction corridor" where the ensemble modifies GLM predictions.
#'
#' @param ensemble An ensemble model object (typically of class "ens") containing
#'   both GLM and XGBoost components, with a predict method
#' @param explainer An explainer object containing an input_frame element with
#'   the data to be used for predictions
#' @param trim_vals Numeric vector of trim values to test. Default is
#'   c(NA, 4, 1, 0.2, 0.15, 0.1, 0.05, 0). NA represents no trimming
#' @param sample_perc Numeric value between 0 and 1 specifying the fraction
#'   of data to sample for visualization. Default is 0.2 (20%)
#' @param var Character string specifying an optional variable name from the
#'   data to use for coloring points. Default is NA (no coloring variable)
#'
#' @return A ggplot2 object showing a faceted scatter plot with:
#'   \itemize{
#'     \item X-axis: GLM predictions
#'     \item Y-axis: Ensemble predictions
#'     \item Facets: Different trim values
#'     \item Reference line: y = x (perfect agreement line)
#'     \item Optional: Points colored by specified variable
#'   }
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Sampling a subset of data from the explainer's input_frame
#'   \item Computing GLM predictions once for efficiency
#'   \item Generating ensemble predictions for each trim value
#'   \item Creating a faceted visualization to compare prediction patterns
#' }
#'
#' The "correction corridor" refers to the deviation patterns between GLM and
#' ensemble predictions. Points near the diagonal line (y = x) indicate minimal
#' correction, while points further from the line show where the ensemble
#' significantly adjusts GLM predictions.
#'
#' Different trim values control the magnitude of ensemble corrections, and
#' this visualization helps identify optimal trimming parameters by showing
#' how corrections behave across the prediction range.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default trim values
#' correction_corridor(my_ensemble, my_explainer)
#'
#' # With custom trim values and coloring by a variable
#' correction_corridor(
#'   ensemble = my_ensemble,
#'   explainer = my_explainer,
#'   trim_vals = c(NA, 1, 0.5, 0.1),
#'   sample_perc = 0.3,
#'   var = "age_group"
#' )
#'
#' # High-resolution plot with more data
#' correction_corridor(
#'   ensemble = my_ensemble,
#'   explainer = my_explainer,
#'   sample_perc = 0.5
#' )
#' }
#'
#' @export
correction_corridor <- function(ensemble,
                                explainer,
                                trim_vals = c(NA, 4, 1, 0.2, 0.15, 0.1, 0.05, 0),
                                sample_perc = 0.2,
                                var = NA) {

  # Sample data
  df <- explainer$input_frame |>
    dplyr::sample_frac(sample_perc)

  # Store optional variable if given
  var_vals <- if (!is.na(var)) df[[var]] else NULL

  # Compute GLM predictions once
  glm_pred <- stats::predict(ensemble$glm_model, df, type = "response") |>
    as.vector()

  # Generate predictions for each trim value
  df_list <- lapply(trim_vals, function(trim_val) {
    ens_pred <- predict(
      model = ensemble,
      dt = df |>
        dplyr::select(-ClaimNb),
      trim = trim_val
    )

    out <- data.frame(
      glm = glm_pred,
      ens = ens_pred,
      trim = ifelse(is.na(trim_val), "NA", as.character(trim_val))
    )

    # Add var if provided
    if (!is.null(var_vals)) {
      out[[var]] <- var_vals
    }

    return(out)
  })

  # Combine all predictions
  df_all <- dplyr::bind_rows(df_list)

  # Start ggplot
  p <- ggplot2::ggplot(df_all, ggplot2::aes(x = glm, y = ens)) +
    {
      if (!is.na(var)) {
        ggplot2::geom_point(ggplot2::aes(color = .data[[var]]), alpha = 0.4)
      } else {
        ggplot2::geom_point(alpha = 0.4)
      }
    } +
    ggplot2::facet_wrap(~ trim, ncol = min(4, length(trim_vals))) +
    ggplot2::labs(
      x = "GLM Prediction",
      y = "Ensemble Prediction",
      title = "Correction Corridor by Trim Value",
      color = if (!is.na(var)) var else NULL
    ) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::theme_minimal()

  return(p)
}



