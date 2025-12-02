#' Train IBLM Model on XGBoost (using a pre-existing GLM)
#'
#' @description
#' This function trains an interpretable boosted linear model.
#'
#' The function combines a Generalized Linear Model (GLM) with a booster model of XGBoost
#'
#' The "booster" model is trained on:
#' - actual responses / GLM predictions, when the link function is log
#' - actual responses - GLM predictions, when the link function is identity
#'
#' @param df_list A named list containing training and validation datasets. Must have
#'   elements named "train" and "validate", each containing df_list frames with the
#'   same structure. This item is naturally output from the function [split_into_train_validate_test()]
#' @param glm_model Object of class "glm". This should be a model fitted using `glm()`.
#' The data used to fit this object must be the same data as `df_list$train`.
#' @param params Named list of additional parameters to pass to \link[xgboost]{xgb.train}.
#' Note that \link{train_iblm_xgb} will select "objective" and "base_score" for you
#' depending on `family` (see details section). However you may overwrite these (do so with caution)
#' @param nrounds,obj,feval,verbose,print_every_n,early_stopping_rounds,maximize,save_period,save_name,xgb_model,callbacks,... These are passed directly to \link[xgboost]{xgb.train}
#' @param strip_glm TRUE/FALSE, whether to strip superfluous data from the `glm_model` object saved within `iblm` class that is output. Only serves to reduce memory constraints.
#'
#' @return An object of class "iblm" containing:
#'   \item{glm_model}{The GLM model object, fitted on the `df_list$train` data that was provided}
#'   \item{booster_model}{The booster model object, trained on the residuals leftover from the glm_model}
#'   \item{data}{A list containing the data that was used to train and validate this iblm model}
#'   \item{relationship}{String that explains how to combine the `glm_model` and `booster_model`. Currently only either "Additive" or "Multiplicative"}
#'   \item{response_var}{A string describing the response variable used for this iblm model}
#'   \item{predictor_vars}{A list describing the predictor variables used for this iblm model}
#'   \item{cat_levels}{A list describing the categorical levels for the predictor vars}
#'   \item{coeff_names}{A list describing the coefficient names}
#'
#' @details
#' The `family` is determined by the `glm_model` object. Default values for the XGBoost fitting are also selected based on family.
#'
#' Note: Any xgboost configuration below will be overwritten by any explicit arguments input via `params`
#'
#'
#' For "poisson" family the link function is 'log' and XGBoost is configured with:
#' \itemize{
#'   \item objective: "count:poisson"
#'   \item base_score: 1
#' }
#'
#'
#' For "gamma" family the link function is 'log' and XGBoost is configured with:
#' \itemize{
#'   \item objective: "reg:gamma"
#'   \item base_score: 1
#' }
#'
#'
#' For "tweedie" family the link function is 'log' (with a var.power = 1.5) and XGBoost is configured with:
#' \itemize{
#'   \item objective: "reg:tweedie"
#'   \item base_score: 1
#'   \item tweedie_variance_power = 1.5
#' }
#'
#'
#' For "gaussian" family the link function is 'identity' and XGBoost is configured with:
#' \itemize{
#'   \item objective: "reg:squarederror"
#'   \item base_score: 0
#' }
#'
#' @examples
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' formula <- stats::as.formula(paste("ClaimRate", "~ ."))
#'
#' glm_model <- stats::glm(formula, data = df_list[["train"]], family = "poisson")
#'
#' iblm_model <- train_iblm_xgb_with_glm(
#'   df_list,
#'   glm_model
#' )
#'
#' @seealso
#' \link[stats]{glm}, \link[xgboost]{xgb.train}
#'
#' @export
train_iblm_xgb_with_glm <- function(df_list,
                           glm_model,
                           params = list(),
                           nrounds = 1000,
                           obj = NULL,
                           feval = NULL,
                           verbose = 0,
                           print_every_n = 1L,
                           early_stopping_rounds = 25,
                           maximize = NULL,
                           save_period = NULL,
                           save_name = "xgboost.model",
                           xgb_model = NULL,
                           callbacks = list(),
                           ...,
                           strip_glm = TRUE) {

  cli::cli_alert_warning(
    "This function is still in development and has not been thoroughly checked"
  )


  # =========== intro code for ..._with_glm() variant ===========

  if(!inherits(glm_model, "glm"))  {
    cli::cli_abort(
      "'glm_model' must be of class 'glm'"
    )
  }

  family <- tolower(glm_model$family$family)

  response_var <- as.character(stats::formula(glm_model)[[2]])

  # ==================== checks ====================

  check_required_names(df_list, c("train", "validate"))
  check_required_names(df_list[["train"]], response_var)
  check_required_names(df_list[["validate"]], response_var)
  stopifnot(
    length(response_var) == 1,
    names(df_list[["train"]]) == names(df_list[["validate"]])
  )

  if(sum(is.na(df_list$train), is.na(df_list$validate), is.na(df_list$test)) >0 ) {
    cli::cli_abort(
      "'df_list' cannot contain NA values"
    )
  }

  check_data_variability(df_list[["train"]], response_var)

  # ==================== check data are same ==========

  train_data_is_equal <- check_dfs_equal(df_list[["train"]], glm_model$model)

  if(!train_data_is_equal) {
    cli::cli_abort(
      "data in 'df_list$train' is different to data used to train 'glm_model'"
    )
  }


  # ==================== input generation ====================

  train <- list()
  validate <- list()

  predictor_vars <- setdiff(names(df_list[["train"]]), response_var)

  train$responses <- df_list[["train"]] |> dplyr::pull(response_var)
  validate$responses <- df_list[["validate"]] |> dplyr::pull(response_var)

  train$features <- df_list[["train"]] |> dplyr::select(-dplyr::all_of(response_var))
  validate$features <- df_list[["validate"]] |> dplyr::select(-dplyr::all_of(response_var))

  # ==================== xgb distribution and link choices ====================

  if (family == "poisson") {

    xgb_family_params <- list(
      base_score = 1,
      objective = "count:poisson"
    )

  } else if (family == "gamma") {

    xgb_family_params <- list(
      base_score = 1,
      objective = "reg:gamma"
    )

  } else if (family == "tweedie") {

    xgb_family_params <- list(
      base_score = 1,
      objective = "reg:tweedie",
      tweedie_variance_power = 1.5
    )

  } else if (family == "gaussian") {

    xgb_family_params <- list(
      base_score = 0,
      objective = "reg:squarederror"
    )

  } else {

    stop(paste0("family was ", family, " but should be one of: poisson, gamma, tweedie, gaussian"))

  }

  # ==================== GLM fitting ====================

  ## removed compared to `train_iblm_xgb()`

  # ==================== Preparing for XGB  ====================

  link <- glm_model$family$link

  if (family == "tweedie") {
    if (link == "mu^0") {
      link <- "log"
    } else if (link == "mu^1") {
      link <- "identity"
    }
  }

  glm_model$family$link <- link

  train$glm_preds <- unname(stats::predict(glm_model, train$features, type = "response"))
  validate$glm_preds <- unname(stats::predict(glm_model, validate$features, type = "response"))

  if (link == "log") {
    train$targets <- train$responses / train$glm_preds
    validate$targets <- validate$responses / validate$glm_preds
    relationship <- "multiplicative"
  } else if (link == "identity") {
    train$targets <- train$responses - train$glm_preds
    validate$targets <- validate$responses - validate$glm_preds
    relationship <- "additive"
  } else {
    stop(paste0("link function was ", link, " but should be one of: log, identity"))
  }

  train$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(train$features), label = train$targets)
  validate$xgb_matrix <- xgboost::xgb.DMatrix(data.matrix(validate$features), label = validate$targets)


  # ==================== Fitting XGB  ====================

  xgb_additional_params <- c(
    list(
      nrounds = nrounds,
      obj = obj,
      feval = feval,
      verbose = verbose,
      print_every_n = print_every_n,
      early_stopping_rounds = early_stopping_rounds,
      maximize = maximize,
      save_period = save_period,
      save_name = save_name,
      xgb_model = xgb_model,
      callbacks = callbacks
    ),
    list(...)
  )

  params_to_overwrite <- intersect(names(xgb_family_params), names(params))
  if(length(params_to_overwrite) > 0 ) {
    cli::cli_alert_info(
      "The following 'params' were defined in input and used over default settings: {.val {params_to_overwrite}}"
    )
  }
  params <- utils::modifyList(xgb_family_params, params)

  xgb_core_params <- list(
    params = params,
    data = train$xgb_matrix,
    watchlist = list(validation = validate$xgb_matrix)
  )
  xgb_all_params <- utils::modifyList(xgb_core_params, xgb_additional_params)

  booster_model <- do.call(xgboost::xgb.train, xgb_all_params)

  # ==================== Stripping glm object of data  ===================


  if (strip_glm) {
    stripGlmLR <- function(cm) {
      cm$y <- c()

      cm$residuals <- c()
      cm$fitted.values <- c()
      cm$data <- c()

      cm
    }

    glm_model <- stripGlmLR(glm_model)
  }


  # ==================== Initial 'iblm' Class  ====================

  iblm_model <- list()

  iblm_model$glm_model <- glm_model
  iblm_model$booster_model <- booster_model
  iblm_model$data$train <- df_list$train
  iblm_model$data$validate <- df_list$validate
  iblm_model$relationship <- relationship

  # ==================== Additional 'iblm' Metadata  ====================

  # Definitions and global variables
  glm_beta_coeff <- iblm_model$glm_model$coefficients
  coef_names_glm <- names(glm_beta_coeff)

  vartypes <- lapply(df_list$train |> dplyr::select(-dplyr::all_of(response_var)), typeof) |> unlist()
  varclasses <- lapply(df_list$train |> dplyr::select(-dplyr::all_of(response_var)), class) |> unlist()

  # create data objects that explain variables

  predictor_vars <- list()
  predictor_vars$all <- names(vartypes) |> setdiff(response_var)
  predictor_vars$categorical <- predictor_vars$all[(!vartypes %in% c("integer", "double") | varclasses == "factor")]
  predictor_vars$continuous <- predictor_vars$all |> setdiff(predictor_vars$categorical)

  # Factor levels for categorical variables

  cat_levels <- list()
  coeff_names <- list()


  cat_levels$all <- lapply(
    df_list$train |> dplyr::select(dplyr::all_of(predictor_vars$categorical)),
    function(x) sort(unique(x))
  )

  cat_levels$reference <- stats::setNames(
    lapply(
      names(cat_levels$all),
      function(var) {
        all_levels <- cat_levels$all[[var]]
        present_levels <- coef_names_glm[startsWith(coef_names_glm, var)]
        present_levels_clean <- gsub(paste0("^", var), "", present_levels)
        setdiff(all_levels, present_levels_clean)
      }
    ),
    names(cat_levels$all)
  )

  coeff_names$all_cat <- lapply(
    names(cat_levels$all),
    function(x) paste0(x, cat_levels$all[[x]])
  ) |> unlist()

  coeff_names$all <- c("(Intercept)", predictor_vars$continuous, coeff_names$all_cat)

  coeff_names$reference_cat <- setdiff(coeff_names$all, coef_names_glm)

  # ==================== Add Additional 'iblm' Metadata  ====================

  iblm_model$response_var <- response_var
  iblm_model$predictor_vars <- predictor_vars
  iblm_model$cat_levels <- cat_levels
  iblm_model$coeff_names <- coeff_names

  # ==================== Add the parameters used on booster  ====================

  iblm_model$xgb_params <- drop_xgb_data_params(xgb_all_params)

  class(iblm_model) <- "iblm"

  return(iblm_model)
}
