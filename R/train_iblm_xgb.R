#' Train IBLM Model on XGBoost
#'
#' @description
#' This function trains an interpretable boosted linear model.
#'
#' The function combines a Generalized Linear Model (GLM) with a booster model of XGBoost
#'
#' The "booster" model is trained on the residuals of the glm model to the response_var, such that:
#' - when the link function is log, IBLM predictions = GLM predictions * Booster predictions
#' - when the link function is identity, IBLM predictions = GLM predictions + Booster predictions
#'
#' @param df_list A named list containing training and validation datasets. Must have
#'   elements named "train" and "validate", each containing df_list frames with the
#'   same structure. This item is naturally output from the function [split_into_train_validate_test()]
#' @param response_var Character string specifying the name of the response variable
#'   column in the datasets. The string MUST appear in both `df_list$train` and `df_list$validate`.
#' @param weight_var Character string specifying the name of a variable to weight by.
#'  Value of NULL (default) for no weighting. Any string MUST appear in both `df_list$train` and `df_list$validate`.
#' @param offset_var Character string specifying the name of a variable to use as offset.
#'  Value of NULL (default) for no offset. Any string MUST appear in both `df_list$train` and `df_list$validate`.
#'
#' Any transformations required (e.g. log) must be performed BEFORE `df_list` is fed into function.
#' @param family Character string specifying the distributional family for the model.
#'   Currently only "poisson", "quasipoisson", "gamma", "tweedie" and "gaussian" is fully supported. See details for how this impacts fitting.
#' @param params Named list of additional parameters to pass to \link[xgboost]{xgb.train}.
#' Note that \link{train_iblm_xgb} will select "objective" and "base_score" for you
#' depending on `family` (see details section). However you may overwrite these (do so with caution)
#' @param nrounds,objective,custom_metric,verbose,print_every_n,early_stopping_rounds,maximize,save_period,save_name,xgb_model,callbacks,... These are passed directly to \link[xgboost]{xgb.train}
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
#' The `family` argument will be fed into the GLM fitting. Default `params` values for the XGBoost fitting are also selected based on family:
#' \itemize{
#'   \item For "poisson" family, the "objective" is set to "count:poisson"
#'   \item For "quasipoisson" family, the "objective" is set to "count:poisson"
#'   \item For "gamma" family, the "objective" is set to "reg:gamma"
#'   \item For "tweedie" family, the "objective" is set to "reg:tweedie". Also, "tweedie_variance_power = 1.5".
#'   \item For "gaussian" family, the "objective" is set to "reg:squarederror"
#' }
#'
#' Note: Any xgboost configuration below will be overwritten by any explicit arguments input into `train_iblm_xgb()`
#'
#' @examples
#' df_list <- freMTPLmini |> dplyr::mutate(LogExposure = log(Exposure), .keep = "unused") |>  split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimNb",
#'   offset_var = "LogExposure",
#'   family = "poisson"
#' )
#'
#' @seealso
#' \link[stats]{glm}, \link[xgboost]{xgb.train}
#'
#' @export
train_iblm_xgb <- function(df_list,
                           response_var,
                           weight_var = NULL,
                           offset_var = NULL,
                           family = "poisson",
                           params = list(),
                           nrounds = 1000,
                           objective = NULL,
                           custom_metric = NULL,
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

  # ==================== checks ====================

  check_required_names(df_list, c("train", "validate"))
  check_required_names(df_list[["train"]], response_var)
  check_required_names(df_list[["validate"]], response_var)
  if (!is.null(weight_var)) {
    check_required_names(df_list[["train"]], weight_var)
    check_required_names(df_list[["validate"]], weight_var)
  }
  if (!is.null(offset_var)) {
    check_required_names(df_list[["train"]], offset_var)
    check_required_names(df_list[["validate"]], offset_var)
  }
  stopifnot(
    length(response_var) == 1,
    names(df_list[["train"]]) == names(df_list[["validate"]])
  )

  if (sum(is.na(df_list$train), is.na(df_list$validate)) > 0) {
    cli::cli_abort(
      "'df_list' cannot contain NA values"
    )
  }

  if(any(vapply(df_list$train, is.character, logical(1)))) {
    cli::cli_abort(
      "'df_list' cannot contain character columns. Convert to factor."
    )
  }

  check_data_variability(df_list[["train"]], response_var)


  # ==================== input generation ====================

  train <- list()
  validate <- list()

  train$responses <- df_list[["train"]] |> dplyr::pull(response_var)
  validate$responses <- df_list[["validate"]] |> dplyr::pull(response_var)

  train$features <- df_list[["train"]] |> dplyr::select(-dplyr::all_of(c(response_var, weight_var, offset_var)))
  validate$features <- df_list[["validate"]] |> dplyr::select(-dplyr::all_of(c(response_var, weight_var, offset_var)))

  if (!is.null(weight_var)) {
    train$weights <- df_list[["train"]] |> dplyr::pull(weight_var)
    validate$weights <- df_list[["validate"]] |> dplyr::pull(weight_var)
  } else {
    train$weights <- NULL
    validate$weights <- NULL
  }

  if (!is.null(offset_var)) {
    train$offset <- df_list[["train"]] |> dplyr::pull(offset_var)
    validate$offset <- df_list[["validate"]] |> dplyr::pull(offset_var)
  } else {
    train$offset <- NULL
    validate$offset <- NULL
  }

  # ==================== glm distribution choices ====================

  if (family == "poisson") {

    glm_family <- stats::poisson()

  } else if (family == "quasipoisson") {

    glm_family <- stats::quasipoisson()

  } else if (family == "gamma") {

    glm_family <- stats::Gamma(link = "log")

  } else if (family == "tweedie") {

    glm_family <- statmod::tweedie(var.power = 1.5, link.power = 0)
    glm_family$link <- "log"

  } else if (family == "gaussian") {

    glm_family <- stats::gaussian()

  } else {

    stop(paste0("family was ", family, " but should be one of: poisson, quasipoisson, gamma, tweedie, gaussian"))

  }

  # ==================== xgb distribution choices ====================

  xgb_family_params <- list()

  if(is.null(objective)) {

    if (family == "poisson" | family == "quasipoisson") {

      xgb_family_params <- utils::modifyList(xgb_family_params, list(objective = "count:poisson"))

    } else if (family == "gamma") {

      xgb_family_params <- utils::modifyList(xgb_family_params, list(objective = "reg:gamma"))

    } else if (family == "tweedie") {

      xgb_family_params <- utils::modifyList(xgb_family_params, list(tweedie_variance_power = 1.5, objective = "reg:tweedie"))

    } else if (family == "gaussian") {

      xgb_family_params <- utils::modifyList(xgb_family_params, list(objective = "reg:squarederror"))

    } else {

      stop(paste0("family was ", family, " but should be one of: poisson, quasipoisson, gamma, tweedie, gaussian"))

    }

  } else {

    cli::cli_alert_info(
      "The 'objective' was defined in input and used over default settings"
    )

  }

  # ==================== GLM fitting ====================

  predictor_vars <- names(train$features)

  formula <- stats::as.formula(paste(
    response_var, "~",
    paste(predictor_vars, collapse = " + "),
    if(!is.null(offset_var)) {paste0("+ offset(", offset_var, ")")}
  ))

  glm_model <- stats::glm(
    formula,
    data = df_list[["train"]] |> dplyr::select(-dplyr::all_of(weight_var)),
    family = glm_family,
    weights = train$weights
  )

  # ==================== Preparing for XGB  ====================

  link <- glm_family$link

  glm_train_data <- train$features |>
    dplyr::bind_cols(
      df_list[["train"]] |> dplyr::select(dplyr::any_of(offset_var))
    )

  glm_validate_data <- validate$features |>
    dplyr::bind_cols(
      df_list[["validate"]] |> dplyr::select(dplyr::any_of(offset_var))
    )

  train$glm_preds <- unname(stats::predict(
    glm_model,
    newdata = glm_train_data,
    type = "link")
    )

  validate$glm_preds <- unname(stats::predict(
    glm_model,
    newdata = glm_validate_data,
    type = "link")
    )

  # if (!is.null(offset_var)) {
  #   train$glm_preds <- train$glm_preds + train$offset
  #   validate$glm_preds <- validate$glm_preds + validate$offset
  # }

  if (link == "log") {
    relationship <- "multiplicative"
  } else if (link == "identity") {
    relationship <- "additive"
  } else {
    stop(paste0("link function was ", link, " but should be one of: log, identity"))
  }


  train$xgb_matrix <- xgboost::xgb.DMatrix(train$features, label = train$responses, weight = train$weights)
  validate$xgb_matrix <- xgboost::xgb.DMatrix(validate$features, label = validate$responses, weight = validate$weights)

  xgboost::setinfo(train$xgb_matrix, "base_margin", train$glm_preds)
  xgboost::setinfo(validate$xgb_matrix, "base_margin", validate$glm_preds)

  # ==================== Fitting XGB  ====================

  xgb_additional_params <- c(
    list(
      nrounds = nrounds,
      objective = objective,
      custom_metric = custom_metric,
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
    evals = list(validation = validate$xgb_matrix)
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

  vartypes <- lapply(train$features, typeof) |> unlist()

  varisfactor <- lapply(train$features, is.factor) |> unlist()

  # create data objects that explain variables

  predictor_vars <- list()
  predictor_vars$all <- names(vartypes)
  predictor_vars$categorical <- predictor_vars$all[(!vartypes %in% c("integer", "double") | varisfactor)]
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
  iblm_model$weight_var <- weight_var
  iblm_model$offset_var <- offset_var
  iblm_model$predictor_vars <- predictor_vars
  iblm_model$cat_levels <- cat_levels
  iblm_model$coeff_names <- coeff_names

  # ==================== Add the parameters used on booster  ====================

  iblm_model$xgb_params <- drop_xgb_data_params(xgb_all_params)

  class(iblm_model) <- "iblm"

  return(iblm_model)
}
