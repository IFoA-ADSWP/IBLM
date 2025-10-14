#' Check IBLM Ensemble Model
#'
#' Validates an iblm model object has required structure and features
#'
#' @param model Model object to validate, expected class "iblm"
#'
#' @return Invisible TRUE if all checks pass
#'
#' @examples
#' df_list <- freMTPL2freq |> head(10000) |> split_into_train_validate_test()
#'
#' iblm_model <- train_iblm(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' check_iblm_model(iblm_model)
#'
#' @export
check_iblm_model <- function(model) {
  # Check model class
  if (!"iblm" %in% class(model)) {
    cli::cli_abort(c(
      "x" = "Model must be of class {.cls iblm}",
      "i" = "Got class: {.cls {class(model)}}"
    ))
  }

  # Check relationship attribute exists
  if (!"relationship" %in% names(model)) {
    cli::cli_abort(c(
      "x" = "Model missing required {.field relationship} attribute"
    ))
  }

  # Check relationship value
  rel <- model["relationship"]
  if (!rel %in% c("additive", "multiplicative")) {
    cli::cli_abort(c(
      "x" = "Invalid relationship type: {.val {rel}}",
      "i" = "Must be {.val additive} or {.val multiplicative}"
    ))
  }

  # Check required components exist
  check_required_names(model, c("glm_model", "booster_model"))

  # Additional checks
  if (!inherits(model$glm_model, "glm")) {
    cli::cli_abort(c(
      "x" = "{.field glm_model} must be of class {.cls glm}"
    ))
  }

  if (!("xgb.Booster" %in% class(model$booster_model))) {
    cli::cli_abort(c(
      "x" = "{.field booster_model} must be of class {.cls xgb.Booster}"
    ))
  }

  # Check GLM family link
  link <- model$glm_model$family$link
  if (!link %in% c("log", "identity")) {
    cli::cli_abort(c(
      "x" = "Invalid GLM link function: {.val {link}}",
      "i" = "Must be {.val log} or {.val identity}"
    ))
  }

  invisible(TRUE)
}
