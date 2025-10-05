#' Check IBLM Ensemble Model
#'
#' Validates an ensemble model object has required structure and attributes.
#'
#' @param model Model object to validate, expected class "ens"
#'
#' @return Invisible TRUE if all checks pass
#'
#' @export
check_iblm_model <- function(model) {
  # Check model class
  if (!"ens" %in% class(model)) {
    cli::cli_abort(c(
      "x" = "Model must be of class {.cls ens}",
      "i" = "Got class: {.cls {class(model)}}"
    ))
  }

  # Check relationship attribute exists
  if (!"relationship" %in% names(attributes(model))) {
    cli::cli_abort(c(
      "x" = "Model missing required {.field relationship} attribute"
    ))
  }

  # Check relationship value
  rel <- attr(model, "relationship")
  if (!rel %in% c("additive", "multiplicative")) {
    cli::cli_abort(c(
      "x" = "Invalid relationship type: {.val {rel}}",
      "i" = "Must be {.val additive} or {.val multiplicative}"
    ))
  }

  # Check required components exist
  check_required_names(model, c("glm_model", "xgb_model"))

  # Additional checks
  if (!inherits(model$glm_model, "glm")) {
    cli::cli_abort(c(
      "x" = "{.field glm_model} must be of class {.cls glm}"
    ))
  }

  if (!("xgb.Booster" %in% class(model$xgb_model))) {
    cli::cli_abort(c(
      "x" = "{.field xgb_model} must be of class {.cls xgb.Booster}"
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
