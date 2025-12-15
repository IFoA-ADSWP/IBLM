#' Create Pre-Configured Beta Corrected Scatter Plot Function
#'
#' Factory function that returns a plotting function with data pre-configured.
#'
#' @param data_beta_coeff Dataframe. Contains the corrected beta coefficients
#'   for each row of the data.
#' @param data Dataframe. The testing data.
#' @param iblm_model Object of class 'iblm'.
#'
#' @return Function with signature \code{function(varname, q = 0, color = NULL, marginal = FALSE)}.
#'
#' @seealso [beta_corrected_scatter()]
#'
#' @examples
#' # ------- prepare iblm objects required -------
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' test_data <- df_list$test
#' shap <- extract_booster_shap(iblm_model$booster_model, test_data)
#' wide_input_frame <- data_to_onehot(test_data, iblm_model)
#' shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)
#' beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model)
#' data_glm <- data_beta_coeff_glm(test_data, iblm_model)
#' data_booster <- data_beta_coeff_booster(test_data, beta_corrections, iblm_model)
#' data_beta_coeff <- data_glm + data_booster
#'
#' # ------- demonstration of functionality -------
#'
#' # create_beta_corrected_scatter() can create function of type 'beta_corrected_scatter'
#' my_beta_corrected_scatter <- create_beta_corrected_scatter(data_beta_coeff, test_data, iblm_model)
#'
#' # this custom function then acts as per beta_corrected_scatter()
#' my_beta_corrected_scatter(varname = "VehAge")
#'
#' @export
create_beta_corrected_scatter <- function(data_beta_coeff,
                                          data,
                                          iblm_model) {
  function(varname,
           q = 0,
           color = NULL,
           marginal = FALSE) {
    beta_corrected_scatter_internal(
      varname = varname,
      q = q,
      color = color,
      marginal = marginal,
      data_beta_coeff = data_beta_coeff,
      data = data,
      iblm_model = iblm_model
    )
  }
}

#' Create Pre-Configured Beta Corrected Density Plot Function
#'
#' Factory function that returns a plotting function with data pre-configured.
#'
#' @param wide_input_frame Dataframe. Wide format input data (one-hot encoded).
#' @param beta_corrections Dataframe. Output from \code{\link{beta_corrections_derive}}.
#' @param data Dataframe. The testing data.
#' @param iblm_model Object of class 'iblm'.
#'
#' @return Function with signature \code{function(varname, q = 0.05, type = "kde")}.
#'
#' @seealso [beta_corrected_density()]
#'
#' @examples
#' # ------- prepare iblm objects required -------
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' test_data <- df_list$test
#' shap <- extract_booster_shap(iblm_model$booster_model, test_data)
#' wide_input_frame <- data_to_onehot(test_data, iblm_model)
#' shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)
#' beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model)
#'
#' # ------- demonstration of functionality -------
#'
#' # create_beta_corrected_density() can create function of type 'beta_corrected_density'
#' my_beta_corrected_density <- create_beta_corrected_density(
#'   wide_input_frame,
#'   beta_corrections,
#'   test_data,
#'   iblm_model
#' )
#'
#' # this custom function then acts as per beta_corrected_density()
#' my_beta_corrected_density(varname = "VehAge")
#'
#' @export
create_beta_corrected_density <- function(wide_input_frame,
                                          beta_corrections,
                                          data,
                                          iblm_model) {
  function(varname,
           q = 0.05,
           type = "kde") {
    beta_corrected_density_internal(
      varname = varname,
      q = q,
      type = type,
      wide_input_frame = wide_input_frame,
      beta_corrections = beta_corrections,
      data = data,
      iblm_model = iblm_model
    )
  }
}

#' Create Pre-Configured Bias Density Plot Function
#'
#' Factory function that returns a plotting function with data pre-configured.
#'
#' @param shap Dataframe. Contains raw SHAP values.
#' @param data Dataframe. The testing data.
#' @param iblm_model Object of class 'iblm'.
#' @param migrate_reference_to_bias TRUE/FALSE determines whether the shap
#' values of categorical reference levels be migrated to the bias?
#' Default is TRUE
#'
#' @return Function with signature \code{function(q = 0, type = "hist")}.
#'
#' @seealso [bias_density()]
#'
#' @examples
#' # ------- prepare iblm objects required -------
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' test_data <- df_list$test
#' shap <- extract_booster_shap(iblm_model$booster_model, test_data)
#'
#' # ------- demonstration of functionality -------
#'
#' # create_bias_density() can create function of type 'bias_density'
#' my_bias_density <- create_bias_density(shap, test_data, iblm_model)
#'
#' # this custom function then acts as per bias_density()
#' my_bias_density()
#'
#'
#' @export
create_bias_density <- function(shap,
                                data,
                                iblm_model,
                                migrate_reference_to_bias = TRUE) {
  function(q = 0,
           type = "hist") {
    bias_density_internal(
      q = q,
      type = type,
      migrate_reference_to_bias = migrate_reference_to_bias,
      shap = shap,
      data = data,
      iblm_model = iblm_model
    )
  }
}

#' Create Pre-Configured Overall Correction Plot Function
#'
#' Factory function that returns a plotting function with data pre-configured.
#'
#' @param shap Dataframe. Contains raw SHAP values.
#' @param iblm_model Object of class 'iblm'.
#'
#' @return Function with signature \code{function(transform_x_scale_by_link = TRUE)}.
#'
#' @seealso [overall_correction()]
#'
#' @examples
#' # ------- prepare iblm objects required -------
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' test_data <- df_list$test
#' shap <- extract_booster_shap(iblm_model$booster_model, test_data)
#'
#' # ------- demonstration of functionality -------
#'
#' # create_overall_correction() can create function of type 'overall_correction'
#' my_overall_correction <- create_overall_correction(shap, iblm_model)
#'
#' # this custom function then acts as per overall_correction()
#' my_overall_correction()
#'
#' @export
create_overall_correction <- function(shap,
                                      iblm_model) {
  function(transform_x_scale_by_link = TRUE) {
    overall_correction_internal(
      transform_x_scale_by_link = transform_x_scale_by_link,
      shap = shap,
      iblm_model = iblm_model
    )
  }
}






#' Scatter Plot of Beta Corrections for a Variable
#'
#' @description
#' Generates a scatter plot or boxplot showing SHAP corrections for a specified variable from a fitted model.
#' For numerical variables, creates a scatter plot with optional coloring and marginal densities. For categorical
#' variables, creates a boxplot with model coefficients overlaid.
#'
#' \strong{NOTE} This function signature documents the interface of functions created by \code{\link{create_beta_corrected_scatter}}.
#'
#' @param varname Character. Name of the variable to plot SHAP corrections for.
#'   Must be present in the fitted model.
#' @param q Numeric. Quantile threshold for outlier removal. When 0 (default) the function will not remove any outliers
#' @param color Character or NULL. Name of variable to use for point coloring.
#'   Must be present in the model. Currently not supported for categorical variables.
#' @param marginal Logical. Whether to add marginal density plots (numerical variables only).
#'
#' @return A ggplot2 object. For numerical variables: scatter plot with SHAP corrections,
#'   model coefficient line, and confidence bands. For categorical variables: boxplot
#'   with coefficient points overlaid.
#'
#' @details
#' The function handles both numerical and categorical variables differently:
#' \itemize{
#'   \item Numerical: Creates scatter plot of variable values vs. beta + SHAP deviations
#'   \item Categorical: Creates boxplot of SHAP deviations for each level with coefficient overlay
#' }
#'
#' For numerical variables, horizontal lines show the model coefficient (solid) and
#' confidence intervals (dashed). SHAP corrections represent local deviations from
#' the global model coefficient.
#'
#' @seealso \code{\link{create_beta_corrected_scatter}}, \code{\link{explain_iblm}}
#'
#' @export
#'
#' @examples
#' # This function is created inside explain_iblm() and is output as an item
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' explain_objects <- explain_iblm(iblm_model, df_list$test)
#'
#' # plot can be for a categorical variable (box plot)
#' explain_objects$beta_corrected_scatter(varname = "Area")
#'
#' # plot can be for a numerical variable (scatter plot)
#' explain_objects$beta_corrected_scatter(varname = "DrivAge")
#'
#'
#' # This function must be created, and cannot be called directly from the package
#' try(
#' beta_corrected_scatter(varname = "DrivAge")
#' )
beta_corrected_scatter <- function(varname, q = 0, color = NULL, marginal = FALSE) {
  cli::cli_abort(c(
    "This function documents the interface only and cannot be called directly. Instead, try one of the following",
    "i" = "Use explain_iblm()$beta_corrected_scatter()",
    "i" = "Call a function output from create_beta_corrected_scatter()"
  ))
}

#' Density Plot of Beta Corrections for a Variable
#'
#' @description
#' Generates a density plot showing the distribution of corrected Beta values
#' to a GLM coefficient, along with the original Beta coefficient, and standard error bounds around it.
#'
#' \strong{NOTE} This function signature documents the interface of functions created by \code{\link{create_beta_corrected_density}}.
#'
#' @param varname Character string specifying the variable name OR coefficient name is accepted as well.
#' @param q Number, must be between 0 and 0.5. Determines the quantile range of the plot (i.e. value of 0.05 will only show shaps within 5pct --> 95pct quantile range for plot)
#' @param type Character string, must be "kde" or "hist"
#'
#' @return ggplot object(s) showing the density distribution of corrected beta coefficients
#' with vertical lines indicating the original coefficient value and standard error bounds.
#'
#' The item returned will be:
#' \itemize{
#'   \item single ggplot object when `varname` was a numerical variable OR a coefficient name
#'   \item list of ggplot objects when `varname` was a categorical variable
#' }
#'
#' @details The plot shows:
#' \itemize{
#'   \item Density curve of corrected coefficient values
#'   \item Solid vertical line at the original GLM coefficient
#'   \item Dashed lines at plus/minus 1 standard error from the coefficient
#'   \item Automatic x-axis limits that cut off the highest and lowest q pct. If you want axis unaltered, set q = 0
#' }
#'
#' @seealso \code{\link{create_beta_corrected_density}}, \code{\link{explain_iblm}}
#'
#' @export
#'
#' @examples
#' # This function is created inside explain_iblm() and is output as an item
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' explain_objects <- explain_iblm(iblm_model, df_list$test)
#'
#' # plot can be for a single categorical level OR a categorical variable
#' explain_objects$beta_corrected_density(varname = "AreaB")
#'
#' # output can be numerical variable
#' explain_objects$beta_corrected_density(varname = "DrivAge")
#'
#'
#' # This function must be created, and cannot be called directly from the package
#' try(
#' beta_corrected_density(varname = "DrivAge")
#' )
beta_corrected_density <- function(varname, q = 0.05, type = "kde") {
  cli::cli_abort(c(
    "This function documents the interface only and cannot be called directly. Instead, try one of the following",
    "i" = "Use explain_iblm()$beta_corrected_density()",
    "i" = "Call a function output from create_beta_corrected_density()"
  ))
}


#' Density Plot of Bias Corrections from SHAP values
#'
#' @description
#' Visualizes the distribution of SHAP corrections that are migrated to bias terms,
#' showing both per-variable and total bias corrections.
#'
#' \strong{NOTE} This function signature documents the interface of functions created by \code{\link{create_bias_density}}.
#'
#' @param q Numeric value between 0 and 0.5 for quantile bounds. A higher number will trim more from the edges
#'  (useful if outliers are distorting your plot window) Default is 0 (i.e. no trimming)
#' @param type Character string specifying plot type: "kde" for kernel density or "hist" for histogram. Default is "hist".
#'
#' @return A list with two ggplot objects:
#' \itemize{
#'   \item \code{bias_correction_var}: Faceted plot showing bias correction density from each variable.
#'     Note that variables with no records contributing to bias correction are dropped from the plot.
#'   \item \code{bias_correction_total}: Plot showing total corrected bias density.
#' }
#'
#' @seealso \code{\link{create_bias_density}}, \code{\link{explain_iblm}}
#'
#' @export
#'
#' @examples
#' # This function is created inside explain_iblm() and is output as an item
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' explain_objects <- explain_iblm(iblm_model, df_list$test)
#'
#' explain_objects$bias_density()
#'
#'
#' # This function must be created, and cannot be called directly from the package
#' try(
#' bias_density()
#' )
bias_density <- function(q = 0, type = "hist") {
  cli::cli_abort(c(
    "This function documents the interface only and cannot be called directly. Instead, try one of the following",
    "i" = "Use explain_iblm()$bias_density()",
    "i" = "Call a function output from create_bias_density()"
  ))
}

#' Plot Overall Corrections from Booster Component
#'
#' @description
#' Creates a visualization showing for each record the overall booster component (either multiplicative or additive)
#'
#' \strong{NOTE} This function signature documents the interface of functions created by \code{\link{create_overall_correction}}.
#'
#' @param transform_x_scale_by_link TRUE/FALSE, whether to transform the x axis by the link function
#'
#' @return A ggplot2 object.
#'
#' @seealso \code{\link{create_overall_correction}}, \code{\link{explain_iblm}}
#'
#' @export
#'
#' @examples
#' # This function is created inside explain_iblm() and is output as an item
#'
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' explain_objects <- explain_iblm(iblm_model, df_list$test)
#'
#' explain_objects$overall_correction()
#'
#'
#' # This function must be created, and cannot be called directly from the package
#' try(
#' overall_correction()
#' )
overall_correction <- function(transform_x_scale_by_link = TRUE) {
  cli::cli_abort(c(
    "This function documents the interface only and cannot be called directly. Instead, try one of the following",
    "i" = "Use explain_iblm()$overall_correction()",
    "i" = "Call a function output from create_overall_correction()"
  ))
}
