
#' Create Scatter Plot of Beta Corrections for a Variable
#'
#' Generates a scatter plot or boxplot showing SHAP corrections for a specified variable from a fitted model.
#' For numerical variables, creates a scatter plot with optional coloring and marginal densities. For categorical
#' variables, creates a boxplot with model coefficients overlaid.
#'
#' @param varname Character. Name of the variable to plot SHAP corrections for.
#'   Must be present in the fitted model.
#' @param q Numeric. Quantile threshold for outlier removal. When 0 (default) the function will not remove any outliers
#' @param color Character or NULL. Name of variable to use for point coloring.
#'   Must be present in the model. Currently not supported for categorical variables.
#' @param marginal Logical. Whether to add marginal density plots (numerical variables only).
#' @param data_beta_coeff Dataframe, Contains the corrected beta coefficients for each row of the data
#' @param data Dataframe. The testing data.
#' @param iblm_model Object of class 'iblm'
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
#'
#' @keywords internal
#'
#' @import ggplot2
beta_corrected_scatter <- function(varname = "DrivAge",
                                   q = 0,
                                   color=NULL,
                                   marginal=FALSE,
                                   data_beta_coeff,
                                   data,
                                   iblm_model)  {

  check_iblm_model(iblm_model)

  predictor_vars_continuous <- iblm_model$predictor_vars$continuous
  predictor_vars_categorical <- iblm_model$predictor_vars$categorical

  glm_beta_coeff <- iblm_model$glm_model$coefficients


  vartype <- assign_variable_type(
    varname,
    predictor_vars_continuous,
    predictor_vars_categorical
  )

  if (is.null(color)) {
    color_vartype <- "NULL"
  } else {
    color_vartype <- assign_variable_type(
      color,
      predictor_vars_continuous,
      predictor_vars_categorical
    )
  }


  plot_data <- data |> dplyr::mutate(beta_coeff = data_beta_coeff[[varname]])

  if(vartype=="categorical"){

    cat_levels <- data[[varname]] |> unique() |> sort()
    data_beta_coeff_names <- paste0(varname, cat_levels)
    glm_beta_coeff_names <- names(glm_beta_coeff)
    plot_beta_coeff_names <- intersect(data_beta_coeff_names, glm_beta_coeff_names)
    reference_level <- sub(paste0("^", varname), "", setdiff(data_beta_coeff_names, plot_beta_coeff_names))

    beta_glm_coeff_df <- data.frame(
      x = sub(paste0("^", varname), "", plot_beta_coeff_names),
      y = as.numeric(glm_beta_coeff[plot_beta_coeff_names])
    ) |> stats::setNames(c(varname, "beta_coeff"))

    plot_data <- plot_data |> dplyr::filter(get(varname) != reference_level)

    if (!is.null(color)) {
      message("'color' argument not supported when vartype=='categorical' and will be ignored")
    }

    if (q>0) {
      message("'q' values other than 0 are not supported when vartype=='categorical' and will be ignored")
    }

    # Add the lines to the plot
    p <- ggplot(plot_data, aes(x = get(varname), y = .data$beta_coeff)) +
      geom_boxplot() +
      geom_point(
        data = beta_glm_coeff_df,
        color = "#4096C0",
      ) +
      labs(
        title = paste("Beta Coefficients after SHAP corrections for", varname),
        x = varname,
        y = "Beta Coefficients"
      )+
      theme_iblm()

  } else {

    if(q>0) {
      plot_data <- plot_data |>
        dplyr::filter(detect_outliers(.data$beta_coeff, method = "quantile",q=q))
    }

    stderror <- summary(iblm_model$glm_model)$coefficients[varname, "Std. Error"]
    beta <- glm_beta_coeff[varname]

    p  <- plot_data |>
      ggplot()+
      geom_point(
        aes(x = get(varname),
            y=.data$beta_coeff,
            group = if(is.null(color)) NULL else get(color),
            color=if(is.null(color)) NULL else get(color))
        ,alpha=0.4) +
      geom_smooth(aes(x = get(varname), y=.data$beta_coeff))+
      {if(color_vartype=="numerical") scale_color_gradientn(name = color,colors = iblm_colors[c(2,1)])}+
      {if(color_vartype=="categorical") scale_color_discrete(name = color)}+
      labs(
        title = paste("Beta Coefficients after SHAP corrections for", varname),
        subtitle = paste0(varname, " beta: ", round(beta, 4), ", SE: ", round(stderror, 4)),
        x = varname,
        y = "Beta Coefficients"
      )+
      geom_hline(yintercept = beta, color = "black", size = 0.5) +
      geom_hline(yintercept = beta - stderror, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_hline(yintercept = beta + stderror, linetype = "dashed", color = "black", linewidth = 0.5) +
      theme_iblm()

    if(marginal){
      p = ggExtra::ggMarginal(p,type = "density",groupColour = F, groupFill = F)
    }

  }

  return(p)

}
