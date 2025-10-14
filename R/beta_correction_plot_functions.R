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
                                   color = NULL,
                                   marginal = FALSE,
                                   data_beta_coeff,
                                   data,
                                   iblm_model) {

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

  if (vartype == "categorical") {
    cat_levels <- data[[varname]] |>
      unique() |>
      sort()
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

    if (q > 0) {
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
      ) +
      theme_iblm()
  } else {
    if (q > 0) {
      plot_data <- plot_data |>
        dplyr::filter(detect_outliers(.data$beta_coeff, method = "quantile", q = q))
    }

    stderror <- summary(iblm_model$glm_model)$coefficients[varname, "Std. Error"]
    beta <- glm_beta_coeff[varname]

    p <- plot_data |>
      ggplot() +
      geom_point(
        aes(
          x = get(varname),
          y = .data$beta_coeff,
          group = if (is.null(color)) NULL else get(color),
          color = if (is.null(color)) NULL else get(color)
        ),
        alpha = 0.4
      ) +
      geom_smooth(aes(x = get(varname), y = .data$beta_coeff)) +
      {
        if (color_vartype == "numerical") scale_color_gradientn(name = color, colors = iblm_colors[c(2, 1)])
      } +
      {
        if (color_vartype == "categorical") scale_color_discrete(name = color)
      } +
      labs(
        title = paste("Beta Coefficients after SHAP corrections for", varname),
        subtitle = paste0(varname, " beta: ", round(beta, 4), ", SE: ", round(stderror, 4)),
        x = varname,
        y = "Beta Coefficients"
      ) +
      geom_hline(yintercept = beta, color = "black", size = 0.5) +
      geom_hline(yintercept = beta - stderror, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_hline(yintercept = beta + stderror, linetype = "dashed", color = "black", linewidth = 0.5) +
      theme_iblm()

    if (marginal) {
      p <- ggExtra::ggMarginal(p, type = "density", groupColour = FALSE, groupFill = FALSE)
    }
  }

  return(p)
}






#' Create Density Plot of Corrected Beta values for a Variable
#'
#' Generates a density plot showing the distribution of corrected Beta values
#' to a GLM coefficient, along with the original Beta coefficient, and standard error bounds around it.
#'
#' @param varname Character string specifying the variable name OR coefficient name is accepted as well.
#' @param q Number, must be between 0 and 0.5. Determines the quantile range of the plot (i.e. value of 0.05 will only show shaps within 5pct --> 95pct quantile range for plot)
#' @param type Character string, must be "kde" or "hist"
#' @param wide_input_frame Wide format input data frame (one-hot encoded).
#' @param beta_corrections Dataframe. This can be output from [beta_corrections_derive]
#' @param data Dataframe. The testing data.
#' @param iblm_model Object of class 'iblm'
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
#' @keywords internal
#'
#' @import ggplot2
beta_corrected_density <- function(
    varname,
    q = 0.05,
    type = "kde",
    wide_input_frame,
    beta_corrections,
    data,
    iblm_model) {

  check_iblm_model(iblm_model)

  glm_beta_coeff <- iblm_model$glm_model$coefficient
  levels_all_cat <- iblm_model$cat_levels$all
  coef_names_reference_cat <- iblm_model$coeff_names$reference_cat
  x_glm_model <- iblm_model$glm_model
  predictor_vars_continuous <- iblm_model$predictor_vars$continuous
  predictor_vars_categorical <- iblm_model$predictor_vars$categorical

  stopifnot(is.numeric(q), q >= 0, q < 0.5)

  if (varname %in% predictor_vars_continuous) {
    vartype <- "numerical"
  } else if (varname %in% predictor_vars_categorical) {
    vartype <- "categorical"
  } else if (varname %in% coef_names_reference_cat) {
    stop("varname is reference level. Plot cannot be produced as no beta coefficient exists for this level")
  } else if (varname %in% names(glm_beta_coeff)) {
    vartype <- "categorical_level"
  } else {
    stop("varname not found in model!")
  }

  # if the variable is categorical, we will use recursion to plot each unique level and output a list instead...
  if (vartype %in% "categorical") {
    levels_to_plot <- paste0(varname, levels_all_cat[[varname]]) |> intersect(names(glm_beta_coeff))

    output <- purrr::map(
      levels_to_plot,
      ~ beta_corrected_density(
        varname = .x,
        q = q,
        type = type,
        wide_input_frame = wide_input_frame,
        beta_corrections = beta_corrections,
        data = data,
        iblm_model = iblm_model
      )
    ) |> stats::setNames(levels_to_plot)

    return(output)
  }

  # otherwise, we perform the code for a single plot...

  # if the variable is numerical, or if we are dealing with only one categorical_level, there is only 1 Beta value
  if (vartype %in% c("numerical", "categorical_level")) {
    stderror <- summary(x_glm_model)$coefficients[varname, "Std. Error"]
    beta <- glm_beta_coeff[varname]
    shap_deviations <- beta_corrections[, varname]
  }

  # remove policies that do not have the level that was specified via varname (only when varname is a variable-level combo)
  if (vartype == "categorical_level") {
    is_wanted_level <- wide_input_frame[, varname] == 1
    shap_deviations <- shap_deviations[is_wanted_level]
  }

  shap_quantiles <- beta + stats::quantile(shap_deviations, probs = c(q, 1 - q))
  lower_bound <- min(shap_quantiles[1], beta - stderror)
  upper_bound <- max(shap_quantiles[2], beta + stderror)

  if (type == "kde") {
    geom_corrections_density <- list(geom_density(color = iblm_colors[1], fill = iblm_colors[4], alpha = 0.3))
  } else if (type == "hist") {
    geom_corrections_density <- list(geom_histogram(color = iblm_colors[1], fill = iblm_colors[4], alpha = 0.3, bins = 100))
  } else {
    stop("type was not 'kde' or 'hist'")
  }

  data.frame(x = beta + shap_deviations) |>
    ggplot(aes(x = .data$x)) +
    geom_corrections_density +
    geom_vline(xintercept = beta, color = iblm_colors[2], linewidth = 0.5) +
    geom_vline(xintercept = beta - stderror, linetype = "dashed", color = iblm_colors[3], linewidth = 0.5) +
    geom_vline(xintercept = beta + stderror, linetype = "dashed", color = iblm_colors[3], linewidth = 0.5) +
    labs(
      title = paste("Beta density after SHAP corrections for", varname),
      subtitle = paste0(varname, " beta: ", round(beta, 3), ", SE: +/-", round(stderror, 4)),
    ) +
    xlab("Beta Coefficients") +
    xlim(lower_bound, upper_bound) +
    theme_iblm()
}



#' Generate Intercept Correction Analysis and Visualizations
#'
#' Analyzes how SHAP values correct the GLM intercept term, focusing on
#' observations where continuous variables are zero or categorical variables
#' are at their reference levels.
#'
#' @param shap Data frame containing raw SHAP values.
#' @param data Dataframe. The testing data.
#' @param iblm_model Object of class 'iblm'
#'
#' @return A list containing:
#' \describe{
#'   \item{overall_density}{ggplot object showing density of total intercept corrections}
#' }
#'
#' @details This function:
#' \itemize{
#'   \item Identifies observations at reference conditions (zeros/reference levels)
#'   \item Calculates intercept-specific SHAP corrections for these observations
#'   \item Creates visualizations showing the distribution of corrected intercept values
#'   \item Compares corrected values against the original GLM intercept and its standard error
#' }
#'
#'
#' @keywords internal
#'
#' @import ggplot2
shap_intercept <- function(shap,
                           data,
                           iblm_model) {

  check_iblm_model(iblm_model)

  levels_reference_cat <- iblm_model$cat_levels$reference
  x_glm_model <- iblm_model$glm_model
  predictor_vars_continuous <- iblm_model$predictor_vars$continuous
  response_var <- iblm_model$response_var
  no_cat_toggle <- length(iblm_model$predictor_vars$categorical) == 0


  beta_0 <- x_glm_model$coefficients["(Intercept)"] |> as.numeric()
  beta_0_SE <- summary(x_glm_model)$coefficients["(Intercept)", "Std. Error"]
  baseline <- shap$BIAS[1]

  if (no_cat_toggle) {
    shap_mask <- data |>
      dplyr::select(-dplyr::all_of(response_var)) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(predictor_vars_continuous), ~ as.integer(. == 0)))
  } else {
    shap_mask <- data |>
      dplyr::select(-dplyr::all_of(response_var)) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(predictor_vars_continuous), ~ as.integer(. == 0)),
        dplyr::across(
          dplyr::all_of(names(levels_reference_cat)),
          ~ as.integer(. == levels_reference_cat[dplyr::cur_column()])
        )
      )
  }

  intercept_shap <-
    (dplyr::select(shap, dplyr::all_of(names(shap_mask))) * shap_mask) |>
    dplyr::select(names(which(colSums(shap_mask) > 0))) |>
    (\(df) dplyr::filter(df, rowSums(df) != 0))() |>
    dplyr::select(-dplyr::any_of(predictor_vars_continuous))

  overall_density <- intercept_shap |>
    dplyr::mutate(total_correction = rowSums(dplyr::across(dplyr::everything())) + baseline + beta_0) |>
    ggplot(aes(x = .data$total_correction)) +
    geom_density() +
    geom_vline(xintercept = baseline + beta_0, color = iblm_colors[2], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = iblm_colors[1], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = iblm_colors[1], linewidth = 0.5) +
    ggtitle("Overall intercept correction distribution") +
    theme_iblm()

  intercept_shap_long <- intercept_shap |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "name", values_to = "value") |>
    dplyr::filter(.data$value != 0) |>
    dplyr::mutate(
      name = factor(.data$name, levels = names(sort(-colSums(intercept_shap != 0)))),
      value = .data$value + baseline + beta_0
    )

  grouped_density <- intercept_shap_long |>
    ggplot(aes(x = .data$value)) +
    geom_density() +
    facet_wrap(~name, scales = "free_y") +
    geom_vline(xintercept = baseline + beta_0, color = iblm_colors[2], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = iblm_colors[1], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = iblm_colors[1], linewidth = 0.5) +
    ggtitle("Individual intercept correction distributions") +
    xlab("") +
    ylab("") +
    theme_iblm()

  boxplot <- intercept_shap_long |>
    ggplot(aes(x = .data$name, y = .data$value)) +
    geom_boxplot() +
    geom_hline(yintercept = baseline + beta_0, color = iblm_colors[2], linewidth = 0.5) +
    ggtitle(paste0("Jitter chart of beta corrections for intercept"),
            subtitle = paste0("Intercept: ", round(beta_0, 2), " with shap baseline: ", round(baseline, 2))
    ) +
    xlab("") +
    ylab("") +
    theme_iblm()

  return(list(
    overall_density = overall_density,
    grouped_density = grouped_density,
    boxplot = boxplot
  ))
}

#' Generate Overall Corrections from Booster as Distribution Plot
#'
#' Creates a visualization showing for each record the overall booster component (either multiplicative or additive)
#'
#' @param transform_x_scale_by_link TRUE/FALSE, whether to transform the x axis by the link function
#' @param shap Data frame containing raw SHAP values.
#' @param iblm_model Object of class 'iblm'
#'
#' @return A ggplot object showing density of total booster values
#'
#' @keywords internal
#'
#' @import ggplot2
overall_correction <- function(transform_x_scale_by_link = TRUE, shap, iblm_model) {

  check_iblm_model(iblm_model)

  family <- iblm_model$glm_model$family
  relationship <- iblm_model$relationship

  dt <- shap |>
    dplyr::mutate(
      total = rowSums(dplyr::across(dplyr::everything())),
      total_invlink = family$linkinv(.data$total)
    )

  out_the_box_transformations <- c("asn", "atanh", "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", "reverse", "sqrt", "time")


  if (!transform_x_scale_by_link | family$link == "identity") {
    scale_x_link <- list()
  } else if (family$link %in% out_the_box_transformations) {
    scale_x_link <- list(
      labs(caption = paste0("**Please note scale is tranformed by ", family$link, " function")),
      scale_x_continuous(transform = family$link)
    )
  } else {
    scale_x_link <- list(
      labs(caption = paste0("**Please note scale is tranformed by ", family$link, " function")),
      scale_x_continuous(transform = scales::new_transform(
        "link",
        transform = family$linkfun,
        inverse = family$linkinv
      ))
    )
  }

  dt |>
    ggplot(aes(x = .data$total_invlink)) +
    geom_density() +
    geom_vline(xintercept = family$linkinv(0)) +
    theme_iblm() +
    scale_x_link +
    labs(
      title = paste0("Distribution of ", relationship, " corrections to GLM prediction"),
      subtitle = paste0("mean correction: ", round(mean(dt$total_invlink), 3)),
      x = paste0(relationship, " correction") |> tools::toTitleCase()
    )
}
