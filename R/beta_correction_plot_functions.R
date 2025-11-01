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
#' @noRd
#'
#' @import ggplot2
beta_corrected_scatter_internal <- function(varname,
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
      cli::cli_inform(c(
        "!" = "{.var color} argument not supported when {.var vartype} == 'categorical' and will be ignored."
      ))
    }

    # Warn if q > 0
    if (q > 0) {
      cli::cli_inform(c(
        "!" = "{.var q} values other than 0 are not supported when {.var vartype} == 'categorical' and will be ignored."
      ))
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
        dplyr::filter(detect_outliers(.data$beta_coeff, q = q))
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
        subtitle = paste0(varname, " beta: ", round(beta, 3), ", SE: +/-", round(stderror, 4)),
        x = varname,
        y = "Beta Coefficients"
      ) +
      geom_hline(yintercept = beta, color = "black", linewidth = 0.5) +
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
#' @noRd
#'
#' @import ggplot2
beta_corrected_density_internal <- function(
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

  # Validate q
  if (!is.numeric(q) || q < 0 || q >= 0.5) {
    cli::cli_abort(c(
      "!" = "Invalid value for {.var q}.",
      "x" = "{.var q} must be numeric and satisfy 0 <= q < 0.5",
      "i" = paste("You provided:", "q =", q)
    ))
  }

  # Determine variable type
  if (varname %in% predictor_vars_continuous) {
    vartype <- "numerical"
  } else if (varname %in% predictor_vars_categorical) {
    vartype <- "categorical"
  } else if (varname %in% coef_names_reference_cat) {
    cli::cli_abort(c(
      "!" = "{.var varname} is a reference level.",
      "x" = "Plot cannot be produced because no beta coefficient exists for this level."
    ))
  } else if (varname %in% names(glm_beta_coeff)) {
    vartype <- "categorical_level"
  } else {
    cli::cli_abort(c(
      "!" = "{.var varname} not found in model!",
      "i" = "Check that {.var varname} matches one of the model predictors or coefficients."
    ))
  }



  # if the variable is categorical, we will use recursion to plot each unique level and output a list instead...
  if (vartype %in% "categorical") {
    levels_to_plot <- paste0(varname, levels_all_cat[[varname]]) |> intersect(names(glm_beta_coeff))

    output <- purrr::map(
      levels_to_plot,
      ~ beta_corrected_density_internal(
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
    geom_corrections_density <- list(
      geom_density(color = iblm_colors[1], fill = iblm_colors[4], alpha = 0.3, na.rm = TRUE)
      )
  } else if (type == "hist") {
    geom_corrections_density <- list(
      geom_histogram(color = iblm_colors[1], fill = iblm_colors[4], alpha = 0.3, bins = 100, na.rm = TRUE)
      )
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




#' Plot density of bias corrections from SHAP values
#'
#' Visualizes the distribution of SHAP corrections that are migrated to bias terms,
#' showing both per-variable and total bias corrections.
#'
#' @param q Numeric value between 0 and 0.5 for quantile bounds. A higher number will trim more from the edges
#'  (useful if outliers are distorting your plot window) Default is 0 (i.e. no trimming)
#' @param type Character string specifying plot type: "kde" for kernel density or "hist" for histogram. Default is "hist".
#' @param migrate_reference_to_bias Logical, migrate the beta corrections to the bias for reference levels?
#' This applied to categorical vars only. It is recommended to leave this setting on TRUE
#' @param shap Data frame containing raw SHAP values.
#' @param data Dataframe. The testing data.
#' @param iblm_model Object of class 'iblm'
#'
#' @return List with two ggplot objects:
#' \item{bias_correction_var}{Faceted plot showing bias correction density from each variable.
#' Note that variables with no records contributing to bias correction are dropped from the plot}
#' \item{bias_correction_total}{Plot showing total corrected total bias density}
#'
#' @keywords internal
#' @noRd
#'
#' @import ggplot2
bias_density_internal <- function(q = 0,
                           type = "hist",
                          migrate_reference_to_bias = TRUE,
                           shap,
                           data,
                           iblm_model) {



  # --------------- checks ------------

  stopifnot(is.numeric(q), q >= 0, q < 0.5)

  predictor_vars_continuous <- iblm_model$predictor_vars$continuous
  predictor_vars_categorical <- iblm_model$predictor_vars$categorical
  reference_levels <- iblm_model$cat_levels$reference
  reference_cat <- iblm_model$coeff_names$reference_cat


  # --------------- bias_correction_df ------------

  bias_correction_continuous <- predictor_vars_continuous |>
    purrr::map(
      .f = function(var) {
        is_zero <- data[[var]] == 0
        bias_correction <- shap[is_zero, ][[var]]
        row_id <- (1:nrow(data))[is_zero]
        if(length(bias_correction) == 0) { return(NULL)}
        data.frame(
          row_id = row_id,
          var = var,
          bias_correction = bias_correction
        )
      }
    ) |>
    dplyr::bind_rows()


  bias_correction_categorical <- predictor_vars_categorical |>
    purrr::map(
      .f = function(var) {
        ref <- reference_levels[[var]]
        is_ref <- data[[var]] == ref
        bias_correction <- shap[is_ref, ][[var]]
        row_id <- (1:nrow(data))[is_ref]
        if(length(bias_correction) == 0) { return(NULL)}
        data.frame(
          row_id = row_id,
          var = var,
          bias_correction = bias_correction
        )
      }
    ) |>
    dplyr::bind_rows()

  if (type == "kde") {
    geom_corrections_density <- list(
      geom_density(color = "grey70", fill = "grey70", alpha = 0.3, na.rm = TRUE)
      )
  } else if (type == "hist") {
    geom_corrections_density <- list(
      geom_histogram(color = "grey70", fill = "grey70", alpha = 0.3, bins = 100, na.rm = TRUE)
      )
  } else {
    stop("type was not 'kde' or 'hist'")
  }

  if(migrate_reference_to_bias) {

    bias_correction_var_df <- rbind(bias_correction_continuous, bias_correction_categorical)

  } else {

    bias_correction_var_df <- bias_correction_continuous

  }

  remaining_vars <- bias_correction_var_df$var |> unique()



  # --------- if no plot bias correction occurs, exit early ------------


  if(nrow(bias_correction_var_df) == 0) {

    cli::cli_alert_info("No bias migration within dataset when calling bias_density()")

    return(
      list(
        bias_correction_var = NULL,
        bias_correction_total = NULL
      )
    )

  }


  # --------- plot bias correction by var ------------

  stderror <- summary(iblm_model$glm_model)$coefficients[predictor_vars_continuous, "Std. Error"]

  stderror_df <- data.frame(
    var = predictor_vars_continuous,
    stderror_plus = stderror,
    stderror_minus = -stderror
  ) |>
    dplyr::filter(.data$var %in% remaining_vars)

  shap_quantiles <-  stats::quantile(bias_correction_var_df$bias_correction, probs = c(q, 1 - q))
  lower_bound <- min(shap_quantiles[1], stderror_df$stderror_minus)
  upper_bound <- max(shap_quantiles[2], stderror_df$stderror_plus)

  bias_correction_var <-
    bias_correction_var_df |>
    ggplot(aes(x=.data$bias_correction)) +
    geom_corrections_density +
    geom_vline(
      data = stderror_df,
      mapping = aes(xintercept = .data$stderror_plus),
      linetype = "dashed",
      color = iblm_colors[2],
      linewidth = 0.5) +
    geom_vline(
      data = stderror_df,
      mapping = aes(xintercept = .data$stderror_minus),
      linetype = "dashed",
      color = iblm_colors[2],
      linewidth = 0.5) +
    labs(
      title = paste("Density for SHAP corrections that are migrated to bias")
    ) +
    xlab("Bias Value Corrections") +
    ylab("Count") +
    xlim(lower_bound, upper_bound) +
    facet_wrap(vars(.data$var), scales = "free_y") +
    theme_iblm()


  # --------- plot bias correction by id ------------

  stderror_bias <- summary(iblm_model$glm_model)$coefficients["(Intercept)", "Std. Error"]
  estimate_bias <- summary(iblm_model$glm_model)$coefficients["(Intercept)", "Estimate"]

  bias_correction_total_df <-
    bias_correction_var_df |>
    dplyr::group_by(.data$row_id) |>
    dplyr::summarise(bias_correction = sum(.data$bias_correction)) |>
    dplyr::ungroup() |>
    dplyr::mutate(bias_correction = .data$bias_correction + estimate_bias)

  bias_quantiles <-  stats::quantile(bias_correction_total_df$bias_correction, probs = c(q, 1 - q))
  lower_bound_bias <- min(bias_quantiles[1], estimate_bias - stderror_bias)
  upper_bound_bias <- max(bias_quantiles[2], estimate_bias + stderror_bias)

  bias_correction_total <-
    bias_correction_total_df |>
    ggplot(aes(x=.data$bias_correction)) +
    geom_corrections_density +
    geom_vline(
      xintercept = estimate_bias + stderror_bias,
      linetype = "dashed",
      color = iblm_colors[2],
      linewidth = 0.5
    ) +
    geom_vline(
      xintercept = estimate_bias - stderror_bias,
      linetype = "dashed",
      color = iblm_colors[2],
      linewidth = 0.5
    ) +
    geom_vline(
      xintercept = estimate_bias,
      linetype = "solid",
      color = iblm_colors[3],
      linewidth = 0.5
    ) +
    labs(
      title = paste("Density for corrected bias values"),
      subtitle = paste0("bias: ", round(estimate_bias, 3), ", SE: +/-", round(stderror_bias, 4)),
    ) +
    xlab("Bias Values") +
    ylab("Count") +
    xlim(lower_bound_bias, upper_bound_bias) +
    theme_iblm()


output <- list(
  bias_correction_var = bias_correction_var,
  bias_correction_total = bias_correction_total
)

return(output)

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
#' @noRd
#'
#' @import ggplot2
overall_correction_internal <- function(transform_x_scale_by_link = TRUE, shap, iblm_model) {

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
