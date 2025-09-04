#' Explain GLM Model Predictions Using SHAP Values
#'
#' Creates an explainer object that generates SHAP (SHapley Additive exPlanations)
#' values for GLM model predictions, providing various visualization and analysis
#' functions to understand model behavior and feature contributions.
#'
#' @param x A list containing both a GLM model (`glm_model`) and an XGBoost model
#'   (`xgb_model`) fitted on the same data. The XGBoost model is used to generate
#'   SHAP values that correct the GLM predictions.
#' @param d A data frame containing the input data for which explanations are desired.
#'   Should contain the same variables as used in model training.
#' @param as_contribution Logical, currently unused parameter for future functionality.
#'
#' @return A list containing:
#' \describe{
#'   \item{shap_correction_scatter}{Function to create scatter plots showing SHAP corrections vs variable values}
#'   \item{shap_correction_density}{Function to create density plots of SHAP corrections for variables}
#'   \item{shap_intercept}{List containing intercept correction visualizations}
#'   \item{overall_correction}{Function to show global correction distributions}
#'   \item{input_frame}{Original input data frame}
#'   \item{shap_wide}{Wide format SHAP corrections data frame}
#'   \item{raw_shap}{Raw SHAP values from XGBoost}
#'   \item{betas}{GLM model coefficients}
#'   \item{allnames}{Names of all model coefficients except intercept}
#' }
#'
#' @details The function processes both continuous and categorical variables, handles
#' reference levels for factors, and applies corrections to account for zero values
#' and categorical reference categories. The resulting explainer provides multiple
#' visualization methods to understand how SHAP values modify GLM predictions.
#'
#' @examples
#' \dontrun{
#' # Assuming you have fitted both GLM and XGBoost models
#' models <- list(glm_model = my_glm, xgb_model = my_xgb)
#' explainer <- explain(models, test_data)
#'
#' # Generate scatter plot for a variable
#' explainer$shap_correction_scatter("age")
#'
#' # Show density of corrections
#' explainer$shap_correction_density("income")
#' }
#'
#' @export
explain <- function(x, d, as_contribution = FALSE){
  rownames(d) <- NULL

  # Definitions and global variables
  betas <- x$glm_model$coefficients
  coef_names <- names(betas)

  vartypes <- lapply(x$glm_model$data, typeof) |> unlist()

  # Factor levels for categorical variables
  cat_levels <- lapply(
    x$glm_model$data[, !(vartypes %in% c("integer", "double"))],
    function(x) sort(unique(x))
  )

  cat_unique_names <- lapply(
    names(cat_levels),
    function(x) paste0(x, cat_levels[[x]])
  ) |> unlist()

  all_names <- c(
    "(Intercept)",
    names(vartypes[vartypes %in% c("integer","double")]),
    cat_unique_names
  )

  target <- all.vars(x$glm_model$formula)[1]

  reference_levels_raw <- sapply(
    names(cat_levels),
    function(var) {
      all_levels <- cat_levels[[var]]
      present_levels <- coef_names[startsWith(coef_names, var)]
      present_levels_clean <- gsub(paste0("^", var), "", present_levels)
      setdiff(all_levels, present_levels_clean)
    },
    USE.NAMES = TRUE
  )

  reference_levels <- setdiff(all_names, c(names(betas), target))
  cont_vars <- names(which(vartypes != "character" & names(vartypes) != target))
  no_cat_toggle <- (length(cat_levels) == 0)

  custom_colors <- c("#113458", "#D9AB16", "#4096C0", "#DCDCD9", "#113458","#2166AC", "#FFFFFF", "#B2182B")
  chart_theme <- chart_theme_fn(custom_colors)

  # Generate SHAP values
  shap <- predict(
    x$xgb_model,
    newdata = xgboost::xgb.DMatrix(
      data.matrix(
        dplyr::select(d, -dplyr::all_of(target))
      )
    ),
    predcontrib = TRUE
  ) |> data.frame()

  # Prepare wide input frame
  wide_input_frame <- data_dim_helper(
    frame = d,
    all_names = all_names,
    cat_levels = cat_levels,
    target = target,
    no_cat_toggle = no_cat_toggle
  )

  # Apply SHAP dimension helper
  shap_wide <- shap_dim_helper(
    frame = shap,
    wide_frame = wide_input_frame,
    vartypes = vartypes,
    reference_levels = reference_levels,
    cat_levels = cat_levels,
    target = target,
    no_cat_toggle = no_cat_toggle
  )

  # Return explainer object with plotting functions
  list(
    shap_correction_scatter = function(...) shap_correction_scatter(
      ..., betas = betas, vartypes = vartypes,
      cat_levels = cat_levels, wide_input_frame = wide_input_frame,
      shap_wide = shap_wide, d = d, target = target,
      reference_levels = reference_levels, custom_colors = custom_colors,
      chart_theme = chart_theme
    ),

    shap_correction_density = function(...) shap_correction_density(
      ..., betas = betas, vartypes = vartypes,
      wide_input_frame = wide_input_frame, shap_wide = shap_wide,
      x_glm_model = x$glm_model, d = d,
      custom_colors = custom_colors, chart_theme = chart_theme
    ),

    shap_intercept = shap_intercept(
      shp = shap, x_glm_model = x$glm_model, d = d,
      target = target, cont_vars = cont_vars,
      reference_levels_raw = reference_levels_raw,
      no_cat_toggle = no_cat_toggle,
      custom_colors = custom_colors, chart_theme = chart_theme
    ),

    overall_correction = function() global_c(
      shp = shap, custom_colors = custom_colors,
      chart_theme = chart_theme
    ),

    input_frame = d,
    shap_wide = shap_wide,  # beta corrections
    raw_shap = shap,
    betas = betas,
    allnames = names(betas)[-1]
  )
}






# ========================= Helper functions for `explain` ========================

#' Create Custom ggplot2 Theme for SHAP Visualizations
#'
#' Generates a custom ggplot2 theme with specific color scheme and styling
#' optimized for SHAP explanation plots.
#'
#' @param custom_colors A character vector of 8 hex color codes used throughout
#'   the plotting functions for consistent theming.
#'
#' @return A ggplot2 theme object that can be added to plots.
#'
#' @details The theme applies minimal styling with custom colors for titles,
#' subtitles, and grid lines to maintain visual consistency across all
#' SHAP explanation plots.
#'
#' @keywords internal
chart_theme_fn <- function(custom_colors) {
  theme_minimal() +
    theme(
      plot.title = element_text(color = custom_colors[5], face = "bold", size = 14),
      plot.subtitle = element_text(color = custom_colors[2], size = 12),
      panel.grid.major = element_line(color = custom_colors[4], linewidth = 0.3),
      panel.grid.minor = element_line(color = custom_colors[4], linewidth = 0.2)
    )
}

#' Convert Data Frame to Wide One-Hot Encoded Format
#'
#' Transforms categorical variables in a data frame into one-hot encoded format
#' and prepares the data structure needed for SHAP calculations.
#'
#' @param frame Input data frame to be transformed.
#' @param all_names Character vector of all expected variable names including
#'   categorical levels and intercept.
#' @param cat_levels Named list where each element contains the unique levels
#'   for each categorical variable.
#' @param target Character string specifying the name of the target variable.
#' @param no_cat_toggle Logical indicating whether there are any categorical
#'   variables in the data.
#' @param remove_target Logical, whether to remove the target variable from
#'   the output (default TRUE).
#'
#' @return A data frame in wide format with one-hot encoded categorical variables,
#' an intercept column, and all variables ordered according to `all_names`.
#'
#' @details For datasets with categorical variables, this function creates
#' dummy variables for all levels (including reference levels) and ensures
#' proper ordering for downstream SHAP calculations.
#'
#' @keywords internal
data_dim_helper <- function(frame, all_names, cat_levels, target, no_cat_toggle, remove_target = TRUE) {
  if (no_cat_toggle) {
    return(frame)
  }

  main_frame <- data.frame(matrix(0, nrow = nrow(frame), ncol = length(all_names))) |>
    setNames(all_names)

  df_onehot <- frame |>
    fastDummies::dummy_cols(
      select_columns = names(cat_levels),
      remove_first_dummy = FALSE,
      remove_selected_columns = TRUE
    ) |>
    dplyr::rename_with(~ gsub("_", "", .x))

  output_frame <- cbind(
    df_onehot,
    main_frame[, setdiff(all_names, colnames(df_onehot))]
  ) |>
    dplyr::mutate("(Intercept)" = 1) |>
    dplyr::select(dplyr::all_of(all_names))

  if (remove_target) {
    output_frame <- output_frame |> dplyr::select(-dplyr::any_of(target))
  }

  return(output_frame)
}

#' Compute SHAP Value Corrections for GLM Coefficients
#'
#' Processes raw SHAP values to create coefficient corrections that account for
#' zero values in continuous variables and reference levels in categorical variables.
#'
#' @param frame Data frame containing raw SHAP values from XGBoost.
#' @param wide_frame Wide format input data frame (one-hot encoded).
#' @param vartypes Named vector indicating the data type of each variable.
#' @param reference_levels Character vector of reference level names for categorical variables.
#' @param cat_levels Named list of categorical variable levels.
#' @param target Character string specifying the target variable name.
#' @param no_cat_toggle Logical indicating absence of categorical variables.
#' @param beta_correction Logical, whether to apply beta corrections (default TRUE).
#' @param epsilon Numeric threshold for avoiding division by very small numbers (default 0.05).
#'
#' @return A data frame with corrected SHAP values where:
#' \itemize{
#'   \item Categorical variables are properly aggregated by factor level
#'   \item Continuous variables are normalized by their actual values
#'   \item Reference level and zero-value corrections are added to the bias term
#' }
#'
#' @details This function performs several key corrections:
#' 1. Redistributes SHAP values for categorical variables across their levels
#' 2. Adjusts for zero values in continuous variables
#' 3. Accounts for reference category effects
#' 4. Normalizes SHAP values by actual variable values for interpretability
#'
#' @keywords internal
shap_dim_helper <- function(frame, wide_frame, vartypes, reference_levels, cat_levels,
                            target, no_cat_toggle, beta_correction = TRUE, epsilon = 0.05) {
  if (no_cat_toggle) {
    output_frame <- frame |>
      dplyr::mutate(bias = frame$BIAS[1], .before = dplyr::everything())
  } else {
    wide_frame <- wide_frame |> dplyr::select(-dplyr::any_of(c("(Intercept)", target)))

    cat_frame <- lapply(names(cat_levels), function(x) {
      lvl <- cat_levels[[x]]
      mask <- wide_frame |>
        dplyr::select(dplyr::all_of(paste0(x, lvl))) |>
        data.matrix()
      matrix(rep(frame[, x], length(lvl)), byrow = FALSE, ncol = length(lvl)) * mask
    }) |>
      dplyr::bind_cols()

    output_frame <- cbind(
      frame[, setdiff(colnames(wide_frame), colnames(cat_frame))],
      cat_frame
    ) |>
      dplyr::select(colnames(wide_frame)) |>
      dplyr::mutate(bias = frame$BIAS[1], .before = dplyr::everything())
  }

  if (beta_correction) {
    nums <- names(vartypes[vartypes %in% c("integer", "double")])
    nums <- nums[!(nums %in% target)]

    shap_for_zeros <- rowSums(((wide_frame[, nums] == 0) * 1) * output_frame[, nums])
    shap_for_cat_ref <- if (no_cat_toggle) 0 else rowSums(output_frame[, reference_levels])
    output_frame$bias <- output_frame$bias + shap_for_zeros + shap_for_cat_ref

    denominator <- frame[, nums]
    denominator[abs(denominator) < epsilon] <- epsilon
    calc <- output_frame[, nums] / frame[, nums]
    calc[apply(calc, 2, is.infinite)] <- 0
    output_frame[, nums] <- calc
  }
  return(output_frame)
}

#' Create Density Plot of SHAP Corrections for a Variable
#'
#' Generates a density plot showing the distribution of SHAP-based corrections
#' to a GLM coefficient, along with the original coefficient and standard error bounds.
#'
#' @param varname Character string specifying the variable name to plot.
#' @param betas Named numeric vector of GLM coefficients.
#' @param vartypes Named vector indicating data types of variables.
#' @param wide_input_frame Wide format input data frame.
#' @param shap_wide Data frame containing SHAP corrections.
#' @param x_glm_model The fitted GLM model object.
#' @param d Original input data frame.
#' @param custom_colors Character vector of hex colors for plot styling.
#' @param chart_theme ggplot2 theme object for consistent plot appearance.
#'
#' @return A ggplot object showing the density distribution of SHAP corrections
#' with vertical lines indicating the original coefficient value and standard error bounds.
#'
#' @details The plot shows:
#' \itemize{
#'   \item Density curve of corrected coefficient values
#'   \item Solid vertical line at the original GLM coefficient
#'   \item Dashed lines at ±1 standard error from the coefficient
#'   \item Automatic x-axis limits based on SHAP quantiles and standard errors
#' }
#'
#' @keywords internal
shap_correction_density <- function(varname, betas, vartypes, wide_input_frame, shap_wide,
                                    x_glm_model, d, custom_colors, chart_theme) {
  if (!(varname %in% c(names(betas), colnames(d)))) stop("varname not in model!")

  vartype <- if (varname %in% names(betas) & vartypes[varname] %in% c("double", "integer")) "numerical" else "categorical"
  stderror <- summary(x_glm_model)$coefficients[varname, "Std. Error"]
  beta <- betas[varname]

  shap_deviations <- shap_wide[, varname]
  from_shap <- beta + stats::quantile(shap_deviations, probs = c(0.05, 0.95))
  lower_bound <- min(from_shap[1], beta - stderror)
  upper_bound <- max(from_shap[2], beta + stderror)

  data.frame(x = beta + shap_deviations) |>
    ggplot(aes(x = x)) +
    geom_density(color = custom_colors[1], fill = custom_colors[4], alpha = 0.3) +
    geom_vline(xintercept = beta, color = custom_colors[2], linewidth = 0.5) +
    geom_vline(xintercept = beta - stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
    geom_vline(xintercept = beta + stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
    ggtitle(paste("SHAP correction density for", varname)) +
    labs(subtitle = paste0(varname, " beta: ", round(beta, 3), ", SE: ", round(stderror, 3))) +
    xlim(lower_bound, upper_bound) +
    chart_theme
}

#' Create Scatter Plot of SHAP Corrections vs Variable Values
#'
#' Generates a scatter plot showing how SHAP-based coefficient corrections
#' vary with the actual values of a variable, revealing non-linear relationships.
#'
#' @param varname Character string specifying the variable name to plot.
#' @param betas Named numeric vector of GLM coefficients.
#' @param vartypes Named vector indicating data types of variables.
#' @param cat_levels Named list of categorical variable levels.
#' @param wide_input_frame Wide format input data frame.
#' @param shap_wide Data frame containing SHAP corrections.
#' @param d Original input data frame.
#' @param target Character string specifying target variable name.
#' @param reference_levels Character vector of reference level names.
#' @param custom_colors Character vector of hex colors for plot styling.
#' @param chart_theme ggplot2 theme object for consistent plot appearance.
#' @param color Optional variable name for coloring points (currently unused).
#' @param marginal Logical, whether to add marginal density plots (default FALSE).
#' @param excl_outliers Logical, whether to exclude outliers (currently unused).
#'
#' @return A ggplot object (potentially with marginal plots) showing:
#' \itemize{
#'   \item Scatter points of variable values vs corrected coefficients
#'   \item Smooth trend line through the points
#'   \item Horizontal reference lines for original coefficient and standard error bounds
#' }
#'
#' @details This visualization helps identify:
#' \itemize{
#'   \item Whether the linear assumption of GLM holds across the variable's range
#'   \item Regions where the coefficient correction is most pronounced
#'   \item Potential interaction effects or non-linear relationships
#' }
#'
#' @keywords internal
shap_correction_scatter <- function(varname,
                                    betas,
                                    vartypes,
                                    cat_levels,
                                    wide_input_frame,
                                    shap_wide,
                                    d,
                                    target,
                                    reference_levels,
                                    custom_colors,
                                    chart_theme,
                                    color = NULL,
                                    marginal = FALSE,
                                    excl_outliers = FALSE) {
  stderror <- summary(betas)$coefficients[varname, "Std. Error"]
  beta <- betas[varname]

  after_shap <- data.frame(
    x = wide_input_frame[, varname],
    shp = beta + shap_wide[, varname]
  )

  p <- after_shap |>
    ggplot() +
    geom_point(aes(x = x, y = shp), alpha = 0.4) +
    geom_smooth(aes(x = x, y = shp)) +
    xlab(varname) +
    ylab("beta correction") +
    ggtitle(paste("SHAP corrections for", varname)) +
    labs(subtitle = paste0(varname, " beta: ", round(beta, 4), ", SE: ", round(stderror, 4))) +
    geom_hline(yintercept = beta, color = custom_colors[2], linewidth = 0.5) +
    geom_hline(yintercept = beta - stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
    geom_hline(yintercept = beta + stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
    chart_theme

  if (marginal) {
    p <- ggExtra::ggMarginal(p, type = "density", groupColour = FALSE, groupFill = FALSE)
  }
  return(p)
}

#' Generate Intercept Correction Analysis and Visualizations
#'
#' Analyzes how SHAP values correct the GLM intercept term, focusing on
#' observations where continuous variables are zero or categorical variables
#' are at their reference levels.
#'
#' @param shp Data frame containing raw SHAP values.
#' @param x_glm_model The fitted GLM model object.
#' @param d Original input data frame.
#' @param target Character string specifying target variable name.
#' @param cont_vars Character vector of continuous variable names.
#' @param reference_levels_raw Named list of reference levels for categorical variables.
#' @param no_cat_toggle Logical indicating absence of categorical variables.
#' @param custom_colors Character vector of hex colors for plot styling.
#' @param chart_theme ggplot2 theme object for consistent plot appearance.
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
#' The intercept correction is crucial for understanding baseline predictions and
#' how they vary across different subpopulations in the data.
#'
#' @keywords internal
shap_intercept <- function(shp,
                           x_glm_model,
                           d,
                           target,
                           cont_vars,
                           reference_levels_raw,
                           no_cat_toggle,
                           custom_colors,
                           chart_theme) {
  beta_0 <- x_glm_model$coefficients["(Intercept)"] |> as.numeric()
  beta_0_SE <- summary(x_glm_model)$coefficients["(Intercept)", "Std. Error"]
  baseline <- shp$BIAS[1]

  if (no_cat_toggle) {
    shap_mask <- d |>
      dplyr::select(-dplyr::all_of(target)) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(cont_vars), ~ as.integer(. == 0)))
  } else {
    shap_mask <- d |>
      dplyr::select(-dplyr::all_of(target)) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(cont_vars), ~ as.integer(. == 0)),
        dplyr::across(
          dplyr::all_of(names(reference_levels_raw)),
          ~ as.integer(. == reference_levels_raw[dplyr::cur_column()])
        )
      )
  }

  intercept_shap <- (dplyr::select(shp, -"BIAS") * shap_mask) |>
    dplyr::select(names(which(colSums(shap_mask) > 0))) |>
    (\(df) dplyr::filter(df, rowSums(df) != 0))() |>
    dplyr::select(-dplyr::any_of(cont_vars))

  overall_density <- intercept_shap |>
    dplyr::mutate(total_correction = rowSums(dplyr::across(dplyr::everything())) + baseline + beta_0) |>
    ggplot(aes(x = total_correction)) +
    geom_density() +
    geom_vline(xintercept = baseline + beta_0, color = custom_colors[2], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = custom_colors[1], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = custom_colors[1], linewidth = 0.5) +
    ggtitle("Overall intercept correction distribution") +
    chart_theme

  return(list(overall_density = overall_density))
}

#' Generate Global SHAP Correction Distribution Plot
#'
#' Creates a visualization showing the overall distribution of multiplicative
#' corrections that SHAP values apply to GLM predictions.
#'
#' @param shp Data frame containing raw SHAP values including BIAS term.
#' @param custom_colors Character vector of hex colors for plot styling.
#' @param chart_theme ggplot2 theme object for consistent plot appearance.
#'
#' @return A ggplot object showing:
#' \itemize{
#'   \item Density distribution of exponentiated total SHAP corrections
#'   \item Vertical reference line at 1.0 (no correction)
#'   \item Mean correction value in the subtitle
#' }
#'
#' @details This function:
#' \itemize{
#'   \item Sums all SHAP values (including bias) for each observation
#'   \item Exponentiates the totals to show multiplicative corrections
#'   \item Visualizes how much the XGBoost model corrections deviate from GLM predictions
#'   \item Values > 1 indicate upward corrections, < 1 indicate downward corrections
#' }
#'
#' This global view helps assess the overall magnitude and direction of
#' model corrections across the entire dataset.
#'
#' @keywords internal
global_c <- function(shp, custom_colors, chart_theme) {
  dt <- shp |>
    dplyr::mutate(
      total = rowSums(dplyr::across(dplyr::everything())),
      total_exp = exp(total)
    )

  dt |>
    ggplot(aes(x = total_exp)) +
    geom_density() +
    geom_vline(xintercept = 1) +
    ggtitle(
      "Distribution of corrections to GLM prediction",
      subtitle = paste0("mean correction: ", round(mean(dt$total_exp), 3))
    ) +
    chart_theme
}
