#' Explain GLM Model Predictions Using SHAP Values
#'
#' Creates an explainer object that generates SHAP (SHapley Additive exPlanations)
#' values for GLM model predictions, providing various visualization and analysis
#' functions to understand model behavior and feature contributions.
#'
#' @param x A list object of class 'ens'. This should be output by `train_glm_xgb()`
#' @param data A data frame containing the data for which explanations are desired.
#'
#' **This should be the "test" portion of your dataset**
#'
#' @param as_contribution Logical, currently unused parameter for future functionality.
#'
#' @return A list containing:
#' \describe{
#'   \item{beta_corrected_scatter}{Function to create scatter plots showing SHAP corrections vs variable values}
#'   \item{beta_corrected_density}{Function to create density plots of SHAP corrections for variables}
#'   \item{shap_intercept}{List containing intercept correction visualizations}
#'   \item{overall_correction}{Function to show global correction distributions}
#'   \item{input_frame}{Original input data frame}
#'   \item{beta_corrections}{Wide format SHAP corrections data frame}
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
#' explainer$beta_corrected_scatter("age")
#'
#' # Show density of corrections
#' explainer$beta_corrected_density("income")
#' }
#'
#' @export
explain <- function(x, data, as_contribution = FALSE){
  rownames(data) <- NULL

  # Definitions and global variables
  betas <- x$glm_model$coefficients
  coef_names <- names(betas)

  vartypes <- lapply(x$glm_model$data, typeof) |> unlist()
  varclasses <- lapply(x$glm_model$data, class) |> unlist()

  response_var <- all.vars(x$glm_model$formula)[1]
  predictor_vars_all <- names(vartypes)
  predictor_vars_categorical <- predictor_vars_all[(!vartypes %in% c("integer", "double") | varclasses == "factor")]
  predictor_vars_continuous <- predictor_vars_all |> setdiff(response_var) |> setdiff(predictor_vars_categorical)

  # Factor levels for categorical variables
  cat_levels <- lapply(
    x$glm_model$data |> dplyr::select(dplyr::all_of(predictor_vars_categorical)),
    function(x) sort(unique(x))
  )

  cat_unique_names <- lapply(
    names(cat_levels),
    function(x) paste0(x, cat_levels[[x]])
  ) |> unlist()

  all_names <- c("(Intercept)", predictor_vars_continuous, cat_unique_names)

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

  reference_levels <- setdiff(all_names, c(names(betas), response_var))
  no_cat_toggle <- (length(predictor_vars_categorical) == 0)

  custom_colors <- c("#113458", "#D9AB16", "#4096C0", "#DCDCD9", "#113458","#2166AC", "#FFFFFF", "#B2182B")
  chart_theme <- chart_theme_fn(custom_colors)

  # Generate SHAP values
  shap <- predict(
    x$xgb_model,
    newdata = xgboost::xgb.DMatrix(
      data.matrix(
        dplyr::select(data, -dplyr::all_of(response_var))
      )
    ),
    predcontrib = TRUE
  ) |> data.frame()

  # Prepare wide input frame... this is `data` but with categoricals converted to one-hot format
  wide_input_frame <- data_dim_helper(
    frame = data,
    all_names = all_names,
    cat_levels = cat_levels,
    response_var = response_var,
    no_cat_toggle = no_cat_toggle
  )

  # Prepare wide shap corrections... this converts `shap` values to wide format for categoricals
  shap_wide <- shap_dim_helper(
    shap_raw = shap,
    wide_input_frame = wide_input_frame,
    cat_levels = cat_levels,
    response_var = response_var,
    no_cat_toggle = no_cat_toggle
  )

  # Prepare beta corrections... this converts `shap` values be compatible with feature values
  beta_corrections <- beta_corrections_derive(
      shap_wide = shap_wide,
      wide_input_frame = wide_input_frame,
      reference_levels = reference_levels,
      predictor_vars_continuous = predictor_vars_continuous,
      no_cat_toggle = no_cat_toggle
    )


  # Return explainer object with plotting functions
  list(

    beta_corrected_scatter = function(
      varname = "DrivAge",
      q = 0.05,
      color=NULL,
      marginal=FALSE,
      excl_outliers=FALSE
      ) {
        beta_corrected_scatter(
          varname = varname,
          q = q,
          color = color,
          marginal = marginal,
          excl_outliers = excl_outliers,
          betas = betas,
          cat_levels = cat_levels,
          wide_input_frame = wide_input_frame,
          beta_corrections = beta_corrections,
          data = data,
          response_var = response_var,
          predictor_vars_categorical = predictor_vars_categorical,
          predictor_vars_continuous = predictor_vars_continuous,
          reference_levels = reference_levels,
          custom_colors = custom_colors,
          chart_theme = chart_theme,
          all_names = all_names,
          x = x
        )
      },

    beta_corrected_density = function(
      varname = "DrivAge",
      q = 0.05,
      type="kde"
      ) {
        beta_corrected_density(
          varname = varname,
          q=q,
          type=type,
          betas = betas,
          cat_levels = cat_levels,
          reference_levels = reference_levels,
          wide_input_frame = wide_input_frame,
          beta_corrections = beta_corrections,
          x_glm_model = x$glm_model,
          data = data,
          predictor_vars_continuous = predictor_vars_continuous,
          predictor_vars_categorical = predictor_vars_categorical,
          custom_colors = custom_colors,
          chart_theme = chart_theme
        )
      },

    shap_intercept = shap_intercept(
      shap = shap,
      x_glm_model = x$glm_model,
      data = data,
      response_var = response_var,
      predictor_vars_continuous = predictor_vars_continuous,
      reference_levels_raw = reference_levels_raw,
      no_cat_toggle = no_cat_toggle,
      custom_colors = custom_colors,
      chart_theme = chart_theme
    ),

    overall_correction = function(
      transform_x_scale_by_link= TRUE
      ) {
      overall_correction(
        transform_x_scale_by_link = transform_x_scale_by_link,
        shap = shap,
        custom_colors = custom_colors,
        chart_theme = chart_theme,
        family = x$glm_model$family,
        relationship = attr(x, "relationship")
        )
      },

    input_frame = data,

    beta_corrections = beta_corrections,

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
#'
#' @import ggplot2
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
#'
#' @param frame Input data frame to be transformed.
#' @param all_names Character vector of all expected variable names including
#'   categorical levels and intercept.
#' @param cat_levels Named list where each element contains the unique levels
#'   for each categorical variable.
#' @param response_var Character string specifying the name of the response_var variable.
#' @param no_cat_toggle Logical indicating whether there are any categorical
#'   variables in the data.
#' @param remove_target Logical, whether to remove the response_var variable from
#'   the output (default TRUE).
#'
#' @return A data frame in wide format with one-hot encoded categorical variables,
#' an intercept column, and all variables ordered according to `all_names`.
#'
#'
#' @keywords internal
data_dim_helper <- function(frame, all_names, cat_levels, response_var, no_cat_toggle, remove_target = TRUE) {
  if (no_cat_toggle) {
    return(frame)
  }

  main_frame <- data.frame(matrix(0, nrow = nrow(frame), ncol = length(all_names))) |>
    stats::setNames(all_names)

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
    output_frame <- output_frame |> dplyr::select(-dplyr::any_of(response_var))
  }

  return(output_frame)
}

#' Convert Shap values to Wide One-Hot Encoded Format
#'
#' Transforms categorical variables in a data frame into one-hot encoded format
#'
#' @param shap_raw Data frame containing raw SHAP values from XGBoost.
#' @param wide_input_frame Wide format input data frame (one-hot encoded).
#' @param cat_levels Named list of categorical variable levels.
#' @param response_var Character string specifying the response_var variable name.
#' @param no_cat_toggle Logical indicating absence of categorical variables.
#'
#' @return A data frame where SHAP values are in wide format for categorical variables.
#'
#' @keywords internal
shap_dim_helper <- function(shap_raw,
                            wide_input_frame,
                            cat_levels,
                            response_var,
                            no_cat_toggle) {
  if (no_cat_toggle) {

    shap_wide <- shap_raw |>
      dplyr::mutate(bias = shap_raw$BIAS[1], .before = dplyr::everything())

  } else {

    wide_input_frame <- wide_input_frame |> dplyr::select(-dplyr::any_of(c("(Intercept)", response_var)))

    cat_frame <- lapply(names(cat_levels), function(x) {
      lvl <- cat_levels[[x]]
      mask <- wide_input_frame |>
        dplyr::select(dplyr::all_of(paste0(x, lvl))) |>
        data.matrix()
      matrix(rep(shap_raw[, x], length(lvl)), byrow = FALSE, ncol = length(lvl)) * mask
    }) |>
      dplyr::bind_cols()

    shap_wide <- cbind(
      shap_raw[, setdiff(colnames(wide_input_frame), colnames(cat_frame))],
      cat_frame
    ) |>
      dplyr::select(colnames(wide_input_frame)) |>
      dplyr::mutate(bias = shap_raw$BIAS[1], .before = dplyr::everything())
  }

  return(shap_wide)

}


#' Compute Beta Corrections based on SHAP values
#'
#' Processes raw SHAP values to create coefficient corrections. The bias is adjusted to account for
#' zero values in continuous variables and reference levels in categorical variables.
#'
#' @param shap_wide Data frame containing SHAP values from XGBoost that have been converted to wide format by [shap_dim_helper()]
#' @param wide_input_frame Wide format input data frame (one-hot encoded).
#' @param reference_levels Character vector of reference level names for categorical variables.
#' @param predictor_vars_continuous Character vector of numeric variables.
#' @param no_cat_toggle Logical indicating absence of categorical variables.
#'
#' @return A data frame with beta corrections where:
#' \itemize{
#'   \item Categorical variables are properly aggregated by factor level
#'   \item Continuous variables are normalized by their actual values
#'   \item Reference level and zero-value corrections are added to the bias term
#' }
#'
#' @keywords internal
beta_corrections_derive <- function(shap_wide,
                            wide_input_frame,
                            reference_levels,
                            predictor_vars_continuous,
                            no_cat_toggle){

    shap_for_zeros <- rowSums(
      ((wide_input_frame[, predictor_vars_continuous] == 0) * 1) * shap_wide[, predictor_vars_continuous]
      )

    shap_for_cat_ref <- ifelse(
      no_cat_toggle,
      0,
      rowSums(shap_wide[,reference_levels])
      )

    beta_corrections <- shap_wide

    beta_corrections$bias <- beta_corrections$bias + shap_for_zeros + shap_for_cat_ref

    calc <- beta_corrections[, predictor_vars_continuous] / wide_input_frame[, predictor_vars_continuous]
    calc[apply(calc, 2, is.infinite)] <- 0
    beta_corrections[, predictor_vars_continuous] <- calc

    return(beta_corrections)


}

#' Create Density Plot of Corrected Beta values for a Variable
#'
#' Generates a density plot showing the distribution of corrected Beta values
#' to a GLM coefficient, along with the original Beta coefficient, and standard error bounds around it.
#'
#' @param varname Character string specifying the variable name OR coefficient name is accepted as well.
#' @param q Number, must be between 0 and 0.5. Determines the quantile range of the plot
#' (i.e. value of 0.05 will only show shaps within 5% --> 95% quantile range for plot)
#' @param type Character string, must be "kde" or "hist"
#' @param betas Named numeric vector of GLM coefficients.
#' @param cat_levels Names list of categorical variables, with each item a vector of the levels
#' @param wide_input_frame Wide format input data frame.
#' @param beta_corrections Data frame containing SHAP corrections.
#' @param x_glm_model The fitted GLM model object.
#' @param data Original input data frame.
#' @param custom_colors Character vector of hex colors for plot styling.
#' @param chart_theme ggplot2 theme object for consistent plot appearance.
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
#'   \item Dashed lines at ±1 standard error from the coefficient
#'   \item Automatic x-axis limits that cut off the highest and lowest q %. If you want axis unaltered, set q = 0
#' }
#'
#' @keywords internal
#'
#' @import ggplot2
beta_corrected_density <- function(
    varname,
    q = 0.05,
    type="kde",
    betas,
    cat_levels,
    reference_levels,
    wide_input_frame,
    beta_corrections,
    x_glm_model,
    data,
    predictor_vars_continuous,
    predictor_vars_categorical,
    custom_colors,
    chart_theme
    ) {

  stopifnot(is.numeric(q), q >= 0 , q < 0.5)

  if (varname %in% predictor_vars_continuous) {
    vartype <- "numerical"
  } else if (varname %in% predictor_vars_categorical) {
    vartype <- "categorical"
  } else if (varname %in% reference_levels) {
    stop("varname is reference level. Plot cannot be produced as no beta coefficient exists for this level")
  } else if (varname %in% names(betas)){
    vartype <- "categorical_level"
  } else {
    stop("varname not found in model!")
  }

  # if the variable is categorical, we will use recursion to plot each unique level and output a list instead...
  if(vartype %in% "categorical"){

    levels_to_plot <- paste0(varname, cat_levels[[varname]])  |> intersect(names(betas))

    output <- purrr::map(
      levels_to_plot,
      function(x) {
        beta_corrected_density(
          varname = x,
          q = q,
          type = type,
          betas = betas,
          cat_levels = cat_levels,
          reference_levels = reference_levels,
          wide_input_frame = wide_input_frame,
          beta_corrections = beta_corrections,
          x_glm_model = x_glm_model,
          data = data,
          predictor_vars_continuous = predictor_vars_continuous,
          predictor_vars_categorical = predictor_vars_categorical,
          custom_colors = custom_colors,
          chart_theme = chart_theme
        )
      }
    ) |> stats::setNames(levels_to_plot)

    return(output)
  }

  # otherwise, we perform the code for a single plot...

  # if the variable is numerical, or if we are dealing with only one categorical_level, there is only 1 Beta value
  if(vartype %in% c("numerical","categorical_level")){
    stderror <- summary(x_glm_model)$coefficients[varname, "Std. Error"]
    beta <- betas[varname]
    shap_deviations <- beta_corrections[, varname]
  }

  # remove policies that do not have the level that was specified via varname (only when varname is a variable-level combo)
  if(vartype=="categorical_level"){
    is_wanted_level <- wide_input_frame[,varname]==1
    shap_deviations <- shap_deviations[is_wanted_level]
    }

  shap_quantiles <- beta + stats::quantile(shap_deviations, probs = c(q, 1-q))
  lower_bound <- min(shap_quantiles[1], beta - stderror)
  upper_bound <- max(shap_quantiles[2], beta + stderror)

  if(type == "kde") {
    geom_corrections_density <- list(geom_density(color = custom_colors[1], fill = custom_colors[4], alpha = 0.3))
    } else if(type == "hist") {
    geom_corrections_density <- list(geom_histogram(color = custom_colors[1], fill = custom_colors[4], alpha = 0.3, bins = 100))
    } else {
  stop("type was not 'kde' or 'hist'")
      }

  data.frame(x = beta + shap_deviations) |>
    ggplot(aes(x = x)) +
    geom_corrections_density +
    geom_vline(xintercept = beta, color = custom_colors[2], linewidth = 0.5) +
    geom_vline(xintercept = beta - stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
    geom_vline(xintercept = beta + stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
    labs(
      title = paste("Beta density after SHAP corrections for", varname),
      subtitle = paste0(varname, " beta: ", round(beta, 3), ", SE: ±", round(stderror, 4)),
      ) +
    xlab("Beta Coefficients") +
    xlim(lower_bound, upper_bound) +
    chart_theme

}



#' Create Scatter Plot of Beta Corrections for a Variable
#'
#' Generates a scatter plot or boxplot showing SHAP corrections for a specified variable from a fitted model.
#' For numerical variables, creates a scatter plot with optional coloring and marginal densities. For categorical
#' variables, creates a boxplot with model coefficients overlaid.
#'
#' @param varname Character. Name of the variable to plot SHAP corrections for.
#'   Must be present in the fitted model.
#' @param q Numeric. Quantile threshold for outlier detection (when excl_outliers = TRUE).
#' @param color Character or NULL. Name of variable to use for point coloring.
#'   Must be present in the model. Currently not supported for categorical variables.
#' @param marginal Logical. Whether to add marginal density plots (numerical variables only).
#' @param excl_outliers Logical. Whether to exclude outliers based on quantile method.
#' @param betas Named numeric vector. Model coefficients/betas from fitted model.
#' @param cat_levels Named list. Categorical variable levels.
#' @param wide_input_frame Data frame. Wide format input data used in model fitting.
#' @param beta_corrections Data frame. Wide format SHAP values corresponding to input data.
#' @param data Data frame. Original dataset containing variables for coloring.
#' @param response_var Character. Name of response_var/response variable.
#' @param reference_levels Character vector. Reference levels for categorical variables.
#' @param custom_colors Character vector. Custom color palette for plots.
#' @param chart_theme ggplot2 theme object. Theme to apply to the plot.
#' @param all_names Character vector. All variable names from the model.
#' @param x Model object containing the fitted GLM model (used for standard errors).
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
#' @importFrom magrittr %>%
beta_corrected_scatter <- function(varname,
                                         q,
                                         color,
                                         marginal,
                                         excl_outliers,
                                    betas,
                                    cat_levels,
                                    wide_input_frame,
                                    beta_corrections,
                                    data,
                                    response_var,
                                    predictor_vars_categorical,
                                    predictor_vars_continuous,
                                    reference_levels,
                                    custom_colors,
                                    chart_theme,
                                    all_names,
                                    x)  {

  # TODO: warning or error if reference level passed as varname
  color_vartype <- "numerical"

  vartype <- assign_variable_type(
    varname,
    predictor_vars_continuous,
    predictor_vars_categorical
  )

  if (!is.null(color)) {
    color_vartype <- assign_variable_type(
      color,
      predictor_vars_continuous,
      predictor_vars_categorical
    )
  }

  # is reference level (no glm beta)
  if(vartype=="categorical"){

    matched_names <-  sort(all_names[grep(varname, all_names)]) # potential risk of fuzzy matching if similar reference variable names
    reference_level <- reference_levels[grep(varname, reference_levels)]
    helper_names <- matched_names[matched_names!=reference_level]

    x <- wide_input_frame[,matched_names]
    shap_deviations <- beta_corrections[,matched_names]

    beta = c(0,betas[helper_names]) |> stats::setNames(c(reference_level, helper_names))

    after_shap <- sweep(shap_deviations, 2, beta[matched_names], FUN = "+")   |>
      tidyr::pivot_longer(cols = (matched_names),names_to = "x",values_to="shp")

    color = NULL
    message("color for categoricals currently not supported")

    beta_lines_df <- data.frame(
      x = names(beta[matched_names]),
      y = as.numeric(beta[matched_names])
    )

    # Add the lines to the plot
    p <- ggplot(data = after_shap, aes(x = x, y = shp)) +
      geom_boxplot() +
      geom_point(
        data = beta_lines_df,
        aes(x = x, y = y),
        color = "red",
      ) +
      labs(
        title = paste("Beta Coefficients after SHAP corrections for", varname),
        x = varname,
        y = "Beta Coefficients"
      )+
      chart_theme

  }else{

    stderror = summary(x$glm_model)$coefficients[varname, "Std. Error"]
    beta = betas[varname]

    x = wide_input_frame[,varname]
    shap_deviations = beta_corrections[,varname]
    after_shap = data.frame(x = x,
                            shp = beta + shap_deviations) %>%
      {if(is.null(color)) . else dplyr::mutate(.,color = data[,color])} %>%
      {if(excl_outliers) dplyr::filter(., detect_outliers(shp,method = "quantile",q=0.01)) else . }

    p  <- after_shap |>
      ggplot()+
      geom_point(aes(x = x,y=shp,group = color, color=color),alpha=0.4)+
      geom_smooth(aes(x = x,y=shp))+
      {if(color_vartype=="numerical") scale_color_gradientn(name = color,colors = custom_colors[c(2,1)])}+
      labs(
        title = paste("Beta Coefficients after SHAP corrections for", varname),
        subtitle = paste0(varname, " beta: ", round(beta, 4), ", SE: ", round(stderror, 4)),
        x = varname,
        y = "Beta Coefficients"
        )+
      geom_hline(yintercept = beta, color = custom_colors[2], size = 0.5) +
      geom_hline(yintercept = beta - stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
      geom_hline(yintercept = beta + stderror, linetype = "dashed", color = custom_colors[3], linewidth = 0.5) +
      chart_theme

    if(marginal){
      p = ggExtra::ggMarginal(p,type = "density",groupColour = F, groupFill = F)
    }

  }

  return(p)

}


#' Generate Intercept Correction Analysis and Visualizations
#'
#' Analyzes how SHAP values correct the GLM intercept term, focusing on
#' observations where continuous variables are zero or categorical variables
#' are at their reference levels.
#'
#' @param shap Data frame containing raw SHAP values.
#' @param x_glm_model The fitted GLM model object.
#' @param data Original input data frame.
#' @param response_var Character string specifying response_var variable name.
#' @param predictor_vars_continuous Character vector of continuous variable names.
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
#'
#' @keywords internal
#'
#' @import ggplot2
shap_intercept <- function(shap,
                           x_glm_model,
                           data,
                           response_var,
                           predictor_vars_continuous,
                           reference_levels_raw,
                           no_cat_toggle,
                           custom_colors,
                           chart_theme) {
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
          dplyr::all_of(names(reference_levels_raw)),
          ~ as.integer(. == reference_levels_raw[dplyr::cur_column()])
        )
      )
  }

  intercept_shap <- (dplyr::select(shap, -"BIAS") * shap_mask) |>
    dplyr::select(names(which(colSums(shap_mask) > 0))) |>
    (\(df) dplyr::filter(df, rowSums(df) != 0))() |>
    dplyr::select(-dplyr::any_of(predictor_vars_continuous))

  overall_density <- intercept_shap |>
    dplyr::mutate(total_correction = rowSums(dplyr::across(dplyr::everything())) + baseline + beta_0) |>
    ggplot(aes(x = total_correction)) +
    geom_density() +
    geom_vline(xintercept = baseline + beta_0, color = custom_colors[2], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = custom_colors[1], linewidth = 0.5) +
    geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = custom_colors[1], linewidth = 0.5) +
    ggtitle("Overall intercept correction distribution") +
    chart_theme

  intercept_shap_long <- intercept_shap |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::filter(value!=0) |>
    dplyr::mutate(name = factor(name, levels = names(sort(-colSums(intercept_shap!=0)))),
                  value = value + baseline + beta_0)

  grouped_density = intercept_shap_long |>
    ggplot(aes(x=value))+
    geom_density()+
    facet_wrap(~name,scales="free")+
    geom_vline(xintercept = baseline + beta_0, color = custom_colors[2], size = 0.5)+
    geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = custom_colors[1], size = 0.5)+
    geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = custom_colors[1], size = 0.5)+
    ggtitle("Individual intercept correction distributions")+
    xlab("")+
    ylab("")+
    chart_theme

  boxplot <- intercept_shap_long |>
    ggplot(aes(x = name,y=value))+
    geom_boxplot()+
    geom_hline(yintercept = baseline + beta_0, color = custom_colors[2], size = 0.5)+
    ggtitle(paste0("Jitter chart of beta corrections for intercept"),
            subtitle = paste0("Intercept: ", round(beta_0,2)," with shap baseline: ",round(baseline,2)))+
    xlab("")+
    ylab("")+
    chart_theme

  return(list(overall_density = overall_density,
              grouped_density = grouped_density,
              boxplot = boxplot))
}

#' Generate Σ SHAP Correction Distribution Plot
#'
#' Creates a visualization showing for each record the overall booster component (either multiplicative or additive)
#'
#' @param transform_x_scale_by_link TRUE/FALSE, whether to transform the x axis by the link function
#' @param shap Data frame containing raw SHAP values including BIAS term.
#' @param custom_colors Character vector of hex colors for plot styling.
#' @param chart_theme ggplot2 theme object for consistent plot appearance.
#' @param family object of class "family" containing the link information for the fitted GLM. The can be found in `glm_model$family`
#' @param relationship string, should explain what the ensemble relationship is, i.e. "multiplicative" or "additive"
#'
#' @return A ggplot object showing density of total booster values
#'
#' @keywords internal
#'
#' @import ggplot2
overall_correction <- function(transform_x_scale_by_link = TRUE, shap, custom_colors, chart_theme, family, relationship) {

  dt <- shap |>
    dplyr::mutate(
      total = rowSums(dplyr::across(dplyr::everything())),
      total_exp = exp(total),
      total_invlink = family$linkinv(total)
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
    ggplot(aes(x = total_invlink)) +
    geom_density() +
    geom_vline(xintercept = family$linkinv(0)) +
    chart_theme +
    scale_x_link +
    labs(
      title = paste0("Distribution of ", relationship, " corrections to GLM prediction"),
      subtitle = paste0("mean correction: ", round(mean(dt$total_invlink), 3)),
      x = paste0(relationship, " correction") |> tools::toTitleCase()
      )
}
