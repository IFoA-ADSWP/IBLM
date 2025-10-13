#' Explain GLM Model Predictions Using SHAP Values
#'
#' Creates a list that explains the beta values, and their corrections, of the ensemble IBLM model
#'
#' @param iblm_model An object of class 'iblm'. This should be output by `train_glm_xgb()`
#' @param data Data frame.
#' If you have used `split_into_train_validate_test()` this will be the "test" portion of your data.
#' @param migrate_reference_to_bias TRUE/FALSE, should shap corrections for reference levels be moved to the bias values instead?
#'
#' **This should be the "test" portion of your dataset**
#'
#' @return A list containing:
#' \describe{
#'   \item{beta_corrected_scatter}{Function to create scatter plots showing SHAP corrections vs variable values (see \code{\link[IBLMpackage]{beta_corrected_scatter}})}
#'   \item{beta_corrected_density}{Function to create density plots of SHAP corrections for variables (see \code{\link[IBLMpackage]{beta_corrected_density}})}
#'   \item{shap_intercept}{List containing intercept correction visualizations}
#'   \item{overall_correction}{Function to show global correction distributions (see \code{\link[IBLMpackage]{overall_correction}})}
#'   \item{input_frame}{Original input data frame}
#'   \item{beta_corrections}{Wide format SHAP corrections data frame}
#'   \item{shap}{Raw SHAP values from XGBoost}
#'   \item{ glm_beta_coeff}{GLM model coefficients}
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
#' explainer <- explain_iblm(models, test_data)
#'
#' # Generate scatter plot for a variable
#' explainer$beta_corrected_scatter("age")
#'
#' # Show density of corrections
#' explainer$beta_corrected_density("income")
#' }
#'
#' @export
explain_iblm <- function(iblm_model, data, migrate_reference_to_bias = FALSE){

  check_iblm_model(iblm_model)

  # Generate SHAP values
  shap <- stats::predict(
    iblm_model$xgb_model,
    newdata = xgboost::xgb.DMatrix(
      data.matrix(
        dplyr::select(data, -dplyr::all_of(iblm_model$response_var))
      )
    ),
    predcontrib = TRUE
  ) |> data.frame()

  # Prepare wide input frame... this is `data` but with categoricals converted to one-hot format
  wide_input_frame <- data_dim_helper(
    frame = data,
    iblm_model = iblm_model
  )

  # Prepare wide shap corrections... this converts `shap` values to wide format for categoricals
  shap_wide <- shap_dim_helper(
    shap = shap,
    wide_input_frame = wide_input_frame,
    iblm_model = iblm_model
  )

  # Prepare beta corrections... this converts `shap` values be compatible with feature values
  beta_corrections <- beta_corrections_derive(
      shap_wide = shap_wide,
      wide_input_frame = wide_input_frame,
      iblm_model = iblm_model,
      migrate_reference_to_bias = migrate_reference_to_bias
    )

  # Prepare beta values after corrections
  data_beta_coeff_glm <- data_beta_coeff_glm_helper(
    data = data,
    iblm_model = iblm_model)

  data_beta_coeff_shap <- data_beta_coeff_shap_helper(
      data,
      beta_corrections = beta_corrections,
      iblm_model= iblm_model
     )

  data_beta_coeff <- data_beta_coeff_glm + data_beta_coeff_shap

  # Return explainer object with plotting functions
  list(

    beta_corrected_scatter = function(
    varname = "DrivAge",
    q = 0,
    color=NULL,
    marginal=FALSE
    ) {
      beta_corrected_scatter(
        varname = varname,
        q = q,
        color = color,
        marginal = marginal,
        data_beta_coeff = data_beta_coeff,
        data = data,
        iblm_model = iblm_model
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
          wide_input_frame = wide_input_frame,
          beta_corrections = beta_corrections,
          data = data,
          iblm_model = iblm_model
        )
      },

    shap_intercept = shap_intercept(
      shap = shap,
      data = data,
      iblm_model = iblm_model
    ),

    overall_correction = function(
      transform_x_scale_by_link= TRUE
      ) {
      overall_correction(
        transform_x_scale_by_link = transform_x_scale_by_link,
        shap = shap,
        iblm_model = iblm_model
        )
      },

    input_frame = data,

    beta_corrections = beta_corrections,

    shap = shap,

    data_beta_coeff = data_beta_coeff
  )
}






# ========================= Helper functions for `explain` ========================


#' Convert Data Frame to Wide One-Hot Encoded Format
#'
#' Transforms categorical variables in a data frame into one-hot encoded format
#'
#' @param frame Input data frame to be transformed.
#' @param iblm_model Object of class 'iblm'
#' @param remove_target Logical, whether to remove the response_var variable from
#'   the output (default TRUE).
#'
#' @return A data frame in wide format with one-hot encoded categorical variables,
#' an intercept column, and all variables ordered according to `coef_names_all`.
#'
#'
#' @keywords internal
data_dim_helper <- function(frame, iblm_model, remove_target = TRUE) {

  coef_names_all <- iblm_model$coeff_names$all
  levels_all_cat <- iblm_model$cat_levels$all
  response_var <- iblm_model$response_var
  no_cat_toggle <- length(iblm_model$predictor_vars$categorical) == 0

  if (no_cat_toggle) {
    return(frame)
  }

  main_frame <- data.frame(matrix(0, nrow = nrow(frame), ncol = length(coef_names_all))) |>
    stats::setNames(coef_names_all)

  df_onehot <- frame |>
    fastDummies::dummy_cols(
      select_columns = names(levels_all_cat),
      remove_first_dummy = FALSE,
      remove_selected_columns = TRUE
    ) |>
    dplyr::rename_with(~ gsub("_", "", .x))

  output_frame <- cbind(
    df_onehot,
    main_frame[, setdiff(coef_names_all, colnames(df_onehot))]
  ) |>
    dplyr::mutate("(Intercept)" = 1) |>
    dplyr::select(dplyr::all_of(coef_names_all))

  if (remove_target) {
    output_frame <- output_frame |> dplyr::select(-dplyr::any_of(response_var))
  }

  return(output_frame)
}

#' Convert Shap values to Wide One-Hot Encoded Format
#'
#' Transforms categorical variables in a data frame into one-hot encoded format
#'
#' @param shap Data frame containing raw SHAP values from XGBoost.
#' @param wide_input_frame Wide format input data frame (one-hot encoded).
#' @param iblm_model Object of class 'iblm'
#'
#' @return A data frame where SHAP values are in wide format for categorical variables.
#'
#' @keywords internal
shap_dim_helper <- function(shap,
                            wide_input_frame,
                            iblm_model) {

  levels_all_cat <- iblm_model$cat_levels$all
  response_var <- iblm_model$response_var
  no_cat_toggle <- length(iblm_model$predictor_vars$categorical) == 0


  if (no_cat_toggle) {

    shap_wide <- shap |>
      dplyr::mutate(bias = shap$BIAS[1], .before = dplyr::everything())

  } else {

    wide_input_frame <- wide_input_frame |> dplyr::select(-dplyr::any_of(c("(Intercept)", response_var)))

    cat_frame <- lapply(names(levels_all_cat), function(x) {
      lvl <- levels_all_cat[[x]]
      mask <- wide_input_frame |>
        dplyr::select(dplyr::all_of(paste0(x, lvl))) |>
        data.matrix()
      matrix(rep(shap[, x], length(lvl)), byrow = FALSE, ncol = length(lvl)) * mask
    }) |>
      dplyr::bind_cols()

    shap_wide <- cbind(
      shap |> dplyr::select(-dplyr::any_of(names(cat_frame))),
      cat_frame
    ) |>
      dplyr::select(colnames(wide_input_frame)) |>
      dplyr::mutate(bias = shap$BIAS[1], .before = dplyr::everything())
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
#' @param migrate_reference_to_bias Logical, do we want to migrate the shap values for reference coefficients to the bias?
#' @param iblm_model Object of class 'iblm'
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
                            migrate_reference_to_bias = FALSE,
                            iblm_model){

  coef_names_reference_cat <- iblm_model$coeff_names$reference_cat
  predictor_vars_continuous <- iblm_model$predictor_vars$continuous

  beta_corrections <- shap_wide

    shap_for_zeros <- rowSums(
      ((wide_input_frame[, predictor_vars_continuous] == 0) * 1) * shap_wide[, predictor_vars_continuous]
      )

    if(migrate_reference_to_bias) {

    shap_for_cat_ref <- rowSums(shap_wide[,coef_names_reference_cat])

    beta_corrections[, coef_names_reference_cat] <- 0

    } else {

      shap_for_cat_ref <- 0

    }

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
    type="kde",
    wide_input_frame,
    beta_corrections,
    data,
    iblm_model
    ) {

  glm_beta_coeff <- iblm_model$glm_model$coefficient
  levels_all_cat <- iblm_model$cat_levels$all
  coef_names_reference_cat <- iblm_model$coeff_names$reference_cat
  x_glm_model <- iblm_model$glm_model
  predictor_vars_continuous <- iblm_model$predictor_vars$continuous
  predictor_vars_categorical <- iblm_model$predictor_vars$categorical

  stopifnot(is.numeric(q), q >= 0 , q < 0.5)

  if (varname %in% predictor_vars_continuous) {
    vartype <- "numerical"
  } else if (varname %in% predictor_vars_categorical) {
    vartype <- "categorical"
  } else if (varname %in% coef_names_reference_cat) {
    stop("varname is reference level. Plot cannot be produced as no beta coefficient exists for this level")
  } else if (varname %in% names(glm_beta_coeff)){
    vartype <- "categorical_level"
  } else {
    stop("varname not found in model!")
  }

  # if the variable is categorical, we will use recursion to plot each unique level and output a list instead...
  if(vartype %in% "categorical"){

    levels_to_plot <- paste0(varname, levels_all_cat[[varname]])  |> intersect(names(glm_beta_coeff))

    output <- purrr::map(
      levels_to_plot,
      ~beta_corrected_density(
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
  if(vartype %in% c("numerical","categorical_level")){
    stderror <- summary(x_glm_model)$coefficients[varname, "Std. Error"]
    beta <-  glm_beta_coeff[varname]
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
    geom_corrections_density <- list(geom_density(color = iblm_colors[1], fill = iblm_colors[4], alpha = 0.3))
    } else if(type == "hist") {
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

  intercept_shap <- (dplyr::select(shap, -"BIAS") * shap_mask) |>
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
    dplyr::filter(.data$value!=0) |>
    dplyr::mutate(name = factor(.data$name, levels = names(sort(-colSums(intercept_shap!=0)))),
                  value = .data$value + baseline + beta_0)

  grouped_density = intercept_shap_long |>
    ggplot(aes(x=.data$value))+
    geom_density()+
    facet_wrap(~name,scales="free")+
    geom_vline(xintercept = baseline + beta_0, color = iblm_colors[2], size = 0.5)+
    geom_vline(xintercept = baseline + beta_0 - beta_0_SE, color = iblm_colors[1], size = 0.5)+
    geom_vline(xintercept = baseline + beta_0 + beta_0_SE, color = iblm_colors[1], size = 0.5)+
    ggtitle("Individual intercept correction distributions")+
    xlab("")+
    ylab("")+
    theme_iblm()

  boxplot <- intercept_shap_long |>
    ggplot(aes(x = .data$name,y=.data$value))+
    geom_boxplot()+
    geom_hline(yintercept = baseline + beta_0, color = iblm_colors[2], size = 0.5)+
    ggtitle(paste0("Jitter chart of beta corrections for intercept"),
            subtitle = paste0("Intercept: ", round(beta_0,2)," with shap baseline: ",round(baseline,2)))+
    xlab("")+
    ylab("")+
    theme_iblm()

  return(list(overall_density = overall_density,
              grouped_density = grouped_density,
              boxplot = boxplot))
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
