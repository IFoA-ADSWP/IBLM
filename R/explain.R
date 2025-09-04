library(ggplot2)


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

# Helper: theme for plots
chart_theme_fn <- function(custom_colors) {
  theme_minimal() +
    theme(
      plot.title = element_text(color = custom_colors[5], face = "bold", size = 14),
      plot.subtitle = element_text(color = custom_colors[2], size = 12),
      panel.grid.major = element_line(color = custom_colors[4], linewidth = 0.3),
      panel.grid.minor = element_line(color = custom_colors[4], linewidth = 0.2)
    )
}

# Convert source data to wide one-hot format
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

# Compute SHAP corrections
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

# SHAP correction density plot
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

# SHAP correction scatter plot
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

# SHAP intercept plots
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

# Global SHAP corrections
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
