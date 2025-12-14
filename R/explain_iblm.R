#' Explain GLM Model Predictions Using SHAP Values
#'
#' Creates a list that explains the beta values, and their corrections, of the ensemble IBLM model
#'
#' @param iblm_model An object of class 'iblm'. This should be output by `train_iblm_xgb()`
#' @param data Data frame.
#'
#' If you have used `split_into_train_validate_test()` this will be the "test" portion of your data.
#'
#' @param migrate_reference_to_bias Logical, migrate the beta corrections to the bias for reference levels?
#' This applied to categorical vars only. It is recommended to leave this setting on TRUE
#'
#' @return A list containing:
#' \describe{
#'   \item{beta_corrected_scatter}{Function to create scatter plots showing SHAP corrections vs variable values (see \code{\link[IBLM]{beta_corrected_scatter}})}
#'   \item{beta_corrected_density}{Function to create density plots of SHAP corrections for variables (see \code{\link[IBLM]{beta_corrected_density}})}
#'   \item{bias_density}{Function to create density plots of SHAP corrections migrated to bias (see \code{\link[IBLM]{bias_density}})}
#'   \item{overall_correction}{Function to show global correction distributions (see \code{\link[IBLM]{overall_correction}})}
#'   \item{shap}{Dataframe showing raw SHAP values of data records}
#'   \item{beta_corrections}{Dataframe showing beta corrections (in wide/one-hot format) of data records}
#'   \item{data_beta_coeff}{Dataframe showing beta coefficients of data records}
#' }
#'
#' @details The following outputs are functions that can be called to create plots:
#' \itemize{
#'   \item beta_corrected_scatter
#'   \item beta_corrected_density
#'   \item bias_density
#'   \item overall_correction
#' }
#'
#' For each of these, the key data arguments (e.g. data, shap, iblm_model) are already populated by `explain_iblm()`.
#' When calling these functions output from `explain_iblm()` only key settings like variable names, colours...etc need populating.
#'
#' @examples
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' ex <- explain_iblm(iblm_model, df_list$test)
#'
#' # the output contains functions that can be called to visualise iblm
#' ex$beta_corrected_scatter("DrivAge")
#' ex$beta_corrected_density("DrivAge")
#' ex$overall_correction()
#' ex$bias_density()
#'
#' # the output contains also dataframes
#' ex$shap |> dplyr::glimpse()
#' ex$beta_corrections |> dplyr::glimpse()
#' ex$data_beta_coeff |> dplyr::glimpse()
#'
#' @export
explain_iblm <- function(iblm_model, data, migrate_reference_to_bias = TRUE) {

  check_iblm_model(iblm_model)

  # Generate SHAP values
  shap <- extract_booster_shap(iblm_model$booster_model, data)

  # Prepare wide input frame... this is `data` but with categoricals converted to one-hot format
  wide_input_frame <- data_to_onehot(data, iblm_model)

  # Prepare wide shap corrections... this converts `shap` values to wide format for categoricals
  shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)

  # Prepare beta corrections... this converts `shap` values be compatible with feature values
  beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model, migrate_reference_to_bias)

  # Prepare beta values after corrections
  data_glm <- data_beta_coeff_glm(data, iblm_model)
  data_booster <- data_beta_coeff_booster(data, beta_corrections, iblm_model)
  data_beta_coeff <- data_glm + data_booster

  # Return explainer object with plotting functions
  list(

    shap = shap,

    beta_corrections = beta_corrections,

    data_beta_coeff = data_beta_coeff,

    beta_corrected_scatter = create_beta_corrected_scatter(
      data_beta_coeff = data_beta_coeff,
      data = data,
      iblm_model = iblm_model
      ),

    beta_corrected_density = create_beta_corrected_density(
      wide_input_frame = wide_input_frame,
      beta_corrections = beta_corrections,
      data = data,
      iblm_model = iblm_model
      ),

    bias_density = create_bias_density(
      migrate_reference_to_bias = migrate_reference_to_bias,
      shap = shap,
      data = data,
      iblm_model = iblm_model
      ),

    overall_correction = create_overall_correction(
      shap = shap,
      iblm_model = iblm_model
      )

    )
}






# ========================= Helper functions for `explain` ========================


#' Convert Data Frame to Wide One-Hot Encoded Format
#'
#' Transforms categorical variables in a data frame into one-hot encoded format
#'
#' @param data Input data frame to be transformed. This will typically be the "train" data subset
#' @param iblm_model Object of class 'iblm'
#' @param remove_target Logical, whether to remove the response_var variable from
#'   the output (default TRUE).
#'
#' @return A data frame in wide format with one-hot encoded categorical variables,
#' an intercept column, and all variables ordered according to "coeff_names$all" from `iblm_model`
#'
#' @examples
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' wide_input_frame <- data_to_onehot(df_list$test, iblm_model)
#'
#' wide_input_frame |> dplyr::glimpse()
#'
#' @export
data_to_onehot <- function(data, iblm_model, remove_target = TRUE) {

  check_iblm_model(iblm_model)

  coef_names_all <- iblm_model$coeff_names$all
  levels_all_cat <- iblm_model$cat_levels$all
  response_var <- iblm_model$response_var
  no_cat_toggle <- length(iblm_model$predictor_vars$categorical) == 0

  if (no_cat_toggle) {
    return(data)
  }

  main_frame <- data.frame(matrix(0, nrow = nrow(data), ncol = length(coef_names_all))) |>
    stats::setNames(coef_names_all)

  df_onehot <- data |>
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
#' Transforms categorical variables in a data frame into one-hot encoded format. Renames "BIAS" to lowercase.
#'
#' @param shap Data frame containing raw SHAP values from XGBoost.
#' @param wide_input_frame Wide format input data frame (one-hot encoded).
#' @param iblm_model Object of class 'iblm'
#'
#' @return A data frame where SHAP values are in wide format for categorical variables. Column "bias" is moved to start.
#'
#' @examples
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' shap <- extract_booster_shap(iblm_model$booster_model, df_list$test)
#'
#' wide_input_frame <- data_to_onehot(df_list$test, iblm_model)
#'
#' shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)
#'
#' shap_wide |> dplyr::glimpse()
#'
#' @export
shap_to_onehot <- function(shap,
                           wide_input_frame,
                           iblm_model) {

  check_iblm_model(iblm_model)

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
        dplyr::select(dplyr::all_of(paste0(x, lvl)))
      matrix(rep(shap[, x], length(lvl)), byrow = FALSE, ncol = length(lvl)) * mask
    }) |>
      dplyr::bind_cols()

    shap_wide <- cbind(
      shap |> dplyr::select(-dplyr::any_of(names(cat_frame))),
      cat_frame
    ) |>
      dplyr::select(dplyr::all_of(colnames(wide_input_frame))) |>
      dplyr::mutate(bias = shap$BIAS[1], .before = dplyr::everything())
  }

  return(shap_wide)
}


#' Compute Beta Corrections based on SHAP values
#'
#' @description
#' Processes SHAP values in one-hot (wide) format to create beta coefficient corrections.
#'
#' This includes:
#' \itemize{
#'   \item scaling shap values of continuous variables by the predictor value for that row
#'   \item migrating shap values to the bias for continuous variables where the predictor value was zero
#'   \item migrating shap values to the bias for categorical variables where the predictor value was reference level
#' }
#'
#' @param shap_wide Data frame containing SHAP values from XGBoost that have been converted to wide format by [shap_to_onehot()]
#' @param wide_input_frame Wide format input data frame (one-hot encoded).
#' @param migrate_reference_to_bias Logical, migrate the beta corrections to the bias for reference levels?
#' This applied to categorical vars only. It is recommended to leave this setting on TRUE
#' @param iblm_model Object of class 'iblm'
#'
#' @return A data frame with the booster model beta corrections in one-hot (wide) format
#'
#' @examples
#' df_list <- freMTPLmini |> split_into_train_validate_test(seed = 9000)
#'
#' iblm_model <- train_iblm_xgb(
#'   df_list,
#'   response_var = "ClaimRate",
#'   family = "poisson"
#' )
#'
#' shap <- extract_booster_shap(iblm_model$booster_model, df_list$test)
#'
#' wide_input_frame <- data_to_onehot(df_list$test, iblm_model)
#'
#' shap_wide <- shap_to_onehot(shap, wide_input_frame, iblm_model)
#'
#' beta_corrections <- beta_corrections_derive(shap_wide, wide_input_frame, iblm_model)
#'
#' beta_corrections |> dplyr::glimpse()
#'
#' @export
beta_corrections_derive <- function(shap_wide,
                                    wide_input_frame,
                                    iblm_model,
                                    migrate_reference_to_bias = TRUE) {

  check_iblm_model(iblm_model)

  coef_names_reference_cat <- iblm_model$coeff_names$reference_cat
  predictor_vars_continuous <- iblm_model$predictor_vars$continuous

  beta_corrections <- shap_wide

  shap_for_zeros <- rowSums(
    (
      wide_input_frame |>
        dplyr::select(dplyr::all_of(predictor_vars_continuous)) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            \(x) dplyr::if_else(x == 0, 1, 0)
          )
        )
    ) * dplyr::select(shap_wide, dplyr::all_of(predictor_vars_continuous))
  )

  if (length(predictor_vars_continuous) == 0) {
    shap_for_zeros <- rep(0, nrow(beta_corrections))
  }

  if (migrate_reference_to_bias) {
    shap_for_cat_ref <- rowSums(
      dplyr::select(shap_wide, dplyr::all_of(coef_names_reference_cat))
    )

    beta_corrections <- beta_corrections |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(coef_names_reference_cat),
          ~0
        )
      )
  } else {
    shap_for_cat_ref <- rep(0, nrow(beta_corrections))
  }

  beta_corrections$bias <- beta_corrections$bias + shap_for_zeros + shap_for_cat_ref

  beta_corrections <- beta_corrections |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(predictor_vars_continuous),
        function(x) {
          y <- x / wide_input_frame[[dplyr::cur_column()]]
          y <- dplyr::if_else(is.infinite(y), 0, y)
          return(y)
        }
      )
    )

  return(beta_corrections)
}
