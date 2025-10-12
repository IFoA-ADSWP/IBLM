#' Show GLM Beta Coefficients for tabular data
#'
#' Creates dataframe of GLM beta coefficients for each row and predictor variable of data
#'
#' @param data Data frame with predictor variables
#' @param iblm_model Object of class 'iblm'
#'
#' @return Data frame with predictor coefficients, plus a bias column
data_beta_coeff_glm_helper <- function(
    data,
    iblm_model
    ) {

  response_var <- iblm_model$response_var
  glm_beta_coeff <- iblm_model$glm_model$coefficients
  levels_all_cat <- iblm_model$cat_levels$all
  levels_reference_cat <- iblm_model$cat_levels$reference
  predictor_vars_continuous <- iblm_model$predictor_vars$continuous
  predictor_vars_categorical <- iblm_model$predictor_vars$categorical


  glm_coeffs_all_cat <- purrr::imap(
    levels_all_cat,
    function(x, i) {
      coeff_name <- paste0(i, x)
      dplyr::if_else(
        levels_reference_cat[i] == x,
        0,
        glm_beta_coeff[coeff_name]
      ) |> unname()
    }
  )

  data |>
    dplyr::select(-dplyr::any_of(response_var)) |>
    dplyr::mutate(
      dplyr::across(
        predictor_vars_categorical,
        function(x) {
          glm_coeffs_all_cat[[dplyr::cur_column()]][
            match(x, levels_all_cat[[dplyr::cur_column()]])
          ]
        }),
      dplyr::across(
        predictor_vars_continuous,
        function(x) glm_beta_coeff[[dplyr::cur_column()]]
      )
    ) |>
    dplyr::mutate(bias = glm_beta_coeff[["(Intercept)"]], .before = 1)


}



#' Show Shap Beta Corrections for tabular data
#'
#' Creates dataframe of Shap beta corrections for each row and predictor variable of data
#'
#' @param data A data frame containing the dataset for analysis
#' @param beta_corrections A data frame or matrix containing beta correction values for all variables and bias
#' @param iblm_model Object of class 'iblm'
#'
#' @return A data frame with beta coefficient corrections for all predictor variables plus bias term
data_beta_coeff_shap_helper <- function(data,
                                        beta_corrections,
                                        iblm_model
                                 ) {

  response_var <- iblm_model$response_var
  levels_all_cat <- iblm_model$cat_levels$all
  levels_reference_cat <- iblm_model$cat_levels$reference
  predictor_vars_continuous <- iblm_model$predictor_vars$continuous
  predictor_vars_categorical <- iblm_model$predictor_vars$categorical

  levels_non_ref_cat <- purrr::map(
    names(levels_all_cat),
    function(var) {levels_all_cat[[var]] |> setdiff(levels_reference_cat[var])}
  ) |>
    stats::setNames(names(levels_all_cat))

  data |>
    dplyr::select(-dplyr::any_of(response_var)) |>
    dplyr::mutate(
      dplyr::across(
        predictor_vars_categorical,
        function(x) {
          beta_corrections |>
            dplyr::select(
              dplyr::any_of(
                paste0(dplyr::cur_column(), levels_non_ref_cat[[dplyr::cur_column()]])
              )
            ) |>
            rowSums()
        }),
      dplyr::across(
        predictor_vars_continuous,
        function(x) beta_corrections[[dplyr::cur_column()]]
      )
    ) |>
    dplyr::mutate(bias = beta_corrections[["bias"]], .before = 1)

}

