#' Show GLM Beta Coefficients for tabular data
#'
#' Creates dataframe of GLM beta coefficients for each row and predictor variable of data
#'
#' @param data Data frame with predictor variables
#' @param response_var Response variable name to exclude
#' @param betas Named vector of GLM coefficients
#' @param levels_all_cat Named list of categorical variable levels
#' @param predictor_vars_categorical Character vector of categorical variable names
#' @param predictor_vars_continuous Character vector of continuous variable names
#'
#' @return Data frame with predictor coefficients, plus a bias column
data_beta_coeff_glm <- function(
    data,
    response_var,
    betas,
    levels_all_cat,
    predictor_vars_categorical,
    predictor_vars_continuous) {


  glm_coeffs_all_cat <- purrr::imap(
    levels_all_cat,
    function(x, i) {
      coeff_name <- paste0(i, x)
      dplyr::if_else(
        levels_reference_cat[i] == x,
        0,
        betas[coeff_name]
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
        function(x) betas[[dplyr::cur_column()]]
      )
    ) |>
    dplyr::mutate(bias = betas[["(Intercept)"]], .before = 1)


}



#' Show Shap Beta Corrections for tabular data
#'
#' Creates dataframe of Shap beta corrections for each row and predictor variable of data
#'
#' @param data A data frame containing the dataset for analysis
#' @param levels_all_cat A named list containing all categorical levels for each categorical variable
#' @param levels_reference_cat A named vector specifying the reference category for each categorical variable
#' @param response_var Character string specifying the name of the response variable to exclude
#' @param predictor_vars_categorical Character vector of categorical predictor variable names
#' @param predictor_vars_continuous Character vector of continuous predictor variable names
#' @param beta_corrections A data frame or matrix containing beta correction values for all variables and bias
#'
#' @return A data frame with beta coefficient corrections for all predictor variables plus bias term
data_beta_coeff_shap <- function(data,
                                 levels_all_cat,
                                 levels_reference_cat,
                                 response_var,
                                 predictor_vars_categorical,
                                 predictor_vars_continuous,
                                 beta_corrections) {

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

