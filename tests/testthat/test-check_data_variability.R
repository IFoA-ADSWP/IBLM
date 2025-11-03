testthat::test_that("test explain errors when some fields have single value", {

    df <- freMTPLmini |>
      dplyr::mutate(DrivAge = 50, Area = factor("A"))

  testthat::expect_error(
    check_data_variability(df, "ClaimRate")
  )

})
