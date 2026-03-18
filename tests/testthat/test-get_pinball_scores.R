
testthat::test_that("test against Karol original script", {

  # test takes too long for CRAN
  testthat::skip_on_cran()


  # ============================ Download data =====================

  # download the correct version of freMTPL2freq dataset to complete the rec

  commit <- "2a718359896bee4edf852721364ac5eaae442fc1"  # <- use this commit

  url <- paste0("https://github.com/dutangc/CASdatasets/raw/", commit, "/data/freMTPL2freq.rda")

  temp <- tempfile()

  download.file(url, temp)

  load(temp)

  freMTPL2freq <- freMTPL2freq |>
    dplyr::mutate(
      ClaimRate = ClaimNb / Exposure,
      ClaimRate = pmin(ClaimRate, quantile(ClaimRate, 0.999))
    ) |>
    dplyr::select(-dplyr::all_of(c("IDpol", "Exposure", "ClaimNb")))


  # ============================ Input data =====================

  withr::with_seed(1, {
    data <- freMTPL2freq |> split_into_train_validate_test()
  })

  # changing factors to characters... this is necessary as bug in original script handles factors incorrectly
  # changing "ClaimRate" to use "ClaimNb"... this is necessary as "ClaimNb" hardcoded in KG script and easier to modify in package script
  # changing "ClaimNb" to round to integer values. This is to avoid warnings in the test environment.
  splits <- data |>
    purrr::modify(.f = function(x) dplyr::rename(x, "ClaimNb" = "ClaimRate")) |>
    purrr::modify(.f = function(x) dplyr::mutate(x, ClaimNb = round(ClaimNb)))

  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    family = "poisson",
    params = list(seed=0, tree_method = "auto")
  )

  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores(splits$test, IBLM)


  # ============================ Karol (og) process =====================

  # IBLM v1.0.1...

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM_testing` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_pinball_score_test

  # model = c("homog", "glm", "iblm"),
  # poisson_deviance = c(0.6821738013621665, 0.6614369861117713, 0.6561672090472287),
  # pinball_score = c(0, 0.03039814078023506, 0.03812311798402079)

  # IBLM v1.0.2... (test re-set following data.matrix() correction)

  # model = c("homog", "glm", "iblm"),
  # poisson_deviance = c(0.6821739935523775, 0.6614371784998352, 0.6557417511577236),
  # pinball_score = c(0, 0.030398131926074545, 0.038747068408471086)

  # IBLM v1.0.3... (test re-set following base_margin use)

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(0.682173993552377, 0.6614371784998351, 0.6560075047435496),
    pinball_score = c(0, 0.03039813192607399, 0.03835749978178882)
  )

  testthat::expect_equal(ps_nu, ps_og)

})





testthat::test_that("rec against Karol paper", {


  # test takes too long for CRAN
  testthat::skip_on_cran()

  # ============================ Input data =====================

  splits <- load_freMTPL2freq() |>
    dplyr::rename(ClaimNb = ClaimRate) |>
    dplyr::select(-Exposure) |>
    split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  # warning are given because of non-integer response vars and a poisson predictor...
  # ...just have to suppress for this test as we cannot change data...

    IBLM <- train_iblm_xgb(
      splits,
      response_var = "ClaimNb",
      family = "quasipoisson",
      # additional param settings required for rec...
      params = list(
        base_score = 0.5,
        objective = "count:poisson",
        seed=0,
        tree_method = "auto"
      ),
      nrounds = 1000,
      verbose = 0,
      early_stopping_rounds = 25
    )


  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores(splits$test, IBLM)


  # ============================ Karol (og) process =====================

  # the following data objects are taken from Karol original script, using the same seed, input and settings

  # For audit, the inputs were constructed in the `https://github.com/IFoA-ADSWP/IBLM_testing` repo
  # The inputs are created in:
  # branch: testing_object_construction
  # script: construct_pinball_score_test

  # Note the results are rounded to a only 2 d.p. because there are minor corrections that mean values
  # will not be the same, however they should be in the similar ballpark

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(1.4195,1.3606, 1.2483),
    pinball_score = c(0.00,4.15,12.06)/100
  )

  ps_nu <- ps_nu |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("poisson_deviance", "pinball_score")),
        function(x) round(x, 4)
      )
    )

  # expect homog and glm to match
  testthat::expect_equal(
    ps_nu |> dplyr::filter(model %in% c("homog", "glm")),
    ps_og |> dplyr::filter(model %in% c("homog", "glm"))
    )

  # expect iblm to have an improved pinball score
  testthat::expect_gt(
    ps_nu |> dplyr::filter(model %in% c("iblm")) |> dplyr::pull(pinball_score),
    ps_og |> dplyr::filter(model %in% c("iblm")) |> dplyr::pull(pinball_score)
  )

  # put hardcoded anchor (against v1.0.3) into iblm pinball score
  testthat::expect_equal(
    ps_nu |> dplyr::filter(model %in% c("iblm")) |> dplyr::pull(pinball_score),
    0.1253
  )

})



testthat::test_that("test against v1.0.3 saved results - mini", {

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    dplyr::mutate(LogExposure = log(Exposure)) |>
    dplyr::select(-Exposure) |>
    split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  # warning are given because of non-integer response vars and a poisson predictor...
  # ...just have to suppress for this test as we cannot change data...

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    family = "poisson",
    # additional param settings required for rec...
    params = list(
      objective = "count:poisson",
      seed=0,
      tree_method = "auto"
    ),
    nrounds = 1000,
    verbose = 0,
    early_stopping_rounds = 25
  )


  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores(splits$test, IBLM)


  # ============================ Anchored to v1.0.3 =====================

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(0.27060354864044134, 0.26491771075722376, 0.25416025239183015),
    pinball_score = c(0, 0.021011690023224783, 0.0607652646509077)
  )

  # expect homog and glm to match
  testthat::expect_equal(ps_nu, ps_og)

})


testthat::test_that("test against v1.0.3 saved results", {


  # test takes too long for CRAN
  testthat::skip_on_cran()

  # ============================ Input data =====================

  splits <- load_freMTPL2freq() |>
    dplyr::rename(ClaimNb = ClaimRate) |>
    dplyr::mutate(LogExposure = log(Exposure)) |>
    dplyr::select(-Exposure) |>
    split_into_train_validate_test(seed = 1)

  # ============================ IBLM package process =====================

  # warning are given because of non-integer response vars and a poisson predictor...
  # ...just have to suppress for this test as we cannot change data...

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    offset_var = "LogExposure",
    family = "quasipoisson",
    # additional param settings required for rec...
    params = list(
      base_score = 0.5,
      objective = "count:poisson",
      seed=0,
      tree_method = "auto"
    ),
    nrounds = 1000,
    verbose = 0,
    early_stopping_rounds = 25
  )


  # `migrate_reference_to_bias = FALSE` for purposes of test as trying to reconile with KG original script
  ps_nu <- get_pinball_scores(splits$test, IBLM)


  # ============================ Anchored to v1.0.3 =====================

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(1.990195839743657, 1.882002346255756, 1.7340033938216008),
    pinball_score = c(0, 0.05436323970099177, 0.12872725427616938)
  )

  # expect homog and glm to match
  testthat::expect_equal(ps_nu, ps_og)

})

testthat::test_that("test error for character fields", {

  # ============================ Input data =====================

  data <- freMTPLmini |> split_into_train_validate_test(seed = 1)

  # get data where categoricals are factors
  splits_fct <- data |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.character), function(field) as.factor(field))))

  # get identical data where categoricals are strings
  splits_chr <- splits_fct |>
    purrr::modify(.f = function(x) x |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), function(field) as.character(field))))

  testthat::expect_error(
    IBLM_chr <- train_iblm_xgb(
      splits_chr,
      response_var = "ClaimNb",
      family = "poisson"
    )
  )


})






