testthat::test_that("test against anchored values - full set", {

  # test takes too long for CRAN
  testthat::skip_on_cran()


  # ============================ Input data =====================

  # download the correct version of freMTPL2freq dataset to complete the rec

  splits <- load_freMTPL2freq() |>
    head(600000) |>
    dplyr::mutate(train_validate_test = rep(c("train", "train", "train", "train", "validate", "test"), times = 100000)) |>
    dplyr::mutate(LogExposure = log(Exposure)) |>
    dplyr::select(-Exposure) |>
    split(~train_validate_test) |>
    purrr::map(function(x) x |> dplyr::select(-dplyr::all_of("train_validate_test")))


  # ============================ IBLM package process =====================

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    family = "quasipoisson",
    offset_var = "LogExposure",
    params = list(
      seed=0,
      tree_method = "auto"
      )
  )

  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test)

  # ============================ Anchored GLM fit to v1.0.3 =====================

  coeff_nu <- IBLM$glm_model$coefficients

  coeff_og <- c(
    `(Intercept)` = -3.851483920923259,
    AreaB = 0.042648846259151066,
    AreaC = 0.08505350181726866,
    AreaD = 0.16564441796292947,
    AreaE = 0.1843695588565849,
    AreaF = 0.12354950288550583,
    VehPower = 0.0037145883778674574,
    VehAge = -0.03837068962800157,
    DrivAge = 0.007128935065930836,
    BonusMalus = 0.02298288012951094,
    VehBrandB10 = 0.039887162312297414,
    VehBrandB11 = 0.10482012480591664,
    VehBrandB12 = 0.4248144345766385,
    VehBrandB13 = 0.0060155379904895025,
    VehBrandB14 = -0.3071944968446458,
    VehBrandB2 = -0.007145489039407805,
    VehBrandB3 = 0.005557653942387832,
    VehBrandB4 = -0.07041126112509567,
    VehBrandB5 = 0.06250694199504907,
    VehBrandB6 = -0.0782696047561491,
    VehGasRegular = 0.04000354310196918,
    Density = -1.758130038873787e-07,
    RegionR21 = 0.017817170322585136,
    RegionR22 = 0.10349135738699143,
    RegionR23 = -0.11955259358986396,
    RegionR24 = -0.05011718534750852,
    RegionR25 = -0.15297694218320854,
    RegionR26 = -0.0952269093060245,
    RegionR31 = -0.1763956979224515,
    RegionR41 = -0.3479035382751202,
    RegionR42 = -0.0805382994353449,
    RegionR43 = -0.2430524312147626,
    RegionR52 = -0.11843311917387073,
    RegionR53 = -0.014450207115500237,
    RegionR54 = -0.13514946846264897,
    RegionR72 = -0.16473700699844002,
    RegionR73 = -0.16800739555096506,
    RegionR74 = 0.10243969524174937,
    RegionR82 = -0.011231450012576622,
    RegionR83 = -0.350868002823383,
    RegionR91 = -0.1058852541156366,
    RegionR93 = -0.07539522043282068,
    RegionR94 = 0.06405613318450151
  )

  testthat::expect_equal(coeff_nu, coeff_og)

  # ============================ Anchored Booster fit to v1.0.3 =====================

  booster_score_nu <- IBLM$booster_model |> xgboost::xgb.attr("best_score")
  booster_score_og <- 0.21384179890905866

  booster_iteration_nu <- IBLM$booster_model |> xgboost::xgb.attr("best_iteration")
  booster_iteration_og <- 71

  testthat::expect_equal(booster_score_nu, booster_score_og)
  testthat::expect_equal(booster_iteration_nu, booster_iteration_og)

  # ============================ Anchored Corrections fit to v1.0.3 =====================


  shap_wide_colsums <- c(
    bias = 4098.064910554031,
    VehPower = -194.64714136305133,
    VehAge = -2963.882501223408,
    DrivAge = -38.87871377804541,
    BonusMalus = -34.95054096970523,
    Density = -18.756607099348223,
    AreaA = 0,
    AreaB = -26.190764646078605,
    AreaC = 116.53240467864089,
    AreaD = -291.6933306899882,
    AreaE = -127.952601032237,
    AreaF = 62.76755008449254,
    VehBrandB1 = 0,
    VehBrandB10 = 1.203411410228,
    VehBrandB11 = -56.56196885902318,
    VehBrandB12 = -5062.473561759209,
    VehBrandB13 = -11.553639184479835,
    VehBrandB14 = 0.09185002319281921,
    VehBrandB2 = 493.122788999628,
    VehBrandB3 = 213.43844020326287,
    VehBrandB4 = 229.5069665168412,
    VehBrandB5 = 69.67228306233301,
    VehBrandB6 = 124.96899946473422,
    VehGasDiesel = 0,
    VehGasRegular = -723.2834576012101,
    RegionR11 = 0,
    RegionR21 = -7.1795986702200025,
    RegionR22 = -55.092336681787856,
    RegionR23 = -9.56001206219662,
    RegionR24 = 83.90984205347922,
    RegionR25 = -29.874381124565843,
    RegionR26 = -32.74438238122093,
    RegionR31 = 101.61301778642519,
    RegionR41 = -7.068528137693647,
    RegionR42 = -25.483009991818108,
    RegionR43 = -14.285656785301398,
    RegionR52 = -23.371059049051837,
    RegionR53 = -48.30043430585465,
    RegionR54 = -77.12399033198017,
    RegionR72 = -162.46765506524025,
    RegionR73 = -201.59179286615108,
    RegionR74 = -57.48065675527323,
    RegionR82 = -201.29628922400298,
    RegionR83 = 21.925616116262972,
    RegionR91 = -475.31859824109415,
    RegionR93 = -175.90376081979412,
    RegionR94 = 18.73199634393677
  )

  data_beta_coeff_colsums <- c(
    bias = -381050.3271817722,
    Area = 10202.876336090865,
    VehPower = 176.81169642369434,
    VehAge = -6800.951464023562,
    DrivAge = 674.0147928150382,
    BonusMalus = 2263.3374719813887,
    VehBrand = 2832.5536287052632,
    VehGas = 1341.1393877190117,
    Density = -18.77418839973696,
    Region = -8498.190586863284
  )

  testthat::expect_equal(
    explainer_nu$beta_corrections |> colSums(),
    shap_wide_colsums,
    tolerance = 1E-6
  )

  testthat::expect_equal(
    explainer_nu$data_beta_coeff  |> colSums(),
    data_beta_coeff_colsums,
    tolerance = 1E-6
  )

  # ============================ Anchored IBLM scores v1.0.3 =====================

  ps_nu <- get_pinball_scores(splits$test, IBLM)

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(0.3526789116671274, 0.3367941478961098, 0.3181535743351558),
    pinball_score = c(0, 0.0450402993928094, 0.09789453293016293)
  )

  testthat::expect_equal(ps_nu, ps_og)
})





testthat::test_that("test against v1.0.3 saved results - mini", {

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    dplyr::mutate(train_validate_test = rep(c("train", "train", "train", "validate", "test"), times = 5000)) |>
    dplyr::mutate(LogExposure = log(Exposure)) |>
    dplyr::select(-Exposure) |>
    split(~train_validate_test) |>
    purrr::map(function(x) x |> dplyr::select(-dplyr::all_of("train_validate_test")))

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
      tree_method = "hist",
      nthread = 1
    ),
    nrounds = 1000,
    verbose = 0,
    early_stopping_rounds = 25
  )


  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test)

  # ============================ Anchored GLM fit to v1.0.3 =====================

  coeff_nu <- IBLM$glm_model$coefficients

  coeff_og <- c(
    `(Intercept)` = -3.8594708173404872,
    AreaB = -0.11190174836580087,
    AreaC = -0.444917144255031,
    AreaD = -0.2484358290731008,
    AreaE = -0.1478457642136018,
    BonusMalus = 0.021559157295387592,
    DrivAge = 0.008528529315074463,
    VehAge = -0.042704795183391375,
    VehBrandB12 = 0.0011925765912662008,
    VehBrandB2 = -0.023898926969100273,
    VehBrandB3 = 0.07594077341978828,
    VehBrandB4 = -0.26545705985518625,
    VehBrandB5 = 0.29149098741982354,
    VehBrandB6 = -0.004878175985223516,
    VehPower = 0.05894266359794995
  )

  testthat::expect_equal(coeff_nu, coeff_og)

  # ============================ Anchored Booster fit to v1.0.3 =====================

  booster_score_nu <- IBLM$booster_model |> xgboost::xgb.attr("best_score")
  booster_score_og <- 0.18048168787918548

  booster_iteration_nu <- IBLM$booster_model |> xgboost::xgb.attr("best_iteration")
  booster_iteration_og <- 13

  testthat::expect_equal(booster_score_nu, booster_score_og)
  testthat::expect_equal(booster_iteration_nu, booster_iteration_og)

  # ============================ Anchored Corrections fit to v1.0.3 =====================


  shap_wide_colsums <- c(
    bias = 48.13453136815224,
    BonusMalus = -0.20176153548366793,
    DrivAge = -5.775991371225044,
    VehAge = -173.63288308576503,
    VehPower = 5.272630189453419,
    AreaA = 0,
    AreaB = 10.82191365386825,
    AreaC = 27.738016427087132,
    AreaD = -32.716441521886736,
    AreaE = -1.529093350225594,
    VehBrandB1 = 0,
    VehBrandB12 = -197.57205333554884,
    VehBrandB2 = -21.20856424618978,
    VehBrandB3 = -9.44511840166524,
    VehBrandB4 = -0.21352318883873522,
    VehBrandB5 = -6.173628297285177,
    VehBrandB6 = -20.332186447805725
  )

  data_beta_coeff_colsums <- c(
    bias = -19249.219555334283,
    Area = -1289.2881240635083,
    BonusMalus = 107.5940249414543,
    DrivAge = 36.86665520414727,
    VehAge = -387.15685900272194,
    VehBrand = -237.55534919289332,
    VehPower = 299.98594817920315
  )

  testthat::expect_equal(
    explainer_nu$beta_corrections |> colSums(),
    shap_wide_colsums,
    tolerance = 1E-6
  )

  testthat::expect_equal(
    explainer_nu$data_beta_coeff  |> colSums(),
    data_beta_coeff_colsums,
    tolerance = 1E-6
  )

  # ============================ Anchored IBLM scores v1.0.3 =====================

  ps_nu <- get_pinball_scores(splits$test, IBLM)

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    poisson_deviance = c(0.27083016955873457, 0.26744791989655214, 0.25633025817301236),
    pinball_score = c(0, 0.012488452330451705, 0.05353875976722611)
  )

  testthat::expect_equal(ps_nu, ps_og)

})








testthat::test_that("test against v1.0.3 saved results - mini gaussian", {

  # ============================ Input data =====================

  splits <- freMTPLmini |>
    dplyr::mutate(train_validate_test = rep(c("train", "train", "train", "validate", "test"), times = 5000)) |>
    dplyr::mutate(ClaimNb = dplyr::if_else(dplyr::row_number() %% 2 == 1, 1, -1) * ClaimNb + (DrivAge * VehAge)/1000) |>
    dplyr::select(-Exposure) |>
    split(~train_validate_test) |>
    purrr::map(function(x) x |> dplyr::select(-dplyr::all_of("train_validate_test")))

  # ============================ IBLM package process =====================

  # warning are given because of non-integer response vars and a poisson predictor...
  # ...just have to suppress for this test as we cannot change data...

  IBLM <- train_iblm_xgb(
    splits,
    response_var = "ClaimNb",
    family = "gaussian",
    # additional param settings required for rec...
    params = list(
      seed = 42,
      tree_method      = "hist",
      nthread          = 1,
      eta              = 0.05,
      lambda           = 2.0
    ),
    nrounds = 1000,
    verbose = 0,
    early_stopping_rounds = 100
  )

  explainer_nu <- explain_iblm(iblm_model = IBLM, data = splits$test)

  # ============================ Anchored GLM fit to v1.0.3 =====================

  coeff_nu <- IBLM$glm_model$coefficients

  coeff_og <- c(
    `(Intercept)` = -0.2025413916786566,
    AreaB = -0.023471030505357086,
    AreaC = -0.012140587927466915,
    AreaD = -0.010996950580295135,
    AreaE = -0.007953646029726714,
    BonusMalus = -0.0002338431728971184,
    DrivAge = 0.0058192444991261374,
    VehAge = 0.04071492793289529,
    VehBrandB12 = -0.004293462462660974,
    VehBrandB2 = -0.002182175681227498,
    VehBrandB3 = 0.002837980312847773,
    VehBrandB4 = 9.35937469434587e-08,
    VehBrandB5 = 0.007794208317519717,
    VehBrandB6 = -0.002785564379060474,
    VehPower = -0.0020011279950285315
  )

  testthat::expect_equal(coeff_nu, coeff_og)

  # ============================ Anchored Booster fit to v1.0.3 =====================

  booster_score_nu <- IBLM$booster_model |> xgboost::xgb.attr("best_score")
  booster_score_og <- 0.24031355492261605

  booster_iteration_nu <- IBLM$booster_model |> xgboost::xgb.attr("best_iteration")
  booster_iteration_og <- 99

  testthat::expect_equal(booster_score_nu, booster_score_og)
  testthat::expect_equal(booster_iteration_nu, booster_iteration_og)

  # ============================ Anchored Corrections fit to v1.0.3 =====================


  shap_wide_colsums <- c(
    bias = 4.142185995416639,
    BonusMalus = -0.024477002752042034,
    DrivAge = -0.1165565642249349,
    VehAge = -4.385297857851767,
    VehPower = -0.13806569857106246,
    AreaA = 0,
    AreaB = 0.27601326090530165,
    AreaC = -0.18794983586803937,
    AreaD = -0.5743794106633686,
    AreaE = -0.006268041955934223,
    VehBrandB1 = 0,
    VehBrandB12 = 1.3853131091244677,
    VehBrandB2 = -1.196247227967433,
    VehBrandB3 = -0.35058218140443387,
    VehBrandB4 = -0.15855255764290632,
    VehBrandB5 = 0.29630929163431574,
    VehBrandB6 = -0.013268982924955708
  )

  data_beta_coeff_colsums <- c(
    bias = -1008.5647723978664,
    Area = -54.9651351375618,
    BonusMalus = -1.1936928672376341,
    DrivAge = 28.97966593140575,
    VehAge = 199.18934180662467,
    VehBrand = -6.515258712434496,
    VehPower = -10.14370567371372
  )

  testthat::expect_equal(
    explainer_nu$beta_corrections |> colSums(),
    shap_wide_colsums,
    tolerance = 1E-6
  )

  testthat::expect_equal(
    explainer_nu$data_beta_coeff  |> colSums(),
    data_beta_coeff_colsums,
    tolerance = 1E-6
  )

  # ============================ Anchored IBLM scores v1.0.3 =====================

  ps_nu <- get_pinball_scores(splits$test, IBLM)

  ps_og <- data.frame(
    model = c("homog", "glm", "iblm"),
    gaussian_deviance = c(0.09755753957408, 0.04795265336814491, 0.044498189422960646),
    pinball_score = c(0, 0.508467991530965, 0.5438774940693221)
  )

  testthat::expect_equal(ps_nu, ps_og)

})




