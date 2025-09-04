#' French Motor Third-Party Liability (MTPL) Insurance Frequency Data
#'
#' This dataset contains automobile insurance policies from France with
#' exposure and number of claims. It is a benchmark dataset often used for
#' actuarial modeling of claim frequency (e.g., generalized linear models,
#' credibility theory, and machine learning applications).
#'
#' @format A data frame with 678,013 observations and 12 variables:
#' \describe{
#'   \item{PolicyID}{Unique policy identifier}
#'   \item{Exposure}{Exposure period in years (≤ 1)}
#'   \item{ClaimNb}{Number of claims reported for the policy}
#'   \item{Power}{Car power}
#'   \item{CarAge}{Age of the car (in years)}
#'   \item{DriverAge}{Age of the driver (in years)}
#'   \item{Brand}{Categorical variable indicating brand category}
#'   \item{Gas}{Fuel type (e.g., Gasoline, Diesel)}
#'   \item{Region}{Region code of the policyholder}
#'   \item{Density}{Population density of the policyholder’s area}
#'   \item{Area}{Urban/rural indicator of the insured’s area}
#'   \item{...}{Additional categorical covariates depending on dataset version}
#' }
#'
#' @details
#' The dataset is a subset of French motor third-party liability (MTPL)
#' insurance data. It is typically used together with \code{\link{freMTPL2sev}},
#' which contains claim severity information, to model pure premiums as the
#' product of frequency and severity.
#'
#' @source CASdatasets package \url{https://github.com/dutangc/CASdatasets/blob/master/data/freMTPL2freq.rda}
#'
#' @examples
#' data(freMTPL2freq)
#' str(freMTPL2freq)
#' summary(freMTPL2freq$ClaimNb)
"freMTPL2freq"
