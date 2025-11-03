#' French Motor Insurance Claims Dataset
#'
#' @description
#' A dataset containing information about French motor insurance policies and claims,
#' commonly used for actuarial modeling and risk assessment studies.
#'
#' This is a "mini" subset of the CASdatasets `freMTPL2freq` data, with some manipulation (see details)
#'
#'
#' @format A data frame with 25,000 rows and 8 variables:
#' \describe{
#'   \item{ClaimRate}{Number of claims made, at an annualised rate, rounded (integer)}
#'   \item{VehPower}{Vehicle power rating or engine horsepower category (integer)}
#'   \item{VehAge}{Age of the vehicle in years (integer)}
#'   \item{DrivAge}{Age of the driver in years (integer)}
#'   \item{BonusMalus}{Bonus-malus coefficient, a rating factor used in French
#'     insurance where lower values indicate better driving records (integer)}
#'   \item{VehBrand}{Vehicle brand/manufacturer code (factor with levels like B6, B12, etc.)}
#'   \item{VehGas}{Type of fuel used by the vehicle (factor with levels: Regular, Diesel)}
#'   \item{Area}{Area classification where the policy holder resides
#'     (factor with levels A through F)}
#' }
#'
#' @details
#' The dataset is a random sample of 50,000 records from `freMTPL2freq` from the `CASdatasets` pacakge.
#' Other modifications applied are:
#'   \itemize{
#'     \item \code{ClaimRate}: Converted to ClaimNb per Exposure, winsorized at the 99.9th percentile, and rounded.
#'     \item \code{VehAge}: Ceiling of 50 years applied
#'     \item Dropped columns: Region, Density, Exposure, ClaimNb, IDpol
#'   }
#'
#' @source
#' ['https://github.com/dutangc/CASdatasets/raw/c49cbbb37235fc49616cac8ccac32e1491cdc619/data/freMTPL2freq.rda']
#'
#' @examples
#' head(freMTPLmini)
#'
#' @keywords datasets insurance actuarial
"freMTPLmini"
