
#' French Motor Insurance Claims Dataset
#'
#' A dataset containing information about French motor insurance policies and claims,
#' commonly used for actuarial modeling and risk assessment studies.
#'
#' @format A data frame with 677,991 rows and 10 variables:
#' \describe{
#'   \item{ClaimRate}{Number of claims made, at an annualised rate (numeric)}
#'   \item{VehPower}{Vehicle power rating or engine horsepower category (integer)}
#'   \item{VehAge}{Age of the vehicle in years (integer)}
#'   \item{DrivAge}{Age of the driver in years (integer)}
#'   \item{BonusMalus}{Bonus-malus coefficient, a rating factor used in French
#'     insurance where lower values indicate better driving records (integer)}
#'   \item{VehBrand}{Vehicle brand/manufacturer code (factor with levels like B6, B12, etc.)}
#'   \item{VehGas}{Type of fuel used by the vehicle (factor with levels: Regular, Diesel)}
#'   \item{Area}{Area classification where the policy holder resides
#'     (factor with levels A through F)}
#'   \item{Density}{Population density of the area where the policy holder lives
#'     (integer, inhabitants per square kilometer)}
#'   \item{Region}{French administrative region where the policy holder resides
#'     (factor with regions like Rhone-Alpes, Picardie, Aquitaine, etc.)}
#' }
#'
#' @details
#' This dataset is frequently used in actuarial science and insurance analytics
#' for modeling claim frequency and severity. The ClaimRate variable represents
#' the response (i.e. number of claims, annualised) while the other variables serve as
#' potential predictors for risk assessment.
#'
#' The BonusMalus system is a characteristic feature of French motor insurance,
#' where coefficients typically range from 50 (best) to over 100 (worst),
#' reflecting the policyholder's claims history.
#'
#' @source
#' ['https://github.com/dutangc/CASdatasets/blob/master/data/freMTPL2freq.rda']
#'
#' @examples
#' \dontrun{
#' head(freMTPL2freq)
#' }
#'
#' @keywords datasets insurance actuarial
"freMTPL2freq"
