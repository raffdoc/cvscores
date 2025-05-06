#' The Cockcroft and Gault formula (1973)
#' Abbreviations/ Units CCr (creatinine clearance) = mL/minute, Age = years
#' Weight = kg, SCr (serum creatinine) = mg/dL
#' CCr={((140–age) x weight)/(72xSCr)}x 0.85 (if female)
#' @title Calculate creatinine clearance using the Cockcroft-Gault formula
#' @description
#' A short description...
#' This function calculates the creatinine clearance (CCr) using the Cockcroft-Gault formula.
#' @param age Age in years
#' @param weight Weight in kg
#' @param scr Serum creatinine in mg/dL
#' @return a data frame with the calculated creatinine clearance (CCr)
#' @export scr_clear

scr_clear <- function(data, ...){
  # Check if the input data is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  # Check if the required columns are present in the data frame
  if (!all(c("age", "weight", "scr", "female") %in% names(data))) {
    stop("Data frame must contain columns: age, weight, serum creatinine, and if female")
  }
  # Check if the columns are numeric
  if (!is.numeric(data$age) || !is.numeric(data$weight) || !is.numeric(data$scr)) {
    stop("Columns age, weight, and serum creatinine must be numeric.")
  }
  # Check if the columns are within valid ranges
  if (any(data$age < 0 | data$age > 120)) {
    stop("Age must be between 0 and 120.")
  }
  if (any(data$weight < 0 | data$weight > 300)) {
    stop("Weight must be greater than 0 and less then 300.")
  }
  if (any(data$scr < 0 | data$scr > 20)) {
    stop("Serum creatinine must be greater than 0 and less then 20.")
  }
  if (!all(sapply(data$female, is.logical))) {
    stop("female must be a logical value (TRUE or FALSE).")
  }
  data |>
    mutate( ccr = ((140 - age)*weight)/(72*scr)*if_else(female, 0.85, 1)) |>
    dplyr::select(ccr) |>
    mutate(ccr = round(ccr, 0))
}
