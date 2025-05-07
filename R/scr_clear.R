#' The Cockcroft and Gault formula (1973)
#' @description
#' This function calculates the creatinine clearance (CCr) using the Cockcroft-Gault formula.
#' @param data
#' @param ... Additional arguments (not used).
#' @return a data frame with the calculated creatinine clearance (CCr)
#' @export scr_clear

scr_clear <- function(data, ...){
  # Check if the input data is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  # Check if the required columns are present in the data frame
  if (!all(c("age", "weight", "scr", "female", "dialysis") %in% names(data))) {
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
  if (!all(sapply(data$dialysis, is.logical))) {
    stop("female must be a logical value (TRUE or FALSE).")
  }
  data |>
    dplyr::mutate(ccr = if_else(dialysis, 999, ((140 - age)*weight)/(72*scr)*dplyr::if_else(female, 0.85, 1))) |>
    dplyr::select(ccr) |>
    dplyr::mutate(ccr = round(ccr, 0))
}
