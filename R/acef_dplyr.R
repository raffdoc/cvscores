#' ACEF Score Calculation with dplyr environment
#' @description This function calculates the ACEF score based on age, creatinine, and ejection fraction using dplyr.
#' @param data A data frame containing the columns: age, creatinine, and ejection_fraction.
#' @param ... Additional arguments passed to dplyr functions.
#' @return A data frame with an additional column for ACEF score.

require(dplyr)
acef = function(data, ...) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!all(c("age", "creatinine", "ejection_fraction") %in% names(data))) {
    stop("Data frame must contain columns: age, creatinine, and ejection_fraction.")
  }
  if (!all(sapply(data$age, is.numeric)) || any(data$age < 0)) {
    stop("Age must be a non-negative numeric value.")
  }
  if (!all(sapply(data$creatinine, is.numeric)) || any(data$creatinine < 0)) {
    stop("Creatinine must be a non-negative numeric value.")
  }
  if (!all(sapply(data$ejection_fraction, is.numeric)) || any(data$ejection_fraction < 0) || any(data$ejection_fraction > 100)) {
    stop("Ejection fraction must be a numeric value between 0 and 100.")
  }
  data |> dplyr::mutate(
    acef = age/ejection_fraction + dplyr::if_else(creatinine >= 2, 1,0)
  ) |>
    dplyr::select(acef)
}
