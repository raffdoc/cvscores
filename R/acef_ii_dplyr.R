#' ACEF II Score Calculation with dplyr environment
#' @description This function calculates the ACEF II score based on age, creatinine, and ejection fraction, emerjency state of surger and hematocrit valus if inferor to 36 using dplyr.
#' @param data A data frame containing the columns: age, creatinine, and ejection_fraction, emerjency_operation, hct.
#' @name acef_ii_dplyr
#' @param ... Additional arguments passed to dplyr functions.
#' @return A data frame with an additional column for ACEF score.

require(dplyr)
acef_ii = function(data, ...) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!all(c("age", "creatinine", "ejection_fraction", "emergency_operation", "hct") %in% names(data))) {
    stop("Data frame must contain columns: age, creatinine,ejection_fraction, emerjency_state and hct")
  }
  if (!all(sapply(data$age, is.numeric)) || any(data$age < 0)) {
    stop("Age must be a non-negative numeric value.")
  }
  if (!all(sapply(data$creatinine, is.numeric)) || any(data$creatinine < 0)) {
    stop("Creatinine must be a non-negative numeric value.")
  }
  if (!all(sapply(data$ejection_fraction, is.numeric)) || any(data$ejection_fraction < 10) || any(data$ejection_fraction > 90)) {
    stop("Ejection fraction must be a numeric value between 10 and 90.")
  }
  if (!all(sapply(data$emergency_operation, is.logical))) {
    stop("Emergency operation must be a logical value (TRUE or FALSE).")
  }
  if (!all(sapply(data$hct, is.numeric)) || any(data$hct < 0)) {
    stop("HCT must be a non-negative numeric value.")
  }
  data |> dplyr::mutate(
    acef_ii = age/ejection_fraction + 2*dplyr::if_else(creatinine >= 2, 1,0) + 3*dplyr::if_else(emergency_operation, 1, 0) + 0.2*dplyr::if_else(hct >= 36, 0, abs(hct - 36))
  ) |>
    dplyr::mutate(acef_ii = round(acef_ii, 3)) |>
    dplyr::select(acef_ii)
}

