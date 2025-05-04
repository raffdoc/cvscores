#' ACEF II = Age/EF + 2.0 (if serum creatinine > 2.0 mg/dL) + 3.0 (if emergency surgery) + 0.2 HCT points below 36%
#' @title ACEF II Score Calculator
#' @description This function calculates the ACEF II score based on age, creatinine, emergency operation, HCT (hematocrit bellwo 36 %) and ejection fraction.
#' @param age Age of the patient in years.
#' @param creatinine Serum creatinine level in mg/dL.
#' @param ejection_fraction Ejection fraction percentage
#' @param emergency_operation Logical value indicating if the operation is an emergency.
#' @param hct Hematocrit level in percentage below 36 %, positive number.
#' @return ACEF score in numbers.
#' @examples
#' acef_score(age = 65, creatinine = 1.2, ejection_fraction = 55)
#' @export
acef_ii_score <- function(
    age = 60,
    creatinine =2,
    ejection_fraction = 60,
    emergency_operation = FALSE,
    hct = 36) {
  # Validate inputs
  if (!is.numeric(age) || age <= 0) {
    stop("Age must be a non-negative, non zero numeric value.")
  }
  if (!is.numeric(creatinine) || creatinine <= 0) {
    stop("Creatinine must be a non-negative, non zero numeric value.")
  }
  if (!is.numeric(ejection_fraction) || ejection_fraction <= 10 || ejection_fraction > 90) {
    stop("Ejection fraction must be a numeric value between 10 and 90.")
  }
  if (!is.logical(emergency_operation)) {
    stop("Emergency operation must be a logical value (TRUE or FALSE).")
  }
  if (!is.numeric(hct) || hct <= 0) {
    stop("HCT must be a non-negative, non zero numeric value.")
  }
  # Calculate ACEF II score
  acef_ii_score <- age/ejection_fraction + 2*ifelse(creatinine > 2, 1, 0) + 3*ifelse(emergency_operation, 1, 0) + 0.2*ifelse(hct >= 36, 0, abs(hct - 36))
  return(acef_ii_score)
}
