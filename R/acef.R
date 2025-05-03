# age creatinine ejection fraction score calculator

#' @title ACEF Score Calculator
#' @description This function calculates the ACEF score based on age, creatinine, and ejection fraction.
#' @param age Age of the patient in years.
#' @param creatinine Serum creatinine level in mg/dL.
#' @param ejection_fraction Ejection fraction percentage.
#' @return A list containing the ACEF score and the risk category.
#' @examples
#' acef_score(age = 65, creatinine = 1.2, ejection_fraction = 55)
#' @export
acef_score <- function(age, creatinine, ejection_fraction) {
  # Validate inputs
  if (!is.numeric(age) || age < 0) {
    stop("Age must be a non-negative numeric value.")
  }
  if (!is.numeric(creatinine) || creatinine < 0) {
    stop("Creatinine must be a non-negative numeric value.")
  }
  if (!is.numeric(ejection_fraction) || ejection_fraction < 0 || ejection_fraction > 100) {
    stop("Ejection fraction must be a numeric value between 0 and 100.")
  }

  # Calculate ACEF score
  acef_score <- age/ejection_fraction + 1*ifelse(creatinine>= 2, 0)

  # Determine risk category
  risk_category <- ifelse(acef_score < 1, "Low Risk",
                          ifelse(acef_score < 2, "Moderate Risk", "High Risk"))

  # Return results
  return(list(score = acef_score, risk_category = risk_category))
}
