#' EuroSCORE II
#' @description This function calculates the EuroSCORE II risk score for patients undergoing cardiac surgery.
#' It uses the dplyr package for data manipulation.
#' @param data A data frame containing the required variables.
#' @param ... Additional arguments (not used).
#' @return A data frame with the calculated EuroSCORE II risk score.
#' @export

es_ii <- function(data, ...){
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!all(c("age",
             "female",
             "nyha",
             "ccs4",
             "iddm",
             "eca",
             "cpd",
             "nm_mob",
             "redo",
             "renal_dysfunc",
             "active_endo",
             "critical",
             "lv_func",
             "recent_mi",
             "spap",
             "urgency",
             "proc_weight",
             "thoracic_aorta"
      ) %in% names(data))) {
    stop("Data frame must contain columns:
    age,nyha,ccs4,iddm,eca,cpd,nm_mob,
    redo, renal_dysfunc, active_endo,
    critical, lv_func, recent_mi, spap,
    urgency, proc_weight, thoracic_aorta")
  }
    if (!all(sapply(data$age, is.numeric)) || any(data$age < 0)) {
      stop("Age must be a non-negative numeric value.")
    }
  data |> dplyr::mutate(
    log_odds = -5.324537 + # coefficent b0
    0.0285181*dplyr::if_else(age <= 60, 1, (age - 59)) + # age
    0.2196434*female +
    dplyr::if_else(nyha ==1, 0,
    dplyr::if_else(nyha == 2, 0.1070545,
    dplyr::if_else(nyha ==3,0.2958358, 0.5597929))) + # nyha calss
    0.2226147*ccs4 + # ccs4
    0.3542749*iddm +
    0.5360268*eca +
    0.1886564*cpd +
    0.2407818*nm_mob +
    1.118599*redo +
    dplyr::if_else(renal_dysfunc > 85,0,
    dplyr::if_else(renal_dysfunc >50,0.303553,
    dplyr::if_else(renal_dysfunc <50, 0.8592256, 0.6421508))) +
    0.6194522*active_endo +
    1.086517*critical +
    dplyr::if_else(lv_func > 50, 0,
    dplyr::if_else(lv_func > 30, 0.3150652,
    dplyr::if_else(lv_func > 20, 0.8084096, 0.9346919))) +
    0.1528943*recent_mi +
    dplyr::if_else(spap < 31, 0,
    dplyr::if_else(spap < 55, 0.1788899,0.3491475)) +
    dplyr::if_else(urgency %in% 'urgent', 0.3174673,
    dplyr::if_else(urgency %in% 'emergency', 0.7039121,
    dplyr::if_else(urgency %in% 'salvage', 1.362947,0))) +
    dplyr::if_else(proc_weight %in% "single-non-CABG", 0.0062118,
    dplyr::if_else(proc_weight %in% "2-procedures", 0.5521478,
    dplyr::if_else(proc_weight %in% "3-procedures", 0.9724533, 0))) +
    0.6527205*thoracic_aorta
  ) |>
  dplyr::mutate(
    es_ii = round(exp(log_odds)/(1 + exp(log_odds)),4)*100
  ) |>
  dplyr::select(es_ii)

}
