#' These functions perform common CHEEDR cleaning jobs when working
#' with La Trobe University enrolment data
#'
#' @importFrom dplyr mutate if_else "%>%"

#' @export
# Clean up indigenous variable into a dichotomous variable
Indigenous_var <- function(data) {
  if ("aboriginal_or_torres_strait_islander_code" %in% colnames(data) == F) {
    stop("Does your data have the variable 'aboriginal_or_torres_strait_islander_code'?\n
         Spoiler alert: It doesn't.\n
         Consider getting it.")
  }
  data %>%
    mutate(Indigenous = if_else(str_detect(aboriginal_or_torres_strait_islander_code,"B|Y|T"),
                                "Y","N"))
}

#' @export
# Trim off rogue ATAR values - no lower than 10 and no higher than 100
ATAR_trim <- function(data) {
  if ("latest_atar" %in% colnames(data) == F) {
    stop("Does your data have the variable 'latest_atar'?\n
         Spoiler alert: It doesn't.\n
         Consider getting it.")
  }
  data %>%
    mutate(atar = if_else(latest_atar > 10 & latest_atar < 100, latest_atar,NaN))
}



# Course commencement year

