#' Common filter operations
#' @export
no_zero_units <- function(data) {

  if ("subject_credit_value" %in% colnames(data) == F) {
    warning("Your data is missing the variable 'subject_credit_value'.\n
            Zero credit point subjects will not be filtered out.")

    data

  }
  data %>%
    dplyr::filter(subject_credit_value > 0)

}


#'
#' @export
no_offshore <- function(data) {

  if ("course_on_off_shore" %in% colnames(data) == F) {
    warning("Your data is missing the variable 'course_on_off_shore'.\n
            Offshore study will not filtered out of your data.")

    data

  }
  else {
  data %>%
    dplyr::filter(course_on_off_shore == "On-shore")
  }
}


#'
#' @export
no_third_party <- function(data) {

  if ("course_liability_category" %in% colnames(data) == F) {

    warning("Your data is missing the variable 'course_liability_category'.\n
            Partner providers will not filtered out of your data.")

    data

  }
  else {
  data %>%
    dplyr::filter(stringr::str_detect(course_liability_category,"HECS|International|Research|Employer|Extension"))
}
}


#'
#' @export
no_research <- function(data, honours = T) {

  if ("govt_course_type" %in% colnames(data) == F) {
    warning("Your data is missing the variable 'govt_course_type'.\n
            Research courses will not be filtered out of your data.")

    data

  }
  if (honours == F) {

    data %>%
      dplyr::filter(!stringr::str_detect(govt_course_type,"Doctorate|research"))
  }
  else {
    data %>%
      dplyr::filter(!stringr::str_detect(govt_course_type,"Doctorate|research|Honours"))
  }
  }



#'
#' @export
only_bachelor <- function(data, honours = T) {

  if ("govt_course_type" %in% colnames(data) == F) {
    warning("Your data is missing the variable 'govt_course_type'.\n
            Bachelor courses will not be isolated in your data.")

    data
  }
  if (honours == F) {
    data %>%
      dplyr::filter(stringr::str_detect(govt_course_type,"Bachelor's Pass|Bachelor's Graduate"))
  }
  else {
    data %>%
      dplyr::filter(stringr::str_detect(govt_course_type,"Bachelor"))
  }
  }


