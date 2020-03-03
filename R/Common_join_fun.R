#' These functions join in common external data
#' (ASGS,SEIFA,country,language,basis of admission)
#'
#' @importFrom dplyr "%>%"  left_join mutate if_else rename
#'


#' @export
# Join in postcodes
Postcode_vars <- function(data) {
  if ("postcode" %in% colnames(data) == F) {
    stop("Does your data have the variable 'postcode'?\n
         Spoiler alert: it doesn't.
         Consider getting it.")
  } else{
    data %>%
      left_join(Postcode.match)
  }
}

#' @export
# Join in basis of admission
BOA_var <- function(data) {
  if ("basis_for_admission_code" %in% colnames(data) == F) {
    stop("Does your data have the variable 'basis_for_admission_code'?\n
         Spoiler alert: it doesn't.
         Consider getting it.")
  } else{
    data %>%
      left_join(boa.match)
  }
}

#' @export
# Join in ACARA variables
ACARA_vars <- function(data) {
  if ("latest_secondary_school_code" %in% colnames(data) == F) {
    stop("Does your data have the variable 'latest_secondary_school_code'?\n
         Spoiler alert: it doesn't.
         Consider getting it.")
  } else{
    data %>%
      left_join(ACARA.match)
  }
}

#' @export
# Join in language variables
Language_vars <- function(data,NESB = T) {
  if ("main_language_spoken_code" %in% colnames(data) == F) {
    stop("Does your data have the variable 'main_language_spoken_code'?\n
         Spoiler alert: it doesn't.
         Consider getting it.")
  } else if (NESB == T & "year_of_entry_to_australia" %in% colnames(data) == F & "enrolment_year" %in% colnames(data) == F) {
    stop("Does your data have the variable 'year_of_entry_to_australia' or 'enrolment_year'?\n
         Spoiler alert: it is missing one of those variables.
         Either set 'NESB = F', or come back later when you have the right variables.")
  } else if (NESB == T) {
    data %>%
      left_join(Language.match) %>%
      mutate(NESB = if_else(enrolment_year - year_of_entry_to_australia < 10, "Y", "N"))
  } else if (NESB == F) {
    data %>%
      left_join(Language.match)
  }
}

#' @export
# Join in country variables
Country_vars <- function(data, birth_country = T, home_country = T) {

  if (birth_country == T & home_country == T & "country_of_birth" %in% colnames(data) == F & "country_of_home_residency" %in% colnames(data) == F) {
    stop("Does your data have the variables 'country_of_birth' and 'country_of_home_residency'?\n
         Spoiler alert: it doesn't.
         Come back with both or change country parameter selection to only include the present variables, and then we're back baby!")
  } else if (birth_country == T & home_country == T & "country_of_birth" %in% colnames(data) == T & "country_of_home_residency" %in% colnames(data) == T)  {
    data %>%
      left_join((Country.match %>%
                  rename(birth_broad_region = broad_region,
                         birth_narrow_region = narrow_region)), by = c("country_of_birth" = "bo_country")) %>%
      left_join((Country.match %>%
                   rename(home_broad_region = broad_region,
                          home_narrow_region = narrow_region)), by = c("country_of_home_residency" = "bo_country"))
  } else if (birth_country == T & "country_of_birth" %in% colnames(data) == T) {
    data %>%
      left_join((Country.match %>%
                   rename(birth_broad_region = broad_region,
                          birth_narrow_region = narrow_region)), by = c("country_of_birth" = "bo_country"))
  } else if (home_country == T & "country_of_home_residency" %in% colnames(data) == T) {
    data %>%
      left_join((Country.match %>%
                   rename(home_broad_region = broad_region,
                          home_narrow_region = narrow_region)), by = c("country_of_home_residency" = "bo_country"))
  } else {
    warning("You have excluded all parameters relevant to the function: 'Country_vars' or you don't have the right variables.")
  }
}



