#'
#'
#'
#' \code{Postcode_vars} returns new columns to the df joined by 'postcode', including ASGS and SEIFA variables for 2011 and 2016.
#'
#' @importFrom dplyr "%>%"  left_join mutate if_else rename
#'
#' @export
postcode_vars <- function(data, first_postcode = F) {    # need an argument for whether to use the first, or enrolment year, postcode.
  if ("postcode" %in% colnames(data) == F & first_postcode == F) {
    stop("Your data is missing the variable 'postcode'.
         Did you mean 'first_postcode'?
         If so, change the function argument to 'first_postcode = T'.")
  }
  if ("first_postcode" %in% colnames(data) == T & first_postcode == T) {

    data %>%
      dplyr::left_join(Postcode.match, by = c("first_postcode" = "postcode"))

  }
  else{
    data %>%
      dplyr::left_join(Postcode.match, by = "postcode")
  }
}


#' Join in HEIMS basis of admission variable
#'
#'\code{BOA_var} returns a column of simplified basis of admission categories, consistent with the HEIMS definition, and joined by 'basis_for_admission_code'.
#' @export
BOA_var <- function(data) {
  if ("basis_for_admission_code" %in% colnames(data) == F) {
    stop("Does your data have the variable 'basis_for_admission_code'?\n
         Spoiler alert: it doesn't.
         Consider getting it.")
  } else{
    data %>%
      dplyr::left_join(boa.match)
  }
}


#' Join in ACARA variables
#'
#' \code{ACARA_vars} returns school sector, ICSEA, and gelocation, columns from the ACARA series dataset, matched by 'latest_secondary_school_code'.
#' Warning: 'ACARA_vars' should only be used in cases where the student was in secondary school between 2015 & 2017. A more accurate match file needs to be developed that specifies year.
#' @export
ACARA_vars <- function(data) {
  if ("latest_secondary_school_code" %in% colnames(data) == F) {
    stop("Does your data have the variable 'latest_secondary_school_code'?\n
         Spoiler alert: it doesn't.
         Consider getting it.")
  } else {
    data %>%
      dplyr::left_join(ACARA.match)
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
      dplyr::left_join(Language.match) %>%
      dplyr::mutate(NESB = dplyr::if_else(enrolment_year - year_of_entry_to_australia < 10, "Y", "N"))
  } else if (NESB == F) {
    data %>%
      dplyr::left_join(Language.match)
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
      dplyr::left_join((Country.match %>%
                  dplyr::rename(birth_broad_region = broad_region,
                         birth_narrow_region = narrow_region)), by = c("country_of_birth" = "bo_country")) %>%
      dplyr::left_join((Country.match %>%
                   dplyr::rename(home_broad_region = broad_region,
                          home_narrow_region = narrow_region)), by = c("country_of_home_residency" = "bo_country"))
  } else if (birth_country == T & "country_of_birth" %in% colnames(data) == T) {
    data %>%
      dplyr::left_join((Country.match %>%
                   dplyr::rename(birth_broad_region = broad_region,
                          birth_narrow_region = narrow_region)), by = c("country_of_birth" = "bo_country"))
  } else if (home_country == T & "country_of_home_residency" %in% colnames(data) == T) {
    data %>%
      dplyr::left_join((Country.match %>%
                   dplyr::rename(home_broad_region = broad_region,
                          home_narrow_region = narrow_region)), by = c("country_of_home_residency" = "bo_country"))
  } else {
    warning("You have excluded all parameters relevant to the function: 'Country_vars' or you don't have the right variables.")
  }
}


#' Join all ext vars
#'
#' @export
all_external_joins <- function(data,postcodes) {

}

