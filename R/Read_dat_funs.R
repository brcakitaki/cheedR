#' Set up directories and read in data
#' Create directory of folders for saving the csv BO extracts into (change default date to SysDate)
#' Consider including retention and completion data
#'
#'
#' @export
new_csv_dir <- function(dir_path = "~/", postcode = T, parent = T, date_stamp = Sys.Date()) {

  dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}"))

  if (postcode == T & parent == T) {
    dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}/Enrolments"))
    dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}/Postcodes"))
    dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}/Parents"))
  } else if (postcode == T & parent == F) {
    dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}/Enrolments"))
    dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}/Postcodes"))
  } else if (postcode == F & parent == T) {
    dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}/Enrolments"))
    dir.create(glue::glue("{dir_path}/Data_extracts_{as.character(date_stamp)}/Postcodes"))
  }
}




#' Enrolment data
#' @export
enrolment_read <- function(path) {
  x <- purrr::map(
    list.files(
      path = path,
      full.names = T
    ), ~ .x %>%
      readr::read_csv(guess_max = 300000) %>%
      janitor::clean_names()
  )

  if (purrr::map(tester.list, ~ purrr::has_element(colnames(.x), "main_language_spoken_code")) %>% purrr::every(isTRUE) == T) {

    x %>%
      map_dfr( ~ .x %>%
                 mutate(main_language_spoken_code = as.numeric(main_language_spoken_code))) %>%
      distinct()

  } else {
    x %>%
      bind_rows() %>%
      distinct()
  }
}


#' Parents educational attainment data
#' @export
parent_read <- function(path, pattern = ".csv") {

  x <- purrr::map_dfr(
    list.files(
      path = path,
      pattern = pattern,
      full.names = T
    ), ~ .x %>%
      readr::read_csv(guess_max = 200000) %>%
      janitor::clean_names()
    ) %>%
    dplyr::distinct()

  if ("parent_highest_educ_attainment_code" %in% colnames(x) == F) {
    stop("You are missing the variable 'parent_highest_educ_attainment_code'.\n
         It is pretty important here, so go back and get it.")
  } else {

    x %>%
      dplyr::distinct(student_id, parent_highest_educ_attainment_code) %>%
      dplyr::mutate(
        hierarchy = dplyr::case_when(
          parent_highest_educ_attainment_code == "PG" ~ 1,
          parent_highest_educ_attainment_code == "BACH" ~ 2,
          T ~ 3)
        ) %>%
      dplyr::group_by(student_id) %>%
      dplyr::mutate(parent_ed = min(hierarchy)) %>%
      dplyr::mutate(parent_ed = dplyr::case_when(
        parent_ed == 1 ~ "Postgraduate",
        parent_ed == 2 ~ "Bachelor",
        parent_ed == 3 ~ "No HE")
        ) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(student_id,parent_ed)

  }
}



#' postcode data
#' @export
postcode_read <- function(path, pattern = ".csv") {
  x <- purrr::map_dfr(
    list.files(
      path = path,
      pattern = pattern,
      full.names = T
    ), ~ .x %>%
      readr::read_csv(guess_max = 100000) %>%
      janitor::clean_names()
  )

  if ("australian_postcode_e320" %in% colnames(x) == F) {
    stop("You appear to be missing the variable 'australian_postcode_e320'.\n
         We do really need that one. It would be swell if you got it.")
  } else if ("postcode_year_e320" %in% colnames(x) == F) {
    stop("You appear to be missing the variable 'postcode_year_e320'.\n
         We do really need that one. It would be swell if you got it.")
  } else if ("commencing_postcode" %in% colnames(x) == T) {

  x %>%
    dplyr::rename(postcode = australian_postcode_e320,
                  postcode_year = postcode_year_e320) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::mutate(first_postcode = dplyr::if_else(!is.na(commencing_postcode), commencing_postcode, postcode)) %>%
      dplyr::distinct(student_id,
               first_postcode,
               postcode,
               postcode_year)
  } else {

    warning("You appear to be missing the variable 'commencing_postcode'.\n
            We can proceed without it, but your output df will not have the variable 'first_postcode'.
            You will still have 'postcode' though. So as long as you are cool with that, lets move on.")

    x %>%
      dplyr::rename(postcode = australian_postcode_e320,
                    postcode_year = postcode_year_e320) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::mutate(first_postcode = dplyr::if_else(!is.na(commencing_postcode), commencing_postcode, postcode)) %>%
      dplyr::distinct(student_id,
               first_postcode,
               postcode,
               postcode_year)
    }
  }


#' join dfs
#' @export
combine_basic <- function(enrolment_df, postcode_df = NULL, parent_df = NULL) {
  if (is.null(enrolment_df)) {
    stop("At the very least, you need to include an enrolment df.")
  }
  if (is.null(postcode_df) & is.null(parent_df)) {
    warning("You have only included an enrolment df.\n
            If that's what you're into, fine, carry on.
            If you want to join in a postcode or parent df, you need to specify\n
            in the 'postcode_df' and 'parent_df' arguments.")

    enrolment_df
  }
  if (is.null(postcode_df)) {
    enrolment_df %>%
      left_join(parent_df)

    warning("Only parent df will be joined to enrolment df.")
  }
  if (is.null(parent_df)) {
    enrolment_df %>%
      left_join(postcode_df)

    warning("Only postcode df will be joined to enrolment df.")
  }
  else {
    enrolment_df %>%
      left_join(parent_df) %>%
      left_join(postcode_df)
  }
  }



