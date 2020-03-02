# Parent_attain()

# This function reads in a .csv of La Trobe enrolment parent attainment data, 
# and converts it into a standardised format

# NOTE: You will need to at least include the following variables in the LTU BO extract: 

        # - `Student ID`
        # - `Parent Highest Educ Attainment Code`


# Create a function that converts parent highest ed into standard highest ed measure
Parent_attain <- function(path) {
  
  x <- readr::read_csv(path) %>% 
    dplyr::rename_all(make.names) %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::distinct()
  
  if ("Parent.Highest.Educ.Attainment.Code" %in% colnames(x) == F) {
    stop("Sigh...\n You are missing the variable 'Parent.Highest.Educ.Attainment.Code' from your df.\n Its really not that hard...\n Please just go back and get it.")   
  } else if ("Parent.Highest.Educ.Attainment" %in% colnames(x) == F) {
    stop("Sigh...\n You are missing the variable 'Parent.Highest.Educ.Attainment' from your df.\n Its really not that hard...\n Please just go back and get it.")
  } else if ("Student.ID" %in% colnames(x) == F) {
    stop("Sigh...\n You are missing the 'Student.ID' from your df.\n Well done... Very on brand for La Trobe.\n Please just go back and get it.")
  }
  message("You have all the right variables, and they are all correctly named! Excellent work comrade!")
  
  x <- x %>%
    mutate(
      Ed.hierarchy = case_when(
        Parent.Highest.Educ.Attainment.Code == "PG" ~ 1,
        Parent.Highest.Educ.Attainment.Code == "BACH" ~ 2,
        T ~ 3
      )
    ) %>% 
    group_by(Student.ID) %>% 
    mutate(Parent.attainment = min(Ed.hierarchy)) %>% 
    mutate(Parent.attainment = case_when(
      Parent.attainment == 1 ~ "Postgraduate",
      Parent.attainment == 2 ~ "Bachelor",
      Parent.attainment == 3 ~ "No HE"
    )) %>% 
    ungroup() %>% 
    distinct(Student.ID,Parent.attainment)
}




