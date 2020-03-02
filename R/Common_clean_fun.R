# These functions perform common cleaning jobs


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


# Rename variables to accord with the cheedr package

