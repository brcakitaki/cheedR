# Load internal data into sysdata.rda

# Load packages
require(tidyverse)
require(readxl)

# Basis of Admission match file
boa.match <- read_csv("G:/Predictive Analytics/External match files/Match files/BOAmatchCodes.csv") %>%
  janitor::clean_names() %>%
  rename(basis_for_admission_code = boa_code,
         basis_admission = translation_code_description) %>%
  distinct(basis_for_admission_code,basis_admission)

save(boa.match, file = "R/sysdata.rda/boa_match.rda")

# Postcode match file - need 2011 & 2016
Postcode.match <- list(
  census.2011 = read_xlsx("G:/Predictive Analytics/External match files/Match files/PostcodeData2011.xlsx") %>%
    select(-e320) %>%
    rename(
      ses_2011 = ses_aust_2011,
      metro_2011 = Urban_2011,
      regional_2011 = Rural_2011,
      remote_2011 = Remote_2011
    ) %>%
    mutate(rrr_2011 = metro_2011 + regional_2011 + remote_2011) %>%
    distinct(),
  census.2016 = read_csv("G:/Predictive Analytics/External match files/Match files/SEIFA_match_2016_v2.csv") %>%
    rename(
      ses_2016 = A_SES2016,
      metro_2016 = ASGS2016_metro,
      regional_2016 = ASGS2016_regional,
      remote_2016 = ASGS2016_remote
      ) %>%
    mutate(rrr_2016 = metro_2016 + regional_2016 + remote_2016,
           ses_2016 = if_else(ses_2016 == "x",NA_character_,ses_2016))
  ) %>%
  map(~ .x %>%
        mutate_all(as.character) %>%
        mutate(Postcode = as.numeric(Postcode))) %>%
  reduce(left_join) %>%
  distinct() %>%
  janitor::clean_names()

save(Postcode.match, file = "R/sysdata.rda/Postcode_match.rda")

#SEIFA match file for 2016 (matches full SEIFA suite of indices to postcode for 2016)


# Institutional grouping match file (match file to give institutions into their groupings for national analysis)


# ACARA school match file (Matches VIc school codes to ACARA school geosocial variables)
ACARA.match <- read_csv("G:/Predictive Analytics/External match files/Match files/ACARA_2016_match_key.csv") %>%
  janitor::clean_names() %>%
  rename(latest_secondary_school_code = latest_2nd_school_code,
         school_geolocation = geolocation,
         school_icsea = icsea)

save(ACARA.match, file = "R/sysdata.rda/ACARA_match.rda")

# Country codes match file
Country.match <- read_csv("G:/Predictive Analytics/External match files/Match files/Country_codes_SACC_2016.csv") %>%
  janitor::clean_names() %>%
  distinct(bo_country,broad_region,narrow_region) %>%
  mutate_all(str_to_upper)

save(Country.match, file = "R/sysdata.rda/Country_match.rda")

# Language match file
Language.match <- read_csv("G:/Predictive Analytics/External match files/Match files/Main_Language_Lookup.csv") %>%
  janitor::clean_names() %>%
  select(-student_course,-enrolment_year) %>%
  distinct(main_lang_spoken,
           lang_detailed_desc,
           lang_narrow_desc,
           lang_broad_desc) %>%
  rename(main_language_spoken_code = main_lang_spoken,
         detailed_language = lang_detailed_desc,
         narrow_language = lang_narrow_desc,
         broad_language = lang_broad_desc)


save(Language.match, file = "R/sysdata.rda/Language_match.rda")



# Load in toy data
LTU.df <- read_csv("C:/Users/brcakitaki/Documents/LTU Enrolment data/Enrolment 18-20/LTU_2018_80000.csv") %>%
  janitor::clean_names()


# save using usethis

usethis::use_data(Postcode.match,Language.match,Country.match,boa.match,ACARA.match ,internal = T)


LTU.df %>%
  cheedR::Indigenous_var() %>%
  cheedR::Language_vars() %>%
  glimpse()
