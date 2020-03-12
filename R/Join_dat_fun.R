# Join function for enrolment data

# Postcode join function
join_postcodes <- function(enrol_df, postcode_df) {

  enrol_df %>%
    left_join(postcode_df, by = c("student_id", "enrolment_year" = "postcode_year"))

}
