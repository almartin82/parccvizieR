
generate_growth_data <- function(pv) {

  #generate a scaffoldd

  #match start/end testids in parccviz clean object

  #add in cohort

  #return

}

build_growth_scaffold <- function(pv_srf, start, end) {

  #limited srf
  simple <- pv_srf %>%
    select(
      parcc_student_identifier,
      state_student_identifier,
      local_student_identifier,
      subject_area,
      academic_year,
      responsible_school_code,
      responsible_school_name,
      assessment_grade_numeric,
      grade_level_when_assessed,
      subject,
      test_code
    ) %>%
    rename(
      assessment_grade = assessment_grade_numeric,
      student_grade = grade_level_when_assessed
    ) %>%
    mutate(
      key = paste(state_student_identifier, test_code, sep = '_')
    )

  start_srf <- simple %>% filter(academic_year == start)
  end_srf <- simple %>% filter(academic_year == end)

  #define the columns for the output
  #this lets us return a zero-length data frame if there's no
  #matching data
  output_cols <- list(
    parcc_student_identifier = character(),
    state_student_identifier = character(),
    local_student_identifier = character(),
    subject_area = character(),
    start_year = integer(),
    start_responsible_school_code = character(),
    start_responsible_school_name = character(),
    start_assessment_grade = integer(),
    start_student_grade = integer(),
    start_subject = character(),
    start_test_code = character(),
    start_test_scale_score = integer(),
    start_test_performance_level = integer(),

    end_year = integer(),
    end_responsible_school_code = character(),
    end_responsible_school_name = character(),
    end_assessment_grade = integer(),
    end_student_grade = integer(),
    end_subject = character(),
    end_test_code = character(),
    end_test_scale_score = integer(),
    end_test_performance_level = integer()
  )

  #empty tibble
  empty <- as_tibble(output_cols)

  #if there's no data, don't worry about matching; just return a zero row df
  if (nrow(start_srf) == 0) {
    return(empty)
  }

  start_prefixes <- c(rep('', 4), rep('start_', 8))
  end_prefixes <- c(rep('', 4), rep('end_', 8))

  names(start_srf) <- paste0(start_prefixes, names(start_srf))
  names(end_srf) <- paste0(end_prefixes, names(end_srf))

  #using start, make a key of the matching end row
  start_srf <- start_srf %>%
    mutate(
      next_test = next_test(start_test_code),
      target_end_key = paste(state_student_identifier, next_test, sep = '_')
    )

  matched_rows <- inner_join(
    start_srf, end_srf[,c(5:12)], by = c('target_end_key'='end_key')
  )
}


#' Returns the next sequential test code
#'
#' @param x a test code
#'
#' @return the follownig test code using standard PARCC progression
#' @export

next_test <- function(x) {
  case_when(
    x == 'ELA03' ~ "ELA04",
    x == 'ELA04' ~ "ELA05",
    x == 'ELA05' ~ "ELA06",
    x == 'ELA06' ~ "ELA07",
    x == 'ELA07' ~ "ELA08",
    x == 'ELA08' ~ "ELA09",
    x == 'ELA09' ~ "ELA10",
    x == 'ELA10' ~ "ELA11",

    x == 'MAT03' ~ "MAT04",
    x == 'MAT04' ~ "MAT05",
    x == 'MAT05' ~ "MAT06",
    x == 'MAT06' ~ "MAT07",
    x == 'MAT07' ~ "MAT08",
    x == 'MAT08' ~ "ALG01",
    x == 'ALG01' ~ "GEO01",
    x == 'GEO01' ~ "ALG02"
  )
}
