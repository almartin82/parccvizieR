
generate_growth_data <- function(pv) {

  #determine all of the academic years
  unq_years <- unique(pv$srf$academic_year)
  unq_years <- c(unq_years, min(unq_years) - 1) %>% sort()

  #build growth df
  growth_df <- map_df(
    .x = unq_years,
    .f = function(.x) build_growth_df(pv$srf, .x, .x + 1)
  )

  #calculate growth metrics


  #add in cohort

  #return

}


#' Build a growth df, given a start and end year
#'
#' @param srf a PARCC summative record file
#' @param start start academic year
#' @param end end academic year
#' @param verbose print status updates as the data is processed?
#' default is TRUE
#'
#' @return growth data frame
#' @export

build_growth_df <- function(srf, start, end, verbose = TRUE) {

  #limited srf
  simple <- srf %>%
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
      test_code,
      test_scale_score,
      test_performance_level
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

  start_prefixes <- c(rep('', 4), rep('start_', 9))
  end_prefixes <- c(rep('', 4), rep('end_', 9))

  names(start_srf) <- paste0(start_prefixes, names(start_srf))
  names(end_srf) <- paste0(end_prefixes, names(end_srf))

  #using start, make a key of the matching end row
  start_srf <- start_srf %>%
    mutate(
      next_test = next_test(start_test_code),
      target_end_key = paste(state_student_identifier, next_test, sep = '_')
    )

  #matched
  matched_rows <- inner_join(
    start_srf, end_srf[,c(5:13)], by = c('target_end_key'='end_key')
  ) %>%
  mutate(
    match_status = 'start and end',
    complete_obsv = TRUE
  )

  #start only
  only_start <- dplyr::anti_join(
    start_srf, matched_rows, by = 'start_key'
  ) %>%
  mutate(
    end_academic_year = end,
    end_subject = subject_area(next_test),
    match_status = 'only_start',
    complete_obsv = FALSE
  ) %>%
  rename(
    end_test_code = next_test
  ) %>%
  select(-start_key, -target_end_key)

  #end only
  only_end <- dplyr::anti_join(
    end_srf, matched_rows, by = c('end_key'='target_end_key')
  ) %>%
  mutate(
    start_academic_year = start,
    start_test_code = prior_test(end_test_code),
    start_subject = subject_area(start_test_code),
    match_status = 'only_end',
    complete_obsv = FALSE
  ) %>%
  select(-end_key)

  matched_rows <- matched_rows %>%
    select(-target_end_key, -start_key, -next_test)

  bind_rows(matched_rows, only_start, only_end)
}


#' Returns sequential test code (next or prior)
#'
#' @param x a test code
#'
#' @return the next or prior test code using standard PARCC progression
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

#' @export

prior_test <- function(x) {
  case_when(
    x == 'ELA11' ~ "ELA10",
    x == 'ELA10' ~ "ELA09",
    x == 'ELA09' ~ "ELA08",
    x == 'ELA08' ~ "ELA07",
    x == 'ELA07' ~ "ELA06",
    x == 'ELA06' ~ "ELA05",
    x == 'ELA05' ~ "ELA04",
    x == 'ELA04' ~ "ELA03",

    x == 'ALG02' ~ "GEO01",
    x == 'GEO01' ~ "ALG01",
    x == 'ALG01' ~ "MAT08",
    x == 'MAT08' ~ "MAT07",
    x == 'MAT07' ~ "MAT06",
    x == 'MAT06' ~ "MAT05",
    x == 'MAT05' ~ "MAT04",
    x == 'MAT04' ~ "MAT03"
  )
}
