#' Calculate a student's cohort
#'
#' @description A student's cohort is defined as the year they would
#' graduate from 12th grade, assuming a normal pattern of promotion and
#' on time graduation.  A student can move between cohorts if they
#' are retained
#'
#' @param end_year 4-digit year, where year is the ending year of
#' an academic year (eg 2015 is 2014-2015 school year).
#' @param grade student's grade level
#'
#' @return numeric vector
#' @export

cohort_year <- function(end_year, grade) {
  end_year + 1 + 12 - grade
}


#' Limit a parccvizieR SRF data frame
#'
#' @param pv_obj a conforming parccvizieR object
#' @param studentids vector of state_student_identifiers
#' @param subject_area optional, ELA or Math
#' @param academic_year optional, ending YYYY
#' @param grade_level_when_assessed optional, gr 3-11
#' @param test_code optional, PARCC test code

pv_limit_srf <- function(
  pv_obj, studentids, subject_area = NA, academic_year = NA,
  grade_level_when_assessed = NA, test_code = NA
) {
  #nse
  subject_area_in <- subject_area
  academic_year_in <- academic_year
  grade_level_when_assessed_in <- grade_level_when_assessed
  test_code_in <- test_code

  #extract the object
  srf <- pv_obj$srf

  #only these kids
  out <- srf %>%
    filter(
      state_student_identifier %in% studentids
    )

  #subject area?
  if(!is.na(subject_area)) out <- out %>% filter(subject_area == subject_area_in)
  #academic year
  if(!is.na(academic_year)) out <- out %>% filter(academic_year == academic_year_in)
  #grade level when assessed
  if(!is.na(grade_level_when_assessed)) out <- out %>% filter(grade_level_when_assessed == grade_level_when_assessed_in)
  #test code
  if(!is.na(test_code)) out <- out %>% filter(test_code == test_code_in)

  out
}
