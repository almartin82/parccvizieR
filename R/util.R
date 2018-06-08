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

pv_limit_srf <- function(pv_obj, studentids, subject_area = NA, academic_year = NA) {
  #nse
  subject_area_in <- subject_area

  #extract the object
  srf <- pv_obj$srf

  #only these kids
  out <- srf %>%
    dplyr::filter(
      studentid %in% studentids,
      subject_area == subject_area_in
    )

  return(out)
}
