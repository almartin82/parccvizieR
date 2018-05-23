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
