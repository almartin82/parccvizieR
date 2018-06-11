#' Calculate basic summary stats.  Useful for initial 'how did we do' cuts and questions.
#'
#' @inheritParams pv_limit_srf
#'
#' @return summary data frame
#' @export

basic_summary_stats <- function(
  pv_obj, studentids,
  subject_area = NA, academic_year = NA, grade_level_when_assessed = NA
) {

  df <- pv_limit_srf(pv_obj, studentids, subject_area, academic_year, grade_level_when_assessed)

  df
}
