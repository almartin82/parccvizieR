#' @title Calculate basic summary stats using summary method.
#' Useful for initial 'how did we do' cuts and questions.
#'
#' @param object a \code{parccvizieR_srf} object
#' @param ... other arguments to be passed to other functions (not currently supported)
#' @return summary stats as a \code{parccvizieR_srf_summary} object.
#' @export

summary.parccvizieR_srf <- function(object, ...) {

  #summary.parccvizieR_srf requires grouping vars on the cdf
  #process_cdf_long sets them as part of construction of the mv object
  #if there are NO grouping vars, this will set them by default
  existing_groups <- attr(object, 'vars') %>% as.character()

  if (is.null(existing_groups)) {
    object <- object %>%
      dplyr::group_by(
        academic_year, subject_area, responsible_school_code,
        responsible_school_name, assessment_grade_numeric, test_code
      )
  }

  df <- object %>%
    dplyr::summarize(
      mean_test_scale_score = mean(test_scale_score, na.rm = TRUE),
      mean_test_performance_level = mean(test_performance_level, na.rm = TRUE),
      n_students = n()
    )

  class(df) <- c("parccvizieR_srf_summary", class(df))

  df
}
