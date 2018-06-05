#' Dedupe a SRF to return one row per studentid per testcode per year
#'
#' @param srf a parcvizieR summative record file
#'
#' @return SRF with a rn column where 1 = best test record to use
#' @export

dedupe_srf <- function(srf) {

  #make some dummy variables
  srf <- srf %>%
    mutate(
      is_void = !is.na(void_score_code),
      is_assigned = test_status == 'Assign',
      is_assigned = ifelse(is.na(is_assigned), FALSE, is_assigned),
      is_not_tested = !is.na(not_tested_code),
      is_not_attempted = test_attemptedness_flag == 'N',
      is_not_attempted = ifelse(is.na(is_not_attempted), FALSE, is_not_attempted)
    )

  #rank.  for all the dummies above, TRUE is bad, FALSE is good/preferred
  srf <- srf %>%
    group_by(state_student_identifier, test_code, academic_year) %>%
    mutate(
      rn = rank(
        order(is_void, is_assigned, is_not_tested,
              is_not_attempted, desc(total_test_items_attempted))
      )
    ) %>%
    select(
      -is_void, -is_assigned, -is_not_tested, -is_not_attempted
    ) %>%
    ungroup()

  srf
}
