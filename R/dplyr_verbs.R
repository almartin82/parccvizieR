#' @title makes data a parccvizieR_data object
#'
#' @description see https://github.com/tidyverse/dplyr/issues/719
#'
#' @param df a data frame
#' @return same data frame with parccvizieR_data class
#' @export

parccvizieR_data <- function(df) UseMethod("parccvizieR_data")

#' @export
parccvizieR_data.default <- function(df) {
  class(df) <- c('parccvizieR_data', class(df))
  df
}


#' filter dplyr wrapper
#'
#' @param df a tibble/data frame
#' @param ... additional args
#' @rdname filter
#'
#' @return data.frame a tibble/data frame with the same classes
#' @export

filter.parccvizieR_data <- function(df, ...) {
  #store the incoming class info
  old_classes <- class(df)

  #strip mavpvizieR_data class so that method dispatch doesn't lead to
  #infinite calls
  class(df) <- old_classes[!old_classes == 'parccvizieR_data']

  #call normal (should go to dplyr)
  out <- dplyr::filter(df, ...)

  #restore class info
  class(out) <- old_classes

  out
}


#' select dplyr wrapper
#'
#' @param df a tibble/data frame
#' @param ... additional args
#' @rdname select
#'
#' @return data.frame a tibble/data frame with the same classes
#' @export

select.parccvizieR_data <- function(df, ...) {
  #store the incoming class info
  old_classes <- class(df)

  #strip mavpvizieR_data class so that method dispatch doesn't lead to
  #infinite calls
  class(df) <- old_classes[!old_classes == 'parccvizieR_data']

  #call normal (should go to dplyr)
  out <- dplyr::select(df, ...)

  #restore class info
  class(out) <- old_classes

  out
}

#' group_by dplyr wrapper
#'
#' @param df a tibble/data frame
#' @param ... additional args
#' @rdname group_by
#'
#' @return data.frame a tibble/data frame with the same classes
#' @export

group_by.parccvizieR_data <- function(df, ...) {
  #store the incoming class info
  old_classes <- class(df)

  #strip mavpvizieR_data class so that method dispatch doesn't lead to
  #infinite calls
  class(df) <- old_classes[!old_classes == 'parccvizieR_data']

  #call normal (should go to dplyr)
  out <- dplyr::group_by(df, ...)

  #restore class info
  class(out) <- old_classes

  out
}

#' ungroup dplyr wrapper
#'
#' @param df a tibble/data frame
#' @param ... additional args
#' @rdname ungroup
#'
#' @return data.frame a tibble/data frame with the same classes
#' @export

ungroup.parccvizieR_data <- function(df, ...) {
  #store the incoming class info
  old_classes <- class(df)

  #strip mavpvizieR_data class so that method dispatch doesn't lead to
  #infinite calls
  class(df) <- old_classes[!old_classes == 'parccvizieR_data']

  #call normal (should go to dplyr)
  out <- dplyr::ungroup(df, ...)

  #restore class info
  class(out) <- old_classes

  out
}


#' arrange dplyr wrapper
#'
#' @param df a tibble/data frame
#' @param ... additional args
#' @rdname arrange
#'
#' @return data.frame a tibble/data frame with the same classes
#' @export

arrange.parccvizieR_data <- function(df, ...) {
  #store the incoming class info
  old_classes <- class(df)

  #strip mavpvizieR_data class so that method dispatch doesn't lead to
  #infinite calls
  class(df) <- old_classes[!old_classes == 'parccvizieR_data']

  #call normal (should go to dplyr)
  out <- dplyr::arrange(df, ...)

  #restore class info
  class(out) <- old_classes

  out
}


#' dplyr wrapper
#'
#' @param df a tibble/data frame
#' @param ... additional args
#' @rdname mutate
#'
#' @return data.frame a tibble/data frame with the same classes
#' @export

mutate.parccvizieR_data <- function(df, ...) {
  #store the incoming class info
  old_classes <- class(df)

  #strip mavpvizieR_data class so that method dispatch doesn't lead to
  #infinite calls
  class(df) <- old_classes[!old_classes == 'parccvizieR_data']

  #call normal (should go to dplyr)
  out <- dplyr::mutate(df, ...)

  #restore class info
  class(out) <- old_classes

  out
}
