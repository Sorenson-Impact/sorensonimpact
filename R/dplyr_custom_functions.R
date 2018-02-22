#' Extract duplicate rows
#' @description Extract all rows with duplicated values in the given columns
#' @importFrom magrittr "%>%"
#' @param ... Columns to evaluate for duplication. Works via \code{group_by()}.
#' @return Filtered dataframe with duplicates in given columns
#' @examples
#' mtcars %>% duplicates(mpg)
#' @export
duplicates <- function(data, ...) {
  columns <- rlang::enquos(...)
  data %>%
    dplyr::group_by(!!!columns) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!!columns)
}


#' Count the NAs in each column
#' @description Count all the NAs in each column of a data frame
#' @importFrom magrittr "%>%"
#' @return NA count for each
#' @export
col_sum_na <- function(data) {
  data %>%
    purrr::map_dfc(is.na) %>%
    purrr::map_dfc(sum)
}

#' Generate a frequency tibble
#' @description Generate a frequency table with marginal values
#' @importFrom magrittr "%>%"
#' @return A tibble
#' @export
freq_tibble <- function(data, var1, var2) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)

  data %>%
    dplyr::count(!!var1, !!var2) %>%
    tidyr::spread(!!var2, n, fill = 0) %>%
    dplyr::mutate(Total := rowSums(dplyr::select(., -!!var1))) %>%
    dplyr::bind_rows(dplyr::bind_cols(!!rlang::quo_name(var1) := "Total", dplyr::summarize_if(., is.numeric, sum)))
}
