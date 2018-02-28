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
#' @param rows The primary rows of the table (use groups for additional)
#' @param cols The columns of the table
#' @param ... Additional grouping variables that will subdivide rows.
#' @return A tibble
#' @export
freq_tibble <- function(data, rows, cols, ...) {
  rows <- enquo(rows)
  cols <- enquo(cols)
  groups <- rlang::enquos(...)

  if(length(groups) == 0) {

    data %>%
      count(!!rows, !!cols) %>%
      spread(!!cols, n, fill = 0) %>%
      mutate(Total := rowSums(select(., -!!rows))) %>%
      bind_rows(bind_cols(!!quo_name(rows) := "Total", summarize_if(., is.numeric, sum)))

  }
  else{
    groupnum <- data %>% distinct(!!!groups) %>% nrow()

    data %>%
      count(!!rows, !!cols, !!!groups) %>%
      spread(!!cols, n, fill = 0) %>%
      mutate(Total := rowSums(select(., -!!rows, -c(!!!groups)))) %>%
      group_by(!!!groups) %>%
      bind_rows(bind_cols(!!quo_name(rows) := rep("Subtotal", groupnum), summarize_if(., is.numeric, sum)),
                bind_cols(!!quo_name(rows) := "Total", summarize_if(ungroup(.), is.numeric, sum)))
  }
}
