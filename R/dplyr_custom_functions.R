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


