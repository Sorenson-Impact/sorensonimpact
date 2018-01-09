#' Extract duplicate rows
#' @description Extract all rows with duplicated values in the given columns
#' @importFrom magrittr "%>%"
#' @param ... Columns to evaluate for duplication. Works via \code{group_by()}.
#' @return Filtered dataframe with duplicates in given columns
#' @examples
#' mtcars %>% duplicates(mpg)
#' @export
duplicates <- function(data, ...) {
  data %>%
    group_by_(.dots = lazyeval::lazy_dots(...)) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    arrange_(.dots = lazyeval::lazy_dots(...))
}


#' Count the NAs in each column
#' @description Count all the NAs in each column of a data frame
#' @importFrom magrittr "%>%"
#' @return NA count for each
#' @export
col_sum_na <- function(data) {
  data %>%
    summarize_all(funs(sum(is.na(.))))
}
