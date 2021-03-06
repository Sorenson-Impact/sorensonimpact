#' Extract duplicate rows
#' @description
#' \lifecycle{defunct}
#' Extract all rows with duplicated values in the given columns
#' @importFrom magrittr "%>%"
#' @param ... Columns to evaluate for duplication. Works via \code{group_by()}.
#' @return Filtered dataframe with duplicates in given columns
#' @examples
#' \dontrun{
#' mtcars %>% duplicates(mpg)
#' }
#' @export
duplicates <- function(data, ...) {
  lifecycle::deprecate_stop(when = "0.0.1.9034", what = "duplicates()", with = "janitor::get_dupes()")
  columns <- rlang::enquos(...)
  data %>%
    dplyr::group_by(!!!columns) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(!!!columns)
}


#' Sum selected columns by row
#' @description
#' \lifecycle{experimental}
#' Sum selected columns within mutate without \code{rowwise()} (which can be very slow).
#' @importFrom magrittr "%>%"
#' @param ... Columns to sum.
#' @param sum_col Name of sum column. Defaults to "sum".
#' @param na.rm Remove NAs? Passed to rowSums
#' @return Vector with rowwise sums.
#' @examples
#' \dontrun{
#' cars %>% sum_rowwise(speed, dist, na.rm = T, sum_col = "mysum"))
#' }
#' @export
sum_rowwise <- function(data, ..., sum_col = "sum", na.rm = FALSE) {
  columns <- rlang::enquos(...)

  data %>%
    dplyr::select(!!! columns) %>%
    dplyr::transmute(!!sum_col := rowSums(., na.rm = na.rm)) %>%
    dplyr::bind_cols(data, .)
}


#' Count the NAs in each column
#' @description
#' \lifecycle{maturing}
#' Count all the NAs in each column of a data frame
#' @importFrom magrittr "%>%"
#' @return NA count for each
#' @export
col_sum_na <- function(data) {
  data %>%
    purrr::map_dfc(is.na) %>%
    purrr::map_dfc(sum)
}

#' Generate a frequency tibble
#' @description
#' \lifecycle{defunct}
#' Generate a frequency table with marginal values
#' @importFrom magrittr "%>%"
#' @param rows The primary rows of the table (use groups for additional)
#' @param cols The columns of the table
#' @param ... Additional grouping variables that will subdivide rows.
#' @return A tibble
#' @export
freq_tibble <- function(data, rows, cols, ...) {
  lifecycle::deprecate_stop(when = "0.0.1.9034", what = "freq_tibble()", with = "janitor::tabyl()")
  rows <- rlang::enquo(rows)
  cols <- rlang::enquo(cols)
  groups <- rlang::enquos(...)

  if(length(groups) == 0) {

    data %>%
      dplyr::count(!!rows, !!cols) %>%
      tidyr::spread(!!cols, n, fill = 0) %>%
      dplyr::mutate(Total := rowSums(dplyr::select(., -!!rows))) %>%
      dplyr::bind_rows(dplyr::bind_cols(!!rlang::quo_name(rows) := "Total", dplyr::summarize_if(., is.numeric, sum)))

  }
  else{
    groupnum <- data %>% dplyr::distinct(!!!groups) %>% nrow()

    data %>%
      dplyr::count(!!rows, !!cols, !!!groups) %>%
      tidyr::spread(!!cols, n, fill = 0) %>%
      dplyr::mutate(Total := rowSums(dplyr::select(., -!!rows, -c(!!!groups)))) %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::bind_rows(dplyr::bind_cols(!!rlang::quo_name(rows) := rep("Subtotal", groupnum), dplyr::summarize_if(., is.numeric, sum)),
                dplyr::bind_cols(!!rlang::quo_name(rows) := "Total", dplyr::summarize_if(dplyr::ungroup(.), is.numeric, sum)))
  }
}


# unmix <- function(data, col) {
#   col <- rlang::enquo(col)
#
#   numname <- paste(quo(col), "num", sep = "_")
#   charname <- paste(quo_name(col), "char", sep = "_")
#
#
#
#   data %>%
#     mutate(numname = as.numeric(!!col),
#            charname = case_when(is.na(!!quo(numname)) ~ !!enquo(numname)))
# }
# unmix(x, fu)


#' Tibble Preview
#' @description
#' \lifecycle{experimental}
#' Show a sample of all tibble data without hiding columns.
#' @importFrom magrittr "%>%"
#' @return A preview of a tibble.
#' @export
tp <- function(data, rows = 10) {
  data <- dplyr::sample_n(data, size = rows)
  print(data, n = Inf, width = Inf)
}

#' Ordered Factor case_when()
#' @description
#' \lifecycle{experimental}
#' Can replace `case_when()` syntax and outputs an ordered factor in the same order as the cases, useful for meaningful ordering in plots and tables.  This is because for `case_when()` the arguments are evaluated in order, so you must proceed from the most specific to the most general. Tables and plots will therefor be ordered by the evaluation order.
#' @param ... A sequence of two-sided formulas. See ?dplyr::case_when for details
#' @return An ordered factor vector of length 1 or n, matching the length of the logical input or output vectors, with the type (and attributes) of the first RHS. Inconsistent lengths or types will generate an error.
#' @importFrom magrittr "%>%"
#' @export
fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  ordered(dplyr::case_when(...), levels=levels)
}

#' Remove variables from tibble
#' @description
#' \lifecycle{maturing}
#' This is a simple negation of `dplyr::select`.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). See Methods, below, for more details.
#' @param ... <tidy-select> One or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.
#' @return An object of the same type as .data, with the specified columns removed.
#' @importFrom magrittr "%>%"
#' @export
deselect <- function(.data, ...) {
  dplyr::select(.data, -c(...))
}
