#' Adorn percentages, percent formatting, and ns to a janitor::tabyl with better percents
#' @description Adorn percentages, percent formatting, and ns to a \code{janitor::tabyl()} with better percents
#' @importFrom magrittr "%>%"
#' @param pct_direction Should the cell percentages use the "col" or "row" totals as denominator. Defaults to "col".
#' @return tabyl with percents, percent formatting, ns, and better total percents.
#' @examples
#' \dontrun{
#' mtcars %>% tabyl(cyl, gear) %>% adorn_everything(pct_direction = "col")
#' }
#' @export
adorn_everything <- function(dat, pct_direction = "col") {

  if(!(pct_direction %in% c("row", "col"))) stop("pct_direction must be either \"row\" or \"col\".")

  pct_direction_rev <- ifelse(pct_direction == "col", "row", "col")

  dat <- dat %>%
    adorn_totals("row", name = "Column Total") %>%
    adorn_totals("col") %>%
    adorn_percentages(pct_direction) %>%
    adorn_pct_formatting() %>%
    adorn_ns()

  if(pct_direction == "col") {
    dat <- dat %>%
      rows_update(y = attr(., "core") %>% adorn_totals("row", name = "Column Total") %>%
                    adorn_percentages(pct_direction_rev) %>%
                    adorn_pct_formatting() %>%
                    adorn_ns() %>%
                    slice(nrow(.)),
                  by = colnames(.)[1])
  }

  if(pct_direction == "row") {

    attr_temp <- attributes(dat)

    dat <- dat %>%
      select(-last_col()) %>%
      add_column(attr(dat, "core") %>%
                   adorn_totals("row", name = "Column Total") %>%
                   adorn_totals("col") %>%
                   adorn_percentages(pct_direction_rev) %>%
                   adorn_pct_formatting() %>%
                   adorn_ns() %>%
                   select(last_col()))

    attributes(dat) <- attr_temp
  }

  return(dat)
}
