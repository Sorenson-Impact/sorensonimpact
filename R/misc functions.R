#' Write Clip shortcut
#' @description Copies an object to the clipboard. Defaults to .Last.value
#' @export
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
  message("Value copied to clipboard")
}

#' Not In: Inverse Value Matching
#' @description Returns a logical vector indicating if there is NOT a match for the LHS vector anywhere in the RHS vector. Opposide of \code{\%in\%}.
#' @usage x %ni% y
#' @param x vector or NULL: the values to check for non-match
#' @param y vector or NULL: the values to be matched against
#' @return A logical vector, indicating if there was no match for each element of x. Values are TRUE or FALSE and never NA
#' @rdname ni
#' @export
"%ni%" <- Negate("%in%")

#' View selected data frame
#'
#' The RStudio Environment pane variable name column is too narrow for long
#' names, so difficutl to find the right data frame in long list of similar
#' names. Select the variable and use shortcut to use data viewer on it.
#'
#' Stolen from: https://github.com/dracodoc/mischelper/blob/master/R/misc.R
#' @export

view_df <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    # this will show "View(get(selected))" in viewer, not optimal
    # View(get(selected))
    formated <- stringr::str_c("View(", selected, ')')
    rstudioapi::sendToConsole(formated, execute = TRUE)
  }
}


#' Install all the packages we've ever found useful.
#'
#' This is a convenience function for after R has been updated and all
#' the packages need to be reinstalled.
#'
#' @export
si_install_packages <- function() {
  message("work in progress, see siverse code for how to do this elegantly.")
  #leaflet.extras
  #DataExplorer
}

#' Scale large dollar amounts to shortened dollar amounts (k, M, B, T)
#' @description Format a vector of numeric values according to monetary abbreviations.
#'
#' (ie: 1,000 becomes "1 k", 2,500,000 becomes "2.5 B") See: https://www.wallstreetoasis.com/forums/abbreviation-for-thousands-millions-billions-trillion
#' @param sep Seperator to use between number and unit (defaults to " ").
#' @param suffix_n Use "Bn" and "Tn" instead of "B" and "T".
#'
#' @return Character vector of formatted dollar values.
#'
#' @examples
#' \dontrun{
#' si_scale_big_dollar(1000)
#' si_scale_big_dollar(1000000000)
#' si_scale_big_dollar(1000000000, suffix_n = T)
#' }
#' @export
si_scale_big_dollar <- function(x, sep = " ", suffix_n = F) {
  x <- as.numeric(x)
  limits <- c(1, 1000, 1e+06, 1e+09, 1e+12)
  suffix <- c(" ", "k", "M", "B", "T")
  if(suffix_n) suffix <- c(" ", "k", "M", "Bn", "Tn")
  i <- findInterval(abs(x), limits)
  i <- ifelse(i == 0, which(limits == 1), i)
  paste0("$", format(round(x/limits[i], 1), trim = TRUE, scientific = FALSE), sep, suffix[i])
}


#' Update the sorensonimpact package
#' @description Automatically unloads, updates, and reloads the sorensonimpact package.
#' @export
update_si <- function() {
  cli::cli_alert_info("Unloading and updating \`sorensonimpact\`...")
  devtools::unload("sorensonimpact")
  devtools::update_packages("sorensonimpact", upgrade = "always")
  library(sorensonimpact)
  cli::cli_alert_success("Successfully updated \`sorensonimpact\`")
}
