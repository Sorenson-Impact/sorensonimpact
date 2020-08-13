#' Write Clip shortcut
#' @description
#' \lifecycle{stable}
#' Copies an object to the clipboard. Defaults to .Last.value
#' @export
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
  cli::cli_alert_success("Value copied to clipboard")
}

#' Not In: Inverse Value Matching
#' @description
#' \lifecycle{stable}
#' Returns a logical vector indicating if there is NOT a match for the LHS vector anywhere in the RHS vector. Opposide of \code{\%in\%}.
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
#' @description
#' \lifecycle{experimental}
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
#' @description
#' \lifecycle{maturing}
#' Format a vector of numeric values according to monetary abbreviations.
#'
#' (ie: 1,000 becomes "1 k", 2,500,000 becomes "2.5 B") See: https://www.wallstreetoasis.com/forums/abbreviation-for-thousands-millions-billions-trillion
#' @param sep Seperator to use between number and unit (defaults to " ").
#' @param suffix_n Use "Bn" and "Tn" instead of "B" and "T".
#'
#' @return Character vector of formatted dollar values.
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


#' Show news for new version
#' @description
#' \lifecycle{experimental}
#' Shows the news for the package
#' @param in_viewer Logical. Show news in viewer instead of console (defaults to TRUE)
#' @export
si_news <- function(in_viewer = TRUE) {

  if(in_viewer) {
    news(package = "sorensonimpact")
  } else {

    newsdb <- news(Version == as.character(packageVersion("sorensonimpact")), package = "sorensonimpact")

    cli::cat_line()

    print(cli::rule(
        center = paste0("Update news for sorensonimpact ", packageVersion("sorensonimpact"), ""),
        line_col = "yellow"
        ))

    newstext <- newsdb$Text %>%
      stringr::str_split("  - ") %>%
      unlist() %>%
      stringr::str_remove("\\\n") %>%
      stringr::str_squish() %>%
      .[-1] #end up with an empty first line

    cli::cat_line()
    cli::cli_li(newstext)
    cli::cat_line()

  }

}


#' Update the sorensonimpact package
#' @description
#' \lifecycle{experimental}
#' Automatically unloads, updates, and reloads the sorensonimpact package.
#' @export
si_update <- function() {
  old_version <- packageVersion("sorensonimpact")

  if(remotes:::local_sha("sorensonimpact") == remotes:::remote_sha(structure(remotes:::package2remote("sorensonimpact"), class = "github_remote"))) return(cli::cli_alert_success("Package is already up to date."))

  cli::cli_alert_info("Unloading and updating \`sorensonimpact\`...")
  devtools::unload("sorensonimpact")
  devtools::update_packages("sorensonimpact", upgrade = "always")
  library(sorensonimpact)
  cli::cli_alert_success("Successfully updated \`sorensonimpact\`")

  new_version <- packageVersion("sorensonimpact")

  if(old_version != new_version) {
    si_news(in_viewer = F)
  } else {
    cli::cat_line()

    print(cli::rule(
      left = paste0("sorensonimpact update"),
      line_col = "blue"
    ))

    cli::cat_line()
    cli::cli_alert_info("Minor update, no new features added.  Run \'si_news()\` to view changelog for previous major updates.")
    cli::cat_line()
  }
}


#' common_vars
#' @description
#' \lifecycle{experimental}
#' Shows the variable names that are in common between two or more tibbles.
#' @param ... Bare, unquoted tibble object names.
#' @export
common_vars <- function(...) {

  objects <- lst(...)

  if(length(objects) < 2) stop("At least two objects required.")

  varnames <- map(objects, function(var) {
    enframe(names(var), name = NULL, value = "name")
  }) %>% bind_rows(.id = "object")

  varnames %>%
    add_column(present = T) %>%
    complete(name, object) %>%
    pivot_wider(names_from = object, values_from = present)
}
