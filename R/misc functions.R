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
#' @param all Logical. Show all previous news in viewer. Defaults to FALSE.
#' @export
si_news <- function(all = FALSE) {

  if(all) {
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
  devtools::update_packages("sorensonimpact", upgrade = "always", dependencies = FALSE)
  library(sorensonimpact)
  cli::cli_alert_success("Successfully updated \`sorensonimpact\`")

  new_version <- packageVersion("sorensonimpact")

  if(old_version != new_version) {
    #sorensonimpact::si_news() #Removed for now.
      #See: https://github.com/jimhester/devtools/commit/f2f077b6c8c8180ae71c53d6fb6744368c5225b7
      #and: https://github.com/r-lib/devtools/issues/942
    cli::cat_line()

    print(cli::rule(
      center = paste0("Major Update"),
      line_col = "red"
    ))
    cli::cat_line()
    cli::cli_alert_info("New features added. Run \`si_news()\` to view notes.")
    cli::cat_line()
    si_news()
  } else {
    cli::cat_line()

    print(cli::rule(
      left = paste0("sorensonimpact update"),
      line_col = "blue"
    ))

    cli::cat_line()
    cli::cli_alert_info("Minor update, no new features added.  Run \'si_news(all = T)\` to view changelog for previous major updates.")
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



#' idf
#' @description
#' \lifecycle{experimental}
#' This is a shortcut for interactive data exploration that makes it easy to
#' filter rows that match a value in the key column of a tibble.
#'
#'
#' It produces the same result as filter(your_key_col == key_value). However it
#' has the following advantages:
#'
#' \itemize{
#' \item Less typing
#' \item Saves the key value to a hidden environment
#' variable, so that calling it subsequently (perhaps on a different table) it
#' is not necessary to specify the key value. }
#'
#' @section Motivation: When working with a set of tables that have an in-common
#'   id column (for example "unitid" in IPEDS or offender number in UDC data) it
#'   is often the case that when I find an individual with some issue in one
#'   table, I then want to see what the data for that individual is in another
#'   table. Rather than constantly having to use filter() with a
#'   matching argument, this function lets you just specify the id value on its
#'   own. And because the goal is often to look up an id value in one table and
#'   then immediately look it up in the other table, this function also saves
#'   whatever the last id value was so that it isn't necessary to specify it on
#'   subsequent function calls.
#'
#' @section Warning: This function should not be used in non-interactive (ie in
#'   console) or non-diagnostic script code (i.e. anywhere that an object is
#'   written vs just printed).
#'
#' @usage options(idf_data_key = "your_data_key_column")
#'
#' df \%>\% idf(id = .last_id)
#'
#' @param id A vector containing the key value(s) you want to filter. After the first time the function
#'   is called with a specified key value, the value is stored in a hidden
#'   object and does not need to be specified again.
#' @param glimpse Print output using dplyr::glimpse, defaults to FALSE.
#' @return The data filtered by the key value on the key column stored in options("idf_data_key")
#' @export
idf <- function(.data, id = .last_id, glimpse = F) {

  .last_id <<- id

  key <- getOption("idf_data_key")

  if(is.null(key)) {
    cli::cli_alert_danger("{.val idf_data_key} is not set.")
    key <- readline(prompt = "Please enter the column name: ") %>%
      stringr::str_remove_all(., pattern = "\"|\'")

    options("idf_data_key" = key)
    cli::cli_alert_success("{.val idf_data_key} set to {.val {key}}.")
    cli::cli_alert_info("You can avoid this prompt by setting {.code options(idf_data_key = \"your_data_key_column\"} at the start of a script.")
  }

  key <- rlang::sym(key)

  result <- .data %>%
    dplyr::filter(!!key %in% !!id)

  if(glimpse) result %>% dplyr::glimpse()
    else result

}


# Start of working on a quick summary of vars with few categories
# hd %>%
#   select_if(~n_distinct(.) < 10) %>%
#   mutate(across(everything(), as.character)) %>%
#   pivot_longer(everything()) %>% ggplot(aes(x = name, fill = value, label = value)) + geom_bar(position = "fill") + coord_flip()  + guides(fill = F)
