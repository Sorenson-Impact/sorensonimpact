#' vis_dat for grouped data
#' @description Produce a vis_dat plot for ipeds data split by year with optional sampling.
#' \lifecycle{experimental}
#' @importFrom magrittr "%>%"
#' @param ... bare, unquoted column(s) to use as the index to group by. Alternatively will accept a grouped df.
#' @param .sample_frac Percent of observations to sample from each year.  Default "auto" samples down to 100,000 rows, split evenly between groups for vis_dat. For vis_miss and vis_value, "auto" uses all data.
#' @param method Which visdat function to use. One of "vis_dat", "vis_miss", or "vis_value".  Accepts shorthand "dat", "val", and "miss".
#' @return visdat plot separated by grouping variable.
#' @examples
#' \dontrun{
#' diamonds %>% si_visdat_group(facet_group = cut)
#' }
#' @export

si_visdat_grouped <- function(.data, ..., method = "vis_dat", .sample_frac = "auto") {

  is_pregrouped <- dplyr::is_grouped_df(.data) #Does the data already have grouping structure?

  #Set the visdat function to use
  if(stringr::str_detect(method, "dat")) method <- "dat"
  if(stringr::str_detect(method, "val")) method <- "val"
  if(stringr::str_detect(method, "miss")) method <- "miss"

  # for val and miss we want to see all the data, hence auto = 1
  if((method == "val" | method == "miss") & .sample_frac == "auto") .sample_frac = 1

  # Otherwise downsmample
  if(.sample_frac == "auto") {
    if(nrow(.data) > 100000) {
      .sample_frac <- 100000 / nrow(.data)
      cli::cli_alert_info("Large data, automatically down-sampling data at {round(.sample_frac * 100)}%. To disable or change, set .sample_frac to a value between 0 and 1.")
    } else .sample_frac <- 1
  }

  #Group the data
  if(is_pregrouped) {
    .data <- .data %>%
      tibble::add_column(group_index = dplyr::group_indices(.)) %>%
      tidyr::unite(group_name, dplyr::group_vars(.), sep = "\n", remove = F) %>%
      dplyr::arrange(group_index)
  } else {
    .data <- .data %>%
      dplyr::group_by(...) %>%
      tibble::add_column(group_index = dplyr::group_indices(.)) %>%
      tidyr::unite(group_name, ..., sep = "\n", remove = F) %>%
      dplyr::arrange(group_index)
  }

  # Do any sampling
  if(.sample_frac < 1) {
    #cli::cli_alert_info("Sampling data at {.sample_frac * 100}% per year.")

    .data <- .data %>%
      dplyr::sample_frac(.sample_frac / dplyr::n_groups(.))

  } else cli::cli_alert_info("Using 100% of data, this may be slow.")

  #Split the data
  .data <- .data %>% dplyr::group_split(.keep = F)

  #Methods for each visdat graph
  if(method == "dat") {
    plist <- .data %>%
      purrr::map(function(...) {
        .data <- as_tibble(...)

        group_name <- .data %>% dplyr::distinct(group_name) %>% pull(group_name)
        group_index <- .data %>% dplyr::distinct(group_index) %>% pull(group_index)

        .data <- .data %>% dplyr::select(-group_name, -group_index)

        p <- .data %>%
          visdat::vis_dat(warn_large_data = F, sort_type = F, palette = "qual") +
          ggplot2::labs(y = group_name) +
          ggplot2::theme(plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))

        if(group_index > 1) {
          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                  plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))
        }
        return(p)
      })
  }



  if(method == "val") {
    plist <- .data %>%
      purrr::map(function(...) {
        .data <- as_tibble(...)

        group_name <- .data %>% dplyr::distinct(group_name) %>% pull(group_name)
        group_index <- .data %>% dplyr::distinct(group_index) %>% pull(group_index)

        .data <- .data %>% dplyr::select(-group_name, -group_index)

        p <- .data %>%
          dplyr::select(tidyselect:::where(is.numeric)) %>%
          visdat::vis_value() +
          ggplot2::labs(y = group_name) +
          ggplot2::theme(plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))

        if(group_index > 1) {
          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                  plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))
        }
        return(p)
      })
  }

  if(method == "miss") {
    plist <- .data %>%
      purrr::map(function(...) {
        .data <- as_tibble(...)

        group_name <- .data %>% dplyr::distinct(group_name) %>% pull(group_name)
        group_index <- .data %>% dplyr::distinct(group_index) %>% pull(group_index)

        .data <- .data %>% dplyr::select(-group_name, -group_index)

        p <- .data %>%
          dplyr::select(tidyselect:::where(is.numeric)) %>%
          visdat::vis_miss(show_perc = T, warn_large_data = F) +
          ggplot2::labs(y = group_name) +
          ggplot2::theme(plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))

        if(group_index > 1) {
          p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                                  plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))
        }
        return(p)
      })
  }

  patchwork::wrap_plots(plist, ncol = 1, guides = "collect")

}
