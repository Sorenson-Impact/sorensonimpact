#' Show IPEDS survey info
#' @description
#' \lifecycle{experimental}
#' Shows the distinct overview and notes fields from the SI ipeds dictionary
#' @importFrom magrittr "%>%"
#' @param survey_group String value matching the abbreviated survey group
#' @return Printed info about the survey
#' @examples
#' \dontrun{
#' ipeds_info("efc")
#' }
#' @export

ipeds_info <- function(survey_group) {

  if(!is.character(survey_group)) return(cli::cli_alert_danger("\`survey_group\` argument must be a string. Did you attempt to pass a bare variable name?"))

  if(!exists("ipeds_dictionary")) ipeds_dictionary <<- readr::read_rds("~/Google Drive/SI/DataScience/data/maps_project/cleaned_data/ipeds/ipeds_dictionary.rds")

  survey_group <- str_to_lower(survey_group)

  avail_sgs <- ipeds_dictionary %>% dplyr::distinct(survey_group) %>% dplyr::pull(survey_group)
  if(!(survey_group %in% avail_sgs)) {
    stop(paste0("Provided survey group \"", survey_group, "\" does not exist in IPEDS dictionary.  Available groups are:\n", paste(avail_sgs, collapse = ",\n")))
  }

  ipeds_dictionary %>%
    dplyr::filter(survey_group == !!survey_group) %>%
    dplyr::distinct(overview) %>%
    dplyr::filter(!stringr::str_detect(overview, "^Preliminary")) %>%
    dplyr::mutate(sdist = stringdist::stringdist(overview, dplyr::lag(overview), method = "jw")) %>%
    dplyr::filter(sdist > .8 | is.na(sdist)) %>%
    dplyr::mutate(overview = stringr::str_wrap(overview, 80, indent = 5)) %>%
    dplyr::pull(overview) %>%
    cat(crayon::blue("\n\nOverview:\n"), ., sep = "\n")

  notes <- ipeds_dictionary %>%
    dplyr::arrange(desc(year)) %>%
    dplyr::filter(survey_group == !!survey_group) %>%
    dplyr::distinct(notes) %>%
    dplyr::filter(!stringr::str_detect(notes, "^Preliminary")) %>%
    dplyr::mutate(sdist = stringdist::stringdist(notes, dplyr::lag(notes), method = "jw")) %>%
    dplyr::filter(sdist > .8 | is.na(sdist)) %>%
    dplyr::mutate(notes = stringr::str_wrap(notes, 80, indent = 5)) %>%
    dplyr::pull(notes)

  if(!purrr::is_empty(notes)) cat(crayon::red("\n\nNotes:"), notes, sep = "\n", fill = 80) else
    cat(crayon::red("\n\nNotes:\n\n"), crayon::italic("     (No notes for this survey group)\n"))
}

#' Show quick summary info on institution given a unitid
#' @description
#' \lifecycle{experimental}
#' Shows quick info on an institution.
#' @importFrom magrittr "%>%"
#' @param unitid Numeric `unitid value.
#' @param return_tibble Logical indicating whether to return the data as a tibble rather than formatted output. Defaults to formatted output.
#' @return Printed info about the institution.
#' @examples
#' \dontrun{
#' ipeds_inst_lookup(230764)
#' }
#' @export
ipeds_inst_lookup <- function(unitid, return_tibble = FALSE) {

  hd_lookup <- readr::read_rds(fs::path_expand("~/Google Drive/SI/DataScience/data/maps_project/modified_data/hd lookup table.rds"))

  if(unitid %in% hd_lookup$unitid) inst <- hd_lookup %>% dplyr::filter(unitid == !!unitid) else return(cli::cli_alert_danger("unitid \`{unitid}\` not found`."))

  if(return_tibble) return(inst) #If user wants a tibble, return and exit

  # inst_env <- new_environment()
  # inst %>%
  #   select(-unitid) %>%
  #   mutate_all(as.character) %>%
  #   pivot_longer(everything()) %>%
  #   mutate(assignexp = glue("{name} <- '{value}'")) %>%
  #   select(assignexp) %>%
  #   rowwise() %>%
  #   pwalk( ~eval(parse_expr(.x), envir = .GlobalEnv))
  #
  print_info <- function(inst) {
    cli::cli_h1(glue::glue_data(inst, "{institution_entity_name} ({year})"))
    cli::cli_text("")
    cli::cli_text(glue::glue_data(inst, "Sector: {sector_of_institution}"))
    cli::cli_text(glue::glue_data(inst, "Size Category: {institution_size_category}"))
    cli::cli_text(glue::glue_data(inst, "HBCU: {historically_black_college_or_university}"))
    cli::cli_text("")
    cli::cli_text(glue::glue_data(inst, "{city_location_of_institution}, {state_abbreviation}"))
    cli::cli_text(glue::glue_data(inst, "County: {county_name}"))
    cli::cli_text(glue::glue_data(inst, "FIPS: {fips_county_code}"))
    cli::cli_text("")
    if(inst$unitid_for_merged_schools != -2) {
      cli::cli_text(glue::glue_data(inst, "Merged unitid: {unitid_for_merged_schools}"))
      cli::cli_text("")
    }
    if(inst$date_institution_closed != -2) {
      cli::cli_text(cli::col_white(cli::bg_red(glue::glue_data(inst, "Institution closed: {date_institution_closed}"))))
      cli::cli_text("")
    }
  }

  print_info(inst)
}

#' Find unitids that match a string in the institution name
#' @description
#' \lifecycle{experimental}
#' Shows unitid's for institution names that match the string provided
#' @importFrom magrittr "%>%"
#' @param instname Full or partial string to detect in instution name.
#' @return Printed info about the institution.
#' @examples
#' \dontrun{
#' ipeds_unitid_lookup("Utah")
#' }
#' @export
ipeds_unitid_lookup <- function(instname) {

  instname <- str_to_lower(instname)

  hd_lookup <- readr::read_rds(fs::path_expand("~/Google Drive/SI/DataScience/data/maps_project/modified_data/hd lookup table.rds")) #This is generated in the HD cleaning file.

  hd_matches <- hd_lookup %>% dplyr::filter(stringr::str_detect(str_to_lower(institution_entity_name), !!instname))

  #If no matches found:
  if(nrow(hd_matches) == 0) return(cli::cli_alert_info("No institution names matching \"{instname}\"."))

  cli::cli_h1(glue::glue("Found {nrow(hd_matches)} institutions matching \"{instname}\""))

  hd_matches %>%
    dplyr::select(unitid, institution_entity_name, city_location_of_institution, state_abbreviation)

}



#' vis_dat for ipeds
#' @description
#' \lifecycle{deprecated}
#' Produce a vis_dat plot for ipeds data split by year with optional sampling.
#' @importFrom magrittr "%>%"
#' @param years Single year or vector of years to plot.  Defaults to all years in data.
#' @param .sample_frac Percent of observations to sample from each year.  Defaults to .10.
#' @return Plot of ipeds survey by year.
#' @examples
#' \dontrun{
#' hd %>% ipeds_visdat(years = 2008:2011)
#' }
#' @export

ipeds_visdat <- function(.data, years = "all", .sample_frac = .10) {
  lifecycle::deprecate_warn(when = "0.0.1.9035", what = "ipeds_visdat()", with = "si_visdat_grouped()")
  #Check that data is ipeds survey
  if(!all(c("unitid", "year") %in% names(.data))) warning(".data does not contain a unitid or year column.  Are you sure you passed an ipeds survey?")

  #Make sure years is set
  if(!all(years == "all" | is.numeric(years))) stop("\`years\` must be \"all\" or a numeric vector of 4-digit years.")

  if(all(years == "all")) years <- min(.data$year):max(.data$year)


  if(.sample_frac < 1) {
    cli::cli_alert_info("Sampling data at {.sample_frac * 100}% per year.")
    if(nrow(.data < 9e+04)) cli::cli_alert_warning("Data is smallish, are you sure you want to sample? Set .sample_frac to 1 to show all data.")

    .data <- .data %>%
      dplyr::group_by(year) %>%
      dplyr::sample_frac(.sample_frac) %>%
      dplyr::ungroup()
  } else cli::cli_alert_info("Using 100% of data, this may be slow.")

  p1 <- .data %>%
    dplyr::filter(year == years[1]) %>% visdat::vis_dat(warn_large_data = F, sort_type = F, palette = "qual") +
      ggplot2::labs(y = years[1]) + ggplot2::theme(plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))

  plist <- tibble::lst()
  plist[[1]] <- p1

  if(length(years > 1)) {
    for(i in 2:length(years)) {
      plist[[i]] <- .data %>%
        dplyr::filter(year == years[{i}]) %>%
        visdat::vis_dat(warn_large_data = F, sort_type = F, palette = "qual") +
        ggplot2::labs(y = years[{i}]) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), plot.margin = ggplot2::margin(0, 5.5, 0, 5.5, "pt"))
    }

  }

  patchwork::wrap_plots(plist, ncol = 1, guides = "collect")

}

#' Table of cleaned IPEDs data
#' @description
#' \lifecycle{experimental}
#' Shows a table of the cleaned IPEDs data.
#' @importFrom magrittr "%>%"
#' @param .show_details Include the path and the survey description? Defaults to FALSE for easier reading
#' @return IPEDS data table
#' @examples
#' \dontrun{
#' ipeds_data()
#' }
#' @export
ipeds_data <- function(.show_details = F) {

  if(!exists("ipeds_dictionary")) ipeds_dictionary <<- readr::read_rds("~/Google Drive/SI/DataScience/data/maps_project/cleaned_data/ipeds/ipeds_dictionary.rds")

  ipeds_table <- tibble::enframe(fs::dir_ls("~/Google Drive/SI/DataScience/data/maps_project/cleaned_data/ipeds/", glob = "*.rds"), name = NULL, value = "file") %>%
    dplyr::mutate(survey_group = basename(file) %>%
             stringr::str_extract("^\\S*") %>% stringr::str_remove(stringr::fixed(".rds"))) %>%
    dplyr::left_join(ipeds_dictionary %>%
                dplyr::distinct(year, survey, survey_group, overview) %>%
                dplyr::group_by(survey_group) %>%
                dplyr::filter(year == max(year)) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(overview = stringr::str_remove(overview, "This file contains the |This file contains data on the |This file contains ") %>% stringr::str_to_sentence()) %>%
                dplyr::filter(year > 2012), by = "survey_group") %>%
    dplyr::filter(!is.na(survey)) %>%
    dplyr::select(-year) %>%
    dplyr::mutate(filename = basename(file)) %>%
    dplyr::relocate(file, .after = dplyr::last_col()) %>%
    dplyr::relocate(filename, .after = survey_group) %>%
    dplyr::rename(path = file)

  if(!.show_details) ipeds_table <- ipeds_table %>% dplyr::select(-path, -overview)

  return(ipeds_table)
}

#' Quick load cleaned ipeds data
#' @description
#' \lifecycle{experimental}
#' Convenience function to quick load a cleaned IPEDs rds.
#' @importFrom magrittr "%>%"
#' @param survey_file string containing all or part of the filename. The extension is not required. If the string matches more than one file, the list of matching files will be returned instead of the data. If the string exactly matches a file despite their being other partial matches, that file is returned.
#' @return Cleaned IPEDS survey data from rds file.  Or, if multiple matches are found, a table of matches.
#' @examples
#' \dontrun{
#' hd %>% ipeds_visdat(years = 2008:2011)
#' }
#' @export
ipeds_load <- function(survey_file) {

  files <- tibble::enframe(fs::dir_ls("~/Google Drive/SI/DataScience/data/maps_project/cleaned_data/ipeds/", glob = "*.rds"), name = NULL, value = "file")

  file_match <- files %>% dplyr::mutate(name = basename(file)) %>% dplyr::filter(stringr::str_detect(name, stringr::fixed(!!survey_file)))

   if(nrow(file_match) == 1) {
    cli::cli_alert_info("Loading {file_match$file} ...")
    readr::read_rds(file_match$file)
  } else
    if(nrow(file_match) == 0) {
      cli::cli_alert_info("No matching files found. The available files are:")
      sorensonimpact::ipeds_data()
  } else
    if(nrow(file_match) > 1) {

      #If the others are the long versions, ignore.
      if(nrow(file_match %>% dplyr::filter(!stringr::str_detect(name, stringr::fixed("long")))) == 1) {
        longname <- file_match %>% dplyr::filter(stringr::str_detect(name, stringr::fixed("long"))) %>% dplyr::pull(name)
        cli::cli_alert_warning("Loading the wide version of this survey filename. However, a (long) version of this survey filename exists:\n \t\"{longname}\". \nTo load the long version, further specify the survey_file string to match the long version.")
        file_match <- file_match %>% dplyr::filter(!stringr::str_detect(name, stringr::fixed("(long)")))
        return(readr::read_rds(file_match$file))
      }


      #Perfect match ignores longer version of same file name:
      if(length(mrow <- which(stringr::str_remove(survey_file, "\\..*$") == stringr::str_remove(file_match$name, stringr::fixed(".rds")))) == 1) {

        cli::cli_alert_info("Files exist that match the string plus additional characters. Because the string provided is an exact match for \"{file_match %>% dplyr::slice(mrow) %>% dplyr::pull(file) %>% basename()}\", it is being loaded rather than the list of additional matches.")

        file_match <- file_match[mrow, ]

        return(readr::read_rds(file_match$file))
      }


      cli::cli_alert_info("Multiple matching files found. Please specify a unique survey filename from the list below:")
      sorensonimpact::ipeds_data() %>%
        dplyr::filter(stringr::str_detect(filename, !!survey_file))
    }

}
