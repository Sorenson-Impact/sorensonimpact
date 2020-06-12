#' Show IPEDS survey info
#' @description Shows the distinct overview and notes fields from the SI ipeds dictionary
#' @importFrom magrittr "%>%"
#' @param survey_group String value matching the abbreviated survey group
#' @return Printed info about the survey
#' @examples
#' \dontrun{
#' ipeds_info("efc")
#' }
#' @export

ipeds_info <- function(survey_group) {

  if(!exists("ipeds_dictionary")) ipeds_dictionary <<- readr::read_rds("~/Google Drive/SI/DataScience/data/maps_project/cleaned_data/ipeds/ipeds_dictionary.rds")

  avail_sgs <- ipeds_dictionary %>% distinct(survey_group) %>% pull(survey_group)
  if(survey_group %ni% avail_sgs) {
    warning(paste0("Provided survey group \"", survey_group, "\" does not exist in IPEDS dictionary.  Available groups are: ", paste(avail_sgs, collapse = ",\n")))
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
#' @description Shows quick info on an institution.
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
#' @description Show's unitid's for institution names that match the string provided
#' @importFrom magrittr "%>%"
#' @param instname Full or partial string to detect in instution name.
#' @return Printed info about the institution.
#' @examples
#' \dontrun{
#' ipeds_unitid_lookup("Utah")
#' }
#' @export
ipeds_unitid_lookup <- function(instname) {

  hd_lookup <- readr::read_rds(fs::path_expand("~/Google Drive/SI/DataScience/data/maps_project/modified_data/hd lookup table.rds")) #This is generated in the HD cleaning file.

  hd_matches <- hd_lookup %>% dplyr::filter(stringr::str_detect(institution_entity_name, !!instname))

  if(nrow(hd_matches))

  cli::cli_h1(glue::glue("Found {nrow(hd_matches)} institutions matching \"{instname}\""))

  hd_matches %>%
    dplyr::select(unitid, institution_entity_name, city_location_of_institution, state_abbreviation)

}
