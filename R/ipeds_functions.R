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
    dplyr::mutate(overview = stringr::str_wrap(overview, 80, indent = 5)) %>%
    dplyr::pull(overview) %>%
    cat(crayon::blue("\n\nOverview:\n"), ., sep = "\n")

  ipeds_dictionary %>%
    dplyr::filter(survey_group == !!survey_group) %>%
    dplyr::distinct(notes) %>%
    dplyr::filter(!stringr::str_detect(notes, "^Preliminary")) %>%
    dplyr::mutate(notes = stringr::str_wrap(notes, 80, indent = 5)) %>%
    dplyr::pull(notes) %>%
    cat(crayon::red("\n\nNotes:"), ., sep = "\n", fill = 80)

}