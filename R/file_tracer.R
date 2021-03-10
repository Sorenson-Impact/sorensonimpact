#' Trace inputs and outputs for a file
#' @description
#' \lifecycle{experimental}
#' Produces a connection graphic linking all inputs to a given file, and all outputs from the file.  Currently only works on code files.  The ability to specify a data file will be added.
#' @section Note:
#' This is a very early alpha release.  Please provide feedback and bug reports.
#'
#' You will want to use the zoom window to view the result, and be aware that you can zoom in on it using the standard mouse actions, as some will be quite large and will appear impossible to read at the default zoom.
#' @param file The .R or .Rmd file you want to trace. Can be a partial match for code file, extension not required. For data file, full file name is required with extension.
#' @param direction A string either "down" (the default) or "right" specifying the direction the relationships are drawn.
#' @param code_path The top-level path to search for .R and .Rmd files. The search is recursive to cover all child directories. Defaults to "project" which will search the active project directory.
#' @param trim_data_path Optional string to remove from the final displayed output for data file nodes. If most of the data files are in a common directory and thus the information is not informative, specifying that directory will clean up the output by shortening the full path to only the relative path.
#' @param levels_up_max \Sexpr[results=rd]{lifecycle::badge("experimental")} How many levels up the hierarchy to search and draw. Defaults to 10, values less than 2 are ignored.
#' @param levels_down_max \Sexpr[results=rd]{lifecycle::badge("experimental")} How many levels down the hierarchy to search and draw. Defaults to 10, values less than 3 are ignored.
#' @return A connection graphic of code files and data files leading into the specified file, and resulting from the specified file.
#' @examples
#' \dontrun{
#' file_trace("institution_base")
#' }
#' @export
file_trace <- function(file, code_path = "project", trim_data_path = NULL, direction = "down", levels_up_max = 10, levels_down_max = 10) {

  if(code_path == "project") code_path <- usethis::proj_path()


  rw <- sorensonimpact:::rw_lines(code_path, trim_data_path)

  #if it's a data file, we'll do that first and exit
  if(stringr::str_detect(file, "\\.csv$|\\.xls$|\\.xlsx$|\\.txt$|\\.rds$")) {
    x <- rw %>% dplyr::filter(stringr::str_detect(to, !!file) | stringr::str_detect(from, !!file))

    tree_formatted <- x %>% dplyr::mutate(from = ifelse(stringr::str_detect(from, !!file),
                                                        paste0("<origin>", from),
                                                        paste0("<", type_from, ">", from))) %>%
      dplyr::mutate(to = ifelse(stringr::str_detect(to, !!file),
                                paste0("<origin>", to),
                                paste0("<", type_to, ">", to))) %>%
      dplyr::distinct(from, to)

    nom <- tree_formatted %>%
      dplyr::mutate(nom = glue::glue("[{from}] --> [{to}]")) %>%
      dplyr::pull(nom) %>%
      as.character() %>%
      paste(collapse = "\n")

    nom_out <- paste0("#direction: ", direction, "
#font: Menlo
#.origin: visual=database fill=#ff5330 bold
#.datar: visual=database fill=#fadb75
#.datatext: visual=database fill=#45ff30
#.code: visual=note fill=#75affa
#.dynpathsithink: fill=#f58142

",
nom)

    return(nomnoml::nomnoml(nom_out))

  }

  origin <- rw %>%
    dplyr::filter(stringr::str_detect(file, stringr::str_remove(!!file, code_path)))

  #Check for match
  if(nrow(origin) == 0) stop(cli::cli_alert_danger(paste0("No files found that match `", file, "`.")))

  #Check for multiple matches and error if so.
  if(length(unique(origin$file)) > 1) {
    cli::cli_alert_danger("Multiple files match. Please further specify part or all of the path.")
    origin %>% dplyr::distinct(file) %>% dplyr::pull(file) %>% cli::cli_li()
    return(invisible(NULL))
  }

  inputs <- origin %>% dplyr::filter(rl) %>%
    tibble::add_column(position = 0)
  matched <- rw %>%
    dplyr::semi_join(inputs %>% dplyr::select(to = from), by = "to") %>%
    tibble::add_column(position = -1)
  inputs <- dplyr::bind_rows(matched, inputs)
  for(i in -2:(levels_up_max*-1)) {

    matched <- rw %>%
      dplyr::semi_join(matched %>% dplyr::select(to = from), by = "to") %>%
      tibble::add_column(position = i) %>%
      dplyr::anti_join(inputs %>% dplyr::select(index), by = "index") #prevents loops if something self-references

    inputs <- dplyr::bind_rows(matched, inputs)

    if(nrow(matched) == 0) break #No more connections, done.
  }

  outputs <- origin %>% dplyr::filter(wl) %>%
    tibble::add_column(position = 1)
  matched <- rw %>%
    dplyr::semi_join(outputs %>% dplyr::select(from = to), by = "from") %>%
    tibble::add_column(position = 2)
  outputs <- dplyr::bind_rows(outputs, matched)
  for(i in 3:levels_down_max){

    matched <- rw %>%
      dplyr::semi_join(matched %>% dplyr::select(from = to), by = "from") %>%
      tibble::add_column(position = i) %>%
      dplyr::anti_join(outputs %>% dplyr::select(index), by = "index") #prevents loops if something self-references

    outputs <- dplyr::bind_rows(outputs, matched)

    if(nrow(matched) == 0) break #No more connections, done.
  }

  full_tree <- dplyr::bind_rows(inputs, outputs) %>% dplyr::distinct()


  tree_formatted <- full_tree %>%
    dplyr::mutate(from = paste0("<", type_from, ">", from)) %>%
    dplyr::mutate(to = ifelse(position == 0,
                              paste0("<origin>", to),
                              paste0("<", type_to, ">", to)
    )
    ) %>%
    dplyr::distinct(from, to)

  nom <- tree_formatted %>%
    dplyr::mutate(nom = glue::glue("[{from}] --> [{to}]")) %>%
    dplyr::pull(nom) %>%
    as.character() %>%
    paste(collapse = "\n")

  print(tree_formatted)

nom_out <- paste0("#direction: ", direction, "
#font: Menlo
#.origin: visual=roundrect fill=#ff5330 bold
#.datar: visual=database fill=#fadb75
#.datatext: visual=database fill=#45ff30
#.code: visual=note fill=#75affa
#.dynpathsithink: fill=#f58142

",
nom)

  nomnoml::nomnoml(nom_out)

}

rw_lines <- function(code_path, data_path) {

  if(!fs::dir_exists(code_path)) stop(cli::cli_alert_danger(paste("Path `", code_path, "` does not exist.")))
  if(!is.null(data_path) && !fs::dir_exists(data_path)) stop(cli::cli_alert_danger(paste("Path `", data_path, "` does not exist.")))

  code_path <- fs::path_expand(code_path) %>% paste0(.,"/")
  if(!is.null(data_path)) data_path <- fs::path_expand(data_path) %>% paste0(.,"/")


  rfiles <- fs::dir_ls(code_path, type = "file", recurse = T, regexp = ".*\\.(R|r|Rmd)$")

  all_code <- rfiles %>%
    purrr::map(function(rfile) {

      readr::read_lines(rfile) %>%
        tibble::enframe(name = "line", value = "code") %>%
        tibble::add_column(file_full = as.character(rfile))
    }) %>%
    dplyr::bind_rows()

  rw <- all_code %>% #filter(file_full == "/Users/u0982704/Github/maps_project/modify/ipeds/adult_learners.R") %>%
    dplyr::mutate(rl = stringr::str_detect(code, "read_"),
                  wl = stringr::str_detect(code, "write_"),
                  path_assign = stringr::str_detect(code, "path")) %>%
    dplyr::filter(rl|wl) %>%
    dplyr::mutate(operation = dplyr::case_when(rl ~ "read",
                                               wl ~ "write",
                                               path_assign ~ "set path")) %>%
    dplyr::mutate(is_dynamic_path = stringr::str_detect(code, "paste|glue")) %>%
    dplyr::mutate(code = stringr::str_remove(code, " %>%.*")) %>%
    dplyr::mutate(target_full = stringr::str_extract(code, "(?<=\\\")(.*?)(?=\\\")")) %>% #get the file name between quotes
    dplyr::filter(stringr::str_detect(target_full, "~\\/Google Drive|\\/Volumes\\/GoogleDrive")) %>% #temporarily filter out anything that isn't an actual legitimate path (no objects and no fucking PC paths)
    dplyr::mutate(target_full = stringr::str_replace(target_full, "/Volumes/GoogleDrive/My Drive", fs::path_expand("~/Google Drive"))) %>%
    dplyr::mutate(target_full = fs::path_expand(target_full)) %>%
    dplyr::mutate(target_full = stringr::str_replace(target_full, "Google Drive File Stream", "Google Drive")) %>% dplyr::mutate(target = target_full)
  # {if(!is.null(data_path)) dplyr::mutate(non_code_path_target = !fs::path_has_parent(target_full, data_path)) else .} %>%

  if(!is.null(data_path)) {
    rw <- rw %>% mutate(target = stringr::str_remove(target_full, data_path))
  }

  rw <- rw %>%
    dplyr::mutate(target = stringr::str_remove(target, fs::path_expand("~/Google Drive/SI/DataScience"))) %>%
    dplyr::mutate(file = stringr::str_remove(file_full, code_path)) %>%
    dplyr::mutate(object = stringr::str_extract(code, ".*(?= <-)")) %>%
    dplyr::mutate(gates_pointer = stringr::str_detect(target, "data/gates")) %>%
    dplyr::mutate(code = stringr::str_trim(code)) %>%
    dplyr::filter(!stringr::str_detect(code, "^#")) #remove lines that are commented out



  #Now deal with dynpaths
  dynpath_lines <- rw %>% dplyr::filter(is_dynamic_path) #For some reason this is 100x faster than ifelse in mutate

  dynfiles_expanded <- dynpath_lines %>%
    dplyr::mutate(dynpath = stringr::str_extract(code, "\\(\\\".*(?=\\))") %>%
             stringr::str_remove_all("\\(|\\)") %>%
             stringr::str_split(pattern = ",\\s?|\\{|\\}") %>%
             purrr::map_chr(function(...) {
               tibble::enframe(..., value = "component", name = NULL) %>%
                 dplyr::mutate(is_wild = !stringr::str_detect(component, "\\\"")) %>%
                 dplyr::mutate(component = ifelse(is_wild, ".+", component)) %>% #detect where we need to replace with wildcards
                 dplyr::mutate(component = stringr::str_remove_all(component, "\"")) %>%
                 tibble::add_row(component = "\\", .before = nrow(.)) %>% #to literalize the . in extension
                 #pull(component) %>%
                 #paste0(collapse = "")
                 dplyr::mutate(dynpath = paste0(component, collapse = "")) %>%
                 dplyr::distinct(dynpath) %>%
                 dplyr::pull(dynpath)
             })) %>%
    dplyr::mutate(dynfiles = purrr::map(dynpath, ~fs::dir_ls(dirname(.x), regex = basename(.x)))) %>%
    tidyr::unnest(dynfiles) %>%
    sorensonimpact::deselect(target, dynpath) %>%
    dplyr::rename(target = dynfiles) %>%
    dplyr::mutate(target = as.character(target))

  rw <- rw %>%
    suppressMessages(dplyr::anti_join(dynpath_lines)) %>%
    dplyr::bind_rows(dynfiles_expanded) %>%
    dplyr::mutate(index = dplyr::row_number())

  rw <- rw %>%
    dplyr::mutate(from = dplyr::case_when(rl ~ target,
                            wl ~ file),
           to = dplyr::case_when(rl ~ file,
                          wl ~ target)) %>%
    dplyr::mutate(ext_to = fs::path_ext(to) %>% stringr::str_to_lower()) %>%
    dplyr::mutate(ext_from = fs::path_ext(from) %>% stringr::str_to_lower()) %>%
    dplyr::mutate(type_to = dplyr::case_when(ext_to %in% c("r", "rmd") ~ "code",
                               ext_to == "rds" ~ "datar",
                               ext_to %in% c("csv", "xls", "xlsx", "txt") ~ "datatext")) %>%
    dplyr::mutate(type_from = dplyr::case_when(ext_from %in% c("r", "rmd") ~ "code",
                                 ext_from == "rds" ~ "datar",
                                 ext_from %in% c("csv", "xls", "xlsx", "txt") ~ "datatext")) %>%
    sorensonimpact::deselect(dplyr::ends_with("_full"), target, code, operation, path_assign)

  return(rw)

}
