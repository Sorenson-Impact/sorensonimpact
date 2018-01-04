#' Install template files for SI to a project directory.
#' @description Installs SI template files to project directory
#' @param local_github_dir Where the cloned repo is.
#' @param overwrite If the files already exist, do you want to overwrite (default = FALSE)_
#' @examples
#' install_project_files()
#' @export

install_project_files <- function(local_github_dir, overwrite = F) {

  #make and check the provided github dir
  local_github_dir <- file.path(local_github_dir)
  if(!dir.exists(local_github_dir)) stop(paste0("The provided directory (", local_github_dir, ") does not exist."))

  package_path = system.file(package = "sorensonimpact") #Get path to package files that will be copied

  #Add .gitignore
  file.copy(from = file.path(package_path, "project_files/.gitignore"), to = file.path(local_github_dir), overwrite = overwrite)
  file.copy(from = file.path(package_path, "project_files/Rstudio Project File.Rproj"), to = file.path(local_github_dir, paste0(basename(local_github_dir), ".Rproj")), overwrite = overwrite)

  #copy template files
    file.copy(from = file.path(package_path, "ioslides"), to = file.path(local_github_dir, "template_files/ioslides"), overwrite = overwrite, recursive = T)

    file.copy(from = file.path(package_path, "rmarkdown/templates"), to = file.path(local_github_dir, "template_files", "rmarkdown"), overwrite = overwrite, recursive = T)

}
#rstudioapi::getSourceEditorContext()$path
#basename(rstudioapi::getSourceEditorContext()$path)

# full_report <- function() {
#   rmarkdown::word_document(
#     reference_docx: ~/Github/SI_Project_Template/template_files/report_template/mystyles.docx
#     fig_width: 6
#     fig_height: 4
#   )
# }
