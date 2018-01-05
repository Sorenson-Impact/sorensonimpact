#' Build .gitignore
#' @description Generates a .gitignore file and adds it to the repo.
#' @param dir The location to create the .gitignore.  Defaults to working directory.
#' @examples
#' build_gitignore()
#' @export

build_gitignore <- function(dir = getwd()) {

  # builds the gitignore in the given workind directory

  to_ignore <- c(".Rhistory", ".Rprofile", ".Rapp.history", ".RData", "*-Ex.R", "/*.tar.gz", "/*.Rcheck/",
".Rproj.user/", "vignettes/*.html", "vignettes/*.pdf", ".httr-oauth", "/*_cache/", "/cache/",
"*.utf8.md", "*.knit.md", ".Rproj.user", "*.Rda", "*.pdf", "*.csv", "*.html", "*.DS_Store")

  fileConn <- file(file.path(dir, ".gitignore"))
  writeLines(text = to_ignore, con = fileConn)
  close(fileConn)
}
