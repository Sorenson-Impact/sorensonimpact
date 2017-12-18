build_gitignore <- function(wd) {
  
  # builds the gitignore in the given workind directory
  
  to_ignore <- c(".Rhistory", ".Rprofile", ".Rapp.history", ".RData", "*-Ex.R", "/*.tar.gz", "/*.Rcheck/", 
".Rproj.user/", "vignettes/*.html", "vignettes/*.pdf", ".httr-oauth", "/*_cache/", "/cache/", 
"*.utf8.md", "*.knit.md", ".Rproj.user", "*.Rda", "*.pdf", "*.csv", "*.html", "*.DS_Store")
  
  # Todo: if wd does not have a backslash at the end
  fileConn <- file(paste0(wd, ".gitignore"))
  writeLines(to_ignore, fileConn)
  close(fileConn)
}
