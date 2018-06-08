# .onAttach <- function(...) {
#   library(conflicted)
#   library(tidyverse)
#   library(knitr)
#   library(rmarkdown)
#   library(fs)
#   library(readxl)
# }

# check_si_up_to_date <- function(pkg, repo="github") {
#   installed_version <- tryCatch(packageVersion(gsub(".*/", "", pkg)), error=function(e) NA)
#
#   if(repo == "github") {
#     url <- paste0("https://raw.githubusercontent.com/", pkg, "/master/DESCRIPTION")
#   } else if (repo == "gitlab") {
#     url <- paste0("https://gitlab.com/", pkg, "/raw/master/DESCRIPTION")
#   } else {
#     stop("only work with github and gitlab")
#   }
#
#   x <- readLines(url)
#   remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])
#
#   res <- list(package = pkg,
#               installed_version = installed_version,
#               latest_version = remote_version,
#               up_to_date = NA)
#
#   if (is.na(installed_version)) {
#     message(paste("##", pkg, "is not installed..."))
#     message(msg)
#   } else {
#     if (remote_version > installed_version) {
#       msg <- paste("##", pkg, "is out of date...")
#       message(msg)
#       res$up_to_date <- FALSE
#     } else if (remote_version == installed_version) {
#       message("package is up-to-date devel version")
#       res$up_to_date <- TRUE
#     }
#   }
#
#   return(res)
# }
