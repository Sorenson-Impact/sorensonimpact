#' Apply all SI knitr options
#' @description Applies all Sorenson Impact custom knitr options.
#' @return Invisibly sets knitr options
#' @examples
#' SI_knitr_update()
#' @export

si_knitr_settings <- function(verbose = F) {
  # Echo=FALSE means the code does not show up in the document
  # Caching allows you to re-run the report quickly
  knitr::opts_chunk$set(echo=FALSE)
  knitr::opts_chunk$set(cache=TRUE)
  knitr::opts_chunk$set(message = T) #Use F to suppress all messages from chunks for finalized report
  knitr::opts_chunk$set(warning = T) #Use F to suppress all warnings from chunks for finalized report

  # Setting the default resolution of plots
  knitr::opts_chunk$set(dpi = 300)

  # Setting how numbers are displayed
  knit_hooks$set(inline = function(x) { #This puts a nice comma in large inline numbers to group by 3 digits
    prettyNum(x, big.mark=",")
  })

  if(verbose) message("knitr opts_chunk set to: echo = F, cache = T, message = T, warning = T, dpi = 300, comma bigmark for inline numbers.")

}


#' Draft a Sorenson Impact Full Report
#' @description Drafts a new Sorenson Impact fulll rmarkdown report
#' @param file File name for the draft
#' @examples
#' draft_full_report("My presentation.Rmd")
#' @export
si_draft_full_report <- function(file) {
  if(dirname(file) == ".") { #If path wasn't provided, place assume the working directory is the base dir and create subdir for markdown file.
    message("Directory not provided. Using working directory.")
    file <- file.path(getwd(), file)
  }

  if(tools::file_ext(file) != "Rmd") { #Fix the capitalization of Rmd
    message("Setting file extension to .Rmd")
    file <- paste0(tools::file_path_sans_ext(file), ".Rmd")
  }

  rmarkdown::draft(file = file,
                   template = "si_full_report",
                   package = "sorensonimpact",
                   edit = F)

  file.edit(file.path(tools::file_path_sans_ext(file), basename(file)))
}


#' Draft a Sorenson Impact Presentation
#' @description Drafts a new Sorenson Impact ioslides presentation.
#' @param file File name for the draft
#' @examples
#' si_draft_presentation("My presentation.Rmd")
#' @export
si_draft_presentation <- function(file) {

  if(dirname(file) == ".") { #If path wasn't provided, place assume the working directory is the base dir and create subdir for markdown file.
    message("Directory not provided. Using working directory.")
    file <- file.path(getwd(), file)
  }

  if(tools::file_ext(file) != "Rmd") { #Fix the capitalization of Rmd
    message("Setting file extension to .Rmd")
    file <- paste0(tools::file_path_sans_ext(file), ".Rmd")
  }

  rmarkdown::draft(file = file,
                  template = "si_ioslides",
                  package = "sorensonimpact",
                  edit = F)
  file.edit(file.path(tools::file_path_sans_ext(file), basename(file)))
}
