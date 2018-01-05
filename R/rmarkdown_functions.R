#' Apply all SI knitr options
#' @description Applies all Sorenson Impact custom knitr options.
#' @return Invisibly sets knitr options
#' @examples
#' SI_knitr_update()
#' @export

SI_knitr_update <- function() {
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
}


#' Draft a Sorenson Impact Full Report
#' @description Drafts a new Sorenson Impact fulll rmarkdown report
#' @param file File name for the draft
#' @examples
#' draft_full_report("My presentation.Rmd")
#' @export
si_draft_full_report <- function(file) {
  rmarkdown::draft(file = file,
                   template = "si_full_report",
                   package = "sorensonimpact",
                   edit = F)
  file.edit(file.path(tools::file_path_sans_ext(file), file))
}


#' Draft a Sorenson Impact Presentation
#' @description Drafts a new Sorenson Impact ioslides presentation.
#' @param file File name for the draft
#' @examples
#' si_draft_presentation("My presentation.Rmd")
#' @export
si_draft_presentation <- function(file) {
  rmarkdown::draft(file = file,
                  template = "si_ioslides",
                  package = "sorensonimpact",
                  edit = F)
  file.edit(file.path(tools::file_path_sans_ext(file), file))
}
