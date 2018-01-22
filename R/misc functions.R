#' Write Clip shortcut
#' @description Copies an object to the clipboard. Defaults to .Last.value
#' @export
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
  message("Value copied to clipboard")
}

#' Value not matching (oppsoite of %in%)
#' @description Returns a logical vector indicating where there is not a match for its left operation.  Opposite of %in%
#' @return A logical vector, indicating if there was no match for each element.
#' @export
"%ni%" <- Negate("%in%")
