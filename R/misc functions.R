#' Write Clip shortcut
#' @description Copies an object to the clipboard. Defaults to .Last.value
#' @export
wc <- function(x = .Last.value) {
  clipr::write_clip(x)
  message("Value copied to clipboard")
}

#' Not In: Inverse Value Matching
#' @description Returns a logical vector indicating if there is NOT a match for the LHS vector anywhere in the RHS vector. Opposide of \code{\%in\%}.
#' @usage x %ni% y
#' @param x vector or NULL: the values to check for non-match
#' @param y vector or NULL: the values to be matched against
#' @return A logical vector, indicating if there was no match for each element of x. Values are TRUE or FALSE and never NA
#' @rdname ni
#' @export
"%ni%" <- Negate("%in%")
