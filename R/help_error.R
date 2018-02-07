help_error_so <- function(error = geterrmessage(), add_error_term = T) {
  error_plus <- stringr::str_replace_all(geterrmessage(), pattern = " ", replacement = "+")
  if(add_error_term) error_plus <- paste("error", error_plus, sep = "+")
  url <-paste0("https://stackoverflow.com/search?q=%5Br%5D+", error_plus)
  browseURL(url)
}

help_error_google <-  function(error = geterrmessage(), add_error_term = T) {
  error_plus <- stringr::str_replace_all(geterrmessage(), pattern = " ", replacement = "+")
  if(add_error_term) error_plus <- paste("error", error_plus, sep = "+")
  url <- paste0("https://www.google.com/search?q=R+", error_plus)
  browseURL(url)
}
