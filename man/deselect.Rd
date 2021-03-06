% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_custom_functions.R
\name{deselect}
\alias{deselect}
\title{Remove variables from tibble}
\usage{
deselect(.data, ...)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). See Methods, below, for more details.}

\item{...}{<tidy-select> One or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.}
}
\value{
An object of the same type as .data, with the specified columns removed.
}
\description{
\lifecycle{maturing}
This is a simple negation of `dplyr::select`.
}
