% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/janitor_custom_functions.R
\name{adorn_everything}
\alias{adorn_everything}
\title{Adorn Everything}
\usage{
adorn_everything(dat, pct_direction = "col")
}
\arguments{
\item{pct_direction}{Should the cell percentages use the "col" or "row" totals as denominator. Defaults to "col".}
}
\value{
tabyl with percents, percent formatting, ns, and better total percents.
}
\description{
\lifecycle{experimental}
Adorn percentages, percent formatting, and ns to a \code{janitor::tabyl()} with better percents
}
\examples{
\dontrun{
mtcars \%>\% tabyl(cyl, gear) \%>\% adorn_everything(pct_direction = "col")
}
}
