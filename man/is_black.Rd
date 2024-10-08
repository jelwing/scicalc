% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{is_black}
\alias{is_black}
\title{Takes character input and returns TRUE/FALSE if black/other also checks for "African American" and "Black or African American"}
\usage{
is_black(x)
}
\arguments{
\item{x}{input character representing race}
}
\value{
boolean representing Race == Black
}
\description{
Takes character input and returns TRUE/FALSE if black/other also checks for "African American" and "Black or African American"
}
\examples{
is_black("WHITE")

is_black(c("AFRICAN AMERICAN", "BLACK"))
}
