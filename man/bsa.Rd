% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bsa.R
\name{bsa}
\alias{bsa}
\title{Calculates Body Surface Area based on Weight and Height using the method specified. Default is Dubois.}
\usage{
bsa(weight, height, method = "Dubois")
}
\arguments{
\item{weight}{weight of a subject (kg)}

\item{height}{height of a subject (cm)}

\item{method}{String to dictate which equation to use. Dubois or Mosteller.}
}
\value{
bsa (m^2)
}
\description{
Calculates Body Surface Area based on Weight and Height using the method specified. Default is Dubois.
}
\examples{
bsa(70, 170)
bsa(70, 170, method = "Mosteller")
bsa(70, 170, method = "Dubois")
}
