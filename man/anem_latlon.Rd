% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/field-data-funs.R
\name{anem_latlon}
\alias{anem_latlon}
\title{Pull in the location of anemones}
\usage{
anem_latlon(anem_ids)
}
\arguments{
\item{anem_ids}{}
}
\value{
A tibble/data frame of locations for anem_ids
}
\description{
Pull in the location of anemones
}
\examples{
location <- anem_latlon(c(2554, 0815))
}
