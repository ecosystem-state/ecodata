% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rreas.R
\name{get_rreas}
\alias{get_rreas}
\title{get_rreas is the primary function for retrieving RREAS data via ERDDAP}
\usage{
get_rreas(min_year = 1985, lat_max = 38.1884, lat_min = 36.544)
}
\arguments{
\item{min_year}{The minimum year used, defaults to 1985}

\item{lat_max}{The maximum latitude boundary as an optional bounding box for stations to include, defaults to 38.1884}

\item{lat_min}{The minimum latitude boundary as an optional bounding box for stations to include, defaults to 36.544}
}
\value{
null, data file written to inst/ folder
}
\description{
get_rreas is the primary function for retrieving RREAS data via ERDDAP
}
\examples{
\dontrun{
get_rreas(min_year = 1990, min_years = 10, min_n = 100)
}
}
