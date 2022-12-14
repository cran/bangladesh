% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_map.R
\name{get_map}
\alias{get_map}
\title{get shapefile for different administrative levels}
\usage{
get_map(level = "country")
}
\arguments{
\item{level}{administrative level of bangladesh. Should be one of: "country", "division", "district", "upazila","union'}
}
\value{
shapefile for given administrative level
}
\description{
get shapefile for different administrative levels
}
\examples{
country <- get_map("country")
division <- get_map("division")
district <- get_map("district")
}
