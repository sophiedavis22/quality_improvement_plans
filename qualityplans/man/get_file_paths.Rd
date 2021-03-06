% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{get_file_paths}
\alias{get_file_paths}
\title{Extract file names and locations}
\usage{
get_file_paths(config_file = "D:/Repos/quality_improvement_plans/config.yaml")
}
\arguments{
\item{config_file}{.yaml file containing config, with default settings (data directory, null values, metric ranges etc)}
}
\value{
All file paths as character vector (alphabetical)
}
\description{
Function to extract list of file paths from folder with QIPs, sorted alphabetically
}
