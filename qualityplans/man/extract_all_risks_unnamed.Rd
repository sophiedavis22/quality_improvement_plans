% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_risks.R
\name{extract_all_risks_unnamed}
\alias{extract_all_risks_unnamed}
\title{Extract Quality Risks for all divisions}
\usage{
extract_all_risks_unnamed(
  file_paths,
  config_file = "D:/Repos/quality_improvement_plans/config.yaml"
)
}
\arguments{
\item{file_paths}{Character vector of file paths}

\item{config_file}{.yaml file containing config, with default settings (data directory, null values, metric ranges etc)}
}
\value{
List of tables (list by division, sub-lists by risk)
}
\description{
Function to extract tab names (for each division) and extract Quality Risk tables for all divisions
}
