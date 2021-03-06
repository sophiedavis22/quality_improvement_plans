% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_risks.R
\name{merge_risks}
\alias{merge_risks}
\title{Collate Quality Risks for single division}
\usage{
merge_risks(risk_list, division_order)
}
\arguments{
\item{risk_list}{List of quality risks for single division}

\item{division_order}{Numeric vector from 1 to number of divisions}
}
\value{
Table with total column
}
\description{
Function to merge all Quality Risks for one division into one table, with row totals for number of risks affecting each dimension
}
