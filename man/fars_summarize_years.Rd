% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summary table of number of incidents per month per year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{numeric vector defining years to read in from raw file}
}
\value{
table summarising the number incidents per calendar month (represented as rows) in each year (represented in columns)
}
\description{
This function generates a summary table of the number incidents per calendar month in each of the yearly files defined
}
\note{
ERROR if following conditions not met: Files must be in working directory and filenames must be of format "accident_<year>.csv.bz2"
}
\examples{
\dontrun{
fars_summarize_years(2013:2015)
}

}
