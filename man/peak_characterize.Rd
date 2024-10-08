% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peak.characterize.R
\name{peak_characterize}
\alias{peak_characterize}
\title{Internal function: Updates the information of a feature for the known feature table.}
\usage{
peak_characterize(existing_row = NA, metadata_row, ftrs_row, rt_row)
}
\arguments{
\item{existing_row}{The existing row in the known feature table.}

\item{ftrs_row}{The row of the matched feature in the new aligned feature table.}

\item{rt_row}{The row of the matched feature in the new retention time table of aligned features.}
}
\value{
A vector, the updated row for the known feature table.
}
\description{
The function takes the information about the feature in the known feature table (if available), and updates it using the
information found in the current dataset.
}
