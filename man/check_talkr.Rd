% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_talkr}
\alias{check_talkr}
\title{Check the presence of talkr-workflow columns in the dataset.}
\usage{
check_talkr(data)
}
\arguments{
\item{data}{dataset to check}
}
\description{
Uses check_columns() to check for:
- begin
- end
- participant
- utterance
- source
- uid
}
\details{
Verifies that begin and end columns are numeric, and likely indicate milliseconds.
}
