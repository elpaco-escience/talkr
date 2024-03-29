% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenize.R
\name{tokenize}
\alias{tokenize}
\title{Generate a token-specific dataframe}
\usage{
tokenize(data, utterancecol = "utterance")
}
\arguments{
\item{data}{a talkr dataset}

\item{utterancecol}{the name of the column containing the clean utterance (defaults to "utterance")}
}
\value{
a dataframe with details about each token in the utterance
}
\description{
From a dataframe with utterances, generate a dataframe that separates tokens
in utterances, and assesses their relative timing.
The returned data contains information about the original utterance (`uid`),
as well as the number of tokens in the utterance (`nwords`), and the relative
time of the token in the utterance (`relative_time`).
}
\details{
The relative time is calculated with each token in an utterance having an equal
duration (the duration of the utterance divided by the number of words), and
the first token in the utterance beginning at the beginning of the utterance.

The input column provided with the argument `utterancecol` is used to generate
the tokens. It is advised to provide a version of the utterance that has been
cleaned and stripped of special characters. Cleaning is not performed in this
function. Spaces are used to separate tokens.
}
