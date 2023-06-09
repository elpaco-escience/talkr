% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convplot.R
\name{convplot}
\alias{convplot}
\title{convplot}
\usage{
convplot(
  data,
  uids = NULL,
  lang = NULL,
  n_uid = 10,
  window = NULL,
  before = 10000,
  after = 10000,
  printuids = T,
  verbose = T,
  dyads = F,
  content = F,
  highlight = F,
  center = F,
  datamode = F,
  alldata = F,
  debug = F
)
}
\arguments{
\item{data}{dataset}

\item{uids}{set of uids to plot (optional; if omitted, n uids are sampled)}

\item{lang}{language from which to sample uids (if not supplied)}

\item{n_uid}{number of uids to sample}

\item{window}{time window in ms (optional; if supplied, window will be split into before and after)}

\item{before}{stretch to include before selected turn (default: 10000ms, unless `window` is supplied)}

\item{after}{stretch to include after selected turn (default: 0, unless `window` is supplied)}

\item{printuids}{print the sampled uids}

\item{verbose}{print language and information about selected uids}

\item{dyads}{if TRUE, select only dyadic interactions for display}

\item{content}{if TRUE, render annotation content (EXPERIMENTAL)}

\item{highlight}{if TRUE, highlight the uid in focus in the plot}

\item{center}{if TRUE, center the plot around the uid in focus}

\item{datamode}{if TRUE, outputs dataframe instead of plot, for more advanced plotting}

\item{alldata}{if TRUE, output all data, not just the selected dyads}

\item{debug}{if TRUE, print the selected data and some other diagnostics}
}
\description{
Note: slightly more up to date forks in convplot-dev repository
https://github.com/mdingemanse/convoplot-dev
}
