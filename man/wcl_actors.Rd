\name{wcl_actors}
\alias{wcl_actors}
\title{Retrieve actors from a report}
\usage{
wcl_actors(report_code, type = NULL, client = NULL)
}
\arguments{
\item{report_code}{Report code or report URL.}

\item{type}{Optional actor type filter passed to the Warcraft Logs
\code{masterData(...actors(type = ))} query.}

\item{client}{An optional \code{wcl_client} object.}
}
\value{
A tibble with one row per actor.
}
\description{
Retrieve and normalize report actors from Warcraft Logs master data.
}
