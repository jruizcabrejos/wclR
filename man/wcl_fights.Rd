\name{wcl_fights}
\alias{wcl_fights}
\alias{wcl_distinct_fights}
\title{Retrieve and de-duplicate fights}
\usage{
wcl_fights(report_code, client = NULL)

wcl_distinct_fights(
  fights,
  by = c(
    "encounterID",
    "difficulty",
    "size",
    "kill",
    "bossPercentage",
    "duration_s",
    "averageItemLevel"
  ),
  keep_all = TRUE
)
}
\arguments{
\item{report_code}{Report code or report URL.}

\item{client}{An optional \code{wcl_client} object.}

\item{fights}{A tibble returned by \code{wcl_fights()} or a compatible data
frame.}

\item{by}{Columns used to define duplicate fights.}

\item{keep_all}{Whether to keep all columns from the first matching row.}
}
\value{
\code{wcl_fights()} returns a tibble with one row per fight.
\code{wcl_distinct_fights()} returns a tibble with duplicate fights removed.
}
\description{
Helpers for retrieving fight-level metadata from a report and for applying the
same duplicate removal logic used throughout the existing analysis scripts.
}
