\name{wcl_reports}
\alias{wcl_reports}
\alias{wcl_report_code}
\title{Retrieve public reports and normalize report codes}
\usage{
wcl_report_code(x)

wcl_reports(zone_id, pages = 1:3, client = NULL)
}
\arguments{
\item{x}{A bare report code or a full Warcraft Logs report URL.}

\item{zone_id}{Warcraft Logs zone ID.}

\item{pages}{Page number or vector of exact page numbers. Defaults to pages
1 through 3. Values above 25 are capped with a warning. Use \code{NULL} to
discover pages automatically through page 25.}

\item{client}{An optional \code{wcl_client} object.}
}
\value{
\code{wcl_report_code()} returns a character vector of report codes.
\code{wcl_reports()} returns a tibble with one row per report.
}
\description{
Helpers for normalizing Warcraft Logs report identifiers and crawling public
report listings for a zone. After every requested page, \code{wcl_reports()}
emits a suppressible message with the page count and running total.
}
