\name{wcl_reports}
\alias{wcl_reports}
\alias{wcl_report_code}
\title{Retrieve public reports and normalize report codes}
\usage{
wcl_report_code(x)

wcl_reports(zone_id, pages = NULL, client = NULL)
}
\arguments{
\item{x}{A bare report code or a full Warcraft Logs report URL.}

\item{zone_id}{Warcraft Logs zone ID.}

\item{pages}{Optional page number or vector of page numbers.}

\item{client}{An optional \code{wcl_client} object.}
}
\value{
\code{wcl_report_code()} returns a character vector of report codes.
\code{wcl_reports()} returns a tibble with one row per report.
}
\description{
Helpers for normalizing Warcraft Logs report identifiers and crawling public
report listings for a zone.
}
