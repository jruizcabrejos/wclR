\name{wcl_player_details}
\alias{wcl_player_details}
\title{Retrieve player details for a specific fight}
\usage{
wcl_player_details(report_code, fight_id, client = NULL)
}
\arguments{
\item{report_code}{Report code or report URL.}

\item{fight_id}{Fight ID inside the report.}

\item{client}{An optional \code{wcl_client} object.}
}
\value{
A tibble with one row per player and a \code{role} column.
}
\description{
Retrieve the Warcraft Logs \code{playerDetails} payload for a single fight and
flatten tanks, healers, and DPS into one tidy table.
}
