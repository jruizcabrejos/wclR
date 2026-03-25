\name{wcl_events}
\alias{wcl_events}
\title{Retrieve paginated event data for a fight}
\usage{
wcl_events(
  report_code,
  fight_id,
  data_type,
  client = NULL,
  kill_type = "Encounters",
  hostility_type = "All",
  source_id = NULL,
  target_id = NULL,
  filter_expression = NULL,
  start_time = 0,
  end_time = 999999999999,
  paginate = TRUE
)
}
\arguments{
\item{report_code}{Report code or report URL.}

\item{fight_id}{Fight ID inside the report.}

\item{data_type}{Event data type such as \code{"DamageTaken"},
\code{"Buffs"}, \code{"Casts"}, or \code{"All"}.}

\item{client}{An optional \code{wcl_client} object.}

\item{kill_type}{Kill type filter.}

\item{hostility_type}{Hostility filter.}

\item{source_id}{Optional source actor ID.}

\item{target_id}{Optional target actor ID.}

\item{filter_expression}{Optional Warcraft Logs filter expression.}

\item{start_time}{Event query start time in report milliseconds.}

\item{end_time}{Event query end time in report milliseconds.}

\item{paginate}{Whether to keep following \code{nextPageTimestamp}.}
}
\value{
A tibble with one row per event.
}
\description{
Retrieve and normalize paginated Warcraft Logs event data for a single fight.
}
