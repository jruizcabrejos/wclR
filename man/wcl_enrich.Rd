\name{wcl_enrich}
\alias{wcl_join_actors}
\alias{wcl_join_players}
\alias{wcl_add_relative_time}
\title{Enrich events and roster tables}
\usage{
wcl_join_actors(events, actors)

wcl_join_players(data, players)

wcl_add_relative_time(
  events,
  unit = c("seconds", "milliseconds", "minutes")
)
}
\arguments{
\item{events}{A tibble with event-style timing or actor ID columns.}

\item{actors}{A tibble returned by \code{wcl_actors()} or a compatible data
frame.}

\item{data}{A tibble with one or more of \code{actorID}, \code{sourceID}, or
\code{targetID}.}

\item{players}{A tibble returned by \code{wcl_player_details()} or a compatible
data frame.}

\item{unit}{Output unit for \code{time_since_fight_start}.}
}
\value{
\code{wcl_join_actors()} and \code{wcl_join_players()} return the input table
with actor or roster metadata appended.
\code{wcl_add_relative_time()} returns the input event table with a numeric
\code{time_since_fight_start} column.
}
\description{
Helpers for the repeated post-retrieval joins and relative-time calculations
that appear throughout Warcraft Logs analysis workflows.
}
