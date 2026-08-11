\name{wcl_rankings}
\alias{wcl_rankings}
\title{Retrieve encounter rankings for a zone}
\usage{
wcl_rankings(
  zone_id,
  metric,
  page,
  encounter_id = NULL,
  class_name = NULL,
  client = NULL
)
}
\arguments{
\item{zone_id}{Warcraft Logs zone ID.}

\item{metric}{Ranking metric. Without \code{class_name}, use a
\href{https://www.warcraftlogs.com/v2-api-docs/warcraft/fightrankingmetrictype.doc.html}{fight-ranking metric}
such as \code{"speed"}, \code{"progress"}, or \code{"execution"}. With
\code{class_name}, use a
\href{https://www.warcraftlogs.com/v2-api-docs/warcraft/characterrankingmetrictype.doc.html}{character-ranking metric}
such as \code{"dps"} or \code{"hps"}.}

\item{page}{Ranking page number or vector of pages.}

\item{encounter_id}{Optional encounter ID filter.}

\item{class_name}{Optional class name. If supplied, character rankings are
requested instead of fight rankings.}

\item{client}{An optional \code{wcl_client} object.}
}
\value{
A tibble with one row per ranking entry.
}
\description{
Retrieve and flatten Warcraft Logs fight rankings or character rankings for a
zone.
}
