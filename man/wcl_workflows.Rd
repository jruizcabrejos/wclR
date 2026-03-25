\name{wcl_workflows}
\alias{wcl_zone_fights}
\alias{wcl_rankings_set}
\title{Batch workflow helpers}
\usage{
wcl_zone_fights(zone_id, pages = NULL, distinct = TRUE, client = NULL)

wcl_rankings_set(
  zone_id,
  metrics,
  pages,
  encounter_id = NULL,
  class_name = NULL,
  client = NULL
)
}
\arguments{
\item{zone_id}{Warcraft Logs zone ID.}

\item{pages}{Optional page number or vector of page numbers.}

\item{distinct}{Whether to apply \code{wcl_distinct_fights()} to the combined
fight table.}

\item{client}{An optional \code{wcl_client} object.}

\item{metrics}{Ranking metrics such as \code{"speed"} or \code{"progress"}.}

\item{encounter_id}{Optional encounter ID filter.}

\item{class_name}{Optional class name for character-ranking mode.}
}
\value{
\code{wcl_zone_fights()} returns a tibble with one row per fight plus the
report-page metadata returned by \code{wcl_reports()}.
\code{wcl_rankings_set()} returns a tibble with one row per ranking entry
across all requested metrics and pages.
}
\description{
Helpers for the two most common retrieval workflows in the package: expanding
zone report pages into fight rows and crawling multiple ranking metrics at
once.
}
