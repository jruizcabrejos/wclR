\name{wcl_spell_travel_time}
\alias{wcl_spell_travel_time}
\title{Measure direct spell travel time}
\usage{
wcl_spell_travel_time(
  cast_events,
  landing_events,
  max_travel_ms,
  source_ids = NULL,
  target_ids = NULL,
  ability_map = NULL,
  landing_types = c("damage", "heal", "miss", "applybuff", "applydebuff"),
  output = c("summary", "pairs")
)
}
\arguments{
\item{cast_events}{A data frame containing completed \code{cast} rows.}

\item{landing_events}{A data frame containing direct landing events such as
damage, healing, misses, or aura applications.}

\item{max_travel_ms}{Required finite positive maximum cast-to-landing delay in
milliseconds.}

\item{source_ids, target_ids}{Optional integer-like source and target report
actor IDs.}

\item{ability_map}{Optional one-to-one cast/landing ability mapping: either a
data frame with \code{cast_ability_id} and \code{landing_ability_id}, or a
named vector from cast IDs to landing IDs. With \code{NULL}, IDs must match.}

\item{landing_types}{Non-empty character vector of accepted landing event
types. Periodic ticks are excluded.}

\item{output}{\code{"summary"} returns match and timing aggregates;
\code{"pairs"} returns one diagnostic row per cast.}
}
\value{
A tibble. Pair output retains every cast with matched, unmatched, or ambiguous
status, candidate counts, and travel time. Summary output contains matching
coverage and travel-time statistics.
}
\description{
Pairs successful casts with one direct target landing using deterministic,
one-to-one FIFO matching inside an explicit travel-time window.
}
\details{
Matching uses report, fight, source and target instances, and mapped ability.
Warcraft Logs exposes no universal projectile correlation ID, so overlapping
eligible casts or landings are flagged as ambiguous. AoE, chain, and multi-hit
fan-out are intentionally out of scope.
}
