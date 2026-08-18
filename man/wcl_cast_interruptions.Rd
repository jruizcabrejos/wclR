\name{wcl_cast_interruptions}
\alias{wcl_cast_interruptions}
\title{Summarize completed and interrupted cast attempts}
\usage{
wcl_cast_interruptions(
  events,
  interrupt_events = NULL,
  source_ids = NULL,
  ability_ids = NULL,
  output = c("summary", "attempts")
)
}
\arguments{
\item{events}{A data frame containing \code{begincast} and \code{cast} rows,
normally returned by \code{wcl_events(..., data_type = "Casts")}. Inline
interrupt rows are also accepted.}

\item{interrupt_events}{Optional separate data frame of explicit interrupt
events. When supplied, these replace inline interrupt rows in \code{events}.}

\item{source_ids}{Optional integer-like caster report actor IDs.}

\item{ability_ids}{Optional integer-like cast ability game IDs. For interrupt
rows this filters the stopped ability.}

\item{output}{\code{"summary"} returns counts per fight, caster instance,
and ability; \code{"attempts"} returns detailed cast attempts.}
}
\value{
A tibble. Attempt output includes status, begin/cast/interrupt timestamps,
elapsed time, cast time, interrupter identity, and confidence flags. Summary
output separates completed casts, confirmed explicit interruptions, and
inferred interruptions.
}
\description{
Pairs Warcraft Logs cast starts with successful cast completions or explicit
interrupt events and retains instant casts.
}
\details{
An explicit interrupt targeting the caster is confirmed. An unmatched
\code{begincast} is classified separately as inferred because cancellation,
death, encounter end, or truncated input can produce the same event pattern.
Matching never crosses reports, fights, caster instances, or abilities.
}
