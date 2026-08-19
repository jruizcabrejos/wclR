\name{wcl_actor_movement}
\alias{wcl_actor_movement}
\title{Track sampled actor movement}
\usage{
wcl_actor_movement(
  events,
  actor_ids = NULL,
  actor_role = c("auto", "source", "target"),
  output = c("summary", "track")
)
}
\arguments{
\item{events}{A data frame returned by \code{wcl_events()} with resource
coordinates. Retrieve it with \code{include_resources = TRUE}.}

\item{actor_ids}{Optional integer-like report actor IDs to retain.}

\item{actor_role}{How coordinate ownership is determined. \code{"auto"}
uses \code{resourceActor} (1 for source and 2 for target). \code{"source"}
or \code{"target"} restricts the role and supplies a fallback when ownership
is missing.}

\item{output}{\code{"summary"} returns one row per report, fight, actor
instance, and map; \code{"track"} returns every retained position sample.}
}
\value{
A tibble. Track output includes normalized \code{x}/\code{y}, raw coordinates,
sample gaps, step distance, cumulative distance, and speed. Summary output
includes observed distance, displacement, duration, step, and sampling-gap
diagnostics.
}
\description{
Resolves the resource actor that owns each Warcraft Logs coordinate sample and
calculates two-dimensional movement between consecutive observations.
}
\details{
Warcraft Logs stores positions multiplied by 100; this helper retains those
values as \code{x_raw}/\code{y_raw} and divides \code{x}/\code{y} by 100 into
game-coordinate units (yards). See the
\href{https://www.warcraftlogs.com/help/pins}{WCL resource-field documentation}
and the
\href{https://forums.combatlogforums.com/t/warcraftlogs-api-positional-data/15492}{positional-data clarification}
for the underlying event semantics.
Movement resets across reports, fights, actor instances, and maps. It is a
sampled lower bound on the actor's path, not a reconstruction of movement
between observations. Conflicting coordinates for one actor and timestamp
warn and keep the first input row.
}
