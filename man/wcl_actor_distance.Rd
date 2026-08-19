\name{wcl_actor_distance}
\alias{wcl_actor_distance}
\title{Measure sampled distance between two actor tracks}
\usage{
wcl_actor_distance(
  track_a,
  track_b,
  max_hold_ms = Inf,
  output = c("summary", "series")
)
}
\arguments{
\item{track_a, track_b}{Movement-track data frames returned by
\code{wcl_actor_movement(..., output = "track")}.}

\item{max_hold_ms}{Maximum age in milliseconds for a carried-forward
position. \code{Inf}, the default, holds positions throughout the shared
observed range.}

\item{output}{\code{"summary"} returns one row per compatible actor pair;
\code{"series"} returns the aligned timestamp series.}
}
\value{
A tibble. Series output contains carried coordinates, source-sample ages,
distance, hold validity, and covered duration. Summary output includes overlap,
observed-overlap and full-fight coverage, maximum held-position ages, and
minimum, time-weighted mean, median, and maximum distance.
}
\description{
Aligns compatible actor tracks on their combined timestamps with
last-observation-carried-forward and calculates two-dimensional separation.
}
\details{
Matching is restricted to the same report, fight, and map and never
extrapolates before the first or after the last observation. A finite
\code{max_hold_ms} invalidates stale comparisons. Coverage fields make sparse
or stale tracks explicit. \code{comparison_coverage_s} is the valid comparison
duration; \code{coverage_fraction} divides it by the observed overlap, and
\code{fight_coverage_fraction} divides it by \code{fight_duration_s} when a
positive finite fight duration is available. \code{max_actor_a_age_ms},
\code{max_actor_b_age_ms}, and \code{max_held_position_age_ms} report the
oldest carried samples among valid comparisons.
}
