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
  hostility_type = "Friendlies",
  source_id = NULL,
  target_id = NULL,
  filter_expression = NULL,
  start_time = 0,
  end_time = 999999999999,
  paginate = TRUE,
  include_resources = TRUE,
  use_ability_ids = TRUE,
  use_actor_ids = TRUE
)
}
\arguments{
\item{report_code}{Report code or report URL.}

\item{fight_id}{Fight ID inside the report.}

\item{data_type}{Event data type such as \code{"DamageTaken"},
\code{"Buffs"}, \code{"Casts"}, or \code{"All"}. See
\href{https://www.warcraftlogs.com/v2-api-docs/warcraft/eventdatatype.doc.html}{EventDataType}
for every supported value.}

\item{client}{An optional \code{wcl_client} object.}

\item{kill_type}{Kill type filter.}

\item{hostility_type}{Hostility filter. Must be exactly \code{"Enemies"} or
\code{"Friendlies"}. See
\href{https://www.warcraftlogs.com/v2-api-docs/warcraft/hostilitytype.doc.html}{HostilityType}.}

\item{source_id}{Optional source actor ID.}

\item{target_id}{Optional target actor ID.}

\item{filter_expression}{Optional Warcraft Logs filter expression.}

\item{start_time}{Event query start time in report milliseconds.}

\item{end_time}{Event query end time in report milliseconds.}

\item{paginate}{Whether to keep following \code{nextPageTimestamp}.}

\item{include_resources}{Whether to include detailed unit resources such as
\code{classResources}. Defaults to \code{TRUE} for compatibility.}

\item{use_ability_ids}{Whether to include detailed ability information from
report master data. Setting this to \code{FALSE} reduces payload size and can
change which ability detail or ID columns are present. Defaults to \code{TRUE}
for compatibility.}

\item{use_actor_ids}{Whether to include detailed actor information from
report master data. Setting this to \code{FALSE} reduces payload size and can
change which actor detail or ID columns are present. Defaults to \code{TRUE}
for compatibility.}
}
\value{
A tibble with one row per event.
}
\description{
Retrieve and normalize paginated Warcraft Logs event data for a single fight.
}
\details{
\href{https://www.warcraftlogs.com/v2-api-docs/warcraft/reporteventpaginator.doc.html}{ReportEventPaginator.data}
is returned as an opaque
\href{https://www.warcraftlogs.com/v2-api-docs/warcraft/json.doc.html}{JSON scalar},
so arbitrary nested columns cannot be excluded server-side by name. Use the
three logical bandwidth controls above instead. The complete event arguments
are documented under
\href{https://www.warcraftlogs.com/v2-api-docs/warcraft/report.doc.html}{Report.events}.
}
