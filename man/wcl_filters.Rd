\name{wcl_filters}
\alias{wcl_filter_abilities}
\alias{wcl_filter_types}
\alias{wcl_filter_and}
\title{Build common Warcraft Logs filter expressions}
\usage{
wcl_filter_abilities(ids)

wcl_filter_types(types)

wcl_filter_and(...)
}
\arguments{
\item{ids}{One or more numeric ability IDs.}

\item{types}{One or more event-type strings such as \code{"cast"}.}

\item{...}{Filter-expression fragments.}
}
\value{
Each helper returns a scalar character string suitable for
\code{filter_expression}.
}
\description{
Small helpers for building common Warcraft Logs filter expressions without
handwriting the full string each time.
}
