\name{wcl_request}
\alias{wcl_request}
\alias{wcl_query}
\alias{wcl_query_custom}
\title{Low-level GraphQL request helpers}
\usage{
wcl_request(
  client = NULL,
  query = NULL,
  token = NULL,
  request = NULL,
  endpoint = "client",
  host = "classic",
  max_retries = 2L
)

wcl_query(query, log, ...)

wcl_query_custom(query, ...)
}
\arguments{
\item{client}{A \code{wcl_client} object, or a bearer token string for
compatibility.}

\item{query}{A GraphQL query string or shortcut name.}

\item{token}{Optional bearer token used instead of a client.}

\item{request}{Compatibility alias for \code{query}.}

\item{endpoint}{API endpoint.}

\item{host}{Warcraft Logs host.}

\item{max_retries}{Number of retries after the initial request.}

\item{log}{Report code, zone ID, or raw template depending on the shortcut.}

\item{...}{Additional values interpolated into the query template.}
}
\value{
\code{wcl_request()} returns a parsed nested list.
\code{wcl_query()} and \code{wcl_query_custom()} return GraphQL query strings.
}
\description{
Low-level helpers for sending raw GraphQL requests and for building query
strings compatible with older scripts.
}
