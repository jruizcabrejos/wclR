\name{wcl_client}
\alias{wcl_client}
\alias{wcl_token}
\alias{print.wcl_client}
\title{Create and inspect a Warcraft Logs API client}
\usage{
wcl_client(
  client_id = Sys.getenv("WCLR_CLIENT_ID", unset = ""),
  client_secret = Sys.getenv("WCLR_CLIENT_SECRET", unset = ""),
  host = "classic"
)

wcl_token(
  clientid = Sys.getenv("WCLR_CLIENT_ID", unset = ""),
  clientsecret = Sys.getenv("WCLR_CLIENT_SECRET", unset = ""),
  host = "classic"
)

\method{print}{wcl_client}(x, ...)
}
\arguments{
\item{client_id}{Warcraft Logs client ID.}

\item{client_secret}{Warcraft Logs client secret.}

\item{host}{Warcraft Logs host.}

\item{clientid}{Compatibility alias for \code{client_id}.}

\item{clientsecret}{Compatibility alias for \code{client_secret}.}

\item{x}{A \code{wcl_client} object.}

\item{...}{Unused arguments passed to the print method.}
}
\value{
\code{wcl_client()} returns a \code{wcl_client} object.
\code{wcl_token()} returns a bearer token string.
}
\description{
Helpers for authenticating against the Warcraft Logs public API. The client
object caches bearer tokens and refreshes them automatically when needed.
}
