#' Send a raw GraphQL request to Warcraft Logs
#'
#' This is the low-level escape hatch used by the higher-level helpers in the
#' package. In new code, prefer [wcl_reports()], [wcl_fights()], [wcl_events()],
#' and the other typed helpers. `wcl_request()` is still useful for queries that
#' are not yet covered by the package.
#'
#' For backward compatibility, the first argument can also be a bearer token
#' string, in which case `query` is treated as the GraphQL request body.
#'
#' @param client A [wcl_client()] object, or a bearer token string for
#'   compatibility.
#' @param query A GraphQL query string.
#' @param token Optional bearer token used instead of a client.
#' @param endpoint API endpoint. Defaults to `"client"`.
#' @param host Warcraft Logs host. Defaults to `"classic"`.
#' @param max_retries Number of retries after the initial request for transient
#'   failures.
#'
#' @return A parsed JSON response as a nested list.
#' @export
wcl_request <- function(
    client = NULL,
    query = NULL,
    token = NULL,
    request = NULL,
    endpoint = "client",
    host = "classic",
    max_retries = 2L) {
  if (is.null(query) && !is.null(request)) {
    query <- request
  }

  if (is.character(client) && length(client) == 1L && is.character(query)) {
    token <- client
    client <- NULL
  }

  if (is.null(query) || !is.character(query) || length(query) != 1L || !nzchar(query)) {
    stop("`query` must be a single GraphQL query string.", call. = FALSE)
  }

  endpoint <- match.arg(endpoint, c("client", "user"))

  if (is.null(token)) {
    client <- .wcl_resolve_client(client, host = host)
    token <- .wcl_get_access_token(client)
    api_url <- sub("/client$", paste0("/", endpoint), client$api_url)
  } else {
    api_url <- sub("/client$", paste0("/", endpoint), .wcl_host_config(host)$api_url)
  }

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", token),
    "Content-Type" = "application/json"
  )

  body_json <- jsonlite::toJSON(
    list(query = query),
    auto_unbox = TRUE,
    null = "null"
  )

  response <- .wcl_perform_graphql_request(
    url = api_url,
    headers = headers,
    body_json = body_json,
    max_retries = as.integer(max_retries)
  )

  payload <- .wcl_parse_json_response(response)
  .wcl_abort_on_graphql_errors(payload)
  payload
}
