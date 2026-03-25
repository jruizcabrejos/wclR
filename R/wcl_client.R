#' Create a Warcraft Logs API client
#'
#' The client caches public API bearer tokens and refreshes them when they are
#' close to expiring.
#'
#' @param client_id Warcraft Logs client ID. Defaults to
#'   `Sys.getenv("WCLR_CLIENT_ID")`.
#' @param client_secret Warcraft Logs client secret. Defaults to
#'   `Sys.getenv("WCLR_CLIENT_SECRET")`.
#' @param host Warcraft Logs host. Defaults to `"classic"`.
#'
#' @return A `wcl_client` object.
#' @export
wcl_client <- function(
    client_id = Sys.getenv("WCLR_CLIENT_ID", unset = ""),
    client_secret = Sys.getenv("WCLR_CLIENT_SECRET", unset = ""),
    host = "classic") {
  if (!nzchar(client_id) || !nzchar(client_secret)) {
    stop(
      "client_id and client_secret are required. Supply them directly or set ",
      "WCLR_CLIENT_ID and WCLR_CLIENT_SECRET.",
      call. = FALSE
    )
  }

  config <- .wcl_host_config(host)
  client <- new.env(parent = emptyenv())

  class(client) <- "wcl_client"
  client$client_id <- client_id
  client$client_secret <- client_secret
  client$host <- config$key
  client$api_url <- config$api_url
  client$oauth_url <- config$oauth_url
  client$base_url <- config$base_url
  client$token <- NULL
  client$expires_at <- .wcl_empty_time()

  cache_key <- .wcl_token_cache_key(client$host, client_id, client_secret)
  if (exists(cache_key, envir = .wcl_token_cache, inherits = FALSE)) {
    cached <- get(cache_key, envir = .wcl_token_cache, inherits = FALSE)
    if (!is.null(cached$token) && isTRUE(cached$expires_at > Sys.time())) {
      client$token <- cached$token
      client$expires_at <- cached$expires_at
    }
  }

  client
}

#' @export
print.wcl_client <- function(x, ...) {
  token_cached <- !is.null(x$token) && nzchar(x$token) && isTRUE(x$expires_at > Sys.time())

  cat("<wcl_client>\n", sep = "")
  cat("  host: ", x$host, "\n", sep = "")
  cat("  api_url: ", x$api_url, "\n", sep = "")
  cat("  token_cached: ", if (token_cached) "TRUE" else "FALSE", "\n", sep = "")

  invisible(x)
}
