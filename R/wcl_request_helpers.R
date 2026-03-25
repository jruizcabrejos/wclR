.wcl_http_post <- function(url, headers, body_json) {
  httr::POST(
    url = url,
    headers,
    body = body_json,
    httr::content_type_json(),
    encode = "raw"
  )
}

.wcl_http_post_token <- function(url, client_id, client_secret) {
  httr::POST(
    url = url,
    httr::authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
}

.wcl_response_status <- function(response) {
  response$status_code %||% httr::status_code(response)
}

.wcl_response_text <- function(response) {
  if (is.list(response) && !is.null(response$text)) {
    return(response$text)
  }

  httr::content(response, as = "text", encoding = "UTF-8")
}

.wcl_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

.wcl_perform_graphql_request <- function(url, headers, body_json, max_retries = 2L) {
  attempts <- max(0L, as.integer(max_retries)) + 1L
  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    response <- tryCatch(
      .wcl_http_post(url, headers, body_json),
      error = function(err) err
    )

    if (!inherits(response, "error")) {
      status <- .wcl_response_status(response)
      if (status < 500L) {
        return(response)
      }

      last_error <- simpleError(
        paste0("Warcraft Logs API returned HTTP ", status, ".")
      )
    } else {
      last_error <- response
    }

    if (attempt < attempts) {
      .wcl_sleep(min(2^(attempt - 1L), 8))
    }
  }

  stop(last_error$message, call. = FALSE)
}

.wcl_parse_json_response <- function(response) {
  text <- .wcl_response_text(response)
  if (!nzchar(text)) {
    return(list())
  }

  payload <- jsonlite::fromJSON(
    text,
    simplifyVector = FALSE,
    bigint_as_char = TRUE
  )

  status <- .wcl_response_status(response)
  if (status >= 400L) {
    message <- .wcl_path_get(payload, c("error", "message"), default = NULL) %||%
      .wcl_path_get(payload, "message", default = NULL) %||%
      paste0("Warcraft Logs API returned HTTP ", status, ".")

    stop(message, call. = FALSE)
  }

  payload
}

.wcl_abort_on_graphql_errors <- function(payload) {
  errors <- .wcl_path_get(payload, "errors", default = list())
  if (!length(errors)) {
    return(invisible(payload))
  }

  messages <- vapply(
    errors,
    function(err) .wcl_path_get(err, "message", default = "Unknown GraphQL error."),
    character(1)
  )

  stop(
    paste0("Warcraft Logs GraphQL error: ", paste(messages, collapse = " | ")),
    call. = FALSE
  )
}

.wcl_get_access_token <- function(client, force = FALSE) {
  if (!inherits(client, "wcl_client")) {
    stop("`client` must be a `wcl_client` object.", call. = FALSE)
  }

  token_valid <- !force &&
    !is.null(client$token) &&
    nzchar(client$token) &&
    isTRUE(client$expires_at > Sys.time())

  if (token_valid) {
    return(client$token)
  }

  response <- .wcl_http_post_token(
    url = client$oauth_url,
    client_id = client$client_id,
    client_secret = client$client_secret
  )

  payload <- .wcl_parse_json_response(response)

  token <- .wcl_path_get(payload, "access_token", default = NULL)
  expires_in <- as.numeric(.wcl_path_get(payload, "expires_in", default = 3600))

  if (is.null(token) || !nzchar(token)) {
    stop("Token response did not contain an `access_token`.", call. = FALSE)
  }

  expires_at <- Sys.time() + max(expires_in - 60, 0)

  client$token <- token
  client$expires_at <- expires_at

  assign(
    .wcl_token_cache_key(client$host, client$client_id, client$client_secret),
    list(token = token, expires_at = expires_at),
    envir = .wcl_token_cache
  )

  token
}
