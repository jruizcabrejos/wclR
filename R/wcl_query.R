#' Build a raw GraphQL query string
#'
#' `wcl_query()` is kept for compatibility with older scripts in
#' `code_examples`. New code should prefer the typed helpers or
#' [wcl_query_custom()].
#'
#' Supported shortcuts are:
#' - `"actors"`
#' - `"encounters"`
#' - `"dmgtaken"`
#' - `"logs"`
#' - `"custom"`
#'
#' @param query Query shortcut name.
#' @param log Report code for report-based shortcuts, zone ID for `"logs"`, or
#'   the raw GraphQL template for `"custom"`.
#' @param ... Additional values interpolated into the query template.
#'
#' @return A GraphQL query string.
#' @export
wcl_query <- function(query, log, ...) {
  query_key <- stringr::str_to_lower(query)
  args <- list(...)

  if (query_key %in% c("actor", "actors")) {
    return(.wcl_query_actors(report_code = wcl_report_code(log)))
  }

  if (query_key %in% c("encounter", "encounters")) {
    return(.wcl_query_fights(report_code = wcl_report_code(log)))
  }

  if (query_key %in% "dmgtaken") {
    if (length(args) < 2L) {
      stop("`dmgtaken` queries require `fight_id` and `filter_expression`.", call. = FALSE)
    }

    return(.wcl_query_events(
      report_code = wcl_report_code(log),
      fight_id = as.integer(args[[1L]]),
      data_type = "DamageTaken",
      kill_type = "All",
      hostility_type = "Friendlies",
      filter_expression = as.character(args[[2L]])
    ))
  }

  if (query_key %in% "logs") {
    page <- if (length(args) >= 1L) as.integer(args[[1L]]) else 1L
    return(.wcl_query_reports_page(zone_id = as.integer(log), page = page))
  }

  if (query_key %in% "custom") {
    return(wcl_query_custom(log, ...))
  }

  stop("Unsupported query shortcut: ", query, call. = FALSE)
}

#' Build a custom GraphQL query
#'
#' @param query GraphQL template string.
#' @param ... Optional values interpolated with [sprintf()].
#'
#' @return A GraphQL query string.
#' @export
wcl_query_custom <- function(query, ...) {
  if (!is.character(query) || length(query) != 1L || !nzchar(query)) {
    stop("`query` must be a single GraphQL template string.", call. = FALSE)
  }

  if (!length(list(...))) {
    return(query)
  }

  sprintf(query, ...)
}
