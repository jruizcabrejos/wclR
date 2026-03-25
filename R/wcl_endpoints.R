#' Normalize a Warcraft Logs report code
#'
#' Accepts either a bare report code or a full Warcraft Logs report URL.
#'
#' @param x Report code or full report URL.
#'
#' @return A character vector of report codes.
#' @export
wcl_report_code <- function(x) {
  x <- trimws(as.character(x))

  report_match <- stringr::str_match(x, "/reports/([A-Za-z0-9]+)")
  is_url <- grepl("^https?://", x)
  bad_url <- is_url & is.na(report_match[, 2])
  if (any(bad_url)) {
    stop("Could not extract a report code from the supplied URL.", call. = FALSE)
  }

  codes <- ifelse(!is.na(report_match[, 2]), report_match[, 2], x)
  codes <- stringr::str_extract(codes, "[A-Za-z0-9]+")

  if (any(is.na(codes) | !nzchar(codes))) {
    stop("Could not extract a valid report code from `x`.", call. = FALSE)
  }

  codes
}

#' Retrieve public report listings for a zone
#'
#' @param zone_id Warcraft Logs zone ID.
#' @param pages Optional page number or vector of page numbers. If `NULL`, pages
#'   are fetched until `has_more_pages` is `FALSE`.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble with one row per report.
#' @export
wcl_reports <- function(zone_id, pages = NULL, client = NULL) {
  client <- .wcl_resolve_client(client)
  zone_id <- as.integer(zone_id)

  page_sequence <- if (is.null(pages)) 1L else unique(as.integer(pages))
  out <- list()
  page_index <- 1L

  repeat {
    current_page <- page_sequence[[page_index]]
    payload <- wcl_request(
      client = client,
      query = .wcl_query_reports_page(zone_id = zone_id, page = current_page)
    )

    out[[length(out) + 1L]] <- .wcl_normalize_reports(
      payload = payload,
      zone_id = zone_id,
      page = current_page,
      host = client$host
    )

    if (!is.null(pages)) {
      page_index <- page_index + 1L
      if (page_index > length(page_sequence)) {
        break
      }
      next
    }

    node <- .wcl_path_get(payload, c("data", "reportData", "reports"), default = list())
    if (!isTRUE(.wcl_path_get(node, "has_more_pages", default = FALSE))) {
      break
    }

    page_sequence <- c(page_sequence, current_page + 1L)
    page_index <- page_index + 1L
  }

  dplyr::bind_rows(out)
}

#' Retrieve fights for a report
#'
#' @param report_code Report code or report URL.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble with one row per fight.
#' @export
wcl_fights <- function(report_code, client = NULL) {
  client <- .wcl_resolve_client(client)
  report_code <- wcl_report_code(report_code)

  payload <- wcl_request(
    client = client,
    query = .wcl_query_fights(report_code = report_code)
  )

  .wcl_normalize_fights(payload = payload, report_code = report_code, host = client$host)
}

#' Retrieve actors from a report
#'
#' @param report_code Report code or report URL.
#' @param type Optional actor type filter passed to `masterData(...actors(type=))`.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble with one row per actor.
#' @export
wcl_actors <- function(report_code, type = NULL, client = NULL) {
  client <- .wcl_resolve_client(client)
  report_code <- wcl_report_code(report_code)

  payload <- wcl_request(
    client = client,
    query = .wcl_query_actors(report_code = report_code, type = type)
  )

  .wcl_normalize_actors(payload = payload, report_code = report_code, host = client$host)
}

#' Retrieve player details for a specific fight
#'
#' @param report_code Report code or report URL.
#' @param fight_id Fight ID inside the report.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble with one row per player and a `role` column.
#' @export
wcl_player_details <- function(report_code, fight_id, client = NULL) {
  client <- .wcl_resolve_client(client)
  report_code <- wcl_report_code(report_code)
  fight_id <- as.integer(fight_id)

  payload <- wcl_request(
    client = client,
    query = .wcl_query_player_details(
      report_code = report_code,
      fight_id = fight_id
    )
  )

  .wcl_normalize_player_details(
    payload = payload,
    report_code = report_code,
    host = client$host
  )
}

#' Retrieve events for a specific fight
#'
#' @param report_code Report code or report URL.
#' @param fight_id Fight ID inside the report.
#' @param data_type Event data type such as `"DamageTaken"`, `"Buffs"`,
#'   `"Casts"`, or `"All"`.
#' @param client Optional [wcl_client()] object.
#' @param kill_type Kill type filter. Defaults to `"Encounters"`.
#' @param hostility_type Hostility filter. Defaults to `"All"`.
#' @param source_id Optional source actor ID.
#' @param target_id Optional target actor ID.
#' @param filter_expression Optional Warcraft Logs filter expression.
#' @param start_time Event query start time in report milliseconds.
#' @param end_time Event query end time in report milliseconds.
#' @param paginate Whether to keep requesting pages until
#'   `nextPageTimestamp` is exhausted.
#'
#' @return A tibble with one row per event.
#' @export
wcl_events <- function(
    report_code,
    fight_id,
    data_type,
    client = NULL,
    kill_type = "Encounters",
    hostility_type = "All",
    source_id = NULL,
    target_id = NULL,
    filter_expression = NULL,
    start_time = 0,
    end_time = 999999999999,
    paginate = TRUE) {
  client <- .wcl_resolve_client(client)
  report_code <- wcl_report_code(report_code)
  fight_id <- as.integer(fight_id)

  rows <- list()
  next_start <- as.numeric(start_time)

  repeat {
    payload <- wcl_request(
      client = client,
      query = .wcl_query_events(
        report_code = report_code,
        fight_id = fight_id,
        data_type = data_type,
        kill_type = kill_type,
        hostility_type = hostility_type,
        source_id = source_id,
        target_id = target_id,
        filter_expression = filter_expression,
        start_time = next_start,
        end_time = end_time
      )
    )

    rows[[length(rows) + 1L]] <- .wcl_normalize_events(
      payload = payload,
      report_code = report_code,
      host = client$host
    )

    if (!isTRUE(paginate)) {
      break
    }

    event_node <- .wcl_path_get(payload, c("data", "reportData", "report", "events"), default = list())
    page_timestamp <- .wcl_path_get(event_node, "nextPageTimestamp", default = NULL)

    if (is.null(page_timestamp) || identical(as.numeric(page_timestamp), next_start)) {
      break
    }

    next_start <- as.numeric(page_timestamp)
  }

  dplyr::bind_rows(rows)
}

#' Retrieve encounter rankings for a zone
#'
#' @param zone_id Warcraft Logs zone ID.
#' @param metric Ranking metric. Examples include `"speed"`, `"progress"`,
#'   `"execution"`, or `"dps"`.
#' @param page Ranking page number or vector of pages.
#' @param encounter_id Optional encounter ID filter.
#' @param class_name Optional class name. If provided, character rankings are
#'   requested instead of fight rankings.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble with one row per ranking entry.
#' @export
wcl_rankings <- function(
    zone_id,
    metric,
    page,
    encounter_id = NULL,
    class_name = NULL,
    client = NULL) {
  client <- .wcl_resolve_client(client)
  zone_id <- as.integer(zone_id)
  pages <- unique(as.integer(page))

  out <- purrr::map(
    pages,
    function(current_page) {
      payload <- wcl_request(
        client = client,
        query = .wcl_query_rankings(
          zone_id = zone_id,
          metric = metric,
          page = current_page,
          class_name = class_name
        )
      )

      .wcl_normalize_rankings(
        payload = payload,
        zone_id = zone_id,
        metric = metric,
        page = current_page,
        encounter_id = encounter_id,
        class_name = class_name
      )
    }
  )

  dplyr::bind_rows(out)
}

#' Remove duplicate fights using analysis-oriented defaults
#'
#' The default key matches the duplicate filtering already used in the existing
#' analysis notebooks: encounter, difficulty, size, kill state, boss
#' percentage, duration, and average item level.
#'
#' @param fights A tibble produced by [wcl_fights()] or a compatible data frame.
#' @param by Columns used to define duplicates.
#' @param keep_all Whether to keep all columns from the first occurrence of each
#'   duplicate key. Defaults to `TRUE`.
#'
#' @return A tibble with duplicate fights removed.
#' @export
wcl_distinct_fights <- function(
    fights,
    by = c(
      "encounterID",
      "difficulty",
      "size",
      "kill",
      "bossPercentage",
      "duration_s",
      "averageItemLevel"
    ),
    keep_all = TRUE) {
  fights <- tibble::as_tibble(fights)
  by <- intersect(by, names(fights))

  if (!length(by)) {
    return(fights)
  }

  duplicate_index <- !duplicated(fights[by])
  out <- fights[duplicate_index, , drop = FALSE]

  if (!isTRUE(keep_all)) {
    out <- out[by]
  }

  tibble::as_tibble(out)
}
