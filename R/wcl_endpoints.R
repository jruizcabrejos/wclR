# Endpoint argument validation --------------------------------------------

.wcl_prepare_report_pages <- function(pages) {
  if (is.null(pages)) {
    return(list(pages = 1L, automatic = TRUE))
  }

  if (!is.numeric(pages) || !length(pages) ||
      any(!is.finite(pages)) || any(pages <= 0) ||
      any(pages != floor(pages))) {
    stop("`pages` must be NULL or a non-empty vector of positive whole numbers.", call. = FALSE)
  }

  if (any(pages > 25)) {
    warning(
      "Warcraft Logs report discovery is limited to page 25; requested pages above 25 were capped.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  list(
    pages = unique(pmin(as.integer(pages), 25L)),
    automatic = FALSE
  )
}

.wcl_confirm_empty_report_page <- function(page) {
  if (!interactive()) {
    warning(
      paste0(
        "`on_empty_page = \"ask\"` cannot prompt in a non-interactive session; ",
        "report retrieval stopped at page ", page, "."
      ),
      call. = FALSE,
      immediate. = TRUE
    )
    return(FALSE)
  }

  choice <- utils::menu(
    choices = c("Continue", "Stop"),
    title = paste0(
      "Report page ", page,
      " returned no reports. Continue with the remaining pages?"
    )
  )

  identical(choice, 1L)
}

.wcl_report_page_is_terminal <- function(
    node,
    report_count,
    cumulative_from_first_page = FALSE) {
  current_page <- suppressWarnings(as.integer(
    .wcl_path_get(node, "current_page", default = NA_integer_)
  ))
  last_page <- suppressWarnings(as.integer(
    .wcl_path_get(node, "last_page", default = NA_integer_)
  ))
  has_more_pages <- .wcl_path_get(node, "has_more_pages", default = NULL)
  total <- suppressWarnings(as.numeric(
    .wcl_path_get(node, "total", default = NA_real_)
  ))

  terminal_has_more <- identical(has_more_pages, FALSE)

  # Report pagination uses negative values when a total or last page is unknown.
  terminal_last_page <- length(current_page) == 1L &&
    !is.na(current_page) &&
    current_page >= 1L &&
    length(last_page) == 1L &&
    !is.na(last_page) &&
    last_page >= 1L &&
    current_page >= last_page

  terminal_total <- isTRUE(cumulative_from_first_page) &&
    length(total) == 1L &&
    is.finite(total) &&
    total >= 0 &&
    report_count >= total

  terminal_has_more || terminal_last_page || terminal_total
}

.wcl_validate_event_hostility <- function(hostility_type) {
  choices <- c("Enemies", "Friendlies")
  if (!is.character(hostility_type) || length(hostility_type) != 1L ||
      is.na(hostility_type) || !hostility_type %in% choices) {
    stop(
      "`hostility_type` must be exactly one of \"Enemies\" or \"Friendlies\".",
      call. = FALSE
    )
  }

  hostility_type
}

.wcl_validate_event_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be a single non-missing logical value.", call. = FALSE)
  }

  x
}

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
#' @param pages Page number or vector of exact page numbers. Defaults to pages
#'   1 through 3. Values above 25 are capped with a warning. If `NULL`, pages
#'   are discovered automatically until `has_more_pages` is `FALSE` or page 25
#'   is reached.
#' @param client Optional [wcl_client()] object.
#' @param on_empty_page Action when a requested page contains no reports:
#'   `"stop"` (the default) returns the reports collected so far, `"ask"`
#'   prompts before continuing in interactive sessions and otherwise stops
#'   with a warning, and `"continue"` retrieves the remaining requested pages.
#'
#' @return A tibble with one row per report.
#' @export
wcl_reports <- function(
    zone_id,
    pages = 1:3,
    client = NULL,
    on_empty_page = c("stop", "ask", "continue")) {
  page_spec <- .wcl_prepare_report_pages(pages)
  page_sequence <- page_spec$pages
  automatic <- page_spec$automatic
  on_empty_page <- match.arg(on_empty_page)
  contiguous_ascending <- !automatic &&
    length(page_sequence) > 1L &&
    all(diff(page_sequence) == 1L)
  cumulative_from_first_page <- contiguous_ascending &&
    identical(page_sequence[[1L]], 1L)

  client <- .wcl_resolve_client(client)
  zone_id <- as.integer(zone_id)

  out <- list()
  page_index <- 1L
  report_count <- 0L

  repeat {
    current_page <- page_sequence[[page_index]]
    payload <- wcl_request(
      client = client,
      query = .wcl_query_reports_page(zone_id = zone_id, page = current_page)
    )

    page_rows <- .wcl_normalize_reports(
      payload = payload,
      zone_id = zone_id,
      page = current_page,
      host = client$host
    )
    out[[length(out) + 1L]] <- page_rows
    report_count <- report_count + nrow(page_rows)
    message(
      "Report page ", current_page, ": ", nrow(page_rows),
      " reports (", report_count, " total)."
    )

    node <- .wcl_path_get(payload, c("data", "reportData", "reports"), default = list())

    if (!nrow(page_rows) && !identical(on_empty_page, "continue")) {
      if (identical(on_empty_page, "stop")) {
        warning(
          paste0(
            "Report page ", current_page,
            " returned no reports; retrieval stopped and ", report_count,
            " report", if (report_count == 1L) " was" else "s were",
            " returned."
          ),
          call. = FALSE,
          immediate. = TRUE
        )
        break
      }

      if (!.wcl_confirm_empty_report_page(current_page)) {
        if (interactive()) {
          message(
            "Report retrieval stopped at page ", current_page,
            " because it returned no reports."
          )
        }
        break
      }
    }

    if (!automatic) {
      has_remaining_pages <- page_index < length(page_sequence)
      if (has_remaining_pages &&
          contiguous_ascending &&
          !identical(on_empty_page, "continue") &&
          .wcl_report_page_is_terminal(
            node = node,
            report_count = report_count,
            cumulative_from_first_page = cumulative_from_first_page
          )) {
        message(
          "Report retrieval stopped at page ", current_page,
          " because Warcraft Logs pagination metadata indicates that no more ",
          "reports are available."
        )
        break
      }

      page_index <- page_index + 1L
      if (page_index > length(page_sequence)) {
        break
      }
      next
    }

    if (!isTRUE(.wcl_path_get(node, "has_more_pages", default = FALSE))) {
      break
    }

    if (current_page >= 25L) {
      warning(
        "Warcraft Logs report discovery stopped at page 25 while more pages were available.",
        call. = FALSE,
        immediate. = TRUE
      )
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
#'   `"Casts"`, or `"All"`. See the Warcraft Logs
#'   [EventDataType documentation](https://www.warcraftlogs.com/v2-api-docs/warcraft/eventdatatype.doc.html)
#'   for all supported values.
#' @param client Optional [wcl_client()] object.
#' @param kill_type Kill type filter. Defaults to `"Encounters"`.
#' @param hostility_type Hostility filter. Must be exactly `"Enemies"` or
#'   `"Friendlies"`; defaults to `"Friendlies"`. See the Warcraft Logs
#'   [HostilityType documentation](https://www.warcraftlogs.com/v2-api-docs/warcraft/hostilitytype.doc.html).
#' @param source_id Optional source actor ID.
#' @param target_id Optional target actor ID.
#' @param filter_expression Optional scalar expression in the Warcraft Logs
#'   site query language, applied server-side to select matching events. It can
#'   be written directly or constructed with [wcl_filter_abilities()],
#'   [wcl_filter_types()], and [wcl_filter_and()]. Raw expression `type` values
#'   such as `"damage"` are distinct from GraphQL `data_type` values such as
#'   `"DamageTaken"`. See the Warcraft Logs
#'   [expression-language and Pins guide](https://www.warcraftlogs.com/help/pins).
#' @param start_time Event query start time in report milliseconds.
#' @param end_time Event query end time in report milliseconds.
#' @param paginate Whether to keep requesting pages until
#'   `nextPageTimestamp` is exhausted.
#' @param include_resources Whether to include detailed unit resources such as
#'   `classResources`. Defaults to `TRUE` for backward compatibility.
#' @param use_ability_ids Whether to include detailed ability information from
#'   report master data. Setting this to `FALSE` reduces payload size and can
#'   change which ability detail or ID columns are present. Defaults to `TRUE`
#'   for backward compatibility.
#' @param use_actor_ids Whether to include detailed actor information from
#'   report master data. Setting this to `FALSE` reduces payload size and can
#'   change which actor detail or ID columns are present. Defaults to `TRUE`
#'   for backward compatibility.
#'
#' @details
#' [`ReportEventPaginator.data`](https://www.warcraftlogs.com/v2-api-docs/warcraft/reporteventpaginator.doc.html)
#' is an opaque [JSON scalar](https://www.warcraftlogs.com/v2-api-docs/warcraft/json.doc.html),
#' so arbitrary event columns cannot be excluded by GraphQL. A
#' `filter_expression` reduces which events match; it does not select fields or
#' remove columns. The three logical controls above are the API-supported
#' bandwidth controls. See the Warcraft Logs
#' [Report events documentation](https://www.warcraftlogs.com/v2-api-docs/warcraft/report.doc.html)
#' for the complete set of request arguments.
#'
#' @return A tibble with one row per event.
#' @export
wcl_events <- function(
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
    use_actor_ids = TRUE) {
  hostility_type <- .wcl_validate_event_hostility(hostility_type)
  include_resources <- .wcl_validate_event_flag(include_resources, "include_resources")
  use_ability_ids <- .wcl_validate_event_flag(use_ability_ids, "use_ability_ids")
  use_actor_ids <- .wcl_validate_event_flag(use_actor_ids, "use_actor_ids")

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
        end_time = end_time,
        include_resources = include_resources,
        use_ability_ids = use_ability_ids,
        use_actor_ids = use_actor_ids
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
#' @param metric Ranking metric. Without `class_name`, use a
#'   [fight-ranking metric](https://www.warcraftlogs.com/v2-api-docs/warcraft/fightrankingmetrictype.doc.html)
#'   such as `"speed"`, `"progress"`, or `"execution"`. With `class_name`, use
#'   a [character-ranking metric](https://www.warcraftlogs.com/v2-api-docs/warcraft/characterrankingmetrictype.doc.html)
#'   such as `"dps"` or `"hps"`.
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
