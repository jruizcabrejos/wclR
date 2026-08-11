# Internal package state ---------------------------------------------------

.wcl_token_cache <- new.env(parent = emptyenv())


# Generic helpers ----------------------------------------------------------

.wcl_empty_time <- function() {
  as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC")
}

.wcl_null_time <- function() {
  as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
}

.wcl_na_time <- function(n = 1L) {
  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) {
    y
  } else {
    x
  }
}

.wcl_host_config <- function(host) {
  host <- stringr::str_to_lower(host %||% "classic")

  configs <- list(
    classic = list(
      key = "classic",
      base_url = "https://classic.warcraftlogs.com",
      api_url = "https://classic.warcraftlogs.com/api/v2/client",
      oauth_url = "https://www.warcraftlogs.com/oauth/token"
    ),
    retail = list(
      key = "retail",
      base_url = "https://www.warcraftlogs.com",
      api_url = "https://www.warcraftlogs.com/api/v2/client",
      oauth_url = "https://www.warcraftlogs.com/oauth/token"
    ),
    sod = list(
      key = "sod",
      base_url = "https://www.sod.warcraftlogs.com",
      api_url = "https://www.sod.warcraftlogs.com/api/v2/client",
      oauth_url = "https://www.warcraftlogs.com/oauth/token"
    ),
    fresh = list(
      key = "fresh",
      base_url = "https://www.fresh.warcraftlogs.com",
      api_url = "https://www.fresh.warcraftlogs.com/api/v2/client",
      oauth_url = "https://www.warcraftlogs.com/oauth/token"
    ),
    vanilla = list(
      key = "vanilla",
      base_url = "https://www.vanilla.warcraftlogs.com",
      api_url = "https://www.vanilla.warcraftlogs.com/api/v2/client",
      oauth_url = "https://www.warcraftlogs.com/oauth/token"
    )
  )

  config <- configs[[host]]
  if (is.null(config)) {
    stop("Unsupported `host`: ", host, call. = FALSE)
  }

  config
}

.wcl_resolve_client <- function(client = NULL, host = "classic") {
  if (inherits(client, "wcl_client")) {
    return(client)
  }

  if (!is.null(client)) {
    stop("`client` must be a `wcl_client` object, a bearer token, or NULL.", call. = FALSE)
  }

  wcl_client(host = host)
}

.wcl_token_cache_key <- function(host, client_id, client_secret) {
  paste(host, client_id, client_secret, sep = "::")
}

.wcl_parse_unix_ms <- function(x) {
  if (length(x) == 0L) {
    return(.wcl_empty_time())
  }

  if (all(is.na(x))) {
    return(as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC"))
  }

  as.POSIXct(as.numeric(x) / 1000, origin = "1970-01-01", tz = "UTC")
}

.wcl_add_ms_to_time <- function(time, offset_ms) {
  if (length(offset_ms) == 0L) {
    return(.wcl_empty_time())
  }

  if (length(time) == 0L || is.na(time)) {
    return(as.POSIXct(rep(NA_real_, length(offset_ms)), origin = "1970-01-01", tz = "UTC"))
  }

  time + as.numeric(offset_ms) / 1000
}

.wcl_path_get <- function(x, path, default = NULL) {
  if (!length(path)) {
    return(x)
  }

  value <- x
  for (element in path) {
    if (is.null(value)) {
      return(default)
    }

    if (is.character(element)) {
      if (!is.list(value) || is.null(value[[element]])) {
        return(default)
      }
      value <- value[[element]]
    } else {
      if (length(value) < element) {
        return(default)
      }
      value <- value[[element]]
    }
  }

  value %||% default
}

.wcl_is_record <- function(x) {
  is.list(x) &&
    length(x) > 0L &&
    !is.null(names(x)) &&
    all(names(x) != "") &&
    !all(vapply(x, function(item) is.list(item) && !inherits(item, "data.frame"), logical(1)))
}

.wcl_records <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(list())
  }

  if (inherits(x, "data.frame")) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }

  if (!is.list(x)) {
    return(list(x))
  }

  if (.wcl_is_record(x)) {
    return(list(x))
  }

  unlist(lapply(x, .wcl_records), recursive = FALSE, use.names = FALSE)
}

.wcl_flatten_record <- function(record, prefix = NULL, preserve = character()) {
  if (inherits(record, "data.frame") && nrow(record) == 1L) {
    record <- as.list(record[1, , drop = FALSE])
  }

  out <- list()
  keys <- names(record) %||% character(length(record))

  for (idx in seq_along(record)) {
    value <- record[[idx]]
    key <- keys[[idx]]
    key <- if (is.null(prefix) || !nzchar(prefix)) key else paste(prefix, key, sep = "_")

    if (key %in% preserve) {
      out[[key]] <- list(value)
      next
    }

    if (is.null(value)) {
      out[[key]] <- NA
      next
    }

    if (inherits(value, "data.frame")) {
      if (nrow(value) == 1L) {
        out <- c(out, .wcl_flatten_record(as.list(value[1, , drop = FALSE]), prefix = key, preserve = preserve))
      } else {
        out[[key]] <- list(value)
      }
      next
    }

    if (is.list(value)) {
      if (.wcl_is_record(value)) {
        out <- c(out, .wcl_flatten_record(value, prefix = key, preserve = preserve))
      } else {
        out[[key]] <- list(value)
      }
      next
    }

    if (length(value) <= 1L) {
      out[[key]] <- value
    } else {
      out[[key]] <- list(value)
    }
  }

  out
}

.wcl_bind_records <- function(records, preserve = character()) {
  if (!length(records)) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(lapply(records, .wcl_flatten_record, preserve = preserve))
}

.wcl_report_link <- function(base_url, logID, fightID = NULL) {
  if (!length(logID) || all(is.na(logID))) {
    return(character(length(logID)))
  }

  link <- paste0(base_url, "/reports/", logID)
  has_fight <- !is.null(fightID) && length(fightID) == length(logID)

  if (has_fight) {
    link <- ifelse(is.na(fightID), link, paste0(link, "#fight=", fightID))
  }

  link
}

.wcl_graphql_string <- function(x) {
  as.character(jsonlite::toJSON(as.character(x), auto_unbox = TRUE))
}

.wcl_graphql_int <- function(x) {
  format(as.integer(x), scientific = FALSE, trim = TRUE)
}

.wcl_graphql_num <- function(x) {
  format(as.numeric(x), scientific = FALSE, trim = TRUE)
}

.wcl_graphql_bool <- function(x) {
  if (isTRUE(x)) "true" else "false"
}

.wcl_graphql_enum <- function(x) {
  x <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", x)
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9]+", " ")
  x <- trimws(x)
  if (!nzchar(x)) {
    return("")
  }

  words <- strsplit(tolower(x), "\\s+")[[1L]]
  paste0(stringr::str_to_sentence(words), collapse = "")
}

.wcl_graphql_metric <- function(x) {
  stringr::str_to_lower(stringr::str_replace_all(x, "[^A-Za-z0-9]+", ""))
}

.wcl_graphql_int_array <- function(x) {
  paste0("[", paste(.wcl_graphql_int(x), collapse = ", "), "]")
}

.wcl_extract_report_meta <- function(report_node, host = "classic", report_code = NULL) {
  logID <- .wcl_path_get(report_node, "code", default = report_code)
  title <- .wcl_path_get(report_node, "title", default = NA_character_)
  startTime <- as.numeric(.wcl_path_get(report_node, "startTime", default = NA_real_))
  endTime <- as.numeric(.wcl_path_get(report_node, "endTime", default = NA_real_))
  config <- .wcl_host_config(host)

  list(
    logID = logID,
    report_title = title,
    startTime = startTime,
    endTime = endTime,
    report_start_at = .wcl_parse_unix_ms(startTime),
    report_end_at = .wcl_parse_unix_ms(endTime),
    report_link = .wcl_report_link(config$base_url, logID)
  )
}

.wcl_extract_fight_meta <- function(report_node, report_meta) {
  fights_tbl <- .wcl_bind_records(.wcl_records(.wcl_path_get(report_node, "fights", default = list())))
  if (!nrow(fights_tbl)) {
    return(NULL)
  }

  if ("id" %in% names(fights_tbl)) {
    fights_tbl <- dplyr::rename(fights_tbl, fightID = id)
  }

  if ("name" %in% names(fights_tbl)) {
    fights_tbl <- dplyr::rename(fights_tbl, encounterName = name)
  }

  fights_tbl <- dplyr::mutate(
    fights_tbl,
    duration = endTime - startTime,
    duration_s = duration / 1000,
    fight_start_at = .wcl_add_ms_to_time(report_meta$report_start_at, startTime),
    fight_end_at = .wcl_add_ms_to_time(report_meta$report_start_at, endTime)
  )

  fights_tbl[1, , drop = FALSE]
}

.wcl_empty_reports <- function() {
  tibble::tibble(
    logID = character(),
    title = character(),
    visibility = character(),
    region = character(),
    revision = integer(),
    segments = integer(),
    startTime = numeric(),
    endTime = numeric(),
    report_start_at = .wcl_empty_time(),
    report_end_at = .wcl_empty_time(),
    report_link = character(),
    zone_id = integer(),
    page = integer(),
    current_page = integer(),
    last_page = integer(),
    has_more_pages = logical(),
    per_page = integer(),
    total = integer()
  )
}

.wcl_empty_fights <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_empty_time(),
    report_end_at = .wcl_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    hardModeLevel = integer(),
    averageItemLevel = numeric(),
    size = integer(),
    kill = logical(),
    lastPhase = integer(),
    startTime = numeric(),
    endTime = numeric(),
    fight_start_at = .wcl_empty_time(),
    fight_end_at = .wcl_empty_time(),
    duration = numeric(),
    duration_s = numeric(),
    fightPercentage = numeric(),
    bossPercentage = numeric(),
    completeRaid = logical(),
    inProgress = logical()
  )
}

.wcl_empty_actors <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_empty_time(),
    report_end_at = .wcl_empty_time(),
    actorID = integer(),
    gameID = integer(),
    name = character(),
    server = character(),
    subType = character(),
    type = character(),
    icon = character(),
    petOwner = integer()
  )
}

.wcl_empty_player_details <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_empty_time(),
    report_end_at = .wcl_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    duration_s = numeric(),
    fight_start_at = .wcl_empty_time(),
    fight_end_at = .wcl_empty_time(),
    role = character(),
    actorID = integer(),
    name = character(),
    type = character(),
    subType = character(),
    server = character(),
    icon = character(),
    specs = list(),
    combatantInfo = list()
  )
}

.wcl_empty_events <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_empty_time(),
    report_end_at = .wcl_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    duration_s = numeric(),
    fight_start_at = .wcl_empty_time(),
    fight_end_at = .wcl_empty_time(),
    timestamp = numeric(),
    event_at = .wcl_empty_time(),
    type = character(),
    sourceID = integer(),
    targetID = integer(),
    abilityGameID = integer()
  )
}

.wcl_empty_rankings <- function() {
  tibble::tibble(
    zone_id = integer(),
    encounterID = integer(),
    encounterName = character(),
    metric = character(),
    ranking_type = character(),
    page = integer(),
    rank = integer(),
    report_code = character(),
    amount = numeric(),
    name = character()
  )
}

.wcl_empty_zone_fights <- function() {
  fights <- .wcl_empty_fights()

  tibble::add_column(
    fights,
    zone_id = integer(),
    report_page = integer(),
    report_current_page = integer(),
    report_last_page = integer(),
    report_has_more_pages = logical(),
    report_per_page = integer(),
    report_total = integer(),
    report_visibility = character(),
    report_region = character(),
    report_revision = integer(),
    report_segments = integer(),
    report_start_ms = numeric(),
    report_end_ms = numeric(),
    .after = "report_end_at"
  )
}
