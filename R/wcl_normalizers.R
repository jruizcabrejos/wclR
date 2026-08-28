.wcl_normalize_reports <- function(payload, zone_id, page, host = "classic") {
  node <- .wcl_path_get(payload, c("data", "reportData", "reports"), default = list())
  records <- .wcl_records(.wcl_path_get(node, "data", default = list()))

  records <- lapply(records, function(record) {
    region <- .wcl_path_get(record, c("region", "name"), default = NA_character_)
    if (length(region) != 1L || is.list(region) || is.na(region)) {
      region <- NA_character_
    }

    record[["region"]] <- as.character(region)
    record
  })

  tbl <- .wcl_bind_records(records)

  if (!nrow(tbl)) {
    return(.wcl_empty_reports())
  }

  if ("code" %in% names(tbl)) {
    tbl <- dplyr::rename(tbl, logID = code)
  }

  config <- .wcl_host_config(host)

  dplyr::mutate(
    tbl,
    report_start_at = .wcl_parse_unix_ms(startTime),
    report_end_at = .wcl_parse_unix_ms(endTime),
    report_link = .wcl_report_link(config$base_url, logID),
    zone_id = as.integer(zone_id),
    page = as.integer(page),
    current_page = as.integer(.wcl_path_get(node, "current_page", default = NA_integer_)),
    last_page = as.integer(.wcl_path_get(node, "last_page", default = NA_integer_)),
    has_more_pages = isTRUE(.wcl_path_get(node, "has_more_pages", default = FALSE)),
    per_page = as.integer(.wcl_path_get(node, "per_page", default = NA_integer_)),
    total = as.integer(.wcl_path_get(node, "total", default = NA_integer_))
  )
}

.wcl_normalize_fights <- function(payload, report_code = NULL, host = "classic") {
  report_node <- .wcl_path_get(payload, c("data", "reportData", "report"), default = list())
  report_meta <- .wcl_extract_report_meta(report_node, host = host, report_code = report_code)
  fights_tbl <- .wcl_bind_records(.wcl_records(.wcl_path_get(report_node, "fights", default = list())))

  if (!nrow(fights_tbl)) {
    return(.wcl_empty_fights())
  }

  if ("id" %in% names(fights_tbl)) {
    fights_tbl <- dplyr::rename(fights_tbl, fightID = id)
  }

  if ("name" %in% names(fights_tbl)) {
    fights_tbl <- dplyr::rename(fights_tbl, encounterName = name)
  }

  dplyr::mutate(
    fights_tbl,
    logID = report_meta$logID,
    report_title = report_meta$report_title,
    report_link = .wcl_report_link(.wcl_host_config(host)$base_url, report_meta$logID, fightID),
    report_start_at = report_meta$report_start_at,
    report_end_at = report_meta$report_end_at,
    duration = endTime - startTime,
    duration_s = duration / 1000,
    fight_start_at = .wcl_add_ms_to_time(report_meta$report_start_at, startTime),
    fight_end_at = .wcl_add_ms_to_time(report_meta$report_start_at, endTime)
  ) |>
    dplyr::relocate(
      logID,
      report_title,
      report_link,
      report_start_at,
      report_end_at,
      fightID,
      encounterID,
      encounterName
    )
}

.wcl_normalize_actors <- function(payload, report_code = NULL, host = "classic") {
  report_node <- .wcl_path_get(payload, c("data", "reportData", "report"), default = list())
  report_meta <- .wcl_extract_report_meta(report_node, host = host, report_code = report_code)
  actors_tbl <- .wcl_bind_records(
    .wcl_records(.wcl_path_get(report_node, c("masterData", "actors"), default = list()))
  )

  if (!nrow(actors_tbl)) {
    return(.wcl_empty_actors())
  }

  if ("id" %in% names(actors_tbl)) {
    actors_tbl <- dplyr::rename(actors_tbl, actorID = id)
  }

  dplyr::mutate(
    actors_tbl,
    logID = report_meta$logID,
    report_title = report_meta$report_title,
    report_link = report_meta$report_link,
    report_start_at = report_meta$report_start_at,
    report_end_at = report_meta$report_end_at
  ) |>
    dplyr::relocate(
      logID,
      report_title,
      report_link,
      report_start_at,
      report_end_at,
      actorID,
      gameID,
      name
    )
}

.wcl_normalize_player_details <- function(payload, report_code = NULL, host = "classic") {
  report_node <- .wcl_path_get(payload, c("data", "reportData", "report"), default = list())
  report_meta <- .wcl_extract_report_meta(report_node, host = host, report_code = report_code)
  fight_meta <- .wcl_extract_fight_meta(report_node, report_meta)

  details <- .wcl_path_get(
    report_node,
    c("playerDetails", "data", "playerDetails"),
    default = list()
  )

  role_map <- c(tanks = "Tank", healers = "Healer", dps = "DPS")
  records <- list()

  for (role_name in names(role_map)) {
    role_records <- .wcl_records(details[[role_name]] %||% list())
    if (!length(role_records)) {
      next
    }

    role_records <- lapply(role_records, function(record) {
      record$role <- role_map[[role_name]]
      record
    })

    records <- c(records, role_records)
  }

  tbl <- .wcl_bind_records(records, preserve = c("specs", "combatantInfo"))

  if (!nrow(tbl)) {
    return(.wcl_empty_player_details())
  }

  if ("id" %in% names(tbl)) {
    tbl <- dplyr::rename(tbl, actorID = id)
  }

  if (!"specs" %in% names(tbl)) {
    tbl$specs <- vector("list", nrow(tbl))
  }

  if (!"combatantInfo" %in% names(tbl)) {
    tbl$combatantInfo <- vector("list", nrow(tbl))
  }

  dplyr::mutate(
    tbl,
    logID = report_meta$logID,
    report_title = report_meta$report_title,
    report_link = .wcl_report_link(.wcl_host_config(host)$base_url, report_meta$logID, fight_meta$fightID %||% NA_integer_),
    report_start_at = report_meta$report_start_at,
    report_end_at = report_meta$report_end_at,
    fightID = fight_meta$fightID %||% NA_integer_,
    encounterID = fight_meta$encounterID %||% NA_integer_,
    encounterName = fight_meta$encounterName %||% NA_character_,
    difficulty = fight_meta$difficulty %||% NA_integer_,
    size = fight_meta$size %||% NA_integer_,
    kill = fight_meta$kill %||% NA,
    duration_s = fight_meta$duration_s %||% NA_real_,
    fight_start_at = fight_meta$fight_start_at %||% .wcl_null_time(),
    fight_end_at = fight_meta$fight_end_at %||% .wcl_null_time()
  ) |>
    dplyr::relocate(
      logID,
      report_title,
      report_link,
      report_start_at,
      report_end_at,
      fightID,
      encounterID,
      encounterName,
      role,
      actorID,
      name
    )
}

.wcl_normalize_events <- function(payload, report_code = NULL, host = "classic") {
  report_node <- .wcl_path_get(payload, c("data", "reportData", "report"), default = list())
  report_meta <- .wcl_extract_report_meta(report_node, host = host, report_code = report_code)
  fight_meta <- .wcl_extract_fight_meta(report_node, report_meta)

  event_node <- .wcl_path_get(report_node, "events", default = list())
  records <- .wcl_records(.wcl_path_get(event_node, "data", default = list()))
  tbl <- .wcl_bind_records(records)

  if (!nrow(tbl)) {
    return(.wcl_empty_events())
  }

  tbl <- dplyr::mutate(
    tbl,
    logID = report_meta$logID,
    report_title = report_meta$report_title,
    report_link = .wcl_report_link(.wcl_host_config(host)$base_url, report_meta$logID, fight_meta$fightID %||% NA_integer_),
    report_start_at = report_meta$report_start_at,
    report_end_at = report_meta$report_end_at,
    fightID = fight_meta$fightID %||% NA_integer_,
    encounterID = fight_meta$encounterID %||% NA_integer_,
    encounterName = fight_meta$encounterName %||% NA_character_,
    difficulty = fight_meta$difficulty %||% NA_integer_,
    size = fight_meta$size %||% NA_integer_,
    kill = fight_meta$kill %||% NA,
    duration_s = fight_meta$duration_s %||% NA_real_,
    fight_start_at = fight_meta$fight_start_at %||% .wcl_null_time(),
    fight_end_at = fight_meta$fight_end_at %||% .wcl_null_time()
  )

  if ("timestamp" %in% names(tbl)) {
    tbl$event_at <- .wcl_add_ms_to_time(report_meta$report_start_at, tbl$timestamp)
  } else {
    tbl$event_at <- .wcl_na_time(nrow(tbl))
  }

  dplyr::relocate(
    tbl,
    logID,
    report_title,
    report_link,
    report_start_at,
    report_end_at,
    fightID,
    encounterID,
    encounterName,
    duration_s,
    fight_start_at,
    fight_end_at,
    timestamp,
    event_at
  )
}

.wcl_normalize_rankings <- function(
    payload,
    zone_id,
    metric,
    page,
    encounter_id = NULL,
    class_name = NULL) {
  encounters <- .wcl_path_get(payload, c("data", "worldData", "zone", "encounters"), default = list())
  encounter_records <- .wcl_records(encounters)
  ranking_field <- if (is.null(class_name)) "fightRankings" else "characterRankings"

  rows <- list()

  for (encounter in encounter_records) {
    current_id <- as.integer(.wcl_path_get(encounter, "journalID", default = NA_integer_))
    if (!is.null(encounter_id) && !identical(current_id, as.integer(encounter_id))) {
      next
    }

    ranking_payload <- .wcl_path_get(encounter, ranking_field, default = list())
    ranking_records <- .wcl_records(.wcl_path_get(ranking_payload, "rankings", default = list()))
    ranking_meta <- .wcl_flatten_record(
      ranking_payload[setdiff(names(ranking_payload) %||% character(), "rankings")] %||% list()
    )
    if (length(ranking_meta)) {
      names(ranking_meta) <- paste0("ranking_", names(ranking_meta))
    }

    if (!length(ranking_records)) {
      next
    }

    for (record in ranking_records) {
      flat <- .wcl_flatten_record(record)
      flat <- c(
        list(
          zone_id = as.integer(zone_id),
          encounterID = current_id,
          encounterName = .wcl_path_get(encounter, "name", default = NA_character_),
          metric = as.character(metric),
          ranking_type = if (is.null(class_name)) "fight" else "character",
          page = as.integer(page)
        ),
        ranking_meta,
        flat
      )

      if ("report_code" %in% names(flat)) {
        flat$report_code <- wcl_report_code(flat$report_code)
      }

      rows[[length(rows) + 1L]] <- flat
    }
  }

  if (!length(rows)) {
    return(.wcl_empty_rankings())
  }

  dplyr::bind_rows(rows)
}
