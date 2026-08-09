#' Retrieve player rankings for a report
#'
#' Retrieves the report-scoped rankings payload from Warcraft Logs. By
#' default, the payload is normalized to one row per character, fight, and
#' role. Set `output = "raw"` to return the exact parsed JSON value supplied by
#' the API instead.
#'
#' Omitting `fight_ids` requests all applicable fights in the report; it does
#' not calculate a single aggregate rank for the report. Rankings are mutable
#' Warcraft Logs data and may change after they are first returned.
#'
#' Tidy rows include the report's `segments`, `exportedSegments`, and a derived
#' `rankings_complete` value. A warning is emitted when Warcraft Logs has not
#' yet processed every uploaded segment for rankings.
#'
#' Direct scalar player and ranking fields become columns. Arrays and nested or
#' unstable values are retained as list-columns. Empty rankings return a typed
#' zero-row tibble; an unrecognized non-empty shape raises an error that directs
#' callers to `output = "raw"`.
#'
#' @param report_code Report code or report URL.
#' @param fight_ids Optional integer vector of fight IDs inside the report.
#' @param encounter_id Optional encounter ID filter.
#' @param difficulty Optional difficulty filter.
#' @param player_metric Player ranking metric. Defaults to `"default"`. See
#'   <https://www.warcraftlogs.com/v2-api-docs/warcraft/reportrankingmetrictype.doc.html>.
#' @param compare Whether scores are compared with each player's best ranking
#'   (`"Rankings"`) or all parses in a two-week window (`"Parses"`). See
#'   <https://www.warcraftlogs.com/v2-api-docs/warcraft/rankingcomparetype.doc.html>.
#' @param timeframe Whether ranks are evaluated against current values
#'   (`"Today"`) or values around the time of the fight (`"Historical"`). See
#'   <https://www.warcraftlogs.com/v2-api-docs/warcraft/rankingtimeframetype.doc.html>.
#' @param output Output form: `"tidy"` returns a tibble with one row per
#'   character, fight, and role; `"raw"` returns the exact parsed value of the
#'   GraphQL `Report.rankings` JSON scalar.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble when `output = "tidy"`; otherwise the unmodified parsed
#'   `Report.rankings` value (usually a nested list, or `NULL`).
#'
#' @seealso [wcl_rankings()] for global encounter leaderboards.
#' @references
#' <https://www.warcraftlogs.com/v2-api-docs/warcraft/report.doc.html>
#'
#' <https://www.warcraftlogs.com/help/ranks/>
#' @export
wcl_report_rankings <- function(
    report_code,
    fight_ids = NULL,
    encounter_id = NULL,
    difficulty = NULL,
    player_metric = "default",
    compare = "Rankings",
    timeframe = "Today",
    output = c("tidy", "raw"),
    client = NULL) {
  output <- match.arg(output)

  report_code <- wcl_report_code(report_code)
  if (length(report_code) != 1L) {
    stop("`report_code` must identify exactly one report.", call. = FALSE)
  }

  fight_ids <- .wcl_validate_report_rankings_integer(
    fight_ids,
    arg = "fight_ids",
    multiple = TRUE,
    minimum = 1L
  )
  encounter_id <- .wcl_validate_report_rankings_integer(
    encounter_id,
    arg = "encounter_id",
    minimum = 1L
  )
  difficulty <- .wcl_validate_report_rankings_integer(
    difficulty,
    arg = "difficulty",
    minimum = 1L
  )
  player_metric <- .wcl_validate_report_rankings_metric(player_metric)
  compare <- .wcl_validate_report_rankings_enum(
    compare,
    choices = c("Rankings", "Parses"),
    arg = "compare"
  )
  timeframe <- .wcl_validate_report_rankings_enum(
    timeframe,
    choices = c("Today", "Historical"),
    arg = "timeframe"
  )

  client <- .wcl_resolve_client(client)
  payload <- wcl_request(
    client = client,
    query = .wcl_query_report_rankings(
      report_code = report_code,
      fight_ids = fight_ids,
      encounter_id = encounter_id,
      difficulty = difficulty,
      player_metric = player_metric,
      compare = compare,
      timeframe = timeframe
    )
  )

  .wcl_warn_incomplete_report_rankings(payload)

  report_node <- .wcl_path_get(
    payload,
    c("data", "reportData", "report"),
    default = list()
  )
  rankings <- if (is.list(report_node) &&
      "rankings" %in% (names(report_node) %||% character())) {
    report_node[["rankings"]]
  } else {
    NULL
  }

  if (identical(output, "raw")) {
    return(rankings)
  }

  .wcl_normalize_report_rankings(
    payload = payload,
    report_code = report_code,
    player_metric = player_metric,
    compare = compare,
    timeframe = timeframe,
    host = client$host
  )
}

.wcl_report_ranking_metrics <- function() {
  c(
    "bosscdps",
    "bossdps",
    "bossndps",
    "bossrdps",
    "default",
    "dps",
    "hps",
    "krsi",
    "playerscore",
    "playerspeed",
    "cdps",
    "ndps",
    "rdps",
    "tankhps",
    "wdps"
  )
}

.wcl_validate_report_rankings_metric <- function(x) {
  choices <- .wcl_report_ranking_metrics()

  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`player_metric` must be one non-empty character value.", call. = FALSE)
  }

  value <- x
  if (!value %in% choices) {
    stop(
      "`player_metric` must be one of: ",
      paste(choices, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  value
}

.wcl_validate_report_rankings_enum <- function(x, choices, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be one non-empty character value.", call. = FALSE)
  }

  index <- match(x, choices)
  if (is.na(index)) {
    stop(
      "`", arg, "` must be one of: ",
      paste(choices, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  choices[[index]]
}

.wcl_validate_report_rankings_integer <- function(
    x,
    arg,
    multiple = FALSE,
    minimum = -.Machine$integer.max) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.numeric(x) || !length(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != trunc(x))) {
    stop("`", arg, "` must contain whole, finite numbers.", call. = FALSE)
  }

  if (!isTRUE(multiple) && length(x) != 1L) {
    stop("`", arg, "` must be a single integer.", call. = FALSE)
  }

  if (any(x < minimum) || any(x > .Machine$integer.max)) {
    stop(
      "`", arg, "` must contain integers between ",
      format(minimum, scientific = FALSE), " and ",
      format(.Machine$integer.max, scientific = FALSE), ".",
      call. = FALSE
    )
  }

  value <- as.integer(x)
  if (isTRUE(multiple)) unique(value) else value
}

.wcl_query_report_rankings <- function(
    report_code,
    fight_ids = NULL,
    encounter_id = NULL,
    difficulty = NULL,
    player_metric = "default",
    compare = "Rankings",
    timeframe = "Today") {
  report_code <- wcl_report_code(report_code)
  if (length(report_code) != 1L) {
    stop("`report_code` must identify exactly one report.", call. = FALSE)
  }

  fight_ids <- .wcl_validate_report_rankings_integer(
    fight_ids,
    arg = "fight_ids",
    multiple = TRUE,
    minimum = 1L
  )
  encounter_id <- .wcl_validate_report_rankings_integer(
    encounter_id,
    arg = "encounter_id",
    minimum = 1L
  )
  difficulty <- .wcl_validate_report_rankings_integer(
    difficulty,
    arg = "difficulty",
    minimum = 1L
  )
  player_metric <- .wcl_validate_report_rankings_metric(player_metric)
  compare <- .wcl_validate_report_rankings_enum(
    compare,
    choices = c("Rankings", "Parses"),
    arg = "compare"
  )
  timeframe <- .wcl_validate_report_rankings_enum(
    timeframe,
    choices = c("Today", "Historical"),
    arg = "timeframe"
  )

  filter_args <- c(
    if (!is.null(fight_ids)) {
      paste0("fightIDs: ", .wcl_graphql_int_array(fight_ids))
    },
    if (!is.null(encounter_id)) {
      paste0("encounterID: ", .wcl_graphql_int(encounter_id))
    },
    if (!is.null(difficulty)) {
      paste0("difficulty: ", .wcl_graphql_int(difficulty))
    }
  )
  ranking_args <- c(
    filter_args,
    paste0("playerMetric: ", player_metric),
    paste0("compare: ", compare),
    paste0("timeframe: ", timeframe)
  )

  fights_field <- if (length(filter_args)) {
    paste0("fights(", paste(filter_args, collapse = ", "), ")")
  } else {
    "fights"
  }

  sprintf(
    paste0(
      "{",
      " reportData {",
      "  report(code: %s) {",
      "   code",
      "   title",
      "   startTime",
      "   endTime",
      "   segments",
      "   exportedSegments",
      "   ", fights_field, " {",
      "    id",
      "    name",
      "    encounterID",
      "    difficulty",
      "    size",
      "    kill",
      "    startTime",
      "    endTime",
      "   }",
      "   rankings(", paste(ranking_args, collapse = ", "), ")",
      "  }",
      " }",
      "}"
    ),
    .wcl_graphql_string(report_code)
  )
}

.wcl_warn_incomplete_report_rankings <- function(payload) {
  report_node <- .wcl_path_get(
    payload,
    c("data", "reportData", "report"),
    default = list()
  )
  segments <- suppressWarnings(as.integer(
    .wcl_path_get(report_node, "segments", default = NA_integer_)
  ))
  exported_segments <- suppressWarnings(as.integer(
    .wcl_path_get(report_node, "exportedSegments", default = NA_integer_)
  ))

  if (length(segments) == 1L && length(exported_segments) == 1L &&
      !is.na(segments) && !is.na(exported_segments) &&
      exported_segments < segments) {
    warning(
      "Warcraft Logs has processed ", exported_segments, " of ", segments,
      " report segments for rankings; returned rankings may be incomplete.",
      call. = FALSE
    )
  }

  invisible(payload)
}

.wcl_empty_report_rankings <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_empty_time(),
    report_end_at = .wcl_empty_time(),
    segments = integer(),
    exportedSegments = integer(),
    rankings_complete = logical(),
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
    player_metric = character(),
    compare = character(),
    timeframe = character(),
    actorID = integer(),
    name = character()
  )
}

.wcl_report_rankings_abort_shape <- function() {
  stop(
    "Warcraft Logs returned a non-empty `rankings` JSON shape that wclR ",
    "does not recognize. Retry with `output = \"raw\"` to inspect the exact ",
    "payload.",
    call. = FALSE
  )
}

.wcl_report_rankings_is_empty <- function(x) {
  is.null(x) || length(x) == 0L ||
    (inherits(x, "data.frame") && nrow(x) == 0L)
}

.wcl_report_rankings_scalar <- function(record, candidates, default = NULL) {
  if (!is.list(record)) {
    return(default)
  }

  for (candidate in candidates) {
    value <- record[[candidate]]
    if (!is.null(value) && length(value) && !is.list(value)) {
      return(value[[1L]])
    }
  }

  default
}

.wcl_report_rankings_integer <- function(value) {
  value <- suppressWarnings(as.integer(value))
  if (length(value)) value[[1L]] else NA_integer_
}

.wcl_report_rankings_number <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value)) value[[1L]] else NA_real_
}

.wcl_report_rankings_logical <- function(value) {
  value <- as.logical(value)
  if (length(value)) value[[1L]] else NA
}

.wcl_report_rankings_character <- function(value) {
  value <- as.character(value)
  if (length(value)) value[[1L]] else NA_character_
}

.wcl_report_rankings_nonempty_scalar <- function(value) {
  !is.null(value) && !is.list(value) && length(value) == 1L &&
    !is.na(value) && nzchar(trimws(as.character(value)))
}

.wcl_report_rankings_positive_integer <- function(value) {
  if (is.null(value) || is.list(value) || length(value) != 1L ||
      is.na(value)) {
    return(NA_integer_)
  }

  number <- suppressWarnings(as.numeric(value))
  if (!is.finite(number) || number < 1 || number != trunc(number) ||
      number > .Machine$integer.max) {
    return(NA_integer_)
  }

  as.integer(number)
}

.wcl_report_rankings_actor_identity <- function(character) {
  for (field in c("id", "actorID", "guid")) {
    value <- .wcl_report_rankings_positive_integer(character[[field]])
    if (!is.na(value)) {
      return(list(value = value, field = field))
    }
  }

  list(value = NA_integer_, field = NULL)
}

.wcl_report_rankings_role <- function(x) {
  key <- tolower(gsub("[^A-Za-z0-9]", "", as.character(x)))
  role_map <- c(
    tank = "Tank",
    tanks = "Tank",
    healer = "Healer",
    healers = "Healer",
    dps = "DPS",
    damage = "DPS",
    damagedealer = "DPS",
    damagedealers = "DPS"
  )

  if (key %in% names(role_map)) role_map[[key]] else as.character(x)
}

.wcl_report_rankings_extras <- function(
    record,
    exclude = character(),
    prefix = NULL) {
  if (!is.list(record) || is.null(names(record))) {
    return(list())
  }

  values <- record[!names(record) %in% exclude]
  if (!length(values)) {
    return(list())
  }

  flat <- .wcl_flatten_report_rankings_record(values)
  if (!is.null(prefix) && length(flat)) {
    names(flat) <- paste0(prefix, names(flat))
  }

  flat
}

.wcl_flatten_report_rankings_record <- function(record) {
  if (inherits(record, "data.frame") && nrow(record) == 1L) {
    record <- as.list(record[1, , drop = FALSE])
  }

  if (!is.list(record) || !length(record)) {
    return(list())
  }

  keys <- names(record) %||% rep.int("", length(record))
  keys[!nzchar(keys)] <- paste0("field_", which(!nzchar(keys)))
  out <- vector("list", length(record))

  for (index in seq_along(record)) {
    value <- record[[index]]
    if (is.null(value)) {
      out[[index]] <- NA
    } else if (is.list(value) || inherits(value, "data.frame") ||
        length(value) != 1L) {
      out[[index]] <- list(value)
    } else {
      out[[index]] <- value
    }
  }

  names(out) <- make.unique(keys, sep = "_")
  out
}

.wcl_report_rankings_rename_collisions <- function(
    x,
    reserved,
    prefix = "character_") {
  if (!length(x)) {
    return(x)
  }

  collisions <- names(x) %in% reserved
  names(x)[collisions] <- paste0(prefix, names(x)[collisions])
  names(x) <- make.unique(names(x), sep = "_")
  x
}

.wcl_report_rankings_find_fight <- function(fights, fight_id) {
  if (!length(fights) || is.na(fight_id)) {
    return(list())
  }

  ids <- vapply(
    fights,
    function(fight) {
      .wcl_report_rankings_integer(
        .wcl_report_rankings_scalar(fight, c("id", "fightID"), NA_integer_)
      )
    },
    integer(1)
  )
  index <- match(fight_id, ids)

  if (is.na(index)) list() else fights[[index]]
}

.wcl_report_rankings_role_entries <- function(roles) {
  if (!is.list(roles)) {
    .wcl_report_rankings_abort_shape()
  }

  if (!length(roles)) {
    return(list())
  }

  role_names <- names(roles)
  if (is.null(role_names)) {
    role_names <- rep.int("", length(roles))
  }

  lapply(seq_along(roles), function(index) {
    role <- roles[[index]]
    fallback_name <- .wcl_report_rankings_scalar(
      role,
      c("role", "name", "type"),
      default = paste0("Role", index)
    )
    role_name <- role_names[[index]]
    if (!nzchar(role_name)) {
      role_name <- fallback_name
    }

    list(name = role_name, value = role)
  })
}

.wcl_report_rankings_characters <- function(role) {
  if (.wcl_report_rankings_is_empty(role)) {
    return(list(characters = list(), extras = list(), recognized = TRUE))
  }

  if (is.list(role) && !is.null(names(role)) && "characters" %in% names(role)) {
    characters <- role[["characters"]]
    extras <- .wcl_report_rankings_extras(
      role,
      exclude = "characters",
      prefix = "role_"
    )
    return(list(characters = characters, extras = extras, recognized = TRUE))
  }

  direct_collection <- inherits(role, "data.frame") ||
    (is.list(role) && is.null(names(role))) ||
    (is.list(role) && any(c("id", "guid", "actorID") %in% names(role)))

  if (direct_collection) {
    return(list(characters = role, extras = list(), recognized = TRUE))
  }

  list(characters = NULL, extras = list(), recognized = FALSE)
}

.wcl_report_rankings_dynamic_family <- function(value) {
  if (is.null(value) ||
      (is.atomic(value) && length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }

  if (is.list(value)) {
    return("list")
  }

  if (is.logical(value)) {
    return("logical")
  }

  if (is.integer(value) || is.double(value)) {
    return("numeric")
  }

  paste(class(value), collapse = "/")
}

.wcl_harmonize_report_ranking_rows <- function(rows, core_names) {
  dynamic_names <- setdiff(
    unique(unlist(lapply(rows, names), use.names = FALSE)),
    core_names
  )

  for (field in dynamic_names) {
    values <- lapply(rows, function(row) row[[field]])
    families <- vapply(
      values,
      .wcl_report_rankings_dynamic_family,
      character(1)
    )
    families <- unique(families[!is.na(families)])
    needs_list_column <- any(vapply(values, is.list, logical(1))) ||
      length(families) > 1L

    if (needs_list_column) {
      rows <- lapply(rows, function(row) {
        value <- row[[field]]
        if (is.null(value)) {
          row[[field]] <- list(NULL)
        } else if (!is.list(value)) {
          row[[field]] <- list(value)
        }
        row
      })
    }
  }

  rows
}

.wcl_normalize_report_rankings <- function(
    payload,
    report_code = NULL,
    player_metric = "default",
    compare = "Rankings",
    timeframe = "Today",
    host = "classic") {
  report_node <- .wcl_path_get(
    payload,
    c("data", "reportData", "report"),
    default = list()
  )
  rankings <- .wcl_path_get(report_node, "rankings", default = NULL)

  if (.wcl_report_rankings_is_empty(rankings)) {
    return(.wcl_empty_report_rankings())
  }

  if (!is.list(rankings) || is.null(names(rankings)) ||
      !"data" %in% names(rankings)) {
    .wcl_report_rankings_abort_shape()
  }

  ranking_data <- rankings[["data"]]
  if (.wcl_report_rankings_is_empty(ranking_data)) {
    return(.wcl_empty_report_rankings())
  }

  rankings_extras <- .wcl_report_rankings_extras(
    rankings,
    exclude = "data",
    prefix = "rankings_"
  )

  ranking_fights <- .wcl_records(ranking_data)
  if (!length(ranking_fights) ||
      any(!vapply(ranking_fights, is.list, logical(1)))) {
    .wcl_report_rankings_abort_shape()
  }

  report_meta <- .wcl_extract_report_meta(
    report_node,
    host = host,
    report_code = report_code
  )
  fights <- .wcl_records(.wcl_path_get(report_node, "fights", default = list()))
  segments <- .wcl_report_rankings_integer(
    .wcl_path_get(report_node, "segments", default = NA_integer_)
  )
  exported_segments <- .wcl_report_rankings_integer(
    .wcl_path_get(report_node, "exportedSegments", default = NA_integer_)
  )
  rankings_complete <- if (is.na(segments) || is.na(exported_segments)) {
    NA
  } else {
    exported_segments >= segments
  }

  rows <- list()
  recognized_fights <- 0L
  recognized_roles <- 0L

  for (ranking_fight in ranking_fights) {
    if (is.null(names(ranking_fight)) || !"roles" %in% names(ranking_fight)) {
      .wcl_report_rankings_abort_shape()
    }
    recognized_fights <- recognized_fights + 1L

    fight_id <- .wcl_report_rankings_integer(
      .wcl_report_rankings_scalar(
        ranking_fight,
        c("fightID", "id"),
        default = NA_integer_
      )
    )
    if (is.na(fight_id) || fight_id < 1L) {
      .wcl_report_rankings_abort_shape()
    }
    fight <- .wcl_report_rankings_find_fight(fights, fight_id)

    encounter_id <- .wcl_report_rankings_integer(
      .wcl_report_rankings_scalar(
        fight,
        "encounterID",
        .wcl_report_rankings_scalar(
          ranking_fight,
          c("encounterID", "encounter"),
          NA_integer_
        )
      )
    )
    encounter_name <- .wcl_report_rankings_character(
      .wcl_report_rankings_scalar(fight, "name", NA_character_)
    )
    fight_difficulty <- .wcl_report_rankings_integer(
      .wcl_report_rankings_scalar(
        fight,
        "difficulty",
        .wcl_report_rankings_scalar(ranking_fight, "difficulty", NA_integer_)
      )
    )
    fight_size <- .wcl_report_rankings_integer(
      .wcl_report_rankings_scalar(
        fight,
        "size",
        .wcl_report_rankings_scalar(ranking_fight, "size", NA_integer_)
      )
    )
    kill <- .wcl_report_rankings_logical(
      .wcl_report_rankings_scalar(fight, "kill", NA)
    )
    fight_start_ms <- .wcl_report_rankings_number(
      .wcl_report_rankings_scalar(fight, "startTime", NA_real_)
    )
    fight_end_ms <- .wcl_report_rankings_number(
      .wcl_report_rankings_scalar(fight, "endTime", NA_real_)
    )
    duration_s <- if (is.na(fight_start_ms) || is.na(fight_end_ms)) {
      NA_real_
    } else {
      (fight_end_ms - fight_start_ms) / 1000
    }

    ranking_extras <- .wcl_report_rankings_extras(
      ranking_fight,
      exclude = c(
        "roles", "fightID", "id", "encounterID", "encounter",
        "difficulty", "size"
      ),
      prefix = "ranking_"
    )

    roles <- ranking_fight[["roles"]]
    role_entries <- .wcl_report_rankings_role_entries(roles)
    recognized_roles <- recognized_roles + 1L

    for (role_entry in role_entries) {
      role_data <- .wcl_report_rankings_characters(role_entry$value)
      if (!isTRUE(role_data$recognized)) {
        .wcl_report_rankings_abort_shape()
      }

      if (.wcl_report_rankings_is_empty(role_data$characters)) {
        next
      }

      characters <- .wcl_records(role_data$characters)
      if (!length(characters) || any(!vapply(
        characters,
        function(character) is.list(character) && !is.null(names(character)),
        logical(1)
      ))) {
        .wcl_report_rankings_abort_shape()
      }

      for (character in characters) {
        actor_identity <- .wcl_report_rankings_actor_identity(character)
        name_value <- character[["name"]]
        guid_value <- character[["guid"]]
        has_name <- .wcl_report_rankings_nonempty_scalar(name_value)
        has_guid <- .wcl_report_rankings_nonempty_scalar(guid_value)
        if (is.na(actor_identity$value) && !has_name && !has_guid) {
          .wcl_report_rankings_abort_shape()
        }

        character_flat <- .wcl_flatten_report_rankings_record(character)
        actor_id <- actor_identity$value
        character_name <- if (has_name) {
          as.character(name_value)
        } else {
          NA_character_
        }
        if (!is.null(actor_identity$field)) {
          character_flat[[actor_identity$field]] <- NULL
        }
        character_flat[["name"]] <- NULL

        core <- list(
          logID = report_meta$logID,
          report_title = report_meta$report_title,
          report_link = .wcl_report_link(
            .wcl_host_config(host)$base_url,
            report_meta$logID,
            fight_id
          ),
          report_start_at = report_meta$report_start_at,
          report_end_at = report_meta$report_end_at,
          segments = segments,
          exportedSegments = exported_segments,
          rankings_complete = rankings_complete,
          fightID = fight_id,
          encounterID = encounter_id,
          encounterName = encounter_name,
          difficulty = fight_difficulty,
          size = fight_size,
          kill = kill,
          duration_s = duration_s,
          fight_start_at = .wcl_add_ms_to_time(
            report_meta$report_start_at,
            fight_start_ms
          ),
          fight_end_at = .wcl_add_ms_to_time(
            report_meta$report_start_at,
            fight_end_ms
          ),
          role = .wcl_report_rankings_role(role_entry$name),
          player_metric = player_metric,
          compare = compare,
          timeframe = timeframe,
          actorID = actor_id,
          name = character_name
        )
        row_role_extras <- .wcl_report_rankings_rename_collisions(
          role_data$extras,
          reserved = names(core),
          prefix = "role_payload_"
        )
        row_ranking_extras <- .wcl_report_rankings_rename_collisions(
          ranking_extras,
          reserved = c(names(core), names(row_role_extras)),
          prefix = "ranking_payload_"
        )
        row_rankings_extras <- .wcl_report_rankings_rename_collisions(
          rankings_extras,
          reserved = c(
            names(core),
            names(row_role_extras),
            names(row_ranking_extras)
          ),
          prefix = "rankings_payload_"
        )
        character_flat <- .wcl_report_rankings_rename_collisions(
          character_flat,
          reserved = c(
            names(core),
            names(row_role_extras),
            names(row_ranking_extras),
            names(row_rankings_extras),
            grep(
              "^(role|ranking|rankings)_",
              names(character_flat),
              value = TRUE
            )
          )
        )

        rows[[length(rows) + 1L]] <- c(
          core,
          character_flat,
          row_role_extras,
          row_ranking_extras,
          row_rankings_extras
        )
      }
    }
  }

  if (!recognized_fights || !recognized_roles) {
    .wcl_report_rankings_abort_shape()
  }

  if (!length(rows)) {
    return(.wcl_empty_report_rankings())
  }

  core_names <- names(.wcl_empty_report_rankings())
  rows <- .wcl_harmonize_report_ranking_rows(rows, core_names)
  out <- dplyr::bind_rows(rows)
  tibble::as_tibble(out[c(core_names, setdiff(names(out), core_names))])
}
