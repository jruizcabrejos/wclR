.wcl_cast_empty_time <- function(n = 0L) {
  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
}

.wcl_cast_parse_time <- function(x) {
  if (!length(x)) {
    return(.wcl_cast_empty_time())
  }

  if (inherits(x, "POSIXct")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }

  values <- as.character(x)
  out <- .wcl_cast_empty_time(length(values))
  remaining <- !is.na(values) & nzchar(values)

  parse_with <- function(format, candidates = values) {
    indices <- which(remaining)
    if (!length(indices)) {
      return(invisible(NULL))
    }

    candidate_values <- candidates[indices]
    unique_values <- unique(candidate_values)
    parsed_unique <- suppressWarnings(as.POSIXct(
      unique_values,
      format = format,
      tz = "UTC"
    ))
    parsed <- parsed_unique[match(candidate_values, unique_values)]
    matched <- !is.na(parsed)
    if (any(matched)) {
      out[indices[matched]] <<- parsed[matched]
      remaining[indices[matched]] <<- FALSE
    }

    invisible(NULL)
  }

  parse_with("%Y-%m-%dT%H:%M:%OSZ")
  offset_values <- sub(
    "([+-][0-9]{2}):([0-9]{2})$",
    "\\1\\2",
    values,
    perl = TRUE
  )
  parse_with("%Y-%m-%dT%H:%M:%OS%z", offset_values)
  parse_with("%Y-%m-%dT%H:%M:%OS")
  parse_with("%Y-%m-%d %H:%M:%OS")
  parse_with("%Y-%m-%d")

  out
}

.wcl_cast_id_filter <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  values <- suppressWarnings(as.numeric(x))
  valid <- length(values) > 0L &&
    length(values) == length(x) &&
    all(is.finite(values)) &&
    all(values == floor(values))

  if (!valid) {
    stop(
      sprintf("`%s` must be a non-empty vector of integer-like IDs without missing values.", name),
      call. = FALSE
    )
  }

  unique(as.integer(values))
}

.wcl_cast_scalar_number <- function(value, keys) {
  if (is.null(value) || !length(value)) {
    return(NA_real_)
  }

  if (inherits(value, "data.frame")) {
    if (!nrow(value)) {
      return(NA_real_)
    }
    value <- as.list(value[1L, , drop = FALSE])
  }

  if (is.list(value)) {
    for (key in keys) {
      candidate <- value[[key]]
      if (is.null(candidate)) {
        next
      }
      parsed <- .wcl_cast_scalar_number(candidate, keys)
      if (is.finite(parsed)) {
        return(parsed)
      }
    }
    return(NA_real_)
  }

  parsed <- suppressWarnings(as.numeric(value[[1L]]))
  if (length(parsed) && is.finite(parsed)) parsed else NA_real_
}

.wcl_cast_row_number <- function(data, row, aliases, keys = aliases) {
  for (alias in aliases) {
    if (!alias %in% names(data)) {
      next
    }

    parsed <- .wcl_cast_scalar_number(data[[alias]][[row]], keys)
    if (is.finite(parsed)) {
      return(parsed)
    }
  }

  NA_real_
}

.wcl_cast_scalar_logical <- function(value) {
  if (is.null(value) || !length(value)) {
    return(NA)
  }

  if (inherits(value, "data.frame")) {
    if (!nrow(value)) {
      return(NA)
    }
    value <- value[[1L, 1L]]
  }

  if (is.list(value)) {
    if (!length(value)) {
      return(NA)
    }
    return(.wcl_cast_scalar_logical(value[[1L]]))
  }

  if (is.logical(value)) {
    return(value[[1L]])
  }

  if (is.numeric(value)) {
    return(if (is.na(value[[1L]])) NA else value[[1L]] != 0)
  }

  text <- tolower(trimws(as.character(value[[1L]])))
  if (text %in% c("true", "t", "1", "yes")) {
    return(TRUE)
  }
  if (text %in% c("false", "f", "0", "no")) {
    return(FALSE)
  }

  NA
}

.wcl_cast_row_logical <- function(data, row, aliases) {
  for (alias in aliases) {
    if (!alias %in% names(data)) {
      next
    }
    parsed <- .wcl_cast_scalar_logical(data[[alias]][[row]])
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  NA
}

.wcl_cast_number_field <- function(data, aliases, keys, n) {
  out <- rep.int(NA_real_, n)

  for (alias in aliases) {
    if (!alias %in% names(data)) {
      next
    }

    missing <- which(!is.finite(out))
    if (!length(missing)) {
      break
    }
    column <- data[[alias]]

    if (is.list(column) || inherits(column, "data.frame")) {
      parsed <- vapply(
        missing,
        function(row) {
          value <- if (inherits(column, "data.frame")) {
            column[row, , drop = FALSE]
          } else {
            column[[row]]
          }
          .wcl_cast_scalar_number(value, keys)
        },
        numeric(1)
      )
      usable <- is.finite(parsed)
      out[missing[usable]] <- parsed[usable]
      next
    }

    if (is.factor(column)) {
      column <- as.character(column)
    }
    parsed <- suppressWarnings(as.numeric(column))
    usable <- is.finite(parsed[missing])
    out[missing[usable]] <- parsed[missing[usable]]
  }

  out
}

.wcl_cast_logical_field <- function(data, aliases, n) {
  out <- rep.int(NA, n)

  for (alias in aliases) {
    if (!alias %in% names(data)) {
      next
    }

    missing <- which(is.na(out))
    if (!length(missing)) {
      break
    }
    column <- data[[alias]]

    if (is.list(column) || inherits(column, "data.frame")) {
      parsed <- vapply(
        missing,
        function(row) {
          value <- if (inherits(column, "data.frame")) {
            column[row, , drop = FALSE]
          } else {
            column[[row]]
          }
          .wcl_cast_scalar_logical(value)
        },
        logical(1)
      )
      usable <- !is.na(parsed)
      out[missing[usable]] <- parsed[usable]
      next
    }

    if (is.logical(column)) {
      parsed <- column
    } else if (is.numeric(column)) {
      parsed <- ifelse(is.na(column), NA, column != 0)
    } else {
      if (is.factor(column)) {
        column <- as.character(column)
      }
      text <- tolower(trimws(as.character(column)))
      parsed <- rep.int(NA, length(text))
      parsed[text %in% c("true", "t", "1", "yes")] <- TRUE
      parsed[text %in% c("false", "f", "0", "no")] <- FALSE
    }

    usable <- !is.na(parsed[missing])
    out[missing[usable]] <- parsed[missing[usable]]
  }

  out
}

.wcl_cast_prepared_empty <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_cast_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    fight_start_at = .wcl_cast_empty_time(),
    timestamp = numeric(),
    event_at = .wcl_cast_empty_time(),
    type = character(),
    sourceID = integer(),
    sourceInstanceID = integer(),
    targetID = integer(),
    targetInstanceID = integer(),
    abilityGameID = integer(),
    stoppedAbilityGameID = integer(),
    is_periodic = logical(),
    .row_order = integer()
  )
}

.wcl_cast_prepare_events <- function(data, name, types = NULL) {
  if (!inherits(data, "data.frame")) {
    stop(sprintf("`%s` must be a data frame.", name), call. = FALSE)
  }

  data <- tibble::as_tibble(data)
  n <- nrow(data)
  if (!n) {
    return(.wcl_cast_prepared_empty())
  }
  if (!"type" %in% names(data)) {
    stop(sprintf("`%s` must include a `type` column.", name), call. = FALSE)
  }

  type_values <- tolower(trimws(as.character(data$type)))
  if (!is.null(types)) {
    keep <- type_values %in% tolower(as.character(types))
    data <- data[keep, , drop = FALSE]
    type_values <- type_values[keep]
    n <- nrow(data)
    if (!n) {
      return(.wcl_cast_prepared_empty())
    }
  }

  get_character <- function(column) {
    if (column %in% names(data)) as.character(data[[column]]) else rep.int(NA_character_, n)
  }
  get_integer <- function(column) {
    if (column %in% names(data)) suppressWarnings(as.integer(data[[column]])) else rep.int(NA_integer_, n)
  }
  get_numeric <- function(column) {
    if (column %in% names(data)) suppressWarnings(as.numeric(data[[column]])) else rep.int(NA_real_, n)
  }
  get_time <- function(column) {
    if (column %in% names(data)) .wcl_cast_parse_time(data[[column]]) else .wcl_cast_empty_time(n)
  }

  source_id <- .wcl_cast_number_field(
    data,
    c("sourceID", "sourceId", "source_id", "source"),
    c("reportID", "reportId", "actorID", "actorId", "id", "gameID", "gameId"),
    n
  )
  source_instance_id <- .wcl_cast_number_field(
    data,
    c("sourceInstanceID", "sourceInstanceId", "sourceInstance", "source_instance_id"),
    c("sourceInstanceID", "sourceInstanceId", "sourceInstance", "instanceID", "instanceId", "instance", "id"),
    n
  )
  target_id <- .wcl_cast_number_field(
    data,
    c("targetID", "targetId", "target_id", "target"),
    c("reportID", "reportId", "actorID", "actorId", "id", "gameID", "gameId"),
    n
  )
  target_instance_id <- .wcl_cast_number_field(
    data,
    c("targetInstanceID", "targetInstanceId", "targetInstance", "target_instance_id"),
    c("targetInstanceID", "targetInstanceId", "targetInstance", "instanceID", "instanceId", "instance", "id"),
    n
  )
  ability_id <- .wcl_cast_number_field(
    data,
    c(
      "abilityGameID", "abilityGameId", "ability_gameID", "ability_gameId",
      "ability_guid", "abilityGUID", "ability"
    ),
    c("gameID", "gameId", "guid", "abilityGameID", "abilityGameId", "id"),
    n
  )
  stopped_ability_id <- .wcl_cast_number_field(
    data,
    c(
      "extraAbilityGameID", "extraAbilityGameId", "extraAbility_gameID",
      "extraAbility_gameId", "extraAbility_guid", "extraAbilityGUID",
      "stoppedAbilityGameID", "stoppedAbilityGameId", "stoppedAbility_gameID",
      "stoppedAbility_gameId", "stoppedAbility_guid", "stoppedAbility"
    ),
    c(
      "gameID", "gameId", "guid", "abilityGameID", "abilityGameId",
      "extraAbilityGameID", "extraAbilityGameId", "stoppedAbilityGameID",
      "stoppedAbilityGameId", "id"
    ),
    n
  )
  periodic <- .wcl_cast_logical_field(
    data,
    c("isTick", "is_tick", "tick", "periodic", "isPeriodic"),
    n
  )

  report_start_at <- get_time("report_start_at")
  timestamp <- get_numeric("timestamp")
  event_at <- get_time("event_at")
  can_derive_event_at <- is.na(event_at) & !is.na(report_start_at) & is.finite(timestamp)
  if (any(can_derive_event_at)) {
    event_at[can_derive_event_at] <- report_start_at[can_derive_event_at] +
      timestamp[can_derive_event_at] / 1000
  }

  tibble::tibble(
    logID = get_character("logID"),
    report_title = get_character("report_title"),
    report_link = get_character("report_link"),
    report_start_at = report_start_at,
    fightID = get_integer("fightID"),
    encounterID = get_integer("encounterID"),
    encounterName = get_character("encounterName"),
    fight_start_at = get_time("fight_start_at"),
    timestamp = timestamp,
    event_at = event_at,
    type = type_values,
    sourceID = suppressWarnings(as.integer(source_id)),
    sourceInstanceID = suppressWarnings(as.integer(source_instance_id)),
    targetID = suppressWarnings(as.integer(target_id)),
    targetInstanceID = suppressWarnings(as.integer(target_instance_id)),
    abilityGameID = suppressWarnings(as.integer(ability_id)),
    stoppedAbilityGameID = suppressWarnings(as.integer(stopped_ability_id)),
    is_periodic = periodic,
    .row_order = seq_len(n)
  )
}

.wcl_cast_validate_identity <- function(rows, name, columns) {
  if (!nrow(rows)) {
    return(invisible(NULL))
  }

  invalid <- logical(nrow(rows))
  for (column in columns) {
    values <- rows[[column]]
    if (is.character(values)) {
      invalid <- invalid | is.na(values) | !nzchar(values)
    } else {
      invalid <- invalid | is.na(values)
      if (is.numeric(values)) {
        invalid <- invalid | !is.finite(values)
      }
    }
  }

  if (any(invalid)) {
    stop(
      sprintf(
        "Relevant rows in `%s` must include non-missing %s.",
        name,
        paste(sprintf("`%s`", columns), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

.wcl_cast_first <- function(x, default = NA) {
  if (inherits(x, "POSIXct")) {
    values <- x[!is.na(x)]
    return(if (length(values)) values[[1L]] else default)
  }

  values <- x[!is.na(x)]
  if (is.character(values)) {
    values <- values[nzchar(values)]
  }
  if (length(values)) values[[1L]] else default
}

.wcl_cast_group_key <- function(log_id, fight_id, actor_id, instance_id, ability_id) {
  paste(
    log_id,
    fight_id,
    actor_id,
    ifelse(is.na(instance_id), "NA", instance_id),
    ability_id,
    sep = "\u001f"
  )
}

.wcl_cast_attempts_empty <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_cast_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    fight_start_at = .wcl_cast_empty_time(),
    sourceID = integer(),
    sourceInstanceID = integer(),
    abilityGameID = integer(),
    targetID = integer(),
    targetInstanceID = integer(),
    attempt_id = integer(),
    status = character(),
    begin_timestamp = numeric(),
    cast_timestamp = numeric(),
    interrupt_timestamp = numeric(),
    resolution_timestamp = numeric(),
    begin_at = .wcl_cast_empty_time(),
    cast_at = .wcl_cast_empty_time(),
    interrupt_at = .wcl_cast_empty_time(),
    resolution_at = .wcl_cast_empty_time(),
    elapsed_ms = numeric(),
    cast_time_ms = numeric(),
    begincast_observed = logical(),
    cast_observed = logical(),
    instant_cast = logical(),
    confirmed_interruption = logical(),
    inferred_interruption = logical(),
    interrupterID = integer(),
    interrupterInstanceID = integer(),
    interruptAbilityGameID = integer()
  )
}

.wcl_cast_summary_empty <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    sourceID = integer(),
    sourceInstanceID = integer(),
    abilityGameID = integer(),
    attempt_count = integer(),
    begincast_count = integer(),
    completed_cast_count = integer(),
    completed_hardcast_count = integer(),
    instant_cast_count = integer(),
    confirmed_interruption_count = integer(),
    inferred_interruption_count = integer(),
    total_interruption_count = integer(),
    confirmed_interruption_fraction = numeric(),
    inferred_interruption_fraction = numeric(),
    mean_cast_time_ms = numeric(),
    median_cast_time_ms = numeric(),
    mean_hardcast_time_ms = numeric()
  )
}

.wcl_cast_attempt_row <- function(
    event_rows,
    actor_id,
    actor_instance_id,
    ability_id,
    begin = NULL,
    cast = NULL,
    interrupt = NULL,
    status) {
  anchor <- begin %||% cast %||% interrupt
  target_id <- NA_integer_
  target_instance_id <- NA_integer_
  for (candidate in list(cast, begin)) {
    if (is.null(candidate)) {
      next
    }
    if (is.na(target_id) && !is.na(candidate$targetID[[1L]])) {
      target_id <- candidate$targetID[[1L]]
    }
    if (is.na(target_instance_id) && !is.na(candidate$targetInstanceID[[1L]])) {
      target_instance_id <- candidate$targetInstanceID[[1L]]
    }
  }

  begin_timestamp <- if (is.null(begin)) NA_real_ else begin$timestamp[[1L]]
  cast_timestamp <- if (is.null(cast)) NA_real_ else cast$timestamp[[1L]]
  interrupt_timestamp <- if (is.null(interrupt)) NA_real_ else interrupt$timestamp[[1L]]
  resolution_timestamp <- if (!is.null(cast)) cast_timestamp else interrupt_timestamp
  elapsed_ms <- if (is.finite(begin_timestamp) && is.finite(resolution_timestamp)) {
    resolution_timestamp - begin_timestamp
  } else {
    NA_real_
  }
  cast_time_ms <- if (identical(status, "completed")) {
    cast_timestamp - begin_timestamp
  } else if (identical(status, "completed_instant")) {
    0
  } else {
    NA_real_
  }

  tibble::tibble(
    logID = as.character(anchor$logID[[1L]]),
    report_title = as.character(.wcl_cast_first(event_rows$report_title, NA_character_)),
    report_link = as.character(.wcl_cast_first(event_rows$report_link, NA_character_)),
    report_start_at = .wcl_cast_first(event_rows$report_start_at, .wcl_cast_empty_time(1L)),
    fightID = as.integer(anchor$fightID[[1L]]),
    encounterID = as.integer(.wcl_cast_first(event_rows$encounterID, NA_integer_)),
    encounterName = as.character(.wcl_cast_first(event_rows$encounterName, NA_character_)),
    fight_start_at = .wcl_cast_first(event_rows$fight_start_at, .wcl_cast_empty_time(1L)),
    sourceID = as.integer(actor_id),
    sourceInstanceID = as.integer(actor_instance_id),
    abilityGameID = as.integer(ability_id),
    targetID = as.integer(target_id),
    targetInstanceID = as.integer(target_instance_id),
    attempt_id = NA_integer_,
    status = as.character(status),
    begin_timestamp = as.numeric(begin_timestamp),
    cast_timestamp = as.numeric(cast_timestamp),
    interrupt_timestamp = as.numeric(interrupt_timestamp),
    resolution_timestamp = as.numeric(resolution_timestamp),
    begin_at = if (is.null(begin)) .wcl_cast_empty_time(1L) else begin$event_at[[1L]],
    cast_at = if (is.null(cast)) .wcl_cast_empty_time(1L) else cast$event_at[[1L]],
    interrupt_at = if (is.null(interrupt)) .wcl_cast_empty_time(1L) else interrupt$event_at[[1L]],
    resolution_at = if (!is.null(cast)) {
      cast$event_at[[1L]]
    } else if (!is.null(interrupt)) {
      interrupt$event_at[[1L]]
    } else {
      .wcl_cast_empty_time(1L)
    },
    elapsed_ms = as.numeric(elapsed_ms),
    cast_time_ms = as.numeric(cast_time_ms),
    begincast_observed = !is.null(begin),
    cast_observed = !is.null(cast),
    instant_cast = identical(status, "completed_instant"),
    confirmed_interruption = identical(status, "interrupted_confirmed"),
    inferred_interruption = identical(status, "interrupted_inferred"),
    interrupterID = if (is.null(interrupt)) NA_integer_ else as.integer(interrupt$sourceID[[1L]]),
    interrupterInstanceID = if (is.null(interrupt)) NA_integer_ else as.integer(interrupt$sourceInstanceID[[1L]]),
    interruptAbilityGameID = if (is.null(interrupt)) NA_integer_ else as.integer(interrupt$abilityGameID[[1L]])
  )
}

.wcl_cast_take_indexed <- function(column, indices) {
  valid <- !is.na(indices)

  if (inherits(column, "POSIXct")) {
    out <- .wcl_cast_empty_time(length(indices))
  } else if (is.integer(column)) {
    out <- rep.int(NA_integer_, length(indices))
  } else if (is.numeric(column)) {
    out <- rep.int(NA_real_, length(indices))
  } else if (is.logical(column)) {
    out <- rep.int(NA, length(indices))
  } else {
    out <- rep.int(NA_character_, length(indices))
  }

  if (any(valid)) {
    out[valid] <- column[indices[valid]]
  }
  out
}

.wcl_cast_attempts_from_indices <- function(
    timeline,
    begin_indices,
    cast_indices,
    interrupt_indices,
    metadata,
    metadata_group_indices,
    attempt_ids,
    status) {
  anchor_indices <- begin_indices
  use_cast <- is.na(anchor_indices) & !is.na(cast_indices)
  anchor_indices[use_cast] <- cast_indices[use_cast]
  use_interrupt <- is.na(anchor_indices)
  anchor_indices[use_interrupt] <- interrupt_indices[use_interrupt]

  resolution_indices <- cast_indices
  use_interrupt_resolution <- is.na(resolution_indices)
  resolution_indices[use_interrupt_resolution] <- interrupt_indices[use_interrupt_resolution]

  begin_timestamp <- .wcl_cast_take_indexed(timeline$timestamp, begin_indices)
  cast_timestamp <- .wcl_cast_take_indexed(timeline$timestamp, cast_indices)
  interrupt_timestamp <- .wcl_cast_take_indexed(timeline$timestamp, interrupt_indices)
  resolution_timestamp <- .wcl_cast_take_indexed(timeline$timestamp, resolution_indices)
  elapsed_ms <- resolution_timestamp - begin_timestamp
  cast_time_ms <- rep.int(NA_real_, length(status))
  hardcast <- status == "completed"
  instant <- status == "completed_instant"
  cast_time_ms[hardcast] <- cast_timestamp[hardcast] - begin_timestamp[hardcast]
  cast_time_ms[instant] <- 0

  cast_target <- .wcl_cast_take_indexed(timeline$targetID, cast_indices)
  begin_target <- .wcl_cast_take_indexed(timeline$targetID, begin_indices)
  target_id <- cast_target
  target_id[is.na(target_id)] <- begin_target[is.na(target_id)]

  cast_target_instance <- .wcl_cast_take_indexed(timeline$targetInstanceID, cast_indices)
  begin_target_instance <- .wcl_cast_take_indexed(timeline$targetInstanceID, begin_indices)
  target_instance_id <- cast_target_instance
  target_instance_id[is.na(target_instance_id)] <- begin_target_instance[is.na(target_instance_id)]

  tibble::tibble(
    logID = as.character(timeline$logID[anchor_indices]),
    report_title = as.character(metadata$report_title[metadata_group_indices]),
    report_link = as.character(metadata$report_link[metadata_group_indices]),
    report_start_at = metadata$report_start_at[metadata_group_indices],
    fightID = as.integer(timeline$fightID[anchor_indices]),
    encounterID = as.integer(metadata$encounterID[metadata_group_indices]),
    encounterName = as.character(metadata$encounterName[metadata_group_indices]),
    fight_start_at = metadata$fight_start_at[metadata_group_indices],
    sourceID = as.integer(timeline$.actorID[anchor_indices]),
    sourceInstanceID = as.integer(timeline$.actorInstanceID[anchor_indices]),
    abilityGameID = as.integer(timeline$.spellID[anchor_indices]),
    targetID = as.integer(target_id),
    targetInstanceID = as.integer(target_instance_id),
    attempt_id = as.integer(attempt_ids),
    status = as.character(status),
    begin_timestamp = begin_timestamp,
    cast_timestamp = cast_timestamp,
    interrupt_timestamp = interrupt_timestamp,
    resolution_timestamp = resolution_timestamp,
    begin_at = .wcl_cast_take_indexed(timeline$event_at, begin_indices),
    cast_at = .wcl_cast_take_indexed(timeline$event_at, cast_indices),
    interrupt_at = .wcl_cast_take_indexed(timeline$event_at, interrupt_indices),
    resolution_at = .wcl_cast_take_indexed(timeline$event_at, resolution_indices),
    elapsed_ms = as.numeric(elapsed_ms),
    cast_time_ms = as.numeric(cast_time_ms),
    begincast_observed = !is.na(begin_indices),
    cast_observed = !is.na(cast_indices),
    instant_cast = instant,
    confirmed_interruption = status == "interrupted_confirmed",
    inferred_interruption = status == "interrupted_inferred",
    interrupterID = as.integer(.wcl_cast_take_indexed(timeline$sourceID, interrupt_indices)),
    interrupterInstanceID = as.integer(.wcl_cast_take_indexed(timeline$sourceInstanceID, interrupt_indices)),
    interruptAbilityGameID = as.integer(.wcl_cast_take_indexed(timeline$abilityGameID, interrupt_indices))
  )
}

#' Summarize completed and interrupted cast attempts
#'
#' `wcl_cast_interruptions()` pairs Warcraft Logs `begincast` rows with either
#' their successful `cast` completion or an explicit `interrupt` event. An
#' explicit interrupt is counted as confirmed. A `begincast` that has neither
#' outcome is retained separately as an inferred interruption because a
#' cancellation, death, truncated query, or encounter end can produce the same
#' event pattern. A `cast` without a preceding `begincast` is treated as a
#' completed instant cast.
#'
#' @param events A data frame containing `begincast` and `cast` rows, normally
#'   returned by [wcl_events()] with `data_type = "Casts"`. It may also contain
#'   inline `interrupt` rows.
#' @param interrupt_events Optional separate data frame of explicit `interrupt`
#'   rows, normally returned by [wcl_events()] with
#'   `data_type = "Interrupts"`. When supplied, it replaces inline interrupt
#'   rows in `events`.
#' @param source_ids Optional integer-like vector of caster report actor IDs.
#'   This refers to the source of the cast and therefore the target of an
#'   explicit interrupt event.
#' @param ability_ids Optional integer-like vector of cast ability game IDs.
#'   For interrupt rows this filters the stopped ability, not the interrupt
#'   ability.
#' @param output Output form. `"summary"` returns counts per fight, caster
#'   instance, and ability. `"attempts"` returns one row per paired, instant,
#'   confirmed-interrupted, or inferred-interrupted attempt.
#'
#' @details
#' Inputs may use the flat ID columns returned by `wcl_events()` or nested
#' source, target, ability, and stopped-ability objects. Matching never crosses
#' report, fight, caster instance, or ability boundaries. Warcraft Logs does
#' not provide a useful begin-cast duration for World of Warcraft, so completed
#' cast duration is derived from the paired timestamps.
#'
#' @return A tibble in the requested output form.
#' @export
wcl_cast_interruptions <- function(
    events,
    interrupt_events = NULL,
    source_ids = NULL,
    ability_ids = NULL,
    output = c("summary", "attempts")) {
  output <- match.arg(output)
  source_ids <- .wcl_cast_id_filter(source_ids, "source_ids")
  ability_ids <- .wcl_cast_id_filter(ability_ids, "ability_ids")

  prepared_events <- .wcl_cast_prepare_events(
    events,
    "events",
    types = if (is.null(interrupt_events)) {
      c("begincast", "cast", "interrupt")
    } else {
      c("begincast", "cast")
    }
  )
  cast_rows <- prepared_events[
    prepared_events$type %in% c("begincast", "cast"),
    ,
    drop = FALSE
  ]

  prepared_interrupts <- if (is.null(interrupt_events)) {
    prepared_events
  } else {
    .wcl_cast_prepare_events(
      interrupt_events,
      "interrupt_events",
      types = "interrupt"
    )
  }
  interrupt_rows <- prepared_interrupts[
    prepared_interrupts$type == "interrupt",
    ,
    drop = FALSE
  ]

  .wcl_cast_validate_identity(
    cast_rows,
    "events",
    c("logID", "fightID", "timestamp", "sourceID", "abilityGameID")
  )
  .wcl_cast_validate_identity(
    interrupt_rows,
    if (is.null(interrupt_events)) "events" else "interrupt_events",
    c("logID", "fightID", "timestamp", "targetID", "stoppedAbilityGameID")
  )

  if (nrow(cast_rows)) {
    cast_rows$.actorID <- cast_rows$sourceID
    cast_rows$.actorInstanceID <- cast_rows$sourceInstanceID
    cast_rows$.spellID <- cast_rows$abilityGameID
    cast_rows$.kind <- ifelse(cast_rows$type == "begincast", "begin", "cast")
  }
  if (nrow(interrupt_rows)) {
    interrupt_rows$.actorID <- interrupt_rows$targetID
    interrupt_rows$.actorInstanceID <- interrupt_rows$targetInstanceID
    interrupt_rows$.spellID <- interrupt_rows$stoppedAbilityGameID
    interrupt_rows$.kind <- "interrupt"
  }

  timeline <- dplyr::bind_rows(cast_rows, interrupt_rows)
  if (!nrow(timeline)) {
    return(if (identical(output, "attempts")) {
      .wcl_cast_attempts_empty()
    } else {
      .wcl_cast_summary_empty()
    })
  }

  if (!is.null(source_ids)) {
    timeline <- timeline[timeline$.actorID %in% source_ids, , drop = FALSE]
  }
  if (!is.null(ability_ids)) {
    timeline <- timeline[timeline$.spellID %in% ability_ids, , drop = FALSE]
  }
  if (!nrow(timeline)) {
    return(if (identical(output, "attempts")) {
      .wcl_cast_attempts_empty()
    } else {
      .wcl_cast_summary_empty()
    })
  }

  timeline$.group_key <- .wcl_cast_group_key(
    timeline$logID,
    timeline$fightID,
    timeline$.actorID,
    timeline$.actorInstanceID,
    timeline$.spellID
  )
  priority <- c(begin = 1L, interrupt = 2L, cast = 3L)
  timeline$.priority <- unname(priority[timeline$.kind])

  groups <- split(seq_len(nrow(timeline)), timeline$.group_key)
  group_count <- length(groups)
  max_attempts <- nrow(timeline)
  begin_indices <- rep.int(NA_integer_, max_attempts)
  cast_indices <- rep.int(NA_integer_, max_attempts)
  interrupt_indices <- rep.int(NA_integer_, max_attempts)
  metadata_group_indices <- rep.int(NA_integer_, max_attempts)
  attempt_ids <- rep.int(NA_integer_, max_attempts)
  statuses <- rep.int(NA_character_, max_attempts)
  attempt_count <- 0L

  group_report_title <- rep.int(NA_character_, group_count)
  group_report_link <- rep.int(NA_character_, group_count)
  group_report_start_at <- .wcl_cast_empty_time(group_count)
  group_encounter_id <- rep.int(NA_integer_, group_count)
  group_encounter_name <- rep.int(NA_character_, group_count)
  group_fight_start_at <- .wcl_cast_empty_time(group_count)

  for (group_index in seq_along(groups)) {
    indices <- groups[[group_index]]
    indices <- indices[order(
      timeline$timestamp[indices],
      timeline$.priority[indices],
      timeline$.row_order[indices]
    )]

    group_report_title[[group_index]] <- as.character(
      .wcl_cast_first(timeline$report_title[indices], NA_character_)
    )
    group_report_link[[group_index]] <- as.character(
      .wcl_cast_first(timeline$report_link[indices], NA_character_)
    )
    group_report_start_at[[group_index]] <- .wcl_cast_first(
      timeline$report_start_at[indices],
      .wcl_cast_empty_time(1L)
    )
    group_encounter_id[[group_index]] <- as.integer(
      .wcl_cast_first(timeline$encounterID[indices], NA_integer_)
    )
    group_encounter_name[[group_index]] <- as.character(
      .wcl_cast_first(timeline$encounterName[indices], NA_character_)
    )
    group_fight_start_at[[group_index]] <- .wcl_cast_first(
      timeline$fight_start_at[indices],
      .wcl_cast_empty_time(1L)
    )

    open_begin <- NA_integer_
    group_attempt_id <- 0L
    for (row_index in indices) {
      kind <- timeline$.kind[[row_index]]
      emitted_begin <- NA_integer_
      emitted_cast <- NA_integer_
      emitted_interrupt <- NA_integer_
      emitted_status <- NA_character_

      if (identical(kind, "begin")) {
        if (!is.na(open_begin)) {
          emitted_begin <- open_begin
          emitted_status <- "interrupted_inferred"
        }
        open_begin <- row_index
      } else if (identical(kind, "cast")) {
        emitted_cast <- row_index
        if (is.na(open_begin)) {
          emitted_status <- "completed_instant"
        } else {
          emitted_begin <- open_begin
          emitted_status <- "completed"
          open_begin <- NA_integer_
        }
      } else if (identical(kind, "interrupt")) {
        emitted_interrupt <- row_index
        emitted_status <- "interrupted_confirmed"
        if (!is.na(open_begin)) {
          emitted_begin <- open_begin
          open_begin <- NA_integer_
        }
      }

      if (!is.na(emitted_status)) {
        attempt_count <- attempt_count + 1L
        group_attempt_id <- group_attempt_id + 1L
        begin_indices[[attempt_count]] <- emitted_begin
        cast_indices[[attempt_count]] <- emitted_cast
        interrupt_indices[[attempt_count]] <- emitted_interrupt
        metadata_group_indices[[attempt_count]] <- group_index
        attempt_ids[[attempt_count]] <- group_attempt_id
        statuses[[attempt_count]] <- emitted_status
      }
    }

    if (!is.na(open_begin)) {
      attempt_count <- attempt_count + 1L
      group_attempt_id <- group_attempt_id + 1L
      begin_indices[[attempt_count]] <- open_begin
      metadata_group_indices[[attempt_count]] <- group_index
      attempt_ids[[attempt_count]] <- group_attempt_id
      statuses[[attempt_count]] <- "interrupted_inferred"
    }
  }

  if (!attempt_count) {
    return(if (identical(output, "attempts")) {
      .wcl_cast_attempts_empty()
    } else {
      .wcl_cast_summary_empty()
    })
  }

  used <- seq_len(attempt_count)
  metadata <- tibble::tibble(
    report_title = group_report_title,
    report_link = group_report_link,
    report_start_at = group_report_start_at,
    encounterID = group_encounter_id,
    encounterName = group_encounter_name,
    fight_start_at = group_fight_start_at
  )
  attempts <- .wcl_cast_attempts_from_indices(
    timeline = timeline,
    begin_indices = begin_indices[used],
    cast_indices = cast_indices[used],
    interrupt_indices = interrupt_indices[used],
    metadata = metadata,
    metadata_group_indices = metadata_group_indices[used],
    attempt_ids = attempt_ids[used],
    status = statuses[used]
  )
  if (!nrow(attempts)) {
    return(if (identical(output, "attempts")) {
      .wcl_cast_attempts_empty()
    } else {
      .wcl_cast_summary_empty()
    })
  }

  attempts <- attempts[order(
    attempts$logID,
    attempts$fightID,
    attempts$sourceID,
    attempts$sourceInstanceID,
    attempts$abilityGameID,
    attempts$attempt_id,
    na.last = TRUE
  ), , drop = FALSE]
  attempts <- tibble::as_tibble(attempts)

  if (identical(output, "attempts")) {
    return(attempts)
  }

  summary_key <- .wcl_cast_group_key(
    attempts$logID,
    attempts$fightID,
    attempts$sourceID,
    attempts$sourceInstanceID,
    attempts$abilityGameID
  )
  summary_groups <- split(seq_len(nrow(attempts)), summary_key)
  summary_count <- length(summary_groups)
  first_indices <- integer(summary_count)
  report_titles <- rep.int(NA_character_, summary_count)
  report_links <- rep.int(NA_character_, summary_count)
  encounter_ids <- rep.int(NA_integer_, summary_count)
  encounter_names <- rep.int(NA_character_, summary_count)
  attempt_counts <- integer(summary_count)
  begincast_counts <- integer(summary_count)
  completed_cast_counts <- integer(summary_count)
  completed_hardcast_counts <- integer(summary_count)
  instant_cast_counts <- integer(summary_count)
  confirmed_counts <- integer(summary_count)
  inferred_counts <- integer(summary_count)
  mean_cast_times <- rep.int(NA_real_, summary_count)
  median_cast_times <- rep.int(NA_real_, summary_count)
  mean_hardcast_times <- rep.int(NA_real_, summary_count)

  for (group_index in seq_along(summary_groups)) {
    indices <- summary_groups[[group_index]]
    first_indices[[group_index]] <- indices[[1L]]
    report_titles[[group_index]] <- as.character(
      .wcl_cast_first(attempts$report_title[indices], NA_character_)
    )
    report_links[[group_index]] <- as.character(
      .wcl_cast_first(attempts$report_link[indices], NA_character_)
    )
    encounter_ids[[group_index]] <- as.integer(
      .wcl_cast_first(attempts$encounterID[indices], NA_integer_)
    )
    encounter_names[[group_index]] <- as.character(
      .wcl_cast_first(attempts$encounterName[indices], NA_character_)
    )
    attempt_counts[[group_index]] <- length(indices)
    begincast_counts[[group_index]] <- sum(attempts$begincast_observed[indices])
    completed_cast_counts[[group_index]] <- sum(attempts$cast_observed[indices])
    completed_hardcast_counts[[group_index]] <- sum(attempts$status[indices] == "completed")
    instant_cast_counts[[group_index]] <- sum(attempts$instant_cast[indices])
    confirmed_counts[[group_index]] <- sum(attempts$confirmed_interruption[indices])
    inferred_counts[[group_index]] <- sum(attempts$inferred_interruption[indices])

    cast_times <- attempts$cast_time_ms[indices]
    cast_times <- cast_times[is.finite(cast_times)]
    if (length(cast_times)) {
      mean_cast_times[[group_index]] <- mean(cast_times)
      median_cast_times[[group_index]] <- stats::median(cast_times)
    }
    hardcast_times <- attempts$cast_time_ms[indices]
    hardcast_times <- hardcast_times[
      attempts$status[indices] == "completed" & is.finite(hardcast_times)
    ]
    if (length(hardcast_times)) {
      mean_hardcast_times[[group_index]] <- mean(hardcast_times)
    }
  }

  tibble::tibble(
    logID = attempts$logID[first_indices],
    report_title = report_titles,
    report_link = report_links,
    fightID = attempts$fightID[first_indices],
    encounterID = encounter_ids,
    encounterName = encounter_names,
    sourceID = attempts$sourceID[first_indices],
    sourceInstanceID = attempts$sourceInstanceID[first_indices],
    abilityGameID = attempts$abilityGameID[first_indices],
    attempt_count = as.integer(attempt_counts),
    begincast_count = as.integer(begincast_counts),
    completed_cast_count = as.integer(completed_cast_counts),
    completed_hardcast_count = as.integer(completed_hardcast_counts),
    instant_cast_count = as.integer(instant_cast_counts),
    confirmed_interruption_count = as.integer(confirmed_counts),
    inferred_interruption_count = as.integer(inferred_counts),
    total_interruption_count = as.integer(confirmed_counts + inferred_counts),
    confirmed_interruption_fraction = confirmed_counts / attempt_counts,
    inferred_interruption_fraction = inferred_counts / attempt_counts,
    mean_cast_time_ms = mean_cast_times,
    median_cast_time_ms = median_cast_times,
    mean_hardcast_time_ms = mean_hardcast_times
  )
}

.wcl_travel_pairs_empty <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_cast_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    fight_start_at = .wcl_cast_empty_time(),
    sourceID = integer(),
    sourceInstanceID = integer(),
    targetID = integer(),
    targetInstanceID = integer(),
    castAbilityGameID = integer(),
    landingAbilityGameID = integer(),
    pair_id = integer(),
    cast_timestamp = numeric(),
    landing_timestamp = numeric(),
    cast_at = .wcl_cast_empty_time(),
    landing_at = .wcl_cast_empty_time(),
    landing_type = character(),
    travel_time_ms = numeric(),
    travel_time_s = numeric(),
    match_status = character(),
    ambiguous = logical(),
    candidate_landing_count = integer(),
    candidate_cast_count = integer(),
    unmatched_reason = character()
  )
}

.wcl_travel_summary_empty <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    sourceID = integer(),
    sourceInstanceID = integer(),
    targetID = integer(),
    targetInstanceID = integer(),
    castAbilityGameID = integer(),
    landingAbilityGameID = integer(),
    cast_count = integer(),
    landing_count = integer(),
    matched_count = integer(),
    unmatched_cast_count = integer(),
    unmatched_landing_count = integer(),
    ambiguous_match_count = integer(),
    match_fraction = numeric(),
    mean_travel_time_ms = numeric(),
    median_travel_time_ms = numeric(),
    min_travel_time_ms = numeric(),
    max_travel_time_ms = numeric()
  )
}

.wcl_travel_ability_map <- function(ability_map) {
  if (is.null(ability_map)) {
    return(NULL)
  }

  if (inherits(ability_map, "data.frame")) {
    required <- c("cast_ability_id", "landing_ability_id")
    missing <- setdiff(required, names(ability_map))
    if (length(missing)) {
      stop(
        "`ability_map` data frames must include `cast_ability_id` and `landing_ability_id`.",
        call. = FALSE
      )
    }
    cast_ids <- ability_map$cast_ability_id
    landing_ids <- ability_map$landing_ability_id
  } else if (is.atomic(ability_map) && !is.null(names(ability_map))) {
    cast_ids <- names(ability_map)
    landing_ids <- unname(ability_map)
  } else {
    stop(
      "`ability_map` must be NULL, a named vector, or a data frame with `cast_ability_id` and `landing_ability_id`.",
      call. = FALSE
    )
  }

  cast_ids <- suppressWarnings(as.numeric(cast_ids))
  valid_cast <- length(cast_ids) > 0L &&
    all(is.finite(cast_ids)) &&
    all(cast_ids == floor(cast_ids))
  if (!valid_cast) {
    stop("`ability_map` cast ability IDs must be non-missing integer-like values.", call. = FALSE)
  }
  cast_ids <- as.integer(cast_ids)
  landing_ids <- suppressWarnings(as.numeric(landing_ids))
  valid_landing <- length(landing_ids) == length(cast_ids) &&
    length(landing_ids) > 0L &&
    all(is.finite(landing_ids)) &&
    all(landing_ids == floor(landing_ids))
  if (!valid_landing) {
    stop("`ability_map` landing ability IDs must be non-missing integer-like values.", call. = FALSE)
  }
  landing_ids <- as.integer(landing_ids)

  if (anyDuplicated(cast_ids) || anyDuplicated(landing_ids)) {
    stop("`ability_map` must be one-to-one with unique cast and landing ability IDs.", call. = FALSE)
  }

  tibble::tibble(
    cast_ability_id = as.integer(cast_ids),
    landing_ability_id = landing_ids
  )
}

.wcl_travel_group_key <- function(
    log_id,
    fight_id,
    source_id,
    source_instance_id,
    target_id,
    target_instance_id,
    cast_ability_id,
    landing_ability_id) {
  paste(
    log_id,
    fight_id,
    source_id,
    ifelse(is.na(source_instance_id), "NA", source_instance_id),
    target_id,
    ifelse(is.na(target_instance_id), "NA", target_instance_id),
    cast_ability_id,
    landing_ability_id,
    sep = "\u001f"
  )
}

#' Measure direct spell travel time from cast to landing
#'
#' `wcl_spell_travel_time()` pairs successful `cast` rows with direct landing
#' events using a deterministic, one-to-one FIFO match. Pairing is constrained
#' to the same report, fight, source instance, target instance, and mapped
#' ability, and every landing must occur within `max_travel_ms` after its cast.
#'
#' @param cast_events A data frame containing successful `cast` rows, normally
#'   returned by [wcl_events()] with `data_type = "Casts"`.
#' @param landing_events A data frame containing events that represent the
#'   spell landing on its target, such as direct damage, healing, misses, or an
#'   aura application.
#' @param max_travel_ms Required finite positive maximum cast-to-landing window
#'   in milliseconds.
#' @param source_ids Optional integer-like vector of caster report actor IDs.
#' @param target_ids Optional integer-like vector of target report actor IDs.
#' @param ability_map Optional one-to-one cast-to-landing ability mapping. Use a
#'   data frame with integer-like `cast_ability_id` and `landing_ability_id`
#'   columns, or a named vector whose names are cast IDs and values are landing
#'   IDs. When supplied, only mapped abilities are analyzed. With `NULL`, cast
#'   and landing ability IDs must be equal.
#' @param landing_types Character vector of accepted landing event types.
#'   Periodic tick rows are excluded even when their type is accepted.
#' @param output Output form. `"summary"` returns match and timing aggregates;
#'   `"pairs"` returns one diagnostic row per cast.
#'
#' @details
#' Warcraft Logs does not expose a universal projectile correlation ID. FIFO
#' matching is therefore deterministic but can still be ambiguous when more
#' than one cast or landing is eligible inside the requested window. Such
#' matches are retained with `match_status = "ambiguous"`,
#' `ambiguous = TRUE`, and candidate counts. They still count as successful
#' pairs in summary match and timing metrics. This helper models one direct
#' landing per cast and is not intended to assign AoE, chain, or multi-hit
#' fan-out events to one projectile.
#'
#' @return A tibble in the requested output form.
#' @export
wcl_spell_travel_time <- function(
    cast_events,
    landing_events,
    max_travel_ms,
    source_ids = NULL,
    target_ids = NULL,
    ability_map = NULL,
    landing_types = c("damage", "heal", "miss", "applybuff", "applydebuff"),
    output = c("summary", "pairs")) {
  output <- match.arg(output)
  if (!is.numeric(max_travel_ms) || length(max_travel_ms) != 1L ||
      is.na(max_travel_ms) || !is.finite(max_travel_ms) || max_travel_ms <= 0) {
    stop("`max_travel_ms` must be one finite positive number.", call. = FALSE)
  }
  max_travel_ms <- as.numeric(max_travel_ms)
  source_ids <- .wcl_cast_id_filter(source_ids, "source_ids")
  target_ids <- .wcl_cast_id_filter(target_ids, "target_ids")
  mapping <- .wcl_travel_ability_map(ability_map)

  if (!is.character(landing_types) || !length(landing_types) ||
      anyNA(landing_types) || any(!nzchar(trimws(landing_types)))) {
    stop("`landing_types` must be a non-empty character vector without missing values.", call. = FALSE)
  }
  landing_types <- unique(tolower(trimws(landing_types)))

  casts <- .wcl_cast_prepare_events(cast_events, "cast_events", types = "cast")
  casts <- casts[casts$type == "cast", , drop = FALSE]
  landings <- .wcl_cast_prepare_events(
    landing_events,
    "landing_events",
    types = landing_types
  )
  landings <- landings[
    landings$type %in% landing_types & !(!is.na(landings$is_periodic) & landings$is_periodic),
    ,
    drop = FALSE
  ]

  .wcl_cast_validate_identity(
    casts,
    "cast_events",
    c("logID", "fightID", "timestamp", "sourceID", "targetID", "abilityGameID")
  )
  .wcl_cast_validate_identity(
    landings,
    "landing_events",
    c("logID", "fightID", "timestamp", "sourceID", "targetID", "abilityGameID")
  )

  if (!is.null(source_ids)) {
    casts <- casts[casts$sourceID %in% source_ids, , drop = FALSE]
    landings <- landings[landings$sourceID %in% source_ids, , drop = FALSE]
  }
  if (!is.null(target_ids)) {
    casts <- casts[casts$targetID %in% target_ids, , drop = FALSE]
    landings <- landings[landings$targetID %in% target_ids, , drop = FALSE]
  }

  if (!is.null(mapping)) {
    cast_lookup <- stats::setNames(mapping$landing_ability_id, mapping$cast_ability_id)
    landing_lookup <- stats::setNames(mapping$cast_ability_id, mapping$landing_ability_id)
    casts <- casts[casts$abilityGameID %in% mapping$cast_ability_id, , drop = FALSE]
    landings <- landings[landings$abilityGameID %in% mapping$landing_ability_id, , drop = FALSE]
    casts$.castAbilityGameID <- casts$abilityGameID
    casts$.landingAbilityGameID <- as.integer(cast_lookup[as.character(casts$abilityGameID)])
    landings$.castAbilityGameID <- as.integer(landing_lookup[as.character(landings$abilityGameID)])
    landings$.landingAbilityGameID <- landings$abilityGameID
  } else {
    casts$.castAbilityGameID <- casts$abilityGameID
    casts$.landingAbilityGameID <- casts$abilityGameID
    landings$.castAbilityGameID <- landings$abilityGameID
    landings$.landingAbilityGameID <- landings$abilityGameID
  }

  if (!nrow(casts)) {
    return(if (identical(output, "pairs")) {
      .wcl_travel_pairs_empty()
    } else {
      .wcl_travel_summary_empty()
    })
  }

  casts$.group_key <- .wcl_travel_group_key(
    casts$logID,
    casts$fightID,
    casts$sourceID,
    casts$sourceInstanceID,
    casts$targetID,
    casts$targetInstanceID,
    casts$.castAbilityGameID,
    casts$.landingAbilityGameID
  )
  if (nrow(landings)) {
    landings$.group_key <- .wcl_travel_group_key(
      landings$logID,
      landings$fightID,
      landings$sourceID,
      landings$sourceInstanceID,
      landings$targetID,
      landings$targetInstanceID,
      landings$.castAbilityGameID,
      landings$.landingAbilityGameID
    )
  } else {
    landings$.group_key <- character()
  }

  cast_groups <- split(seq_len(nrow(casts)), casts$.group_key)
  landing_groups <- split(seq_len(nrow(landings)), landings$.group_key)
  pair_rows <- list()
  landing_stats <- list()

  for (group_key in names(cast_groups)) {
    cast_group <- casts[cast_groups[[group_key]], , drop = FALSE]
    cast_group <- cast_group[order(cast_group$timestamp, cast_group$.row_order), , drop = FALSE]
    landing_indices <- landing_groups[[group_key]]
    landing_group <- if (is.null(landing_indices)) {
      landings[0, , drop = FALSE]
    } else {
      landings[landing_indices, , drop = FALSE]
    }
    landing_group <- landing_group[
      order(landing_group$timestamp, landing_group$.row_order),
      ,
      drop = FALSE
    ]

    n_casts <- nrow(cast_group)
    n_landings <- nrow(landing_group)
    candidates <- if (n_landings) {
      outer(
        cast_group$timestamp,
        landing_group$timestamp,
        function(cast_time, landing_time) {
          landing_time >= cast_time & landing_time - cast_time <= max_travel_ms
        }
      )
    } else {
      matrix(FALSE, nrow = n_casts, ncol = 0L)
    }
    if (n_casts == 1L && n_landings == 1L) {
      candidates <- matrix(candidates, nrow = 1L, ncol = 1L)
    } else if (n_casts == 1L) {
      candidates <- matrix(candidates, nrow = 1L)
    } else if (n_landings == 1L) {
      candidates <- matrix(candidates, ncol = 1L)
    }

    used_landings <- rep.int(FALSE, n_landings)
    group_pairs <- list()

    for (cast_index in seq_len(n_casts)) {
      all_candidates <- which(candidates[cast_index, ])
      available <- all_candidates[!used_landings[all_candidates]]
      matched <- length(available) > 0L
      landing_index <- if (matched) available[[1L]] else NA_integer_
      cast_row <- cast_group[cast_index, , drop = FALSE]
      landing_row <- if (matched) landing_group[landing_index, , drop = FALSE] else NULL

      if (matched) {
        used_landings[[landing_index]] <- TRUE
      }

      candidate_landing_count <- length(all_candidates)
      candidate_cast_count <- if (matched) sum(candidates[, landing_index]) else 0L
      ambiguous <- matched && (candidate_landing_count > 1L || candidate_cast_count > 1L)
      travel_time_ms <- if (matched) {
        landing_row$timestamp[[1L]] - cast_row$timestamp[[1L]]
      } else {
        NA_real_
      }
      unmatched_reason <- if (matched) {
        NA_character_
      } else if (candidate_landing_count > 0L) {
        "candidate_already_matched"
      } else {
        "no_landing_in_window"
      }

      group_pairs[[length(group_pairs) + 1L]] <- tibble::tibble(
        logID = cast_row$logID[[1L]],
        report_title = cast_row$report_title[[1L]],
        report_link = cast_row$report_link[[1L]],
        report_start_at = cast_row$report_start_at[[1L]],
        fightID = cast_row$fightID[[1L]],
        encounterID = cast_row$encounterID[[1L]],
        encounterName = cast_row$encounterName[[1L]],
        fight_start_at = cast_row$fight_start_at[[1L]],
        sourceID = cast_row$sourceID[[1L]],
        sourceInstanceID = cast_row$sourceInstanceID[[1L]],
        targetID = cast_row$targetID[[1L]],
        targetInstanceID = cast_row$targetInstanceID[[1L]],
        castAbilityGameID = cast_row$.castAbilityGameID[[1L]],
        landingAbilityGameID = cast_row$.landingAbilityGameID[[1L]],
        pair_id = as.integer(cast_index),
        cast_timestamp = cast_row$timestamp[[1L]],
        landing_timestamp = if (matched) landing_row$timestamp[[1L]] else NA_real_,
        cast_at = cast_row$event_at[[1L]],
        landing_at = if (matched) landing_row$event_at[[1L]] else .wcl_cast_empty_time(1L),
        landing_type = if (matched) landing_row$type[[1L]] else NA_character_,
        travel_time_ms = as.numeric(travel_time_ms),
        travel_time_s = as.numeric(travel_time_ms) / 1000,
        match_status = if (!matched) {
          "unmatched"
        } else if (ambiguous) {
          "ambiguous"
        } else {
          "matched"
        },
        ambiguous = ambiguous,
        candidate_landing_count = as.integer(candidate_landing_count),
        candidate_cast_count = as.integer(candidate_cast_count),
        unmatched_reason = unmatched_reason,
        .group_key = group_key
      )
    }

    pair_rows[[length(pair_rows) + 1L]] <- dplyr::bind_rows(group_pairs)
    landing_stats[[length(landing_stats) + 1L]] <- tibble::tibble(
      .group_key = group_key,
      landing_count = as.integer(n_landings),
      unmatched_landing_count = as.integer(sum(!used_landings))
    )
  }

  pairs <- dplyr::bind_rows(pair_rows)
  stats_rows <- dplyr::bind_rows(landing_stats)
  if (identical(output, "pairs")) {
    pairs$.group_key <- NULL
    return(tibble::as_tibble(pairs))
  }

  summaries <- lapply(split(seq_len(nrow(pairs)), pairs$.group_key), function(indices) {
    rows <- pairs[indices, , drop = FALSE]
    key <- rows$.group_key[[1L]]
    current_stats <- stats_rows[stats_rows$.group_key == key, , drop = FALSE]
    successful <- rows$match_status %in% c("matched", "ambiguous")
    matched_times <- rows$travel_time_ms[successful & is.finite(rows$travel_time_ms)]
    cast_count <- nrow(rows)
    matched_count <- sum(successful)

    tibble::tibble(
      logID = rows$logID[[1L]],
      report_title = .wcl_cast_first(rows$report_title, NA_character_),
      report_link = .wcl_cast_first(rows$report_link, NA_character_),
      fightID = rows$fightID[[1L]],
      encounterID = as.integer(.wcl_cast_first(rows$encounterID, NA_integer_)),
      encounterName = .wcl_cast_first(rows$encounterName, NA_character_),
      sourceID = rows$sourceID[[1L]],
      sourceInstanceID = rows$sourceInstanceID[[1L]],
      targetID = rows$targetID[[1L]],
      targetInstanceID = rows$targetInstanceID[[1L]],
      castAbilityGameID = rows$castAbilityGameID[[1L]],
      landingAbilityGameID = rows$landingAbilityGameID[[1L]],
      cast_count = as.integer(cast_count),
      landing_count = current_stats$landing_count[[1L]],
      matched_count = as.integer(matched_count),
      unmatched_cast_count = as.integer(cast_count - matched_count),
      unmatched_landing_count = current_stats$unmatched_landing_count[[1L]],
      ambiguous_match_count = as.integer(sum(rows$ambiguous)),
      match_fraction = matched_count / cast_count,
      mean_travel_time_ms = if (length(matched_times)) mean(matched_times) else NA_real_,
      median_travel_time_ms = if (length(matched_times)) stats::median(matched_times) else NA_real_,
      min_travel_time_ms = if (length(matched_times)) min(matched_times) else NA_real_,
      max_travel_time_ms = if (length(matched_times)) max(matched_times) else NA_real_
    )
  })

  tibble::as_tibble(dplyr::bind_rows(summaries))
}
