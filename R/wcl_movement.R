.wcl_movement_empty_time <- function(n = 0L) {
  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
}

.wcl_movement_time <- function(x, n = length(x)) {
  if (is.null(x)) {
    return(.wcl_movement_empty_time(n))
  }

  if (inherits(x, "POSIXct")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    return(as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
  }

  values <- as.character(x)
  output <- .wcl_movement_empty_time(length(values))
  remaining <- !is.na(values) & nzchar(values)

  parse_with <- function(format, candidates = values) {
    indices <- which(remaining)
    if (!length(indices)) {
      return(invisible(NULL))
    }

    parsed <- suppressWarnings(as.POSIXct(
      candidates[indices],
      format = format,
      tz = "UTC"
    ))
    matched <- !is.na(parsed)
    if (any(matched)) {
      output[indices[matched]] <<- parsed[matched]
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

  output
}

.wcl_movement_character <- function(data, name, n = nrow(data)) {
  if (name %in% names(data)) as.character(data[[name]]) else rep.int(NA_character_, n)
}

.wcl_movement_numeric <- function(data, name, n = nrow(data)) {
  if (!name %in% names(data)) {
    return(rep.int(NA_real_, n))
  }

  value <- data[[name]]
  if (is.factor(value)) {
    value <- as.character(value)
  }
  suppressWarnings(as.numeric(value))
}

.wcl_movement_integer <- function(data, name, n = nrow(data)) {
  value <- .wcl_movement_numeric(data, name, n)
  suppressWarnings(as.integer(value))
}

.wcl_movement_logical <- function(data, name, n = nrow(data)) {
  if (name %in% names(data)) as.logical(data[[name]]) else rep.int(NA, n)
}

.wcl_movement_time_column <- function(data, name, n = nrow(data)) {
  if (name %in% names(data)) {
    .wcl_movement_time(data[[name]])
  } else {
    .wcl_movement_empty_time(n)
  }
}

.wcl_movement_integer_alias <- function(data, aliases, n = nrow(data)) {
  present <- aliases[aliases %in% names(data)]
  if (!length(present)) {
    return(rep.int(NA_integer_, n))
  }
  .wcl_movement_integer(data, present[[1L]], n)
}

.wcl_movement_id_filter <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  values <- suppressWarnings(as.numeric(x))
  valid <- length(values) > 0L &&
    length(values) == length(x) &&
    all(is.finite(values)) &&
    all(values == floor(values)) &&
    all(values >= 0) &&
    all(values <= .Machine$integer.max)

  if (!valid) {
    stop(
      "`actor_ids` must be a non-empty vector of non-negative integer-like IDs without missing values.",
      call. = FALSE
    )
  }

  unique(as.integer(values))
}

.wcl_movement_key_value <- function(x) {
  ifelse(is.na(x), "<NA>", as.character(x))
}

.wcl_movement_number_key <- function(x) {
  ifelse(
    is.na(x),
    "<NA>",
    format(as.numeric(x), digits = 17, scientific = FALSE, trim = TRUE)
  )
}

.wcl_movement_group_key <- function(data) {
  paste(
    .wcl_movement_key_value(data$logID),
    .wcl_movement_key_value(data$fightID),
    .wcl_movement_key_value(data$actorID),
    .wcl_movement_key_value(data$actorInstanceID),
    .wcl_movement_key_value(data$mapID),
    sep = "\u001e"
  )
}

.wcl_movement_role_label <- function(x) {
  roles <- unique(stats::na.omit(as.character(x)))
  if (!length(roles)) {
    "unknown"
  } else if (length(roles) == 1L) {
    roles[[1L]]
  } else {
    "mixed"
  }
}

.wcl_movement_empty_track <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_movement_empty_time(),
    report_end_at = .wcl_movement_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    fight_start_at = .wcl_movement_empty_time(),
    fight_end_at = .wcl_movement_empty_time(),
    fight_duration_s = numeric(),
    actorID = integer(),
    actorInstanceID = integer(),
    actor_role = character(),
    mapID = integer(),
    sample_id = integer(),
    timestamp = numeric(),
    event_at = .wcl_movement_empty_time(),
    sourceID = integer(),
    targetID = integer(),
    resourceActor = integer(),
    type = character(),
    abilityGameID = integer(),
    x_raw = numeric(),
    y_raw = numeric(),
    x = numeric(),
    y = numeric(),
    previous_timestamp = numeric(),
    sample_gap_ms = numeric(),
    sample_gap_s = numeric(),
    step_distance = numeric(),
    cumulative_distance = numeric(),
    step_speed = numeric()
  )
}

.wcl_movement_empty_summary <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_movement_empty_time(),
    report_end_at = .wcl_movement_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    fight_start_at = .wcl_movement_empty_time(),
    fight_end_at = .wcl_movement_empty_time(),
    fight_duration_s = numeric(),
    actorID = integer(),
    actorInstanceID = integer(),
    actor_role = character(),
    mapID = integer(),
    sample_count = integer(),
    segment_count = integer(),
    start_timestamp = numeric(),
    end_timestamp = numeric(),
    start_at = .wcl_movement_empty_time(),
    end_at = .wcl_movement_empty_time(),
    observed_span_s = numeric(),
    total_observed_distance = numeric(),
    displacement = numeric(),
    mean_step_distance = numeric(),
    max_step_distance = numeric(),
    mean_observed_speed = numeric(),
    median_sample_gap_s = numeric(),
    max_sample_gap_s = numeric()
  )
}

.wcl_movement_track_summary <- function(track) {
  if (!nrow(track)) {
    return(.wcl_movement_empty_summary())
  }

  group_key <- .wcl_movement_group_key(track)
  rows <- vector("list", length(unique(group_key)))
  row_index <- 0L

  for (key in unique(group_key)) {
    group <- track[group_key == key, , drop = FALSE]
    row_index <- row_index + 1L
    segments <- group$step_distance[is.finite(group$step_distance)]
    gaps <- group$sample_gap_s[is.finite(group$sample_gap_s)]
    span <- (group$timestamp[[nrow(group)]] - group$timestamp[[1L]]) / 1000
    total <- if (length(segments)) sum(segments) else 0
    displacement <- if (nrow(group) > 1L) {
      sqrt(
        (group$x[[nrow(group)]] - group$x[[1L]])^2 +
          (group$y[[nrow(group)]] - group$y[[1L]])^2
      )
    } else {
      0
    }

    rows[[row_index]] <- tibble::tibble(
      logID = group$logID[[1L]],
      report_title = group$report_title[[1L]],
      report_link = group$report_link[[1L]],
      report_start_at = group$report_start_at[[1L]],
      report_end_at = group$report_end_at[[1L]],
      fightID = group$fightID[[1L]],
      encounterID = group$encounterID[[1L]],
      encounterName = group$encounterName[[1L]],
      difficulty = group$difficulty[[1L]],
      size = group$size[[1L]],
      kill = group$kill[[1L]],
      fight_start_at = group$fight_start_at[[1L]],
      fight_end_at = group$fight_end_at[[1L]],
      fight_duration_s = group$fight_duration_s[[1L]],
      actorID = group$actorID[[1L]],
      actorInstanceID = group$actorInstanceID[[1L]],
      actor_role = .wcl_movement_role_label(group$actor_role),
      mapID = group$mapID[[1L]],
      sample_count = as.integer(nrow(group)),
      segment_count = as.integer(max(0L, nrow(group) - 1L)),
      start_timestamp = group$timestamp[[1L]],
      end_timestamp = group$timestamp[[nrow(group)]],
      start_at = group$event_at[[1L]],
      end_at = group$event_at[[nrow(group)]],
      observed_span_s = span,
      total_observed_distance = total,
      displacement = displacement,
      mean_step_distance = if (length(segments)) mean(segments) else NA_real_,
      max_step_distance = if (length(segments)) max(segments) else NA_real_,
      mean_observed_speed = if (span > 0) total / span else NA_real_,
      median_sample_gap_s = if (length(gaps)) stats::median(gaps) else NA_real_,
      max_sample_gap_s = if (length(gaps)) max(gaps) else NA_real_
    )
  }

  dplyr::bind_rows(rows)
}

#' Track sampled actor movement from Warcraft Logs events
#'
#' `wcl_actor_movement()` identifies the actor whose resource data owns each
#' coordinate sample, orders the samples within each fight and map, and
#' calculates straight-line movement between consecutive observations.
#'
#' @param events A data frame returned by [wcl_events()] with resource fields,
#'   including `x` and `y`. Use `include_resources = TRUE` when retrieving it.
#' @param actor_ids Optional integer-like vector of report actor IDs to keep.
#'   Actor IDs are report-scoped.
#' @param actor_role How coordinate ownership is determined. `"auto"` (the
#'   default) uses `resourceActor`, where `1` denotes the event source and `2`
#'   denotes the target. `"source"` or `"target"` restricts the result to that
#'   role and also acts as an explicit fallback for rows where
#'   `resourceActor` is absent or missing. A known opposing resource role is
#'   never reassigned by the fallback.
#' @param output Output form. `"summary"` (the default) returns one row per
#'   report, fight, actor instance, and map. `"track"` returns every retained
#'   position sample with step and cumulative movement columns.
#'
#' @details
#' Warcraft Logs attaches resources for only one actor to an event. Cast and
#' swing events commonly carry source resources, while damage and healing
#' events commonly carry target resources. Consequently, an event's `x` and
#' `y` must not be attributed merely by matching its source or target ID. See
#' the [WCL resource-field documentation](https://www.warcraftlogs.com/help/pins)
#' and [positional-data clarification](https://forums.combatlogforums.com/t/warcraftlogs-api-positional-data/15492).
#'
#' WCL stores positions multiplied by 100. Track output retains those values in
#' `x_raw` and `y_raw` and exposes divided coordinates in `x` and `y`; all
#' distance and speed columns use the divided game-coordinate units (yards).
#' Movement is two-dimensional. It is the sum of straight-line distances
#' between observed samples and is therefore sampled, not a reconstruction of
#' the actor's exact path.
#'
#' Exact duplicate positions at one actor timestamp are collapsed. If one
#' actor has conflicting positions at the same timestamp, the first input row
#' is retained and a warning is emitted. Movement resets across reports,
#' fights, actor instances, and maps.
#'
#' @return A tibble. Track output includes `actorID`, `actorInstanceID`, the
#'   owning `actor_role`, normalized `x`/`y`, `sample_gap_ms`,
#'   `step_distance`, `cumulative_distance`, and `step_speed`. Summary output
#'   includes sample and segment counts, observed time span, total observed
#'   distance, endpoint displacement, step statistics, and sampling-gap
#'   diagnostics.
#' @export
wcl_actor_movement <- function(
    events,
    actor_ids = NULL,
    actor_role = c("auto", "source", "target"),
    output = c("summary", "track")) {
  actor_role <- match.arg(actor_role)
  output <- match.arg(output)
  actor_ids <- .wcl_movement_id_filter(actor_ids)

  if (!inherits(events, "data.frame")) {
    stop("`events` must be a data frame.", call. = FALSE)
  }

  empty <- if (output == "summary") {
    .wcl_movement_empty_summary()
  } else {
    .wcl_movement_empty_track()
  }
  if (!nrow(events)) {
    return(empty)
  }

  required <- c("logID", "fightID", "timestamp", "x", "y")
  role_id <- if (actor_role == "target") "targetID" else "sourceID"
  if (actor_role == "auto") {
    required <- c(required, "resourceActor", "sourceID", "targetID")
  } else {
    required <- c(required, role_id)
  }
  missing_columns <- setdiff(required, names(events))
  if (length(missing_columns)) {
    stop(
      sprintf(
        "`events` is missing required movement column(s): %s.",
        paste(sprintf("`%s`", missing_columns), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  events <- tibble::as_tibble(events)
  n <- nrow(events)
  x_raw <- .wcl_movement_numeric(events, "x")
  y_raw <- .wcl_movement_numeric(events, "y")
  timestamp <- .wcl_movement_numeric(events, "timestamp")
  complete_position <- is.finite(x_raw) & is.finite(y_raw) & is.finite(timestamp)
  if (!any(complete_position)) {
    return(empty)
  }

  resource_actor_present <- "resourceActor" %in% names(events)
  resource_actor <- if (resource_actor_present) {
    .wcl_movement_integer(events, "resourceActor")
  } else {
    rep.int(NA_integer_, n)
  }
  row_role <- rep.int(NA_character_, n)

  if (actor_role == "auto") {
    row_role[which(resource_actor == 1L)] <- "source"
    row_role[which(resource_actor == 2L)] <- "target"
    unknown <- complete_position & is.na(row_role)
    if (any(unknown)) {
      warning(
        sprintf(
          "Ignored %d coordinate row(s) with missing or unsupported `resourceActor` ownership.",
          sum(unknown)
        ),
        call. = FALSE
      )
    }
  } else {
    wanted_code <- if (actor_role == "source") 1L else 2L
    fallback <- !resource_actor_present | is.na(resource_actor)
    row_role[resource_actor == wanted_code | fallback] <- actor_role
    unsupported <- complete_position & !is.na(resource_actor) &
      !resource_actor %in% c(1L, 2L)
    if (any(unsupported)) {
      warning(
        sprintf(
          "Ignored %d coordinate row(s) with unsupported `resourceActor` ownership.",
          sum(unsupported)
        ),
        call. = FALSE
      )
    }
  }

  source_id <- .wcl_movement_integer(events, "sourceID")
  target_id <- .wcl_movement_integer(events, "targetID")
  source_instance <- .wcl_movement_integer_alias(
    events,
    c("sourceInstanceID", "sourceInstanceId", "sourceInstance")
  )
  target_instance <- .wcl_movement_integer_alias(
    events,
    c("targetInstanceID", "targetInstanceId", "targetInstance")
  )
  actor_id <- ifelse(row_role == "source", source_id, target_id)
  actor_instance <- ifelse(row_role == "source", source_instance, target_instance)
  actor_id <- suppressWarnings(as.integer(actor_id))
  actor_instance <- suppressWarnings(as.integer(actor_instance))

  keep <- complete_position & !is.na(row_role) & !is.na(actor_id)
  if (!is.null(actor_ids)) {
    keep <- keep & actor_id %in% actor_ids
  }
  if (!any(keep)) {
    return(empty)
  }

  if (any(is.na(events$logID[keep]) | !nzchar(as.character(events$logID[keep]))) ||
      any(is.na(events$fightID[keep]))) {
    stop(
      "Movement rows must include non-missing `logID` and `fightID` values.",
      call. = FALSE
    )
  }

  report_start_at <- .wcl_movement_time_column(events, "report_start_at")
  event_at <- .wcl_movement_time_column(events, "event_at")
  derive_event_time <- is.na(event_at) & !is.na(report_start_at)
  event_at[derive_event_time] <- report_start_at[derive_event_time] +
    timestamp[derive_event_time] / 1000

  track <- tibble::tibble(
    logID = .wcl_movement_character(events, "logID"),
    report_title = .wcl_movement_character(events, "report_title"),
    report_link = .wcl_movement_character(events, "report_link"),
    report_start_at = report_start_at,
    report_end_at = .wcl_movement_time_column(events, "report_end_at"),
    fightID = .wcl_movement_integer(events, "fightID"),
    encounterID = .wcl_movement_integer(events, "encounterID"),
    encounterName = .wcl_movement_character(events, "encounterName"),
    difficulty = .wcl_movement_integer(events, "difficulty"),
    size = .wcl_movement_integer(events, "size"),
    kill = .wcl_movement_logical(events, "kill"),
    fight_start_at = .wcl_movement_time_column(events, "fight_start_at"),
    fight_end_at = .wcl_movement_time_column(events, "fight_end_at"),
    fight_duration_s = if ("fight_duration_s" %in% names(events)) {
      .wcl_movement_numeric(events, "fight_duration_s")
    } else {
      .wcl_movement_numeric(events, "duration_s")
    },
    actorID = actor_id,
    actorInstanceID = actor_instance,
    actor_role = row_role,
    mapID = .wcl_movement_integer(events, "mapID"),
    sample_id = rep.int(NA_integer_, n),
    timestamp = timestamp,
    event_at = event_at,
    sourceID = source_id,
    targetID = target_id,
    resourceActor = resource_actor,
    type = .wcl_movement_character(events, "type"),
    abilityGameID = .wcl_movement_integer(events, "abilityGameID"),
    x_raw = x_raw,
    y_raw = y_raw,
    x = x_raw / 100,
    y = y_raw / 100,
    previous_timestamp = rep.int(NA_real_, n),
    sample_gap_ms = rep.int(NA_real_, n),
    sample_gap_s = rep.int(NA_real_, n),
    step_distance = rep.int(NA_real_, n),
    cumulative_distance = rep.int(NA_real_, n),
    step_speed = rep.int(NA_real_, n),
    .input_row = seq_len(n)
  )
  track <- track[keep, , drop = FALSE]

  group_key <- .wcl_movement_group_key(track)
  order_index <- order(group_key, track$timestamp, track$.input_row)
  track <- track[order_index, , drop = FALSE]
  group_key <- group_key[order_index]

  timestamp_key <- paste(
    group_key,
    .wcl_movement_number_key(track$timestamp),
    sep = "\u001e"
  )
  coordinate_key <- paste(
    .wcl_movement_number_key(track$x_raw),
    .wcl_movement_number_key(track$y_raw),
    sep = "\u001f"
  )
  conflicting <- vapply(
    split(coordinate_key, timestamp_key),
    function(value) length(unique(value)) > 1L,
    logical(1)
  )
  if (any(conflicting)) {
    warning(
      sprintf(
        "Resolved %d conflicting actor position timestamp(s) by keeping the first input row.",
        sum(conflicting)
      ),
      call. = FALSE
    )
  }

  keep_timestamp <- !duplicated(timestamp_key)
  track <- track[keep_timestamp, , drop = FALSE]
  group_key <- group_key[keep_timestamp]

  for (key in unique(group_key)) {
    index <- which(group_key == key)
    count <- length(index)
    gap_ms <- c(NA_real_, diff(track$timestamp[index]))
    step <- c(
      NA_real_,
      sqrt(diff(track$x[index])^2 + diff(track$y[index])^2)
    )
    cumulative <- c(0, if (count > 1L) cumsum(step[-1L]) else numeric())

    track$sample_id[index] <- seq_len(count)
    track$previous_timestamp[index] <- c(
      NA_real_,
      utils::head(track$timestamp[index], -1L)
    )
    track$sample_gap_ms[index] <- gap_ms
    track$sample_gap_s[index] <- gap_ms / 1000
    track$step_distance[index] <- step
    track$cumulative_distance[index] <- cumulative
    track$step_speed[index] <- ifelse(
      is.finite(gap_ms) & gap_ms > 0,
      step / (gap_ms / 1000),
      NA_real_
    )
  }

  track$.input_row <- NULL
  if (output == "track") track else .wcl_movement_track_summary(track)
}

.wcl_distance_empty_series <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_movement_empty_time(),
    report_end_at = .wcl_movement_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    fight_start_at = .wcl_movement_empty_time(),
    fight_end_at = .wcl_movement_empty_time(),
    fight_duration_s = numeric(),
    mapID = integer(),
    actor_a_id = integer(),
    actor_a_instance_id = integer(),
    actor_a_role = character(),
    actor_b_id = integer(),
    actor_b_instance_id = integer(),
    actor_b_role = character(),
    timestamp = numeric(),
    event_at = .wcl_movement_empty_time(),
    actor_a_sample_timestamp = numeric(),
    actor_b_sample_timestamp = numeric(),
    actor_a_age_ms = numeric(),
    actor_b_age_ms = numeric(),
    actor_a_x = numeric(),
    actor_a_y = numeric(),
    actor_b_x = numeric(),
    actor_b_y = numeric(),
    max_hold_ms = numeric(),
    within_hold = logical(),
    distance = numeric(),
    covered_interval_ms = numeric()
  )
}

.wcl_distance_empty_summary <- function() {
  tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_movement_empty_time(),
    report_end_at = .wcl_movement_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    fight_start_at = .wcl_movement_empty_time(),
    fight_end_at = .wcl_movement_empty_time(),
    fight_duration_s = numeric(),
    mapID = integer(),
    actor_a_id = integer(),
    actor_a_instance_id = integer(),
    actor_a_role = character(),
    actor_b_id = integer(),
    actor_b_instance_id = integer(),
    actor_b_role = character(),
    max_hold_ms = numeric(),
    overlap_start_timestamp = numeric(),
    overlap_end_timestamp = numeric(),
    overlap_start_at = .wcl_movement_empty_time(),
    overlap_end_at = .wcl_movement_empty_time(),
    overlap_span_s = numeric(),
    series_point_count = integer(),
    comparison_count = integer(),
    comparison_coverage_s = numeric(),
    coverage_fraction = numeric(),
    fight_coverage_fraction = numeric(),
    max_actor_a_age_ms = numeric(),
    max_actor_b_age_ms = numeric(),
    max_held_position_age_ms = numeric(),
    min_distance = numeric(),
    mean_distance = numeric(),
    median_distance = numeric(),
    max_distance = numeric()
  )
}

.wcl_distance_track_key <- function(data) {
  paste(
    .wcl_movement_key_value(data$logID),
    .wcl_movement_key_value(data$fightID),
    .wcl_movement_key_value(data$mapID),
    .wcl_movement_key_value(data$actorID),
    .wcl_movement_key_value(data$actorInstanceID),
    sep = "\u001e"
  )
}

.wcl_distance_scope_key <- function(data) {
  paste(
    .wcl_movement_key_value(data$logID),
    .wcl_movement_key_value(data$fightID),
    .wcl_movement_key_value(data$mapID),
    sep = "\u001e"
  )
}

.wcl_distance_prepare <- function(track, name) {
  required <- c("logID", "fightID", "actorID", "timestamp", "x", "y")
  missing_columns <- setdiff(required, names(track))
  if (length(missing_columns)) {
    stop(
      sprintf(
        "`%s` is missing required movement-track column(s): %s.",
        name,
        paste(sprintf("`%s`", missing_columns), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  track <- tibble::as_tibble(track)
  n <- nrow(track)
  prepared <- tibble::tibble(
    logID = .wcl_movement_character(track, "logID"),
    report_title = .wcl_movement_character(track, "report_title"),
    report_link = .wcl_movement_character(track, "report_link"),
    report_start_at = .wcl_movement_time_column(track, "report_start_at"),
    report_end_at = .wcl_movement_time_column(track, "report_end_at"),
    fightID = .wcl_movement_integer(track, "fightID"),
    encounterID = .wcl_movement_integer(track, "encounterID"),
    encounterName = .wcl_movement_character(track, "encounterName"),
    difficulty = .wcl_movement_integer(track, "difficulty"),
    size = .wcl_movement_integer(track, "size"),
    kill = .wcl_movement_logical(track, "kill"),
    fight_start_at = .wcl_movement_time_column(track, "fight_start_at"),
    fight_end_at = .wcl_movement_time_column(track, "fight_end_at"),
    fight_duration_s = if ("fight_duration_s" %in% names(track)) {
      .wcl_movement_numeric(track, "fight_duration_s")
    } else {
      .wcl_movement_numeric(track, "duration_s")
    },
    mapID = .wcl_movement_integer(track, "mapID"),
    actorID = .wcl_movement_integer(track, "actorID"),
    actorInstanceID = .wcl_movement_integer(track, "actorInstanceID"),
    actor_role = .wcl_movement_character(track, "actor_role"),
    timestamp = .wcl_movement_numeric(track, "timestamp"),
    event_at = .wcl_movement_time_column(track, "event_at"),
    x = .wcl_movement_numeric(track, "x"),
    y = .wcl_movement_numeric(track, "y"),
    .input_row = seq_len(n)
  )

  valid <- !is.na(prepared$logID) & nzchar(prepared$logID) &
    !is.na(prepared$fightID) & !is.na(prepared$actorID) &
    is.finite(prepared$timestamp) & is.finite(prepared$x) & is.finite(prepared$y)
  if (any(!valid)) {
    warning(
      sprintf("Ignored %d malformed row(s) in `%s`.", sum(!valid), name),
      call. = FALSE
    )
  }
  prepared <- prepared[valid, , drop = FALSE]
  if (!nrow(prepared)) {
    return(prepared)
  }

  track_key <- .wcl_distance_track_key(prepared)
  order_index <- order(track_key, prepared$timestamp, prepared$.input_row)
  prepared <- prepared[order_index, , drop = FALSE]
  track_key <- track_key[order_index]
  timestamp_key <- paste(
    track_key,
    .wcl_movement_number_key(prepared$timestamp),
    sep = "\u001e"
  )
  coordinate_key <- paste(
    .wcl_movement_number_key(prepared$x),
    .wcl_movement_number_key(prepared$y),
    sep = "\u001f"
  )
  conflicting <- vapply(
    split(coordinate_key, timestamp_key),
    function(value) length(unique(value)) > 1L,
    logical(1)
  )
  if (any(conflicting)) {
    warning(
      sprintf(
        "Resolved %d conflicting timestamp(s) in `%s` by keeping the first row.",
        sum(conflicting),
        name
      ),
      call. = FALSE
    )
  }
  prepared <- prepared[!duplicated(timestamp_key), , drop = FALSE]
  prepared$.input_row <- NULL
  prepared
}

.wcl_distance_meta <- function(group) {
  list(
    logID = group$logID[[1L]],
    report_title = group$report_title[[1L]],
    report_link = group$report_link[[1L]],
    report_start_at = group$report_start_at[[1L]],
    report_end_at = group$report_end_at[[1L]],
    fightID = group$fightID[[1L]],
    encounterID = group$encounterID[[1L]],
    encounterName = group$encounterName[[1L]],
    difficulty = group$difficulty[[1L]],
    size = group$size[[1L]],
    kill = group$kill[[1L]],
    fight_start_at = group$fight_start_at[[1L]],
    fight_end_at = group$fight_end_at[[1L]],
    fight_duration_s = group$fight_duration_s[[1L]],
    mapID = group$mapID[[1L]]
  )
}

.wcl_distance_event_time <- function(timeline, group_a, group_b, report_start_at) {
  a_match <- match(timeline, group_a$timestamp)
  b_match <- match(timeline, group_b$timestamp)
  a_time <- rep.int(NA_real_, length(timeline))
  b_time <- rep.int(NA_real_, length(timeline))
  has_a <- !is.na(a_match)
  has_b <- !is.na(b_match)
  a_time[has_a] <- as.numeric(group_a$event_at[a_match[has_a]])
  b_time[has_b] <- as.numeric(group_b$event_at[b_match[has_b]])
  event_number <- ifelse(!is.na(a_time), a_time, b_time)
  if (!is.na(report_start_at)) {
    event_number[is.na(event_number)] <- as.numeric(report_start_at) +
      timeline[is.na(event_number)] / 1000
  }
  as.POSIXct(event_number, origin = "1970-01-01", tz = "UTC")
}

.wcl_distance_summary_row <- function(
    meta,
    group_a,
    group_b,
    max_hold_ms,
    series = NULL,
    overlap_start = NA_real_,
    overlap_end = NA_real_) {
  actor_a_role <- .wcl_movement_role_label(group_a$actor_role)
  actor_b_role <- .wcl_movement_role_label(group_b$actor_role)

  if (is.null(series) || !nrow(series)) {
    overlap_start_at <- .wcl_movement_empty_time(1L)
    overlap_end_at <- .wcl_movement_empty_time(1L)
    overlap_span_s <- if (is.finite(overlap_start) && is.finite(overlap_end)) {
      max(0, overlap_end - overlap_start) / 1000
    } else {
      0
    }
    series_count <- comparison_count <- 0L
    coverage_s <- 0
    coverage_fraction <- if (overlap_span_s > 0) 0 else NA_real_
    max_actor_a_age_ms <- max_actor_b_age_ms <-
      max_held_position_age_ms <- NA_real_
    min_distance <- mean_distance <- median_distance <- max_distance <- NA_real_
  } else {
    overlap_start_at <- series$event_at[[1L]]
    overlap_end_at <- series$event_at[[nrow(series)]]
    overlap_span_s <- max(0, overlap_end - overlap_start) / 1000
    series_count <- as.integer(nrow(series))
    comparison_count <- as.integer(sum(series$within_hold))
    coverage_s <- sum(series$covered_interval_ms) / 1000
    coverage_fraction <- if (overlap_span_s > 0) {
      min(1, coverage_s / overlap_span_s)
    } else {
      NA_real_
    }
    valid_comparison <- series$within_hold & is.finite(series$distance)
    max_actor_a_age_ms <- if (any(valid_comparison)) {
      max(series$actor_a_age_ms[valid_comparison])
    } else {
      NA_real_
    }
    max_actor_b_age_ms <- if (any(valid_comparison)) {
      max(series$actor_b_age_ms[valid_comparison])
    } else {
      NA_real_
    }
    max_held_position_age_ms <- if (any(valid_comparison)) {
      max(
        series$actor_a_age_ms[valid_comparison],
        series$actor_b_age_ms[valid_comparison]
      )
    } else {
      NA_real_
    }
    valid_distance <- series$distance[series$within_hold & is.finite(series$distance)]
    min_distance <- if (length(valid_distance)) min(valid_distance) else NA_real_
    median_distance <- if (length(valid_distance)) stats::median(valid_distance) else NA_real_
    max_distance <- if (length(valid_distance)) max(valid_distance) else NA_real_
    positive_coverage <- is.finite(series$distance) & series$covered_interval_ms > 0
    mean_distance <- if (any(positive_coverage)) {
      stats::weighted.mean(
        series$distance[positive_coverage],
        series$covered_interval_ms[positive_coverage]
      )
    } else if (length(valid_distance)) {
      mean(valid_distance)
    } else {
      NA_real_
    }
  }

  fight_duration_s <- suppressWarnings(as.numeric(meta$fight_duration_s))
  fight_coverage_fraction <- if (
      length(fight_duration_s) == 1L &&
      is.finite(fight_duration_s) &&
      fight_duration_s > 0) {
    min(1, max(0, coverage_s / fight_duration_s))
  } else {
    NA_real_
  }

  tibble::tibble(
    logID = meta$logID,
    report_title = meta$report_title,
    report_link = meta$report_link,
    report_start_at = meta$report_start_at,
    report_end_at = meta$report_end_at,
    fightID = meta$fightID,
    encounterID = meta$encounterID,
    encounterName = meta$encounterName,
    difficulty = meta$difficulty,
    size = meta$size,
    kill = meta$kill,
    fight_start_at = meta$fight_start_at,
    fight_end_at = meta$fight_end_at,
    fight_duration_s = meta$fight_duration_s,
    mapID = meta$mapID,
    actor_a_id = group_a$actorID[[1L]],
    actor_a_instance_id = group_a$actorInstanceID[[1L]],
    actor_a_role = actor_a_role,
    actor_b_id = group_b$actorID[[1L]],
    actor_b_instance_id = group_b$actorInstanceID[[1L]],
    actor_b_role = actor_b_role,
    max_hold_ms = max_hold_ms,
    overlap_start_timestamp = overlap_start,
    overlap_end_timestamp = overlap_end,
    overlap_start_at = overlap_start_at,
    overlap_end_at = overlap_end_at,
    overlap_span_s = overlap_span_s,
    series_point_count = series_count,
    comparison_count = comparison_count,
    comparison_coverage_s = coverage_s,
    coverage_fraction = coverage_fraction,
    fight_coverage_fraction = fight_coverage_fraction,
    max_actor_a_age_ms = max_actor_a_age_ms,
    max_actor_b_age_ms = max_actor_b_age_ms,
    max_held_position_age_ms = max_held_position_age_ms,
    min_distance = min_distance,
    mean_distance = mean_distance,
    median_distance = median_distance,
    max_distance = max_distance
  )
}

#' Measure sampled distance between two actor tracks
#'
#' `wcl_actor_distance()` aligns two outputs from
#' [wcl_actor_movement()] using last-observation-carried-forward (LOCF) and
#' calculates their two-dimensional separation within their shared observed
#' time range.
#'
#' @param track_a,track_b Movement-track data frames returned by
#'   `wcl_actor_movement(..., output = "track")`. Each input may contain more
#'   than one actor or fight; compatible actor tracks are paired within the
#'   same report, fight, and map.
#' @param max_hold_ms Maximum age in milliseconds for a carried-forward
#'   position. The default, `Inf`, keeps the latest observation throughout the
#'   actors' observed overlap. A finite non-negative value marks older
#'   comparisons invalid and limits reported coverage.
#' @param output Output form. `"summary"` (the default) returns one row per
#'   compatible actor pair. `"series"` returns the union of both tracks'
#'   timestamps inside their observed overlap.
#'
#' @details
#' The function does not extrapolate before an actor's first observation or
#' after its last observation. At each union timestamp, it carries forward each
#' actor's most recent coordinate. `actor_a_age_ms` and `actor_b_age_ms` expose
#' the age of those observations. `within_hold` indicates whether both ages
#' satisfy `max_hold_ms`; invalid distances are `NA`.
#'
#' Series output also includes `covered_interval_ms`, the portion of the
#' interval following that row for which both carried positions remain within
#' the hold limit. Summary `comparison_coverage_s` sums this value and
#' `coverage_fraction` divides it by the actors' observed overlap, while
#' `fight_coverage_fraction` divides it by `fight_duration_s`. The latter is
#' `NA` when a positive finite fight duration is unavailable. The mean distance
#' is weighted by covered time when positive-duration coverage is available.
#' Summary maximum-age fields report the oldest actor A, actor B, and
#' either-actor held position among valid comparisons.
#'
#' @return A tibble. Series output contains both carried coordinates, source
#'   sample timestamps and ages, distance, hold validity, and interval
#'   coverage. Summary output contains overlap and coverage diagnostics plus
#'   separate observed-overlap and full-fight coverage fractions,
#'   `max_actor_a_age_ms`, `max_actor_b_age_ms`,
#'   `max_held_position_age_ms`, and minimum, time-weighted mean, median, and
#'   maximum distance.
#' @export
wcl_actor_distance <- function(
    track_a,
    track_b,
    max_hold_ms = Inf,
    output = c("summary", "series")) {
  output <- match.arg(output)
  if (!inherits(track_a, "data.frame")) {
    stop("`track_a` must be a data frame.", call. = FALSE)
  }
  if (!inherits(track_b, "data.frame")) {
    stop("`track_b` must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(max_hold_ms) || length(max_hold_ms) != 1L ||
      is.na(max_hold_ms) || max_hold_ms < 0 ||
      (!is.finite(max_hold_ms) && !identical(as.numeric(max_hold_ms), Inf))) {
    stop("`max_hold_ms` must be one non-negative number or `Inf`.", call. = FALSE)
  }
  max_hold_ms <- as.numeric(max_hold_ms)

  empty <- if (output == "summary") {
    .wcl_distance_empty_summary()
  } else {
    .wcl_distance_empty_series()
  }
  if (!nrow(track_a) || !nrow(track_b)) {
    return(empty)
  }

  track_a <- .wcl_distance_prepare(track_a, "track_a")
  track_b <- .wcl_distance_prepare(track_b, "track_b")
  if (!nrow(track_a) || !nrow(track_b)) {
    return(empty)
  }

  scope_a <- .wcl_distance_scope_key(track_a)
  scope_b <- .wcl_distance_scope_key(track_b)
  common_scopes <- intersect(unique(scope_a), unique(scope_b))
  if (!length(common_scopes)) {
    return(empty)
  }

  series_rows <- list()
  summary_rows <- list()

  for (scope in common_scopes) {
    scoped_a <- track_a[scope_a == scope, , drop = FALSE]
    scoped_b <- track_b[scope_b == scope, , drop = FALSE]
    key_a <- .wcl_distance_track_key(scoped_a)
    key_b <- .wcl_distance_track_key(scoped_b)

    for (a_key in unique(key_a)) {
      group_a <- scoped_a[key_a == a_key, , drop = FALSE]
      for (b_key in unique(key_b)) {
        group_b <- scoped_b[key_b == b_key, , drop = FALSE]
        meta <- .wcl_distance_meta(group_a)
        overlap_start <- max(min(group_a$timestamp), min(group_b$timestamp))
        overlap_end <- min(max(group_a$timestamp), max(group_b$timestamp))

        if (overlap_start > overlap_end) {
          if (output == "summary") {
            summary_rows[[length(summary_rows) + 1L]] <- .wcl_distance_summary_row(
              meta,
              group_a,
              group_b,
              max_hold_ms
            )
          }
          next
        }

        timeline <- sort(unique(c(
          group_a$timestamp[
            group_a$timestamp >= overlap_start & group_a$timestamp <= overlap_end
          ],
          group_b$timestamp[
            group_b$timestamp >= overlap_start & group_b$timestamp <= overlap_end
          ],
          overlap_start,
          overlap_end
        )))
        a_index <- findInterval(timeline, group_a$timestamp)
        b_index <- findInterval(timeline, group_b$timestamp)
        a_sample <- group_a$timestamp[a_index]
        b_sample <- group_b$timestamp[b_index]
        a_age <- timeline - a_sample
        b_age <- timeline - b_sample
        within_hold <- a_age <= max_hold_ms & b_age <= max_hold_ms
        distance <- sqrt(
          (group_a$x[a_index] - group_b$x[b_index])^2 +
            (group_a$y[a_index] - group_b$y[b_index])^2
        )
        distance[!within_hold] <- NA_real_

        interval_ms <- c(diff(timeline), 0)
        if (is.infinite(max_hold_ms)) {
          covered_interval <- ifelse(within_hold, interval_ms, 0)
        } else {
          a_remaining <- pmax(0, max_hold_ms - a_age)
          b_remaining <- pmax(0, max_hold_ms - b_age)
          covered_interval <- ifelse(
            within_hold,
            pmin(interval_ms, a_remaining, b_remaining),
            0
          )
        }

        event_at <- .wcl_distance_event_time(
          timeline,
          group_a,
          group_b,
          meta$report_start_at
        )
        count <- length(timeline)
        series <- tibble::tibble(
          logID = rep.int(meta$logID, count),
          report_title = rep.int(meta$report_title, count),
          report_link = rep.int(meta$report_link, count),
          report_start_at = rep(meta$report_start_at, count),
          report_end_at = rep(meta$report_end_at, count),
          fightID = rep.int(meta$fightID, count),
          encounterID = rep.int(meta$encounterID, count),
          encounterName = rep.int(meta$encounterName, count),
          difficulty = rep.int(meta$difficulty, count),
          size = rep.int(meta$size, count),
          kill = rep.int(meta$kill, count),
          fight_start_at = rep(meta$fight_start_at, count),
          fight_end_at = rep(meta$fight_end_at, count),
          fight_duration_s = rep.int(meta$fight_duration_s, count),
          mapID = rep.int(meta$mapID, count),
          actor_a_id = rep.int(group_a$actorID[[1L]], count),
          actor_a_instance_id = rep.int(group_a$actorInstanceID[[1L]], count),
          actor_a_role = rep.int(.wcl_movement_role_label(group_a$actor_role), count),
          actor_b_id = rep.int(group_b$actorID[[1L]], count),
          actor_b_instance_id = rep.int(group_b$actorInstanceID[[1L]], count),
          actor_b_role = rep.int(.wcl_movement_role_label(group_b$actor_role), count),
          timestamp = timeline,
          event_at = event_at,
          actor_a_sample_timestamp = a_sample,
          actor_b_sample_timestamp = b_sample,
          actor_a_age_ms = a_age,
          actor_b_age_ms = b_age,
          actor_a_x = group_a$x[a_index],
          actor_a_y = group_a$y[a_index],
          actor_b_x = group_b$x[b_index],
          actor_b_y = group_b$y[b_index],
          max_hold_ms = rep.int(max_hold_ms, count),
          within_hold = within_hold,
          distance = distance,
          covered_interval_ms = covered_interval
        )

        if (output == "series") {
          series_rows[[length(series_rows) + 1L]] <- series
        } else {
          summary_rows[[length(summary_rows) + 1L]] <- .wcl_distance_summary_row(
            meta,
            group_a,
            group_b,
            max_hold_ms,
            series,
            overlap_start,
            overlap_end
          )
        }
      }
    }
  }

  rows <- if (output == "series") series_rows else summary_rows
  if (!length(rows)) {
    return(empty)
  }
  dplyr::bind_rows(rows)
}
