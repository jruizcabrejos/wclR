.wcl_aura_empty_time <- function(n = 0L) {
  as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
}

.wcl_aura_empty_summary <- function(by_source = FALSE) {
  out <- tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_aura_empty_time(),
    report_end_at = .wcl_aura_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    fight_start_at = .wcl_aura_empty_time(),
    fight_end_at = .wcl_aura_empty_time(),
    fight_duration_s = numeric(),
    aura_type = character(),
    targetID = integer(),
    targetInstanceID = integer(),
    abilityGameID = integer(),
    uptime_s = numeric(),
    uptime_fraction = numeric(),
    uptime_pct = numeric(),
    interval_count = integer(),
    application_count = integer(),
    refresh_count = integer(),
    initial_stacks = numeric(),
    max_stacks = numeric(),
    started_at_pull = logical(),
    active_at_end = logical(),
    initial_state_inferred = logical(),
    source_ambiguous = logical()
  )

  if (isTRUE(by_source)) {
    out <- dplyr::relocate(
      dplyr::mutate(out, sourceID = integer(), sourceInstanceID = integer()),
      sourceID,
      sourceInstanceID,
      .after = targetInstanceID
    )
  }

  out
}

.wcl_aura_empty_intervals <- function(by_source = FALSE) {
  out <- tibble::tibble(
    logID = character(),
    report_title = character(),
    report_link = character(),
    report_start_at = .wcl_aura_empty_time(),
    report_end_at = .wcl_aura_empty_time(),
    fightID = integer(),
    encounterID = integer(),
    encounterName = character(),
    difficulty = integer(),
    size = integer(),
    kill = logical(),
    fight_start_at = .wcl_aura_empty_time(),
    fight_end_at = .wcl_aura_empty_time(),
    fight_duration_s = numeric(),
    aura_type = character(),
    targetID = integer(),
    targetInstanceID = integer(),
    abilityGameID = integer(),
    interval_id = integer(),
    interval_start_ms = numeric(),
    interval_end_ms = numeric(),
    interval_start_s = numeric(),
    interval_end_s = numeric(),
    interval_start_at = .wcl_aura_empty_time(),
    interval_end_at = .wcl_aura_empty_time(),
    interval_duration_s = numeric(),
    started_at_pull = logical(),
    ended_at_fight_end = logical(),
    initial_state_inferred = logical(),
    source_ambiguous = logical()
  )

  if (isTRUE(by_source)) {
    out <- dplyr::relocate(
      dplyr::mutate(out, sourceID = integer(), sourceInstanceID = integer()),
      sourceID,
      sourceInstanceID,
      .after = targetInstanceID
    )
  }

  out
}

.wcl_aura_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }

  x
}

.wcl_aura_id_filter <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  values <- suppressWarnings(as.numeric(x))
  valid <- length(values) > 0L &&
    length(values) == length(x) &&
    all(is.finite(values)) &&
    all(values == floor(values))

  if (!valid) {
    stop(sprintf("`%s` must be a non-empty vector of integer-like IDs without missing values.", name), call. = FALSE)
  }

  unique(values)
}

.wcl_aura_time <- function(x) {
  if (!length(x)) {
    return(.wcl_aura_empty_time())
  }

  if (inherits(x, "POSIXct")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }

  values <- as.character(x)
  output <- .wcl_aura_empty_time(length(values))
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

  # `as.POSIXct()` without an explicit format parses ISO strings as date-only
  # on some Windows R builds. Try the WCL/CSV ISO representations first and
  # retain fractional seconds through `%OS`.
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

.wcl_aura_list_scalar <- function(value, keys) {
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
      if (!is.null(candidate)) {
        parsed <- .wcl_aura_list_scalar(candidate, keys)
        if (!is.na(parsed)) {
          return(parsed)
        }
      }
    }
    return(NA_real_)
  }

  parsed <- suppressWarnings(as.numeric(value[[1L]]))
  if (length(parsed) && is.finite(parsed)) parsed else NA_real_
}

.wcl_aura_row_scalar <- function(data, row, aliases, keys = aliases) {
  for (alias in aliases) {
    if (!alias %in% names(data)) {
      next
    }

    column <- data[[alias]]
    value <- if (is.list(column)) column[[row]] else column[[row]]
    parsed <- .wcl_aura_list_scalar(value, keys)
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  NA_real_
}

.wcl_aura_prepare_input <- function(data, force_combatant = FALSE, order_offset = 0L) {
  if (!inherits(data, "data.frame")) {
    stop("Aura event inputs must be data frames.", call. = FALSE)
  }

  data <- tibble::as_tibble(data)
  n <- nrow(data)

  if (!"type" %in% names(data)) {
    if (isTRUE(force_combatant)) {
      data$type <- rep.int("combatantinfo", n)
    } else if (n) {
      stop("`events` must include a `type` column.", call. = FALSE)
    } else {
      data$type <- character()
    }
  }

  get_character <- function(name) {
    if (name %in% names(data)) as.character(data[[name]]) else rep.int(NA_character_, n)
  }
  get_numeric <- function(name) {
    if (name %in% names(data)) suppressWarnings(as.numeric(data[[name]])) else rep.int(NA_real_, n)
  }
  get_integer <- function(name) {
    if (name %in% names(data)) suppressWarnings(as.integer(data[[name]])) else rep.int(NA_integer_, n)
  }
  get_logical <- function(name) {
    if (name %in% names(data)) as.logical(data[[name]]) else rep.int(NA, n)
  }
  get_time <- function(name) {
    if (name %in% names(data)) .wcl_aura_time(data[[name]]) else .wcl_aura_empty_time(n)
  }

  source_id <- source_instance_id <- target_id <- target_instance_id <-
    ability_id <- stack_value <- rep.int(NA_real_, n)
  for (row in seq_len(n)) {
    source_id[[row]] <- .wcl_aura_row_scalar(
      data,
      row,
      c("sourceID", "sourceId", "source_id", "source"),
      c("reportID", "reportId", "actorID", "actorId", "id", "gameID", "gameId")
    )
    source_instance_id[[row]] <- .wcl_aura_row_scalar(
      data,
      row,
      c("sourceInstanceID", "sourceInstanceId", "sourceInstance", "source_instance_id"),
      c("sourceInstanceID", "sourceInstanceId", "sourceInstance", "instanceID", "instanceId", "instance", "id")
    )
    target_id[[row]] <- .wcl_aura_row_scalar(
      data,
      row,
      c("targetID", "targetId", "target_id", "target"),
      c("reportID", "reportId", "actorID", "actorId", "id", "gameID", "gameId")
    )
    target_instance_id[[row]] <- .wcl_aura_row_scalar(
      data,
      row,
      c("targetInstanceID", "targetInstanceId", "targetInstance", "target_instance_id"),
      c("targetInstanceID", "targetInstanceId", "targetInstance", "instanceID", "instanceId", "instance", "id")
    )
    ability_id[[row]] <- .wcl_aura_row_scalar(
      data,
      row,
      c("abilityGameID", "abilityGameId", "ability_gameID", "ability_gameId", "ability"),
      c("gameID", "gameId", "abilityGameID", "abilityGameId", "id")
    )
    stack_value[[row]] <- .wcl_aura_row_scalar(data, row, c("stacks", "stack"), c("stacks", "stack"))
  }

  aura_values <- if ("auras" %in% names(data)) {
    if (is.list(data$auras)) data$auras else as.list(data$auras)
  } else {
    rep(list(NULL), n)
  }

  tibble::tibble(
    logID = get_character("logID"),
    report_title = get_character("report_title"),
    report_link = get_character("report_link"),
    report_start_at = get_time("report_start_at"),
    report_end_at = get_time("report_end_at"),
    fightID = get_integer("fightID"),
    encounterID = get_integer("encounterID"),
    encounterName = get_character("encounterName"),
    difficulty = get_integer("difficulty"),
    size = get_integer("size"),
    kill = get_logical("kill"),
    duration_s = get_numeric("duration_s"),
    fight_start_at = get_time("fight_start_at"),
    fight_end_at = get_time("fight_end_at"),
    startTime = get_numeric("startTime"),
    endTime = get_numeric("endTime"),
    timestamp = get_numeric("timestamp"),
    event_at = get_time("event_at"),
    type = tolower(as.character(data$type)),
    sourceID = suppressWarnings(as.integer(source_id)),
    sourceInstanceID = suppressWarnings(as.integer(source_instance_id)),
    targetID = suppressWarnings(as.integer(target_id)),
    targetInstanceID = suppressWarnings(as.integer(target_instance_id)),
    abilityGameID = suppressWarnings(as.integer(ability_id)),
    stack_value = as.numeric(stack_value),
    auras = aura_values,
    .row_order = as.numeric(order_offset) + seq_len(n)
  )
}

.wcl_aura_first <- function(x, default = NA) {
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

.wcl_aura_fight_key <- function(log_id, fight_id) {
  paste(log_id, fight_id, sep = "\u001f")
}

.wcl_aura_combo_key <- function(log_id, fight_id, target_id, target_instance_id, ability_id, aura_type) {
  paste(
    log_id,
    fight_id,
    target_id,
    ifelse(is.na(target_instance_id), "NA", target_instance_id),
    ability_id,
    aura_type,
    sep = "\u001f"
  )
}

.wcl_aura_fight_meta <- function(rows) {
  durations <- unique(rows$duration_s[is.finite(rows$duration_s)])
  if (length(durations) > 1L && diff(range(durations)) > 1e-6) {
    stop("Aura event rows contain inconsistent `duration_s` values within a fight.", call. = FALSE)
  }

  check_consistent <- function(values, name, tolerance = 1e-6) {
    if (inherits(values, "POSIXct")) {
      values <- as.numeric(values)
    } else {
      values <- suppressWarnings(as.numeric(values))
    }
    values <- unique(values[is.finite(values)])
    if (length(values) > 1L && diff(range(values)) > tolerance) {
      stop(sprintf("Aura event rows contain inconsistent `%s` values within a fight.", name), call. = FALSE)
    }
    if (length(values)) values[[1L]] else NA_real_
  }

  start_at_numeric <- check_consistent(rows$fight_start_at, "fight_start_at")
  end_at_numeric <- check_consistent(rows$fight_end_at, "fight_end_at")
  start_time_numeric <- check_consistent(rows$startTime, "startTime")
  end_time_numeric <- check_consistent(rows$endTime, "endTime")

  start_at <- .wcl_aura_first(rows$fight_start_at, .wcl_aura_empty_time(1L))
  end_at <- .wcl_aura_first(rows$fight_end_at, .wcl_aura_empty_time(1L))
  duration <- if (length(durations)) durations[[1L]] else NA_real_

  if (!is.finite(duration) && !is.na(start_at) && !is.na(end_at)) {
    duration <- as.numeric(difftime(end_at, start_at, units = "secs"))
  }

  if (!is.finite(duration) || duration <= 0) {
    stop("Each fight must have a positive `duration_s` or usable fight start/end times.", call. = FALSE)
  }

  # CSV round trips commonly render the absolute end boundary only to the
  # nearest second even though `duration_s` retains millisecond precision.
  # Accept that representational drift, but reject genuinely inconsistent
  # boundaries. Numeric WCL report offsets remain millisecond-exact below.
  if (is.finite(start_at_numeric) && is.finite(end_at_numeric) &&
      abs((end_at_numeric - start_at_numeric) - duration) > 1 + 1e-6) {
    stop("Fight start/end times are inconsistent with `duration_s`.", call. = FALSE)
  }
  if (is.finite(start_time_numeric) && is.finite(end_time_numeric) &&
      abs((end_time_numeric - start_time_numeric) / 1000 - duration) > 1e-3) {
    stop("`startTime`/`endTime` are inconsistent with `duration_s`.", call. = FALSE)
  }

  # Prefer the precise duration once a start boundary is known. This also
  # canonicalizes a whole-second CSV `fight_end_at` back to subsecond time.
  if (!is.na(start_at)) {
    end_at <- start_at + duration
  } else if (!is.na(end_at)) {
    start_at <- end_at - duration
  }

  report_start_at <- .wcl_aura_first(rows$report_start_at, .wcl_aura_empty_time(1L))
  start_time <- start_time_numeric
  if (!is.finite(start_time) && !is.na(report_start_at) && !is.na(start_at)) {
    start_time <- as.numeric(difftime(start_at, report_start_at, units = "secs")) * 1000
  }

  list(
    logID = as.character(rows$logID[[1L]]),
    report_title = .wcl_aura_first(rows$report_title, NA_character_),
    report_link = .wcl_aura_first(rows$report_link, NA_character_),
    report_start_at = report_start_at,
    report_end_at = .wcl_aura_first(rows$report_end_at, .wcl_aura_empty_time(1L)),
    fightID = as.integer(rows$fightID[[1L]]),
    encounterID = as.integer(.wcl_aura_first(rows$encounterID, NA_integer_)),
    encounterName = .wcl_aura_first(rows$encounterName, NA_character_),
    difficulty = as.integer(.wcl_aura_first(rows$difficulty, NA_integer_)),
    size = as.integer(.wcl_aura_first(rows$size, NA_integer_)),
    kill = as.logical(.wcl_aura_first(rows$kill, NA)),
    fight_start_at = start_at,
    fight_end_at = end_at,
    duration_s = as.numeric(duration),
    fight_start_ms = as.numeric(start_time)
  )
}

.wcl_aura_relative_time <- function(row, meta) {
  timestamp <- row$timestamp[[1L]]
  if (is.finite(timestamp) && is.finite(meta$fight_start_ms)) {
    return((timestamp - meta$fight_start_ms) / 1000)
  }

  event_at <- row$event_at[[1L]]
  if (!is.na(event_at) && !is.na(meta$fight_start_at)) {
    return(as.numeric(difftime(event_at, meta$fight_start_at, units = "secs")))
  }

  if (is.finite(timestamp) && timestamp >= 0 && timestamp <= meta$duration_s * 1000) {
    return(timestamp / 1000)
  }

  stop(
    "Aura lifecycle rows need a usable event time (`timestamp` with a fight offset, or `event_at` with `fight_start_at`).",
    call. = FALSE
  )
}

.wcl_aura_records <- function(value) {
  if (is.null(value) || !length(value)) {
    return(list())
  }

  if (is.character(value) && length(value) == 1L) {
    if (!nzchar(value) || !jsonlite::validate(value)) {
      return(list())
    }
    value <- jsonlite::fromJSON(value, simplifyVector = FALSE)
  }

  if (inherits(value, "data.frame")) {
    return(lapply(seq_len(nrow(value)), function(i) as.list(value[i, , drop = FALSE])))
  }

  if (!is.list(value)) {
    return(list())
  }

  record_names <- names(value)
  record_keys <- c(
    "abilityGameId", "abilityGameID", "ability", "source", "sourceID",
    "sourceId", "stacks", "stack", "name", "icon"
  )
  if (!is.null(record_names) && any(record_names %in% record_keys)) {
    return(list(value))
  }

  unlist(lapply(value, .wcl_aura_records), recursive = FALSE, use.names = FALSE)
}

.wcl_aura_record_scalar <- function(record, aliases, keys = aliases) {
  if (inherits(record, "data.frame") && nrow(record)) {
    record <- as.list(record[1L, , drop = FALSE])
  }
  if (!is.list(record)) {
    return(NA_real_)
  }

  for (alias in aliases) {
    value <- record[[alias]]
    if (!is.null(value)) {
      parsed <- .wcl_aura_list_scalar(value, keys)
      if (!is.na(parsed)) {
        return(parsed)
      }
    }
  }

  NA_real_
}

.wcl_aura_lifecycle <- function(rows, fight_meta) {
  lifecycle <- tibble::tribble(
    ~type, ~aura_type, ~action,
    "applybuff", "buff", "apply",
    "applybuffstack", "buff", "apply_stack",
    "refreshbuff", "buff", "refresh",
    "removebuff", "buff", "remove",
    "removebuffstack", "buff", "remove_stack",
    "applydebuff", "debuff", "apply",
    "applydebuffstack", "debuff", "apply_stack",
    "refreshdebuff", "debuff", "refresh",
    "removedebuff", "debuff", "remove",
    "removedebuffstack", "debuff", "remove_stack"
  )

  matched <- match(rows$type, lifecycle$type)
  index <- which(!is.na(matched))
  if (!length(index)) {
    return(tibble::tibble())
  }

  selected <- rows[index, , drop = FALSE]
  malformed <- is.na(selected$targetID) | is.na(selected$abilityGameID)
  if (any(malformed)) {
    warning(
      sprintf("Ignored %d aura lifecycle row(s) without target or ability IDs.", sum(malformed)),
      call. = FALSE
    )
    selected <- selected[!malformed, , drop = FALSE]
    matched <- matched[index][!malformed]
  } else {
    matched <- matched[index]
  }

  if (!nrow(selected)) {
    return(tibble::tibble())
  }

  time_s <- numeric(nrow(selected))
  for (i in seq_len(nrow(selected))) {
    key <- .wcl_aura_fight_key(selected$logID[[i]], selected$fightID[[i]])
    time_s[[i]] <- .wcl_aura_relative_time(selected[i, , drop = FALSE], fight_meta[[key]])
  }

  tibble::tibble(
    logID = selected$logID,
    fightID = selected$fightID,
    targetID = selected$targetID,
    targetInstanceID = selected$targetInstanceID,
    sourceID = selected$sourceID,
    sourceInstanceID = selected$sourceInstanceID,
    abilityGameID = selected$abilityGameID,
    aura_type = lifecycle$aura_type[matched],
    action = lifecycle$action[matched],
    stack_value = selected$stack_value,
    time_s = time_s,
    .row_order = selected$.row_order,
    .snapshot = FALSE
  )
}

.wcl_aura_snapshots <- function(rows) {
  index <- which(rows$type == "combatantinfo")
  if (!length(index)) {
    return(tibble::tibble())
  }

  output <- list()
  malformed <- 0L

  for (row_index in index) {
    target_id <- rows$sourceID[[row_index]]
    target_instance_id <- rows$sourceInstanceID[[row_index]]
    records <- .wcl_aura_records(rows$auras[[row_index]])
    if (!length(records)) {
      next
    }

    for (record_index in seq_along(records)) {
      record <- records[[record_index]]
      ability_id <- .wcl_aura_record_scalar(
        record,
        c("abilityGameId", "abilityGameID", "ability"),
        c("gameID", "gameId", "abilityGameID", "abilityGameId", "id")
      )
      source_id <- .wcl_aura_record_scalar(
        record,
        c("sourceID", "sourceId", "source"),
        c("reportID", "reportId", "actorID", "actorId", "id", "gameID", "gameId")
      )
      source_instance_id <- .wcl_aura_record_scalar(
        record,
        c("sourceInstanceID", "sourceInstanceId", "sourceInstance"),
        c("sourceInstanceID", "sourceInstanceId", "sourceInstance", "instanceID", "instanceId", "instance", "id")
      )
      stacks <- .wcl_aura_record_scalar(record, c("stacks", "stack"), c("stacks", "stack"))

      if (is.na(target_id) || is.na(ability_id)) {
        malformed <- malformed + 1L
        next
      }

      output[[length(output) + 1L]] <- tibble::tibble(
        logID = rows$logID[[row_index]],
        fightID = rows$fightID[[row_index]],
        targetID = as.integer(target_id),
        targetInstanceID = as.integer(target_instance_id),
        sourceID = as.integer(source_id),
        sourceInstanceID = as.integer(source_instance_id),
        abilityGameID = as.integer(ability_id),
        aura_type = "unknown",
        action = "snapshot",
        stack_value = as.numeric(stacks),
        time_s = 0,
        .row_order = rows$.row_order[[row_index]] + record_index / 100000,
        .snapshot = TRUE
      )
    }
  }

  if (malformed) {
    warning(
      sprintf("Ignored %d malformed CombatantInfo aura entr%s.", malformed, if (malformed == 1L) "y" else "ies"),
      call. = FALSE
    )
  }

  dplyr::bind_rows(output)
}

.wcl_aura_fill_unique_instance <- function(
    events,
    actor_column,
    instance_column,
    extra_columns = character()) {
  if (!nrow(events)) {
    return(events)
  }

  actor_id <- events[[actor_column]]
  instance_id <- events[[instance_column]]
  usable <- !is.na(actor_id)
  if (!any(usable) || !any(usable & is.na(instance_id))) {
    return(events)
  }

  key_parts <- list(
    events$logID,
    events$fightID,
    ifelse(is.na(actor_id), "NA", actor_id)
  )
  for (column in extra_columns) {
    value <- events[[column]]
    key_parts[[length(key_parts) + 1L]] <- ifelse(
      is.na(value),
      "NA",
      as.character(value)
    )
  }
  actor_key <- do.call(paste, c(key_parts, sep = "\u001f"))
  groups <- split(which(usable), actor_key[usable])

  for (indices in groups) {
    known <- unique(instance_id[indices][!is.na(instance_id[indices])])
    if (length(known) == 1L) {
      missing <- indices[is.na(instance_id[indices])]
      instance_id[missing] <- known[[1L]]
    }
  }

  events[[instance_column]] <- as.integer(instance_id)
  events
}

.wcl_aura_reconcile_instances <- function(events) {
  # CombatantInfo exposes the affected actor in the outer event and the caster
  # inside each opaque aura record. Either side can omit its instance ID. A
  # single known actor instance in the same fight is therefore authoritative
  # for otherwise-identical rows and prevents a snapshot-only phantom state.
  # Resolve the most specific unambiguous match first. An actor can have more
  # than one instance in a fight, while only one of those instances may be
  # associated with a particular target/ability combination.
  events <- .wcl_aura_fill_unique_instance(
    events,
    "targetID",
    "targetInstanceID",
    extra_columns = "abilityGameID"
  )
  events <- .wcl_aura_fill_unique_instance(
    events,
    "sourceID",
    "sourceInstanceID",
    extra_columns = c("targetID", "abilityGameID")
  )

  # Fall back to a fight-wide actor match only when that actor still has one
  # unique known instance.
  events <- .wcl_aura_fill_unique_instance(
    events,
    "targetID",
    "targetInstanceID"
  )
  .wcl_aura_fill_unique_instance(
    events,
    "sourceID",
    "sourceInstanceID"
  )
}

.wcl_aura_classify_snapshots <- function(events) {
  snapshot_index <- which(events$.snapshot)
  if (!length(snapshot_index)) {
    return(events)
  }

  lifecycle <- events[!events$.snapshot, , drop = FALSE]
  lifecycle_key <- .wcl_aura_combo_key(
    lifecycle$logID,
    lifecycle$fightID,
    lifecycle$targetID,
    lifecycle$targetInstanceID,
    lifecycle$abilityGameID,
    rep.int("", nrow(lifecycle))
  )
  ambiguous <- 0L

  for (index in snapshot_index) {
    key <- .wcl_aura_combo_key(
      events$logID[[index]],
      events$fightID[[index]],
      events$targetID[[index]],
      events$targetInstanceID[[index]],
      events$abilityGameID[[index]],
      ""
    )
    kinds <- unique(lifecycle$aura_type[lifecycle_key == key])
    kinds <- kinds[!is.na(kinds)]
    if (length(kinds) == 1L) {
      events$aura_type[[index]] <- kinds[[1L]]
    } else if (length(kinds) > 1L) {
      ambiguous <- ambiguous + 1L
    }
  }

  if (ambiguous) {
    warning(
      sprintf("Could not classify %d CombatantInfo aura entr%s as a buff or debuff.", ambiguous, if (ambiguous == 1L) "y" else "ies"),
      call. = FALSE
    )
  }

  events
}

.wcl_aura_source_key <- function(source_id, source_instance_id) {
  if (is.na(source_id)) {
    return("unknown")
  }

  paste0(
    "id:",
    format(source_id, scientific = FALSE, trim = TRUE),
    ":instance:",
    if (is.na(source_instance_id)) "NA" else format(source_instance_id, scientific = FALSE, trim = TRUE)
  )
}

.wcl_aura_new_state <- function(source_id, source_instance_id, order) {
  list(
    sourceID = as.integer(source_id),
    sourceInstanceID = as.integer(source_instance_id),
    first_order = order,
    active = FALSE,
    ever_active = FALSE,
    start_s = NA_real_,
    start_inferred = FALSE,
    intervals = list(),
    n_applications = 0L,
    n_refreshes = 0L,
    initial_stacks = NA_real_,
    current_stacks = NA_real_,
    max_stacks = NA_real_,
    active_at_end = FALSE,
    initial_state_inferred = FALSE,
    source_ambiguous = FALSE,
    interval_source_ambiguous = FALSE
  )
}

.wcl_aura_stack_max <- function(current, value) {
  if (!is.finite(value)) {
    return(current)
  }
  if (!is.finite(current)) value else max(current, value)
}

.wcl_aura_open_state <- function(state, time_s, inferred, stack_value, implied_stack = FALSE) {
  if (!isTRUE(state$active)) {
    state$active <- TRUE
    state$ever_active <- TRUE
    state$start_s <- time_s
    state$start_inferred <- isTRUE(inferred)
    state$initial_state_inferred <- isTRUE(state$initial_state_inferred) || isTRUE(inferred)

    initial <- stack_value
    if (!is.finite(initial) && isTRUE(implied_stack)) {
      initial <- 1
    }
    if (!is.finite(state$initial_stacks) && is.finite(initial)) {
      state$initial_stacks <- initial
    }
  }

  if (is.finite(stack_value)) {
    state$current_stacks <- stack_value
    state$max_stacks <- .wcl_aura_stack_max(state$max_stacks, stack_value)
  } else if (isTRUE(implied_stack) && !is.finite(state$current_stacks)) {
    state$current_stacks <- 1
    state$max_stacks <- .wcl_aura_stack_max(state$max_stacks, 1)
  }

  state
}

.wcl_aura_close_state <- function(state, time_s, ended_at_fight_end = FALSE) {
  if (!isTRUE(state$active)) {
    return(state)
  }

  end_s <- max(state$start_s, time_s)
  if (end_s > state$start_s) {
    state$intervals[[length(state$intervals) + 1L]] <- list(
      start_s = state$start_s,
      end_s = end_s,
      started_at_pull = state$start_s <= 1e-9,
      ended_at_fight_end = isTRUE(ended_at_fight_end),
      initial_state_inferred = isTRUE(state$start_inferred),
      source_ambiguous = isTRUE(state$interval_source_ambiguous)
    )
  }

  state$active <- FALSE
  state$start_s <- NA_real_
  state$start_inferred <- FALSE
  state$current_stacks <- 0
  state$interval_source_ambiguous <- FALSE
  state
}

.wcl_aura_process_state_event <- function(state, action, time_s, stack_value) {
  if (action == "snapshot") {
    return(.wcl_aura_open_state(state, 0, FALSE, stack_value, implied_stack = FALSE))
  }

  if (action %in% c("apply", "apply_stack")) {
    state$n_applications <- state$n_applications + 1L
    state <- .wcl_aura_open_state(state, time_s, FALSE, stack_value, implied_stack = TRUE)
    return(state)
  }

  if (action == "refresh") {
    state$n_refreshes <- state$n_refreshes + 1L
    if (!isTRUE(state$active)) {
      inferred <- !isTRUE(state$ever_active)
      state <- .wcl_aura_open_state(
        state,
        if (inferred) 0 else time_s,
        inferred,
        stack_value,
        implied_stack = TRUE
      )
    } else if (is.finite(stack_value)) {
      state$current_stacks <- stack_value
      state$max_stacks <- .wcl_aura_stack_max(state$max_stacks, stack_value)
    }
    return(state)
  }

  if (action == "remove") {
    if (!isTRUE(state$active) && !isTRUE(state$ever_active)) {
      state <- .wcl_aura_open_state(state, 0, TRUE, NA_real_, implied_stack = TRUE)
    }
    return(.wcl_aura_close_state(state, time_s))
  }

  if (action == "remove_stack") {
    if (!isTRUE(state$active)) {
      inferred <- !isTRUE(state$ever_active)
      state <- .wcl_aura_open_state(
        state,
        if (inferred) 0 else time_s,
        inferred,
        stack_value,
        implied_stack = TRUE
      )
    }
    if (is.finite(stack_value)) {
      state$current_stacks <- stack_value
      state$max_stacks <- .wcl_aura_stack_max(state$max_stacks, stack_value)
      if (stack_value <= 0) {
        state <- .wcl_aura_close_state(state, time_s)
      }
    }
    return(state)
  }

  state
}

.wcl_aura_state_intervals <- function(state) {
  if (!length(state$intervals)) {
    return(tibble::tibble(
      sourceID = integer(),
      sourceInstanceID = integer(),
      start_s = numeric(),
      end_s = numeric(),
      started_at_pull = logical(),
      ended_at_fight_end = logical(),
      initial_state_inferred = logical(),
      source_ambiguous = logical()
    ))
  }

  rows <- lapply(state$intervals, function(interval) {
    tibble::tibble(
      sourceID = state$sourceID,
      sourceInstanceID = state$sourceInstanceID,
      start_s = interval$start_s,
      end_s = interval$end_s,
      started_at_pull = interval$started_at_pull,
      ended_at_fight_end = interval$ended_at_fight_end,
      initial_state_inferred = interval$initial_state_inferred,
      source_ambiguous = interval$source_ambiguous
    )
  })
  dplyr::bind_rows(rows)
}

.wcl_aura_process_group <- function(
    events,
    duration_s,
    allow_source_less_inference = TRUE) {
  order_index <- order(
    events$time_s,
    ifelse(events$action == "snapshot", 0L, 1L),
    events$.row_order,
    na.last = TRUE
  )
  events <- events[order_index, , drop = FALSE]
  states <- list()

  ensure_state <- function(key, source_id, source_instance_id, order) {
    if (is.null(states[[key]])) {
      states[[key]] <<- .wcl_aura_new_state(source_id, source_instance_id, order)
    }
    key
  }

  for (row in seq_len(nrow(events))) {
    action <- events$action[[row]]
    source_id <- events$sourceID[[row]]
    source_instance_id <- events$sourceInstanceID[[row]]
    source_key <- .wcl_aura_source_key(source_id, source_instance_id)
    time_s <- events$time_s[[row]]
    stack_value <- events$stack_value[[row]]

    removal <- action %in% c("remove", "remove_stack")
    target_keys <- character()
    ambiguous <- FALSE

    if (removal && is.na(source_id)) {
      active_keys <- names(states)[vapply(states, function(state) isTRUE(state$active), logical(1))]
      if (length(active_keys)) {
        target_keys <- active_keys
        ambiguous <- length(active_keys) > 1L
      } else if (length(states)) {
        # A later unmatched removal cannot establish another pull-time state
        # after compatible state has already been observed and closed.
        target_keys <- names(states)
        ambiguous <- length(target_keys) > 1L
      } else if (isTRUE(allow_source_less_inference)) {
        target_keys <- ensure_state(source_key, source_id, source_instance_id, events$.row_order[[row]])
      }
    } else if (action == "refresh" && is.na(source_id)) {
      active_keys <- names(states)[vapply(
        states,
        function(state) isTRUE(state$active),
        logical(1)
      )]
      compatible <- if (length(active_keys)) active_keys else names(states)
      if (length(compatible)) {
        target_keys <- compatible
        ambiguous <- length(compatible) > 1L
      } else {
        target_keys <- ensure_state(
          source_key,
          source_id,
          source_instance_id,
          events$.row_order[[row]]
        )
      }
    } else if (removal && !is.na(source_id)) {
      compatible_active <- names(states)[vapply(
        states,
        function(state) {
          same_actor <- isTRUE(state$active) &&
            identical(state$sourceID, as.integer(source_id))
          compatible_instance <- is.na(source_instance_id) ||
            is.na(state$sourceInstanceID) ||
            identical(
              state$sourceInstanceID,
              as.integer(source_instance_id)
            )
          same_actor && compatible_instance
        },
        logical(1)
      )]

      if (length(compatible_active)) {
        target_keys <- compatible_active
        ambiguous <- length(compatible_active) > 1L
      } else {
        unknown_active <- !is.null(states[["unknown"]]) &&
          isTRUE(states[["unknown"]]$active)
        if (unknown_active) {
          target_keys <- "unknown"
          ambiguous <- TRUE
        } else {
          compatible_history <- names(states)[vapply(
            states,
            function(state) {
              same_actor <- identical(
                state$sourceID,
                as.integer(source_id)
              )
              compatible_instance <- is.na(source_instance_id) ||
                is.na(state$sourceInstanceID) ||
                identical(
                  state$sourceInstanceID,
                  as.integer(source_instance_id)
                )
              same_actor && compatible_instance
            },
            logical(1)
          )]
          if (length(compatible_history)) {
            target_keys <- compatible_history
            ambiguous <- length(compatible_history) > 1L
          } else {
            target_keys <- ensure_state(
              source_key,
              source_id,
              source_instance_id,
              events$.row_order[[row]]
            )
          }
        }
      }
    } else if (!is.na(source_id) &&
               (is.na(source_instance_id) || is.null(states[[source_key]]))) {
      # Treat a missing instance ID as a wildcard for the same source actor.
      # This handles CombatantInfo and lifecycle payloads that differ only in
      # whether the optional instance field was serialized.
      compatible <- names(states)[vapply(
        states,
        function(state) {
          same_actor <- identical(state$sourceID, as.integer(source_id))
          same_instance <- is.na(state$sourceInstanceID) ||
            is.na(source_instance_id) ||
            identical(state$sourceInstanceID, as.integer(source_instance_id))
          same_actor && same_instance
        },
        logical(1)
      )]
      compatible_active <- compatible[vapply(
        states[compatible],
        function(state) isTRUE(state$active),
        logical(1)
      )]

      if (length(compatible_active)) {
        target_keys <- compatible_active
        ambiguous <- length(compatible_active) > 1L
      } else if (length(compatible)) {
        target_keys <- compatible
        ambiguous <- length(compatible) > 1L
      } else {
        target_keys <- ensure_state(
          source_key,
          source_id,
          source_instance_id,
          events$.row_order[[row]]
        )
      }

      if (length(target_keys) == 1L && !is.na(source_instance_id)) {
        target_key <- target_keys[[1L]]
        if (is.na(states[[target_key]]$sourceInstanceID)) {
          states[[target_key]]$sourceInstanceID <- as.integer(
            source_instance_id
          )
        }
      }
    } else {
      target_keys <- ensure_state(source_key, source_id, source_instance_id, events$.row_order[[row]])
    }

    for (key in target_keys) {
      state <- states[[key]]
      if (ambiguous) {
        state$source_ambiguous <- TRUE
        # An unmatched removal routed to closed historical states is a useful
        # group-level ambiguity diagnostic, but it must not taint an interval
        # that may be opened later by an exact event.
        if (isTRUE(state$active) || action != "remove" ||
            !isTRUE(state$ever_active)) {
          state$interval_source_ambiguous <- TRUE
        }
      }
      state <- .wcl_aura_process_state_event(state, action, time_s, stack_value)
      states[[key]] <- state
    }
  }

  for (key in names(states)) {
    state <- states[[key]]
    if (isTRUE(state$active)) {
      state$active_at_end <- TRUE
      state <- .wcl_aura_close_state(state, duration_s, ended_at_fight_end = TRUE)
    }
    states[[key]] <- state
  }

  states[order(vapply(states, function(state) state$first_order, numeric(1)))]
}

.wcl_aura_union_intervals <- function(intervals) {
  if (!nrow(intervals)) {
    return(intervals)
  }

  intervals <- intervals[order(intervals$start_s, intervals$end_s), , drop = FALSE]
  output <- list()
  current <- intervals[1L, , drop = FALSE]

  if (nrow(intervals) > 1L) {
    for (row in 2:nrow(intervals)) {
      candidate <- intervals[row, , drop = FALSE]
      if (candidate$start_s[[1L]] <= current$end_s[[1L]] + 1e-9) {
        current$end_s[[1L]] <- max(current$end_s[[1L]], candidate$end_s[[1L]])
        current$started_at_pull[[1L]] <- current$started_at_pull[[1L]] || candidate$started_at_pull[[1L]]
        current$ended_at_fight_end[[1L]] <- current$ended_at_fight_end[[1L]] || candidate$ended_at_fight_end[[1L]]
        current$initial_state_inferred[[1L]] <- current$initial_state_inferred[[1L]] || candidate$initial_state_inferred[[1L]]
        current$source_ambiguous[[1L]] <- current$source_ambiguous[[1L]] || candidate$source_ambiguous[[1L]]
      } else {
        output[[length(output) + 1L]] <- current
        current <- candidate
      }
    }
  }

  output[[length(output) + 1L]] <- current
  dplyr::bind_rows(output)
}

.wcl_aura_state_metric <- function(states, field, fun = sum, default = 0) {
  values <- vapply(states, function(state) state[[field]], numeric(1))
  values <- values[is.finite(values)]
  if (length(values)) fun(values) else default
}

.wcl_aura_metadata_columns <- function(meta) {
  list(
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
    fight_duration_s = meta$duration_s
  )
}

.wcl_aura_summary_row <- function(
    meta,
    group,
    states,
    intervals,
    source_id = NULL,
    source_instance_id = NULL,
    application_count = NULL,
    refresh_count = NULL) {
  uptime <- if (nrow(intervals)) sum(intervals$end_s - intervals$start_s) else 0
  initial_stacks <- .wcl_aura_state_metric(states, "initial_stacks", max, NA_real_)
  max_stacks <- .wcl_aura_state_metric(states, "max_stacks", max, NA_real_)
  base <- .wcl_aura_metadata_columns(meta)
  base$aura_type <- group$aura_type[[1L]]
  base$targetID <- as.integer(group$targetID[[1L]])
  base$targetInstanceID <- as.integer(group$targetInstanceID[[1L]])
  if (!is.null(source_id)) {
    base$sourceID <- as.integer(source_id)
    base$sourceInstanceID <- as.integer(source_instance_id)
  }
  base$abilityGameID <- as.integer(group$abilityGameID[[1L]])
  base$uptime_s <- as.numeric(uptime)
  base$uptime_fraction <- as.numeric(uptime / meta$duration_s)
  base$uptime_pct <- as.numeric(base$uptime_fraction * 100)
  base$interval_count <- as.integer(nrow(intervals))
  if (is.null(application_count)) {
    application_count <- sum(vapply(
      states,
      function(state) state$n_applications,
      integer(1)
    ))
  }
  if (is.null(refresh_count)) {
    refresh_count <- sum(vapply(
      states,
      function(state) state$n_refreshes,
      integer(1)
    ))
  }
  base$application_count <- as.integer(application_count)
  base$refresh_count <- as.integer(refresh_count)
  base$initial_stacks <- initial_stacks
  base$max_stacks <- max_stacks
  base$started_at_pull <- isTRUE(nrow(intervals) > 0L && any(intervals$started_at_pull))
  base$active_at_end <- any(vapply(states, function(state) isTRUE(state$active_at_end), logical(1)))
  base$initial_state_inferred <- any(vapply(states, function(state) isTRUE(state$initial_state_inferred), logical(1)))
  base$source_ambiguous <- any(vapply(states, function(state) isTRUE(state$source_ambiguous), logical(1)))
  tibble::as_tibble(base)
}

.wcl_aura_interval_rows <- function(
    meta,
    group,
    intervals,
    source_id = NULL,
    source_instance_id = NULL) {
  if (!nrow(intervals)) {
    return(NULL)
  }

  output <- vector("list", nrow(intervals))
  for (row in seq_len(nrow(intervals))) {
    base <- .wcl_aura_metadata_columns(meta)
    base$aura_type <- group$aura_type[[1L]]
    base$targetID <- as.integer(group$targetID[[1L]])
    base$targetInstanceID <- as.integer(group$targetInstanceID[[1L]])
    if (!is.null(source_id)) {
      base$sourceID <- as.integer(source_id)
      base$sourceInstanceID <- as.integer(source_instance_id)
    }
    base$abilityGameID <- as.integer(group$abilityGameID[[1L]])
    base$interval_id <- as.integer(row)
    base$interval_start_ms <- if (is.finite(meta$fight_start_ms)) {
      meta$fight_start_ms + intervals$start_s[[row]] * 1000
    } else {
      NA_real_
    }
    base$interval_end_ms <- if (is.finite(meta$fight_start_ms)) {
      meta$fight_start_ms + intervals$end_s[[row]] * 1000
    } else {
      NA_real_
    }
    base$interval_start_s <- intervals$start_s[[row]]
    base$interval_end_s <- intervals$end_s[[row]]
    base$interval_start_at <- if (is.na(meta$fight_start_at)) .wcl_aura_empty_time(1L) else meta$fight_start_at + intervals$start_s[[row]]
    base$interval_end_at <- if (is.na(meta$fight_start_at)) .wcl_aura_empty_time(1L) else meta$fight_start_at + intervals$end_s[[row]]
    base$interval_duration_s <- intervals$end_s[[row]] - intervals$start_s[[row]]
    base$started_at_pull <- intervals$started_at_pull[[row]]
    base$ended_at_fight_end <- intervals$ended_at_fight_end[[row]]
    base$initial_state_inferred <- intervals$initial_state_inferred[[row]]
    base$source_ambiguous <- intervals$source_ambiguous[[row]]
    output[[row]] <- tibble::as_tibble(base)
  }

  dplyr::bind_rows(output)
}

#' Calculate buff and debuff uptime from Warcraft Logs event rows
#'
#' `wcl_aura_uptime()` pairs Warcraft Logs aura lifecycle events into active
#' intervals and summarizes those intervals over the full fight. It supports
#' buffs, debuffs, stack events, and pull-time aura snapshots from CombatantInfo
#' events. Only target/ability combinations observed in the supplied data are
#' returned; the function does not manufacture zero-uptime combinations.
#'
#' @param events A complete, fully paginated, full-fight data frame returned by
#'   [wcl_events()] containing buff or debuff lifecycle events. It may also
#'   contain unrelated event types and inline `combatantinfo` rows; unrelated
#'   rows are ignored.
#' @param combatant_info Optional separate data frame returned by
#'   `wcl_events(..., data_type = "CombatantInfo")`. CombatantInfo `auras`
#'   establish which auras were active at the start of the fight. When omitted,
#'   inline `combatantinfo` rows in `events` are used.
#' @param ability_ids Optional integer-like vector of ability game IDs to keep.
#' @param target_ids Optional integer-like vector of target report actor IDs to
#'   keep.
#' @param source_ids Optional integer-like vector of aura source (caster) report
#'   actor IDs to keep. Filtering is applied before source intervals are
#'   combined, even when `by_source = FALSE`.
#' @param by_source Whether to return separate uptime for each caster. The
#'   default, `FALSE`, unions overlapping caster intervals for each target and
#'   ability. Must be one non-missing logical value.
#' @param output Output form: `"summary"` (the default) returns one row per
#'   observed aura combination; `"intervals"` returns each active interval.
#'
#' @details
#' Supported lifecycle types are `applybuff`, `refreshbuff`, `removebuff`,
#' `applybuffstack`, and `removebuffstack`, plus the corresponding debuff types.
#' A positive `remove*stack` count leaves the aura active; an explicit zero
#' closes it. CombatantInfo entries are treated as pull-time state regardless
#' of their event timestamp. Consequently, an aura present in the snapshot and
#' never removed has 100 percent fight uptime. If no matching snapshot was
#' supplied, a leading refresh or removal is inferred to mean that the aura was
#' already active at pull, and the result is flagged in
#' `initial_state_inferred`.
#'
#' Supply the complete lifecycle for the fight: use `paginate = TRUE` and do
#' not narrow `start_time` or `end_time`. With a partial download, a removal at
#' the beginning of the slice can be mistaken for pull-time activity, and an
#' application whose removal is outside the slice can be extended to fight end.
#'
#' CombatantInfo arrays are documented by Warcraft Logs as containing an
#' ability game ID, source actor, initial stacks, name, and icon. The event
#' payload itself is an opaque JSON scalar, so this helper accepts the current
#' documented names and common ID-encoded aliases. Fetch source-aware data with
#' the [wcl_events()] defaults `use_actor_ids = TRUE` and
#' `use_ability_ids = TRUE`. In a CombatantInfo event, the outer `sourceID` is
#' the player carrying the aura and becomes output `targetID`; the nested aura
#' `source` is its caster and becomes output `sourceID`. A snapshot-only aura is
#' retained with `aura_type = "unknown"`. If matching lifecycle events identify
#' both buff and debuff families, the kind remains unknown and a warning is
#' emitted.
#'
#' See the Warcraft Logs
#' [CombatantInfoEvent documentation](https://www.warcraftlogs.com/scripting-api-docs/warcraft/interfaces/RpgLogs.CombatantInfoEvent.html),
#' [CombatantInfoAura documentation](https://www.warcraftlogs.com/scripting-api-docs/warcraft/types/RpgLogs.CombatantInfoAura.html),
#' and [event type documentation](https://www.warcraftlogs.com/v2-api-docs/warcraft/eventdatatype.doc.html).
#'
#' @return A tibble. Summary output includes report and fight metadata,
#'   `aura_type`, `targetID`, `targetInstanceID`, `abilityGameID`, `uptime_s`,
#'   `uptime_fraction` (zero to one), `uptime_pct` (zero to 100),
#'   `interval_count`, application/refresh counts, stack diagnostics, and
#'   boundary/source flags. `source_ambiguous` indicates that at least one
#'   source-less event could not be assigned to one caster unambiguously.
#'   Interval output
#'   identifies each interval with `interval_id`, report-relative
#'   `interval_start_ms`/`interval_end_ms`, fight-relative
#'   `interval_start_s`/`interval_end_s`, absolute
#'   `interval_start_at`/`interval_end_at`, and `interval_duration_s`.
#'   `sourceID` and `sourceInstanceID` are present when `by_source = TRUE`.
#' @export
wcl_aura_uptime <- function(
    events,
    combatant_info = NULL,
    ability_ids = NULL,
    target_ids = NULL,
    source_ids = NULL,
    by_source = FALSE,
    output = c("summary", "intervals")) {
  by_source <- .wcl_aura_scalar_logical(by_source, "by_source")
  output <- match.arg(output)
  ability_ids <- .wcl_aura_id_filter(ability_ids, "ability_ids")
  target_ids <- .wcl_aura_id_filter(target_ids, "target_ids")
  source_ids <- .wcl_aura_id_filter(source_ids, "source_ids")

  if (!inherits(events, "data.frame")) {
    stop("`events` must be a data frame.", call. = FALSE)
  }
  if (!is.null(combatant_info) && !inherits(combatant_info, "data.frame")) {
    stop("`combatant_info` must be NULL or a data frame.", call. = FALSE)
  }

  empty <- if (output == "summary") {
    .wcl_aura_empty_summary(by_source)
  } else {
    .wcl_aura_empty_intervals(by_source)
  }

  prepared_events <- .wcl_aura_prepare_input(events)
  prepared_combatant <- if (is.null(combatant_info)) {
    prepared_events[0, , drop = FALSE]
  } else {
    .wcl_aura_prepare_input(
      combatant_info,
      force_combatant = TRUE,
      order_offset = nrow(prepared_events)
    )
  }
  rows <- dplyr::bind_rows(prepared_events, prepared_combatant)

  relevant_types <- c(
    "combatantinfo",
    "applybuff", "applybuffstack", "refreshbuff", "removebuff", "removebuffstack",
    "applydebuff", "applydebuffstack", "refreshdebuff", "removedebuff", "removedebuffstack"
  )
  relevant <- rows[rows$type %in% relevant_types, , drop = FALSE]
  if (!nrow(relevant)) {
    return(empty)
  }

  if (any(is.na(relevant$logID) | !nzchar(relevant$logID)) || any(is.na(relevant$fightID))) {
    stop("Aura event rows must include non-missing `logID` and `fightID` values.", call. = FALSE)
  }

  fight_keys <- .wcl_aura_fight_key(relevant$logID, relevant$fightID)
  fight_meta <- list()
  for (key in unique(fight_keys)) {
    fight_meta[[key]] <- .wcl_aura_fight_meta(relevant[fight_keys == key, , drop = FALSE])
  }

  lifecycle <- .wcl_aura_lifecycle(relevant, fight_meta)
  snapshots <- .wcl_aura_snapshots(relevant)
  aura_events <- dplyr::bind_rows(lifecycle, snapshots)
  if (!nrow(aura_events)) {
    return(empty)
  }

  aura_events <- .wcl_aura_reconcile_instances(aura_events)
  aura_events <- .wcl_aura_classify_snapshots(aura_events)

  if (!is.null(ability_ids)) {
    aura_events <- aura_events[aura_events$abilityGameID %in% ability_ids, , drop = FALSE]
  }
  if (!is.null(target_ids)) {
    aura_events <- aura_events[aura_events$targetID %in% target_ids, , drop = FALSE]
  }
  if (!nrow(aura_events)) {
    return(empty)
  }

  if (!is.null(source_ids)) {
    known_source <- !is.na(aura_events$sourceID)
    selected_source <- known_source & aura_events$sourceID %in% source_ids
    removal_without_source <- !known_source &
      aura_events$action %in% c("remove", "remove_stack")

    # A source-less removal may close a selected source only when that source
    # has already established matching state. Otherwise retaining the row
    # would fabricate a pull-to-removal interval for an unknown caster.
    source_filter_key <- .wcl_aura_combo_key(
      aura_events$logID,
      aura_events$fightID,
      aura_events$targetID,
      aura_events$targetInstanceID,
      aura_events$abilityGameID,
      aura_events$aura_type
    )
    wildcard_removal <- rep.int(FALSE, nrow(aura_events))
    for (row in which(removal_without_source)) {
      prior_selected <- selected_source &
        source_filter_key == source_filter_key[[row]] &
        aura_events$time_s <= aura_events$time_s[[row]]
      wildcard_removal[[row]] <- any(prior_selected)
    }

    unresolved <- !known_source & !wildcard_removal
    if (any(unresolved)) {
      warning(
        sprintf("Ignored %d aura event/snapshot row(s) whose source could not be matched to `source_ids`.", sum(unresolved)),
        call. = FALSE
      )
    }
    aura_events <- aura_events[selected_source | wildcard_removal, , drop = FALSE]
  }
  if (!nrow(aura_events)) {
    return(empty)
  }

  fight_key <- .wcl_aura_fight_key(aura_events$logID, aura_events$fightID)
  outside <- logical(nrow(aura_events))
  for (row in seq_len(nrow(aura_events))) {
    duration <- fight_meta[[fight_key[[row]]]]$duration_s
    outside[[row]] <- aura_events$time_s[[row]] < -1e-6 || aura_events$time_s[[row]] > duration + 1e-6
    aura_events$time_s[[row]] <- min(duration, max(0, aura_events$time_s[[row]]))
  }
  if (any(outside)) {
    warning(sprintf("Clamped %d aura event(s) to their fight boundaries.", sum(outside)), call. = FALSE)
  }

  duplicate_key <- paste(
    aura_events$logID,
    aura_events$fightID,
    aura_events$targetID,
    ifelse(is.na(aura_events$targetInstanceID), "NA", aura_events$targetInstanceID),
    ifelse(is.na(aura_events$sourceID), "NA", aura_events$sourceID),
    ifelse(is.na(aura_events$sourceInstanceID), "NA", aura_events$sourceInstanceID),
    aura_events$abilityGameID,
    aura_events$aura_type,
    aura_events$action,
    format(aura_events$time_s, digits = 17, scientific = FALSE),
    ifelse(is.na(aura_events$stack_value), "NA", aura_events$stack_value),
    sep = "\u001e"
  )
  aura_events <- aura_events[!duplicated(duplicate_key), , drop = FALSE]

  combo_key <- .wcl_aura_combo_key(
    aura_events$logID,
    aura_events$fightID,
    aura_events$targetID,
    aura_events$targetInstanceID,
    aura_events$abilityGameID,
    aura_events$aura_type
  )
  output_rows <- list()
  inferred_combinations <- 0L

  for (key in unique(combo_key)) {
    group <- aura_events[combo_key == key, , drop = FALSE]
    meta_key <- .wcl_aura_fight_key(group$logID[[1L]], group$fightID[[1L]])
    meta <- fight_meta[[meta_key]]
    states <- .wcl_aura_process_group(
      group,
      meta$duration_s,
      allow_source_less_inference = is.null(source_ids)
    )
    if (!length(states)) {
      next
    }

    if (any(vapply(states, function(state) isTRUE(state$initial_state_inferred), logical(1)))) {
      inferred_combinations <- inferred_combinations + 1L
    }

    if (isTRUE(by_source)) {
      for (state in states) {
        intervals <- .wcl_aura_state_intervals(state)
        if (output == "summary") {
          output_rows[[length(output_rows) + 1L]] <- .wcl_aura_summary_row(
            meta,
            group,
            list(state),
            intervals,
            source_id = state$sourceID,
            source_instance_id = state$sourceInstanceID
          )
        } else {
          interval_rows <- .wcl_aura_interval_rows(
            meta,
            group,
            intervals,
            source_id = state$sourceID,
            source_instance_id = state$sourceInstanceID
          )
          if (!is.null(interval_rows)) {
            output_rows[[length(output_rows) + 1L]] <- interval_rows
          }
        }
      }
    } else {
      intervals <- dplyr::bind_rows(lapply(states, .wcl_aura_state_intervals))
      intervals <- .wcl_aura_union_intervals(intervals)
      if (output == "summary") {
        output_rows[[length(output_rows) + 1L]] <- .wcl_aura_summary_row(
          meta,
          group,
          states,
          intervals,
          application_count = sum(group$action %in% c("apply", "apply_stack")),
          refresh_count = sum(group$action == "refresh")
        )
      } else {
        interval_rows <- .wcl_aura_interval_rows(meta, group, intervals)
        if (!is.null(interval_rows)) {
          output_rows[[length(output_rows) + 1L]] <- interval_rows
        }
      }
    }
  }

  if (inferred_combinations) {
    warning(
      sprintf(
        "Initial aura state was inferred from a leading refresh/removal for %d observed combination(s) without a matching CombatantInfo snapshot.",
        inferred_combinations
      ),
      call. = FALSE
    )
  }

  if (!length(output_rows)) {
    return(empty)
  }

  result <- dplyr::bind_rows(output_rows)
  result[names(empty)]
}
