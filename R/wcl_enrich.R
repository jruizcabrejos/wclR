.wcl_ensure_columns <- function(df, defaults) {
  n <- nrow(df)

  for (name in names(defaults)) {
    if (!name %in% names(df)) {
      df[[name]] <- rep(defaults[[name]], n)
    }
  }

  df
}

.wcl_normalize_join_ids <- function(df) {
  df <- tibble::as_tibble(df)

  for (col in intersect(c("logID"), names(df))) {
    df[[col]] <- as.character(df[[col]])
  }

  for (col in intersect(c("fightID", "actorID", "sourceID", "targetID"), names(df))) {
    df[[col]] <- suppressWarnings(as.integer(df[[col]]))
  }

  df
}

.wcl_split_icon <- function(icon) {
  values <- as.character(icon)
  values[is.na(values)] <- ""
  pieces <- strsplit(values, "-", fixed = TRUE)

  class <- vapply(
    pieces,
    function(item) {
      if (length(item) >= 1L && nzchar(item[[1L]])) item[[1L]] else NA_character_
    },
    character(1)
  )

  spec <- vapply(
    pieces,
    function(item) {
      if (length(item) >= 2L) paste(item[-1L], collapse = "-") else NA_character_
    },
    character(1)
  )
  spec[!nzchar(spec)] <- NA_character_

  list(class = class, spec = spec)
}

.wcl_join_by <- function(data, lookup, local_id, remote_id, include_fight = FALSE) {
  if (!local_id %in% names(data) || !remote_id %in% names(lookup)) {
    return(NULL)
  }

  by <- c()

  if ("logID" %in% names(data) && "logID" %in% names(lookup)) {
    by <- c(by, logID = "logID")
  }

  if (isTRUE(include_fight) && "fightID" %in% names(data) && "fightID" %in% names(lookup)) {
    by <- c(by, fightID = "fightID")
  }

  by <- c(by, stats::setNames(remote_id, local_id))
  by
}

.wcl_actor_lookup <- function(actors, prefix) {
  actors <- .wcl_normalize_join_ids(actors)
  actors <- .wcl_ensure_columns(
    actors,
    list(
      logID = NA_character_,
      actorID = NA_integer_,
      name = NA_character_,
      subType = NA_character_,
      icon = NA_character_,
      server = NA_character_,
      type = NA_character_,
      gameID = NA_integer_
    )
  )

  actors <- actors[!duplicated(actors[c("logID", "actorID")]), , drop = FALSE]
  actors <- actors[c("logID", "actorID", "name", "subType", "icon", "server", "type", "gameID")]

  names(actors)[names(actors) == "name"] <- paste0(prefix, "_name")
  names(actors)[names(actors) == "subType"] <- paste0(prefix, "_subType")
  names(actors)[names(actors) == "icon"] <- paste0(prefix, "_icon")
  names(actors)[names(actors) == "server"] <- paste0(prefix, "_server")
  names(actors)[names(actors) == "type"] <- paste0(prefix, "_type")
  names(actors)[names(actors) == "gameID"] <- paste0(prefix, "_gameID")

  actors
}

.wcl_player_lookup <- function(players, prefix = NULL) {
  players <- .wcl_normalize_join_ids(players)
  players <- .wcl_ensure_columns(
    players,
    list(
      logID = NA_character_,
      fightID = NA_integer_,
      actorID = NA_integer_,
      name = NA_character_,
      role = NA_character_,
      icon = NA_character_,
      type = NA_character_,
      subType = NA_character_,
      server = NA_character_
    )
  )

  icon_parts <- .wcl_split_icon(players$icon)
  players$player_class <- icon_parts$class
  players$player_spec <- icon_parts$spec

  players <- players[!duplicated(players[c("logID", "fightID", "actorID")]), , drop = FALSE]
  players <- players[c(
    "logID",
    "fightID",
    "actorID",
    "name",
    "role",
    "player_class",
    "player_spec",
    "icon",
    "type",
    "subType",
    "server"
  )]

  if (is.null(prefix) || !nzchar(prefix)) {
    names(players)[names(players) == "name"] <- "player_name"
    names(players)[names(players) == "role"] <- "player_role"
    names(players)[names(players) == "player_class"] <- "player_class"
    names(players)[names(players) == "player_spec"] <- "player_spec"
    names(players)[names(players) == "icon"] <- "player_icon"
    names(players)[names(players) == "type"] <- "player_type"
    names(players)[names(players) == "subType"] <- "player_subType"
    names(players)[names(players) == "server"] <- "player_server"
    return(players)
  }

  names(players)[names(players) == "name"] <- paste0(prefix, "_name")
  names(players)[names(players) == "role"] <- paste0(prefix, "_role")
  names(players)[names(players) == "player_class"] <- paste0(prefix, "_class")
  names(players)[names(players) == "player_spec"] <- paste0(prefix, "_spec")
  names(players)[names(players) == "icon"] <- paste0(prefix, "_icon")
  names(players)[names(players) == "type"] <- paste0(prefix, "_type")
  names(players)[names(players) == "subType"] <- paste0(prefix, "_subType")
  names(players)[names(players) == "server"] <- paste0(prefix, "_server")

  players
}

#' Join source and target actor metadata onto event rows
#'
#' @param events A tibble with event-style actor ID columns such as `sourceID`
#'   and `targetID`.
#' @param actors A tibble returned by [wcl_actors()] or a compatible data frame.
#'
#' @return The input `events` tibble with actor metadata columns appended for
#'   each actor ID column that was present.
#' @export
wcl_join_actors <- function(events, actors) {
  out <- .wcl_normalize_join_ids(events)
  actors <- .wcl_normalize_join_ids(actors)

  if ("sourceID" %in% names(out)) {
    source_lookup <- .wcl_actor_lookup(actors, prefix = "source_actor")
    source_by <- .wcl_join_by(out, source_lookup, local_id = "sourceID", remote_id = "actorID")
    if (!is.null(source_by)) {
      out <- dplyr::left_join(out, source_lookup, by = source_by)
    }
  }

  if ("targetID" %in% names(out)) {
    target_lookup <- .wcl_actor_lookup(actors, prefix = "target_actor")
    target_by <- .wcl_join_by(out, target_lookup, local_id = "targetID", remote_id = "actorID")
    if (!is.null(target_by)) {
      out <- dplyr::left_join(out, target_lookup, by = target_by)
    }
  }

  out
}

#' Join player roster metadata onto actor or event rows
#'
#' @param data A tibble with one or more of `actorID`, `sourceID`, or `targetID`.
#' @param players A tibble returned by [wcl_player_details()] or a compatible
#'   data frame.
#'
#' @return The input `data` tibble with roster metadata appended for each actor
#'   ID column that was present.
#' @export
wcl_join_players <- function(data, players) {
  out <- .wcl_normalize_join_ids(data)
  players <- .wcl_normalize_join_ids(players)

  if ("actorID" %in% names(out)) {
    actor_lookup <- .wcl_player_lookup(players, prefix = NULL)
    actor_by <- .wcl_join_by(out, actor_lookup, local_id = "actorID", remote_id = "actorID", include_fight = TRUE)
    if (!is.null(actor_by)) {
      out <- dplyr::left_join(out, actor_lookup, by = actor_by)
    }
  }

  if ("sourceID" %in% names(out)) {
    source_lookup <- .wcl_player_lookup(players, prefix = "source_player")
    source_by <- .wcl_join_by(out, source_lookup, local_id = "sourceID", remote_id = "actorID", include_fight = TRUE)
    if (!is.null(source_by)) {
      out <- dplyr::left_join(out, source_lookup, by = source_by)
    }
  }

  if ("targetID" %in% names(out)) {
    target_lookup <- .wcl_player_lookup(players, prefix = "target_player")
    target_by <- .wcl_join_by(out, target_lookup, local_id = "targetID", remote_id = "actorID", include_fight = TRUE)
    if (!is.null(target_by)) {
      out <- dplyr::left_join(out, target_lookup, by = target_by)
    }
  }

  out
}

#' Add relative fight timing to an event table
#'
#' @param events A tibble with event timing columns.
#' @param unit Output unit. One of `"seconds"`, `"milliseconds"`, or
#'   `"minutes"`.
#'
#' @return The input `events` tibble with a numeric `time_since_fight_start`
#'   column.
#' @export
wcl_add_relative_time <- function(events, unit = c("seconds", "milliseconds", "minutes")) {
  out <- tibble::as_tibble(events)
  unit <- match.arg(unit)

  if (!nrow(out)) {
    out$time_since_fight_start <- numeric()
    return(out)
  }

  if (all(c("timestamp", "startTime") %in% names(out))) {
    delta_ms <- as.numeric(out$timestamp) - as.numeric(out$startTime)
  } else if (all(c("event_at", "fight_start_at") %in% names(out))) {
    delta_ms <- as.numeric(difftime(out$event_at, out$fight_start_at, units = "secs")) * 1000
  } else {
    stop(
      "`events` must include either `timestamp` and `startTime`, or `event_at` and `fight_start_at`.",
      call. = FALSE
    )
  }

  scale <- switch(
    unit,
    milliseconds = 1,
    seconds = 1000,
    minutes = 60000
  )

  out$time_since_fight_start <- delta_ms / scale
  out
}
