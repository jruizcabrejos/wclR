.wcl_query_reports_page <- function(zone_id, page = 1L) {
  sprintf(
    paste0(
      "{",
      " reportData {",
      "  reports(zoneID: %s, page: %s) {",
      "   data {",
      "    code",
      "    title",
      "    visibility",
      "    region { name }",
      "    revision",
      "    segments",
      "    startTime",
      "    endTime",
      "   }",
      "   total",
      "   per_page",
      "   current_page",
      "   last_page",
      "   has_more_pages",
      "  }",
      " }",
      "}"
    ),
    .wcl_graphql_int(zone_id),
    .wcl_graphql_int(page)
  )
}

.wcl_query_fights <- function(report_code) {
  report_code <- wcl_report_code(report_code)

  sprintf(
    paste0(
      "{",
      " reportData {",
      "  report(code: %s) {",
      "   code",
      "   title",
      "   startTime",
      "   endTime",
      "   fights(killType: Encounters) {",
      "    id",
      "    name",
      "    encounterID",
      "    difficulty",
      "    hardModeLevel",
      "    averageItemLevel",
      "    size",
      "    kill",
      "    lastPhase",
      "    startTime",
      "    endTime",
      "    fightPercentage",
      "    bossPercentage",
      "    completeRaid",
      "    inProgress",
      "   }",
      "  }",
      " }",
      "}"
    ),
    .wcl_graphql_string(report_code)
  )
}

.wcl_query_actors <- function(report_code, type = NULL) {
  report_code <- wcl_report_code(report_code)
  actor_args <- NULL

  if (!is.null(type)) {
    actor_args <- paste0("type: ", .wcl_graphql_string(type))
  }

  actor_clause <- if (is.null(actor_args)) "actors" else paste0("actors(", actor_args, ")")

  sprintf(
    paste0(
      "{",
      " reportData {",
      "  report(code: %s) {",
      "   code",
      "   title",
      "   startTime",
      "   endTime",
      "   masterData(translate: true) {",
      "    ", actor_clause, " {",
      "     id",
      "     gameID",
      "     name",
      "     server",
      "     subType",
      "     type",
      "     icon",
      "     petOwner",
      "    }",
      "   }",
      "  }",
      " }",
      "}"
    ),
    .wcl_graphql_string(report_code)
  )
}

.wcl_query_player_details <- function(report_code, fight_id) {
  report_code <- wcl_report_code(report_code)
  fight_id <- .wcl_graphql_int(fight_id)

  sprintf(
    paste0(
      "{",
      " reportData {",
      "  report(code: %s) {",
      "   code",
      "   title",
      "   startTime",
      "   endTime",
      "   fights(fightIDs: %s) {",
      "    id",
      "    name",
      "    encounterID",
      "    difficulty",
      "    hardModeLevel",
      "    averageItemLevel",
      "    size",
      "    kill",
      "    lastPhase",
      "    startTime",
      "    endTime",
      "    fightPercentage",
      "    bossPercentage",
      "    completeRaid",
      "    inProgress",
      "   }",
      "   playerDetails(",
      "    fightIDs: %s,",
      "    translate: true,",
      "    includeCombatantInfo: true",
      "   )",
      "  }",
      " }",
      "}"
    ),
    .wcl_graphql_string(report_code),
    fight_id,
    fight_id
  )
}

.wcl_query_events <- function(
    report_code,
    fight_id,
    data_type,
    kill_type = "Encounters",
    hostility_type = "All",
    source_id = NULL,
    target_id = NULL,
    filter_expression = NULL,
    start_time = 0,
    end_time = 999999999999,
    include_resources = TRUE) {
  report_code <- wcl_report_code(report_code)
  fight_id <- .wcl_graphql_int(fight_id)

  args <- c(
    paste0("dataType: ", .wcl_graphql_enum(data_type)),
    paste0("fightIDs: ", fight_id),
    paste0("killType: ", .wcl_graphql_enum(kill_type)),
    paste0("hostilityType: ", .wcl_graphql_enum(hostility_type)),
    paste0("startTime: ", .wcl_graphql_num(start_time)),
    paste0("endTime: ", .wcl_graphql_num(end_time)),
    paste0("includeResources: ", .wcl_graphql_bool(include_resources)),
    if (!is.null(source_id)) paste0("sourceID: ", .wcl_graphql_int(source_id)),
    if (!is.null(target_id)) paste0("targetID: ", .wcl_graphql_int(target_id)),
    if (!is.null(filter_expression)) paste0("filterExpression: ", .wcl_graphql_string(filter_expression))
  )

  event_args <- paste(stats::na.omit(args), collapse = ", ")

  sprintf(
    paste0(
      "{",
      " reportData {",
      "  report(code: %s) {",
      "   code",
      "   title",
      "   startTime",
      "   endTime",
      "   fights(fightIDs: %s) {",
      "    id",
      "    name",
      "    encounterID",
      "    difficulty",
      "    hardModeLevel",
      "    averageItemLevel",
      "    size",
      "    kill",
      "    lastPhase",
      "    startTime",
      "    endTime",
      "    fightPercentage",
      "    bossPercentage",
      "    completeRaid",
      "    inProgress",
      "   }",
      "   events(", event_args, ") {",
      "    data",
      "    nextPageTimestamp",
      "   }",
      "  }",
      " }",
      "}"
    ),
    .wcl_graphql_string(report_code),
    fight_id
  )
}

.wcl_query_rankings <- function(zone_id, metric, page, class_name = NULL) {
  query_name <- if (is.null(class_name)) "fightRankings" else "characterRankings"
  args <- c(
    paste0("metric: ", .wcl_graphql_metric(metric)),
    paste0("page: ", .wcl_graphql_int(page)),
    if (!is.null(class_name)) paste0("className: ", .wcl_graphql_string(class_name))
  )

  sprintf(
    paste0(
      "{",
      " worldData {",
      "  zone(id: %s) {",
      "   encounters {",
      "    journalID",
      "    name",
      "    ", query_name, "(", paste(stats::na.omit(args), collapse = ", "), ")",
      "   }",
      "  }",
      " }",
      "}"
    ),
    .wcl_graphql_int(zone_id)
  )
}
