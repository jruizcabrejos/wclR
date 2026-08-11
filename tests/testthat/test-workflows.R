test_that("wcl_zone_fights combines report and fight metadata", {
  ns <- asNamespace("wclR")

  reports_fixture <- tibble::tibble(
    logID = c("ABC123", "DEF456"),
    visibility = c("public", "public"),
    region = c("US", "EU"),
    revision = c(1L, 2L),
    segments = c(1L, 1L),
    startTime = c(1700000000000, 1700007200000),
    endTime = c(1700003600000, 1700010800000),
    report_start_at = as.POSIXct(c(1700000000, 1700007200), origin = "1970-01-01", tz = "UTC"),
    report_end_at = as.POSIXct(c(1700003600, 1700010800), origin = "1970-01-01", tz = "UTC"),
    report_link = c("https://classic.warcraftlogs.com/reports/ABC123", "https://classic.warcraftlogs.com/reports/DEF456"),
    zone_id = c(1020L, 1020L),
    page = c(1L, 2L),
    current_page = c(1L, 2L),
    last_page = c(2L, 2L),
    has_more_pages = c(TRUE, FALSE),
    per_page = c(50L, 50L),
    total = c(2L, 2L)
  )

  fights_fixture <- list(
    ABC123 = tibble::tibble(
      logID = c("ABC123", "ABC123"),
      report_title = c("Raid One", "Raid One"),
      report_link = c("https://classic.warcraftlogs.com/reports/ABC123#fight=7", "https://classic.warcraftlogs.com/reports/ABC123#fight=8"),
      report_start_at = as.POSIXct(c(1700000000, 1700000000), origin = "1970-01-01", tz = "UTC"),
      report_end_at = as.POSIXct(c(1700003600, 1700003600), origin = "1970-01-01", tz = "UTC"),
      fightID = c(7L, 8L),
      encounterID = c(855L, 855L),
      encounterName = c("Sindragosa", "Sindragosa"),
      difficulty = c(4L, 4L),
      hardModeLevel = c(0L, 0L),
      averageItemLevel = c(264.3, 264.3),
      size = c(25L, 25L),
      kill = c(TRUE, TRUE),
      lastPhase = c(3L, 3L),
      startTime = c(1000, 1000),
      endTime = c(241000, 241000),
      fight_start_at = as.POSIXct(c(1700000001, 1700000001), origin = "1970-01-01", tz = "UTC"),
      fight_end_at = as.POSIXct(c(1700000241, 1700000241), origin = "1970-01-01", tz = "UTC"),
      duration = c(240000, 240000),
      duration_s = c(240, 240),
      fightPercentage = c(0.01, 0.01),
      bossPercentage = c(0.01, 0.01),
      completeRaid = c(FALSE, FALSE),
      inProgress = c(FALSE, FALSE)
    ),
    DEF456 = tibble::tibble(
      logID = "DEF456",
      report_title = "Raid Two",
      report_link = "https://classic.warcraftlogs.com/reports/DEF456#fight=9",
      report_start_at = as.POSIXct(1700007200, origin = "1970-01-01", tz = "UTC"),
      report_end_at = as.POSIXct(1700010800, origin = "1970-01-01", tz = "UTC"),
      fightID = 9L,
      encounterID = 856L,
      encounterName = "The Lich King",
      difficulty = 4L,
      hardModeLevel = 0L,
      averageItemLevel = 264.3,
      size = 25L,
      kill = FALSE,
      lastPhase = 2L,
      startTime = 2000,
      endTime = 302000,
      fight_start_at = as.POSIXct(1700007202, origin = "1970-01-01", tz = "UTC"),
      fight_end_at = as.POSIXct(1700007502, origin = "1970-01-01", tz = "UTC"),
      duration = 300000,
      duration_s = 300,
      fightPercentage = 50,
      bossPercentage = 50,
      completeRaid = FALSE,
      inProgress = FALSE
    )
  )

  seen_pages <- "sentinel"

  local_mocked_bindings(
    wcl_reports = function(zone_id, pages = NULL, client = NULL) {
      seen_pages <<- pages
      reports_fixture
    },
    wcl_fights = function(report_code, client = NULL) fights_fixture[[as.character(report_code)]],
    .env = ns,
    .package = "wclR"
  )

  deduplicated <- wclR::wcl_zone_fights(1020, client = mock_client())
  all_rows <- wclR::wcl_zone_fights(1020, client = mock_client(), distinct = FALSE)

  expect_equal(seen_pages, 1:3)
  expect_equal(nrow(deduplicated), 2L)
  expect_equal(nrow(all_rows), 3L)
  expect_true(all(c("zone_id", "report_page", "report_visibility", "report_region") %in% names(deduplicated)))
})

test_that("wcl_zone_fights returns a typed empty tibble when no reports are returned", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    wcl_reports = function(zone_id, pages = NULL, client = NULL) wclR:::.wcl_empty_reports(),
    .env = ns,
    .package = "wclR"
  )

  out <- wclR::wcl_zone_fights(1020, pages = 1:2, client = mock_client())

  expect_equal(nrow(out), 0L)
  expect_true(all(c("fightID", "zone_id", "report_page") %in% names(out)))
})

test_that("wcl_rankings_set iterates over metrics and pages", {
  ns <- asNamespace("wclR")
  calls <- list()

  local_mocked_bindings(
    wcl_rankings = function(zone_id, metric, page, encounter_id = NULL, class_name = NULL, client = NULL) {
      calls[[length(calls) + 1L]] <<- list(
        metric = metric,
        page = page,
        encounter_id = encounter_id,
        class_name = class_name
      )

      tibble::tibble(
        zone_id = rep.int(as.integer(zone_id), length(page)),
        encounterID = rep.int(as.integer(if (is.null(encounter_id)) 641L else encounter_id), length(page)),
        encounterName = rep.int("Twin Val'kyr", length(page)),
        metric = rep.int(metric, length(page)),
        ranking_type = rep.int(if (is.null(class_name)) "fight" else "character", length(page)),
        page = as.integer(page),
        rank = seq_along(page),
        report_code = paste0(metric, "_", page),
        amount = seq_along(page),
        name = rep.int("Example", length(page))
      )
    },
    .env = ns,
    .package = "wclR"
  )

  out <- wclR::wcl_rankings_set(
    zone_id = 1018,
    metrics = c("speed", "progress"),
    pages = 1:2,
    encounter_id = 641,
    class_name = "Priest",
    client = mock_client()
  )

  expect_equal(nrow(out), 4L)
  expect_equal(length(calls), 2L)
  expect_equal(sort(unique(out$metric)), c("progress", "speed"))
  expect_true(all(out$ranking_type == "character"))
  expect_true(all(vapply(calls, function(x) identical(x$encounter_id, 641), logical(1))))
})

test_that("join helpers preserve rows and keep source and target joins separate", {
  events <- tibble::tibble(
    logID = c("ABC123", "ABC123"),
    fightID = c(7L, 7L),
    sourceID = c(1L, 1L),
    targetID = c(2L, 99L),
    amount = c(100L, 200L)
  )

  actors <- tibble::tibble(
    logID = c("ABC123", "ABC123"),
    actorID = c(1L, 2L),
    name = c("Vivax", "Sindragosa"),
    subType = c("Shadow", "Boss"),
    icon = c("Priest-Shadow", "Boss"),
    server = c("Pagle", NA_character_),
    type = c("Player", "NPC"),
    gameID = c(12345L, 855L)
  )

  players <- tibble::tibble(
    logID = c("ABC123", "ABC123"),
    fightID = c(7L, 7L),
    actorID = c(1L, 2L),
    name = c("Vivax", "Tanky"),
    role = c("DPS", "Tank"),
    icon = c("Priest-Shadow", "Warrior-Protection"),
    type = c("Player", "Player"),
    subType = c("Shadow", "Protection"),
    server = c("Pagle", "Pagle")
  )

  actor_joined <- wclR::wcl_join_actors(events, actors)
  player_joined <- wclR::wcl_join_players(events, players)

  expect_equal(nrow(actor_joined), nrow(events))
  expect_equal(actor_joined$source_actor_name, c("Vivax", "Vivax"))
  expect_equal(actor_joined$target_actor_name[[1]], "Sindragosa")
  expect_true(is.na(actor_joined$target_actor_name[[2]]))

  expect_equal(nrow(player_joined), nrow(events))
  expect_equal(player_joined$source_player_role, c("DPS", "DPS"))
  expect_equal(player_joined$source_player_class, c("Priest", "Priest"))
  expect_true(is.na(player_joined$target_player_role[[2]]))
})

test_that("wcl_join_players also supports direct actorID joins", {
  roster <- tibble::tibble(
    logID = c("ABC123", "ABC123"),
    fightID = c(7L, 7L),
    actorID = c(1L, 3L)
  )

  players <- tibble::tibble(
    logID = "ABC123",
    fightID = 7L,
    actorID = 1L,
    name = "Vivax",
    role = "DPS",
    icon = "Priest-Shadow",
    type = "Player",
    subType = "Shadow",
    server = "Pagle"
  )

  out <- wclR::wcl_join_players(roster, players)

  expect_equal(out$player_name[[1]], "Vivax")
  expect_true(is.na(out$player_name[[2]]))
  expect_equal(out$player_spec[[1]], "Shadow")
})

test_that("relative time helper supports populated and empty event tables", {
  ms_events <- tibble::tibble(
    timestamp = c(1500, 2500),
    startTime = c(1000, 1000)
  )

  dt_events <- tibble::tibble(
    event_at = as.POSIXct(c(1700000001, 1700000003), origin = "1970-01-01", tz = "UTC"),
    fight_start_at = as.POSIXct(c(1700000000, 1700000000), origin = "1970-01-01", tz = "UTC")
  )

  empty_events <- tibble::tibble()

  ms_out <- wclR::wcl_add_relative_time(ms_events)
  dt_out <- wclR::wcl_add_relative_time(dt_events, unit = "minutes")
  empty_out <- wclR::wcl_add_relative_time(empty_events)

  expect_equal(ms_out$time_since_fight_start, c(0.5, 1.5))
  expect_equal(dt_out$time_since_fight_start, c(1 / 60, 3 / 60))
  expect_equal(nrow(empty_out), 0L)
  expect_type(empty_out$time_since_fight_start, "double")
})

test_that("filter builders produce reusable expression fragments", {
  expect_equal(
    wclR::wcl_filter_abilities(c(70911, 72293, 70911, NA)),
    "ability.id in (70911, 72293)"
  )
  expect_identical(wclR::wcl_filter_abilities(numeric()), "")

  expect_equal(
    wclR::wcl_filter_types(c(" cast ", "", NA, "cast", "damage")),
    "type = 'cast' or type = 'damage'"
  )
  expect_identical(wclR::wcl_filter_types(character()), "")

  expect_equal(
    wclR::wcl_filter_and(list(" a ", "", NA), c("b", "c")),
    "a and b and c"
  )
  expect_identical(wclR::wcl_filter_and(NULL, NA, ""), "")

  type_filter <- paste0(
    "(",
    wclR::wcl_filter_types(c("damage", "miss")),
    ")"
  )
  expect_equal(
    wclR::wcl_filter_and(wclR::wcl_filter_abilities(70911), type_filter),
    "ability.id in (70911) and (type = 'damage' or type = 'miss')"
  )
})

test_that("event queries serialize and omit filter expressions", {
  expression <- 'source.name = "Foo" and ability.id in (70911)'
  with_filter <- wclR:::.wcl_query_events(
    report_code = "ABC123",
    fight_id = 7,
    data_type = "All",
    filter_expression = expression
  )
  without_filter <- wclR:::.wcl_query_events(
    report_code = "ABC123",
    fight_id = 7,
    data_type = "All",
    filter_expression = NULL
  )

  expect_true(grepl(
    'filterExpression: "source.name = \\"Foo\\" and ability.id in (70911)"',
    with_filter,
    fixed = TRUE
  ))
  expect_false(grepl("filterExpression:", without_filter, fixed = TRUE))
})
