test_that("wcl_reports auto-paginates and normalizes report rows", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      if (grepl("reports\\(zoneID: 1020, page: 1\\)", body_json)) {
        return(fixture_response("reports_page1.json"))
      }
      if (grepl("reports\\(zoneID: 1020, page: 2\\)", body_json)) {
        return(fixture_response("reports_page2.json"))
      }
      stop("Unexpected request")
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- wclR::wcl_reports(1020, client = mock_client())

  expect_equal(nrow(reports), 2L)
  expect_equal(reports$logID, c("ABC123", "DEF456"))
  expect_true(all(c("report_link", "report_start_at", "zone_id") %in% names(reports)))
})

test_that("fight, actor, player, event, and ranking helpers normalize fixtures", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      if (grepl("fights\\(killType: Encounters\\)", body_json)) {
        return(fixture_response("fights_response.json"))
      }
      if (grepl("masterData\\(translate: true\\)", body_json)) {
        return(fixture_response("actors_response.json"))
      }
      if (grepl("playerDetails\\(", body_json)) {
        return(fixture_response("player_details_response.json"))
      }
      if (grepl("events\\(", body_json) && grepl("startTime: 0", body_json)) {
        return(fixture_response("events_page1.json"))
      }
      if (grepl("events\\(", body_json) && grepl("startTime: 1500", body_json)) {
        return(fixture_response("events_page2.json"))
      }
      if (grepl("fightRankings\\(", body_json)) {
        return(fixture_response("rankings_response.json"))
      }
      stop("Unexpected request")
    },
    .env = ns,
    .package = "wclR"
  )

  fights <- wclR::wcl_fights("ABC123", client = mock_client())
  actors <- wclR::wcl_actors("ABC123", client = mock_client())
  players <- wclR::wcl_player_details("ABC123", 7, client = mock_client())
  events <- wclR::wcl_events("ABC123", 7, data_type = "All", client = mock_client())
  rankings <- wclR::wcl_rankings(1018, metric = "speed", page = 1, client = mock_client())

  expect_true(all(c("fightID", "duration_s", "report_link") %in% names(fights)))
  expect_true(all(c("actorID", "name", "report_link") %in% names(actors)))
  expect_true(all(c("role", "fightID", "actorID") %in% names(players)))
  expect_true(all(c("timestamp", "event_at", "fightID") %in% names(events)))
  expect_true(all(c("zone_id", "encounterID", "page") %in% names(rankings)))

  expect_equal(nrow(events), 2L)
})

test_that("report code parsing and distinct fight filtering match the workflow defaults", {
  expect_equal(
    wclR::wcl_report_code("https://classic.warcraftlogs.com/reports/ABC123#fight=7"),
    "ABC123"
  )

  fights <- tibble::tibble(
    encounterID = c(855L, 855L, 856L),
    difficulty = c(4L, 4L, 4L),
    size = c(25L, 25L, 25L),
    kill = c(TRUE, TRUE, FALSE),
    bossPercentage = c(0.01, 0.01, 50),
    duration_s = c(240, 240, 300),
    averageItemLevel = c(264.3, 264.3, 264.3)
  )

  filtered <- wclR::wcl_distinct_fights(fights)
  expect_equal(nrow(filtered), 2L)
})
