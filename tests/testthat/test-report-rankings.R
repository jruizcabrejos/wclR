report_rankings_test_call <- function(...) {
  wclR::wcl_report_rankings(...)
}

test_that("wcl_report_rankings omits fightIDs when fight_ids is NULL", {
  ns <- asNamespace("wclR")
  request_body <- NULL

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      request_body <<- body_json
      fixture_response("report_rankings_null.json")
    },
    .env = ns,
    .package = "wclR"
  )

  rankings <- report_rankings_test_call(
    "RANKNULL",
    fight_ids = NULL,
    output = "raw",
    client = mock_client()
  )

  expect_null(rankings)
  expect_false(grepl("fightIDs:", request_body, fixed = TRUE))
  expect_match(request_body, "fights \\{")
  expect_match(
    request_body,
    "rankings\\(playerMetric: default, compare: Rankings, timeframe: Today\\)"
  )
})

test_that("wcl_report_rankings applies scalar and vector filters to both fields", {
  ns <- asNamespace("wclR")
  request_bodies <- character()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      request_bodies <<- c(request_bodies, body_json)
      fixture_response("report_rankings_null.json")
    },
    .env = ns,
    .package = "wclR"
  )

  report_rankings_test_call(
    "RANKNULL",
    fight_ids = 7,
    encounter_id = 855,
    difficulty = 4,
    player_metric = "dps",
    compare = "Parses",
    timeframe = "Historical",
    output = "raw",
    client = mock_client()
  )
  report_rankings_test_call(
    "RANKNULL",
    fight_ids = c(8, 7, 8),
    encounter_id = 856,
    difficulty = 3,
    output = "raw",
    client = mock_client()
  )

  expect_length(request_bodies, 2L)
  expect_match(
    request_bodies[[1L]],
    "fights\\(fightIDs: \\[7\\], encounterID: 855, difficulty: 4\\)"
  )
  expect_match(
    request_bodies[[1L]],
    paste0(
      "rankings\\(fightIDs: \\[7\\], encounterID: 855, difficulty: 4, ",
      "playerMetric: dps, compare: Parses, timeframe: Historical\\)"
    )
  )
  expect_match(
    request_bodies[[2L]],
    "fights\\(fightIDs: \\[8, 7\\], encounterID: 856, difficulty: 3\\)"
  )
  expect_match(
    request_bodies[[2L]],
    paste0(
      "rankings\\(fightIDs: \\[8, 7\\], encounterID: 856, difficulty: 3, ",
      "playerMetric: default, compare: Rankings, timeframe: Today\\)"
    )
  )
})

test_that("wcl_report_rankings validates metric, compare, and timeframe exactly", {
  ns <- asNamespace("wclR")
  http_calls <- 0L

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      http_calls <<- http_calls + 1L
      fixture_response("report_rankings_null.json")
    },
    .env = ns,
    .package = "wclR"
  )

  invalid_values <- list(
    player_metric = list("DPS", "dp", "dps ", NA_character_, character(), c("dps", "hps"), 1),
    compare = list("rankings", "R", "Rankings ", NA_character_, character(), c("Rankings", "Parses"), 1),
    timeframe = list("today", "T", "Today ", NA_character_, character(), c("Today", "Historical"), 1)
  )
  base_args <- list(report_code = "RANKNULL", client = mock_client())

  for (argument in names(invalid_values)) {
    for (value in invalid_values[[argument]]) {
      args <- c(base_args, stats::setNames(list(value), argument))
      expect_error(
        do.call(report_rankings_test_call, args),
        regexp = argument
      )
    }
  }

  expect_identical(http_calls, 0L)
})

test_that("wcl_report_rankings rejects invalid IDs before HTTP", {
  ns <- asNamespace("wclR")
  http_calls <- 0L

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      http_calls <<- http_calls + 1L
      fixture_response("report_rankings_null.json")
    },
    .env = ns,
    .package = "wclR"
  )

  invalid_ids <- list(
    fight_ids = list(
      integer(), 0, -1, 1.5, NA_integer_, NaN, Inf, "7", list(7),
      .Machine$integer.max + 1
    ),
    encounter_id = list(
      integer(), 0, -1, 1.5, NA_integer_, NaN, Inf, "855", c(855, 856),
      .Machine$integer.max + 1
    ),
    difficulty = list(
      integer(), 0, -1, 1.5, NA_integer_, NaN, Inf, "4", c(3, 4),
      .Machine$integer.max + 1
    )
  )
  base_args <- list(report_code = "RANKNULL", client = mock_client())

  for (argument in names(invalid_ids)) {
    for (value in invalid_ids[[argument]]) {
      args <- c(base_args, stats::setNames(list(value), argument))
      expect_error(
        do.call(report_rankings_test_call, args),
        regexp = argument
      )
    }
  }

  expect_identical(http_calls, 0L)
})

test_that("wcl_report_rankings tidies multi-fight and multi-role rankings", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_multi.json")
    },
    .env = ns,
    .package = "wclR"
  )

  rankings <- report_rankings_test_call(
    "RANK123",
    player_metric = "dps",
    compare = "Parses",
    timeframe = "Historical",
    client = mock_client()
  )

  expect_s3_class(rankings, "tbl_df")
  expect_equal(nrow(rankings), 4L)
  expect_identical(rankings$logID, rep("RANK123", 4L))
  expect_identical(rankings$report_title, rep("Ranking Night", 4L))
  expect_identical(rankings$fightID, c(7L, 7L, 8L, 8L))
  expect_identical(rankings$encounterID, c(855L, 855L, 856L, 856L))
  expect_identical(
    rankings$encounterName,
    c("Sindragosa", "Sindragosa", "The Lich King", "The Lich King")
  )
  expect_identical(rankings$role, c("Tank", "DPS", "Healer", "DPS"))
  expect_identical(rankings$actorID, 101:104)
  expect_identical(rankings$name, c("Bulwark", "Ember", "Mender", "Shadow"))
  expect_identical(rankings$difficulty, rep(4L, 4L))
  expect_identical(rankings$size, rep(25L, 4L))
  expect_identical(rankings$kill, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(rankings$duration_s, c(240, 240, 300, 300))
  expect_identical(rankings$segments, rep(2L, 4L))
  expect_identical(rankings$exportedSegments, rep(2L, 4L))
  expect_true(all(rankings$rankings_complete))
  expect_identical(rankings$player_metric, rep("dps", 4L))
  expect_identical(rankings$compare, rep("Parses", 4L))
  expect_identical(rankings$timeframe, rep("Historical", 4L))
  expect_identical(
    rankings$report_link,
    paste0(
      "https://classic.warcraftlogs.com/reports/RANK123#fight=",
      c(7L, 7L, 8L, 8L)
    )
  )
  expect_s3_class(rankings$report_start_at, "POSIXct")
  expect_s3_class(rankings$fight_start_at, "POSIXct")
  expect_equal(
    rankings$fight_start_at,
    as.POSIXct(
      c(1700000001, 1700000001, 1700000300, 1700000300),
      origin = "1970-01-01",
      tz = "UTC"
    )
  )
})

test_that("wcl_report_rankings preserves unknown scalar and nested fields", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_multi.json")
    },
    .env = ns,
    .package = "wclR"
  )

  rankings <- report_rankings_test_call("RANK123", client = mock_client())

  expect_true(all(c(
    "amount",
    "rankPercent",
    "spec",
    "futureScalar",
    "futureMixed",
    "futureTyped",
    "futureBooleanNumber",
    "futureNested",
    "role_bracket",
    "role_thresholds",
    "ranking_partition",
    "ranking_percentileBands",
    "rankings_schemaVersion",
    "rankings_futureMetadata",
    "character_role_thresholds",
    "role_field_collision",
    "role_field",
    "ranking_field_collision",
    "ranking_field",
    "rankings_field_collision",
    "rankings_field"
  ) %in% names(rankings)))
  expect_identical(
    rankings$futureScalar,
    c("preserved", "also-preserved", "healer-extra", "damage-extra")
  )
  expect_true(is.list(rankings$futureNested))
  expect_true(is.list(rankings$ranking_percentileBands))
  expect_true(is.list(rankings$role_thresholds))
  expect_identical(
    rankings$character_role_thresholds,
    paste0("character-", c("one", "two", "three", "four"))
  )
  expect_identical(rankings$futureNested[[1L]]$label, "alpha")
  expect_identical(rankings$futureNested[[1L]]$values, list(1L, 2L))
  expect_identical(rankings$ranking_percentileBands[[1L]], list(95L, 99L))
  expect_identical(rankings$role_thresholds[[1L]], list(75L, 90L))
  expect_identical(rankings$ranking_partition, c(5L, 5L, 6L, 6L))
  expect_true(is.list(rankings$futureMixed))
  expect_identical(rankings$futureMixed[[1L]], list("one", "two"))
  expect_identical(rankings$futureMixed[[2L]], "scalar")
  expect_identical(rankings$futureMixed[[3L]], 3L)
  expect_null(rankings$futureMixed[[4L]])
  expect_true(is.list(rankings$futureTyped))
  expect_identical(rankings$futureTyped[[1L]], "text")
  expect_identical(rankings$futureTyped[[2L]], 42L)
  expect_identical(rankings$futureTyped[[3L]], TRUE)
  expect_true(is.na(rankings$futureTyped[[4L]]))
  expect_true(is.list(rankings$futureBooleanNumber))
  expect_identical(rankings$futureBooleanNumber[[1L]], TRUE)
  expect_identical(rankings$futureBooleanNumber[[2L]], 1L)
  expect_identical(rankings$futureBooleanNumber[[3L]], FALSE)
  expect_identical(rankings$futureBooleanNumber[[4L]], 0L)
  expect_identical(rankings$rankings_schemaVersion, rep(2L, 4L))
  expect_true(is.list(rankings$rankings_futureMetadata))
  expect_identical(rankings$rankings_futureMetadata[[1L]]$source, "fixture")
  expect_identical(
    rankings$rankings_futureMetadata[[1L]]$flags,
    list("alpha", "beta")
  )
  expect_identical(rankings$role_field_collision[[1L]], "role-flat")
  expect_identical(rankings$role_field[[1L]]$collision, "role-nested")
  expect_identical(rankings$role_field_collision[[3L]], "role-two-flat")
  expect_identical(rankings$role_field[[3L]]$collision, "role-two-nested")
  expect_identical(rankings$ranking_field_collision[[1L]], "fight-flat")
  expect_identical(rankings$ranking_field[[1L]]$collision, "fight-nested")
  expect_identical(rankings$ranking_field_collision[[3L]], "fight-two-flat")
  expect_identical(rankings$ranking_field[[3L]]$collision, "fight-two-nested")
  expect_identical(rankings$rankings_field_collision, rep("top-level-flat", 4L))
  expect_true(is.list(rankings$rankings_field))
  expect_identical(rankings$rankings_field[[1L]]$collision, "top-level-nested")
})

test_that("wcl_report_rankings raw output is the exact rankings JSON value", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_multi.json")
    },
    .env = ns,
    .package = "wclR"
  )

  fixture <- jsonlite::fromJSON(
    read_fixture("report_rankings_multi.json"),
    simplifyVector = FALSE,
    bigint_as_char = TRUE
  )
  expected <- fixture$data$reportData$report$rankings
  rankings <- report_rankings_test_call(
    "RANK123",
    output = "raw",
    client = mock_client()
  )

  expect_identical(rankings, expected)
})

test_that("wcl_report_rankings preserves empty raw JSON scalars exactly", {
  ns <- asNamespace("wclR")
  fixture_name <- "report_rankings_raw_empty_object.json"

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response(fixture_name)
    },
    .env = ns,
    .package = "wclR"
  )

  for (case in list(
    list(
      code = "RANKRAWOBJECT",
      fixture = "report_rankings_raw_empty_object.json"
    ),
    list(
      code = "RANKRAWARRAY",
      fixture = "report_rankings_raw_empty_array.json"
    )
  )) {
    fixture_name <- case$fixture
    parsed <- jsonlite::fromJSON(
      read_fixture(fixture_name),
      simplifyVector = FALSE,
      bigint_as_char = TRUE
    )
    expected <- parsed$data$reportData$report$rankings
    rankings <- report_rankings_test_call(
      case$code,
      output = "raw",
      client = mock_client()
    )

    expect_false(is.null(rankings))
    expect_identical(rankings, expected)
  }
})

test_that("wcl_report_rankings returns typed empty output for NULL and empty data", {
  ns <- asNamespace("wclR")
  fixture_name <- "report_rankings_null.json"

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response(fixture_name)
    },
    .env = ns,
    .package = "wclR"
  )

  expected <- wclR:::.wcl_empty_report_rankings()

  for (case in list(
    list(code = "RANKNULL", fixture = "report_rankings_null.json"),
    list(code = "RANKEMPTY", fixture = "report_rankings_empty.json")
  )) {
    fixture_name <- case$fixture
    rankings <- report_rankings_test_call(case$code, client = mock_client())

    expect_s3_class(rankings, "tbl_df")
    expect_identical(rankings, expected)
  }
})

test_that("wcl_report_rankings rejects a non-empty unknown tidy shape", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_unrecognized.json")
    },
    .env = ns,
    .package = "wclR"
  )

  expect_error(
    report_rankings_test_call("RANKODD", client = mock_client()),
    regexp = "output = \\\"raw\\\""
  )

  raw <- report_rankings_test_call(
    "RANKODD",
    output = "raw",
    client = mock_client()
  )
  expect_named(raw, "entries")
})

test_that("wcl_report_rankings rejects an unknown named role container", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_unrecognized_role.json")
    },
    .env = ns,
    .package = "wclR"
  )

  expect_error(
    report_rankings_test_call("RANKROLEODD", client = mock_client()),
    regexp = "output = \\\"raw\\\""
  )

  raw <- report_rankings_test_call(
    "RANKROLEODD",
    output = "raw",
    client = mock_client()
  )
  expect_named(raw, "data")
})

test_that("wcl_report_rankings rejects ranking rows without a fight ID", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_unrecognized_fight.json")
    },
    .env = ns,
    .package = "wclR"
  )

  expect_error(
    report_rankings_test_call("RANKFIGHTODD", client = mock_client()),
    regexp = "output = \\\"raw\\\""
  )

  raw <- report_rankings_test_call(
    "RANKFIGHTODD",
    output = "raw",
    client = mock_client()
  )
  expect_named(raw, "data")
})

test_that("wcl_report_rankings preserves a nonnumeric GUID identity", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_guid_identity.json")
    },
    .env = ns,
    .package = "wclR"
  )

  rankings <- report_rankings_test_call("RANKGUID", client = mock_client())

  expect_equal(nrow(rankings), 1L)
  expect_true(is.na(rankings$actorID[[1L]]))
  expect_true(is.na(rankings$name[[1L]]))
  expect_identical(rankings$guid, "Player-ABC")
})

test_that("wcl_report_rankings rejects a character without an identity", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_unrecognized_character.json")
    },
    .env = ns,
    .package = "wclR"
  )

  expect_error(
    report_rankings_test_call("RANKCHARODD", client = mock_client()),
    regexp = "output = \\\"raw\\\""
  )

  raw <- report_rankings_test_call(
    "RANKCHARODD",
    output = "raw",
    client = mock_client()
  )
  expect_named(raw, "data")
})

test_that("wcl_report_rankings warns when report segments are incomplete", {
  ns <- asNamespace("wclR")
  warning_messages <- character()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      fixture_response("report_rankings_incomplete.json")
    },
    .env = ns,
    .package = "wclR"
  )

  rankings <- withCallingHandlers(
    report_rankings_test_call("RANKPART", client = mock_client()),
    warning = function(condition) {
      warning_messages <<- c(warning_messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warning_messages, 1L)
  expect_match(warning_messages, "processed 2 of 3.*incomplete")
  expect_equal(nrow(rankings), 1L)
  expect_identical(rankings$segments, 3L)
  expect_identical(rankings$exportedSegments, 2L)
  expect_identical(rankings$rankings_complete, FALSE)
})
