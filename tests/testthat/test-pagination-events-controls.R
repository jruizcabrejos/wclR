pagination_test_report_record <- function(code, offset = 0) {
  list(
    code = code,
    title = paste("Report", code),
    visibility = "public",
    region = list(name = "US"),
    revision = 1L,
    segments = 1L,
    startTime = 1700000000000 + offset,
    endTime = 1700003600000 + offset
  )
}

pagination_test_report_response <- function(
    page,
    codes = paste0("REPORT", page),
    has_more_pages = page < last_page,
    last_page = 25L,
    total = 100L * last_page) {
  records <- lapply(
    seq_along(codes),
    function(index) {
      pagination_test_report_record(
        code = codes[[index]],
        offset = (as.numeric(page) * 10000) + index
      )
    }
  )

  payload <- list(
    data = list(
      reportData = list(
        reports = list(
          data = records,
          total = as.integer(total),
          per_page = as.integer(length(records)),
          current_page = as.integer(page),
          last_page = as.integer(last_page),
          has_more_pages = isTRUE(has_more_pages)
        )
      )
    )
  )

  list(
    text = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
    status_code = 200L
  )
}

pagination_test_report_page <- function(body_json) {
  match <- regexec("reports\\(zoneID: [0-9]+, page: ([0-9]+)\\)", body_json)
  fields <- regmatches(body_json, match)[[1L]]

  if (length(fields) != 2L) {
    stop("Could not find a report page in the GraphQL request.", call. = FALSE)
  }

  as.integer(fields[[2L]])
}

test_that("wcl_reports defaults to the first three pages", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(page)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(wclR::wcl_reports(1020, client = mock_client()))

  expect_identical(requested_pages, 1:3)
  expect_identical(reports$page, 1:3)
  expect_identical(reports$logID, paste0("REPORT", 1:3))
})

test_that("wcl_reports honors one exact requested page", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(page)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(wclR::wcl_reports(1020, pages = 7, client = mock_client()))

  expect_identical(requested_pages, 7L)
  expect_identical(reports$page, 7L)
  expect_identical(reports$logID, "REPORT7")
})

test_that("wcl_reports deduplicates pages without sorting them", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(page)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(1020, pages = c(3, 1, 3, 2, 1), client = mock_client())
  )

  expect_identical(requested_pages, c(3L, 1L, 2L))
  expect_identical(reports$page, c(3L, 1L, 2L))
  expect_identical(reports$logID, c("REPORT3", "REPORT1", "REPORT2"))
})

test_that("wcl_reports warns before HTTP and caps discovery at page 25", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()
  warning_messages <- character()
  warning_seen_before_http <- NA

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      if (!length(requested_pages)) {
        warning_seen_before_http <<- length(warning_messages) > 0L
      }

      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(page)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- withCallingHandlers(
    suppressMessages(
      wclR::wcl_reports(1020, pages = 1:30, client = mock_client())
    ),
    warning = function(condition) {
      warning_messages <<- c(warning_messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(warning_seen_before_http)
  expect_length(warning_messages, 1L)
  expect_match(
    warning_messages,
    "(?i)(25.*(max|cap|limit)|(max|cap|limit).*25)"
  )
  expect_identical(requested_pages, 1:25)
  expect_identical(reports$page, 1:25)
})

test_that("wcl_reports clamps one page above the API maximum to page 25", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()
  warning_messages <- character()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(page)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- withCallingHandlers(
    suppressMessages(
      wclR::wcl_reports(1020, pages = 30, client = mock_client())
    ),
    warning = function(condition) {
      warning_messages <<- c(warning_messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warning_messages, 1L)
  expect_match(
    warning_messages,
    "(?i)(25.*(max|cap|limit)|(max|cap|limit).*25)"
  )
  expect_identical(requested_pages, 25L)
  expect_identical(reports$page, 25L)
})

test_that("wcl_reports uses NULL for response-driven auto-pagination", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)

      if (identical(page, 1L)) {
        return(pagination_test_report_response(
          page = page,
          codes = "AUTO1",
          has_more_pages = TRUE,
          last_page = 2,
          total = 2
        ))
      }

      pagination_test_report_response(
        page = page,
        codes = "AUTO2",
        has_more_pages = FALSE,
        last_page = 2,
        total = 2
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(1020, pages = NULL, client = mock_client())
  )

  expect_identical(requested_pages, 1:2)
  expect_identical(reports$page, 1:2)
  expect_identical(reports$logID, c("AUTO1", "AUTO2"))
})

test_that("wcl_reports applies the empty-page policy during auto-pagination", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      codes <- if (identical(page, 1L)) "AUTO1" else character()
      pagination_test_report_response(
        page = page,
        codes = codes,
        has_more_pages = TRUE,
        last_page = 3L,
        total = 1L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- NULL
  expect_warning(
    reports <- suppressMessages(
      wclR::wcl_reports(1020, pages = NULL, client = mock_client())
    ),
    "page 2.*no reports"
  )

  expect_identical(requested_pages, 1:2)
  expect_identical(reports$logID, "AUTO1")
})

test_that("wcl_reports stops an ascending crawl when no more pages are reported", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        has_more_pages = page < 2L,
        last_page = 25L,
        total = 2500L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(1020, pages = 1:4, client = mock_client())
  )

  expect_identical(requested_pages, 1:2)
  expect_identical(reports$page, 1:2)
})

test_that("wcl_reports stops an ascending crawl at the reported last page", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        has_more_pages = TRUE,
        last_page = 2L,
        total = 2500L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(1020, pages = 1:4, client = mock_client())
  )

  expect_identical(requested_pages, 1:2)
  expect_identical(reports$page, 1:2)
})

test_that("wcl_reports ignores negative unknown pagination metadata", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        has_more_pages = TRUE,
        last_page = -1L,
        total = -1L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(1060, client = mock_client())
  )

  expect_identical(requested_pages, 1:3)
  expect_identical(reports$page, 1:3)
  expect_identical(reports$logID, paste0("REPORT", 1:3))
})

test_that("wcl_reports stops when a page-one crawl reaches the reported total", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()
  counts <- c(100L, 100L, 100L, 100L, 61L, 0L, 0L, 0L)

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        codes = paste0("PAGE", page, "_", seq_len(counts[[page]])),
        has_more_pages = TRUE,
        last_page = 8L,
        total = 461L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(1020, pages = 1:8, client = mock_client())
  )

  expect_identical(requested_pages, 1:5)
  expect_equal(nrow(reports), 461L)
  expect_identical(reports$page, rep(1:5, counts[1:5]))
})

test_that("wcl_reports continue policy bypasses explicit terminal metadata", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        has_more_pages = FALSE,
        last_page = 2L,
        total = 2L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(
      1020,
      pages = 1:4,
      client = mock_client(),
      on_empty_page = "continue"
    )
  )

  expect_identical(requested_pages, 1:4)
  expect_identical(reports$page, 1:4)
})

test_that("wcl_reports ask policy honors explicit terminal metadata", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        has_more_pages = FALSE,
        last_page = 4L,
        total = 4L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(
      1020,
      pages = 1:4,
      client = mock_client(),
      on_empty_page = "ask"
    )
  )

  expect_identical(requested_pages, 1L)
  expect_identical(reports$page, 1L)
})

test_that("wcl_reports does not apply terminal metadata to arbitrary page order", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        has_more_pages = FALSE,
        last_page = page,
        total = 1L
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(1020, pages = c(3, 1, 2), client = mock_client())
  )

  expect_identical(requested_pages, c(3L, 1L, 2L))
  expect_identical(reports$page, c(3L, 1L, 2L))
})

test_that("wcl_reports stops NULL auto-pagination at page 25", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()
  warning_messages <- character()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      pagination_test_report_response(
        page = page,
        has_more_pages = TRUE,
        last_page = 30,
        total = 30
      )
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- withCallingHandlers(
    suppressMessages(
      wclR::wcl_reports(1020, pages = NULL, client = mock_client())
    ),
    warning = function(condition) {
      warning_messages <<- c(warning_messages, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(requested_pages, 1:25)
  expect_false(26L %in% requested_pages)
  expect_identical(reports$page, 1:25)
  expect_length(warning_messages, 1L)
  expect_match(warning_messages, "(?i)(stop|limit|cap).*25|25.*(stop|limit|cap)")
})

test_that("wcl_reports prints per-page and running report counts", {
  ns <- asNamespace("wclR")
  codes_by_page <- list(
    c("PAGE1A", "PAGE1B"),
    character(),
    "PAGE3A"
  )

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      pagination_test_report_response(
        page = page,
        codes = codes_by_page[[page]],
        last_page = 3,
        total = 3
      )
    },
    .env = ns,
    .package = "wclR"
  )

  progress <- capture.output(
    reports <- wclR::wcl_reports(
      1020,
      pages = 1:3,
      client = mock_client(),
      on_empty_page = "continue"
    ),
    type = "message"
  )

  expect_identical(
    progress,
    c(
      "Report page 1: 2 reports (2 total).",
      "Report page 2: 0 reports (2 total).",
      "Report page 3: 1 reports (3 total)."
    )
  )
  expect_identical(reports$logID, c("PAGE1A", "PAGE1B", "PAGE3A"))
})

test_that("wcl_reports stops and returns accumulated rows on an empty page", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      codes <- if (identical(page, 2L)) character() else paste0("PAGE", page)
      pagination_test_report_response(page, codes = codes, last_page = 3L)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- NULL
  expect_warning(
    reports <- suppressMessages(
      wclR::wcl_reports(1020, pages = 1:3, client = mock_client())
    ),
    "page 2.*no reports"
  )

  expect_identical(requested_pages, 1:2)
  expect_identical(reports$logID, "PAGE1")
})

test_that("wcl_reports can continue after an empty page", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      codes <- if (identical(page, 2L)) character() else paste0("PAGE", page)
      pagination_test_report_response(page, codes = codes, last_page = 3L)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(
      1020,
      pages = 1:3,
      client = mock_client(),
      on_empty_page = "continue"
    )
  )

  expect_identical(requested_pages, 1:3)
  expect_identical(reports$logID, c("PAGE1", "PAGE3"))
})

test_that("the empty-page prompt helper stops in non-interactive sessions", {
  skip_if(interactive())

  decision <- NULL
  expect_warning(
    decision <- wclR:::.wcl_confirm_empty_report_page(2L),
    "non-interactive"
  )
  expect_false(decision)
})

test_that("wcl_reports ask policy follows the prompt helper", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()
  prompted_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      codes <- if (identical(page, 2L)) character() else paste0("PAGE", page)
      pagination_test_report_response(page, codes = codes, last_page = 3L)
    },
    .wcl_confirm_empty_report_page = function(page) {
      prompted_pages <<- c(prompted_pages, page)
      TRUE
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(
      1020,
      pages = 1:3,
      client = mock_client(),
      on_empty_page = "ask"
    )
  )

  expect_identical(prompted_pages, 2L)
  expect_identical(requested_pages, 1:3)
  expect_identical(reports$logID, c("PAGE1", "PAGE3"))
})

test_that("wcl_reports ask policy authorizes only one page at a time", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()
  prompted_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      codes <- if (page %in% c(2L, 3L)) character() else paste0("PAGE", page)
      pagination_test_report_response(page, codes = codes, last_page = 4L)
    },
    .wcl_confirm_empty_report_page = function(page) {
      prompted_pages <<- c(prompted_pages, page)
      identical(page, 2L)
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- suppressMessages(
    wclR::wcl_reports(
      1020,
      pages = 1:4,
      client = mock_client(),
      on_empty_page = "ask"
    )
  )

  expect_identical(requested_pages, 1:3)
  expect_identical(prompted_pages, 2:3)
  expect_identical(reports$logID, "PAGE1")
})

test_that("wcl_reports ask policy stops safely when prompting is unavailable", {
  ns <- asNamespace("wclR")
  requested_pages <- integer()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      page <- pagination_test_report_page(body_json)
      requested_pages <<- c(requested_pages, page)
      codes <- if (identical(page, 2L)) character() else paste0("PAGE", page)
      pagination_test_report_response(page, codes = codes, last_page = 3L)
    },
    .wcl_confirm_empty_report_page = function(page) {
      warning(
        "Prompting is unavailable in this non-interactive test session.",
        call. = FALSE
      )
      FALSE
    },
    .env = ns,
    .package = "wclR"
  )

  reports <- NULL
  expect_warning(
    reports <- suppressMessages(
      wclR::wcl_reports(
        1020,
        pages = 1:3,
        client = mock_client(),
        on_empty_page = "ask"
      )
    ),
    "non-interactive"
  )

  expect_identical(requested_pages, 1:2)
  expect_identical(reports$logID, "PAGE1")
})

test_that("wcl_reports rejects invalid pages before HTTP", {
  ns <- asNamespace("wclR")
  http_calls <- 0L

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      http_calls <<- http_calls + 1L
      pagination_test_report_response(1)
    },
    .env = ns,
    .package = "wclR"
  )

  invalid_pages <- list(
    integer(),
    0,
    -1,
    1.5,
    NA_integer_,
    NaN,
    Inf,
    "not-a-page",
    c(1, NA_integer_),
    list(1)
  )

  for (pages in invalid_pages) {
    expect_error(
      wclR::wcl_reports(1020, pages = pages, client = mock_client()),
      regexp = "pages"
    )
  }

  expect_identical(http_calls, 0L)
})

test_that("wcl_reports rejects an invalid empty-page policy before HTTP", {
  ns <- asNamespace("wclR")
  http_calls <- 0L

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      http_calls <<- http_calls + 1L
      pagination_test_report_response(1)
    },
    .env = ns,
    .package = "wclR"
  )

  expect_error(
    wclR::wcl_reports(
      1020,
      client = mock_client(),
      on_empty_page = "invalid"
    ),
    "stop.*ask.*continue"
  )
  expect_identical(http_calls, 0L)
})

test_that("wcl_events defaults to Friendlies and enables payload controls", {
  ns <- asNamespace("wclR")
  request_bodies <- character()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      request_bodies <<- c(request_bodies, body_json)
      fixture_response("events_page2.json")
    },
    .env = ns,
    .package = "wclR"
  )

  default_events <- wclR::wcl_events(
    "ABC123",
    7,
    data_type = "All",
    client = mock_client()
  )
  enemy_events <- wclR::wcl_events(
    "ABC123",
    7,
    data_type = "All",
    hostility_type = "Enemies",
    client = mock_client()
  )

  expect_equal(nrow(default_events), 1L)
  expect_equal(nrow(enemy_events), 1L)
  expect_length(request_bodies, 2L)
  expect_match(request_bodies[[1L]], "hostilityType: Friendlies")
  expect_match(request_bodies[[1L]], "includeResources: true")
  expect_match(request_bodies[[1L]], "useAbilityIDs: true")
  expect_match(request_bodies[[1L]], "useActorIDs: true")
  expect_match(request_bodies[[2L]], "hostilityType: Enemies")
})

test_that("wcl_events strictly rejects unsupported hostility values before HTTP", {
  ns <- asNamespace("wclR")
  http_calls <- 0L

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      http_calls <<- http_calls + 1L
      fixture_response("events_page2.json")
    },
    .env = ns,
    .package = "wclR"
  )

  invalid_hostilities <- list(
    "All",
    "friendlies",
    "F",
    NA_character_,
    character(),
    c("Enemies", "Friendlies"),
    1
  )

  for (hostility in invalid_hostilities) {
    expect_error(
      wclR::wcl_events(
        "ABC123",
        7,
        data_type = "All",
        hostility_type = hostility,
        client = mock_client()
      ),
      regexp = "hostility_type"
    )
  }

  expect_identical(http_calls, 0L)
})

test_that("wcl_events validates each payload control before HTTP", {
  ns <- asNamespace("wclR")
  http_calls <- 0L

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      http_calls <<- http_calls + 1L
      fixture_response("events_page2.json")
    },
    .env = ns,
    .package = "wclR"
  )

  control_names <- c("include_resources", "use_ability_ids", "use_actor_ids")
  invalid_values <- list(NA, logical(), c(TRUE, FALSE), 1, "TRUE", NULL)
  base_args <- list(
    report_code = "ABC123",
    fight_id = 7,
    data_type = "All",
    client = mock_client()
  )

  for (control_name in control_names) {
    for (invalid_value in invalid_values) {
      args <- c(base_args, stats::setNames(list(invalid_value), control_name))

      expect_error(
        do.call(wclR::wcl_events, args),
        regexp = control_name
      )
    }
  }

  expect_identical(http_calls, 0L)
})

test_that("wcl_events serializes each disabled payload control", {
  ns <- asNamespace("wclR")
  request_bodies <- character()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      request_bodies <<- c(request_bodies, body_json)
      fixture_response("events_page2.json")
    },
    .env = ns,
    .package = "wclR"
  )

  controls <- c(
    include_resources = "includeResources",
    use_ability_ids = "useAbilityIDs",
    use_actor_ids = "useActorIDs"
  )
  base_args <- list(
    report_code = "ABC123",
    fight_id = 7,
    data_type = "All",
    client = mock_client()
  )

  for (control_name in names(controls)) {
    args <- c(base_args, stats::setNames(list(FALSE), control_name))
    do.call(wclR::wcl_events, args)
  }

  expect_length(request_bodies, 3L)

  for (index in seq_along(controls)) {
    disabled_field <- controls[[index]]
    enabled_fields <- unname(controls[-index])

    expect_match(
      request_bodies[[index]],
      paste0(disabled_field, ": false")
    )

    for (enabled_field in enabled_fields) {
      expect_match(
        request_bodies[[index]],
        paste0(enabled_field, ": true")
      )
    }
  }
})

test_that("wcl_events keeps payload controls while paginating events", {
  ns <- asNamespace("wclR")
  request_bodies <- character()

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      request_bodies <<- c(request_bodies, body_json)

      if (grepl("startTime: 0", body_json)) {
        return(fixture_response("events_page1.json"))
      }
      if (grepl("startTime: 1500", body_json)) {
        return(fixture_response("events_page2.json"))
      }

      stop("Unexpected event page request.", call. = FALSE)
    },
    .env = ns,
    .package = "wclR"
  )

  events <- wclR::wcl_events(
    "ABC123",
    7,
    data_type = "All",
    include_resources = FALSE,
    use_ability_ids = FALSE,
    use_actor_ids = FALSE,
    client = mock_client()
  )

  expect_equal(nrow(events), 2L)
  expect_identical(events$timestamp, c(1000L, 1500L))
  expect_length(request_bodies, 2L)
  expect_match(request_bodies[[1L]], "startTime: 0")
  expect_match(request_bodies[[2L]], "startTime: 1500")

  for (body_json in request_bodies) {
    expect_match(body_json, "hostilityType: Friendlies")
    expect_match(body_json, "includeResources: false")
    expect_match(body_json, "useAbilityIDs: false")
    expect_match(body_json, "useActorIDs: false")
  }
})
