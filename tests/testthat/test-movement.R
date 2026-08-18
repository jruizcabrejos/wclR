movement_test_events <- function(
    timestamp,
    source_id,
    target_id,
    resource_actor,
    x,
    y,
    fight_id = 1L,
    map_id = 100L,
    source_instance = NA_integer_,
    target_instance = NA_integer_) {
  n <- length(timestamp)
  report_start <- as.POSIXct("2026-08-07 10:00:00", tz = "UTC")

  tibble::tibble(
    logID = rep("REPORT123", n),
    report_title = rep("Movement fixture", n),
    report_link = rep("https://classic.warcraftlogs.com/reports/REPORT123", n),
    report_start_at = rep(report_start, n),
    report_end_at = rep(report_start + 600, n),
    fightID = rep_len(as.integer(fight_id), n),
    encounterID = rep(1234L, n),
    encounterName = rep("Training Dummy", n),
    difficulty = rep(3L, n),
    size = rep(10L, n),
    kill = rep(TRUE, n),
    duration_s = rep(60, n),
    fight_start_at = rep(report_start, n),
    fight_end_at = rep(report_start + 60, n),
    timestamp = as.numeric(timestamp),
    event_at = report_start + as.numeric(timestamp) / 1000,
    type = rep("cast", n),
    sourceID = rep_len(as.integer(source_id), n),
    sourceInstanceID = rep_len(as.integer(source_instance), n),
    targetID = rep_len(as.integer(target_id), n),
    targetInstanceID = rep_len(as.integer(target_instance), n),
    abilityGameID = rep(42L, n),
    resourceActor = rep_len(as.integer(resource_actor), n),
    x = as.numeric(x),
    y = as.numeric(y),
    mapID = rep_len(as.integer(map_id), n)
  )
}

movement_source_track <- function(actor_id, timestamp, x, y, map_id = 100L) {
  events <- movement_test_events(
    timestamp = timestamp,
    source_id = actor_id,
    target_id = 999L,
    resource_actor = 1L,
    x = x,
    y = y,
    map_id = map_id,
    source_instance = 1L
  )
  wclR::wcl_actor_movement(events, actor_ids = actor_id, output = "track")
}

test_that("movement follows resource ownership and normalizes WCL coordinates", {
  events <- movement_test_events(
    timestamp = c(0, 1000, 2000, 1500),
    source_id = c(10, 30, 10, 55),
    target_id = c(20, 10, 99, 10),
    resource_actor = c(1, 2, 1, 1),
    x = c(0, 300, 600, 999),
    y = c(0, 400, 800, 999)
  )

  track <- wclR::wcl_actor_movement(
    events,
    actor_ids = 10,
    output = "track"
  )
  summary <- wclR::wcl_actor_movement(events, actor_ids = 10)

  expect_equal(track$timestamp, c(0, 1000, 2000))
  expect_equal(track$actor_role, c("source", "target", "source"))
  expect_equal(track$x_raw, c(0, 300, 600))
  expect_equal(track$x, c(0, 3, 6))
  expect_equal(track$y, c(0, 4, 8))
  expect_equal(track$step_distance, c(NA, 5, 5))
  expect_equal(track$cumulative_distance, c(0, 5, 10))
  expect_equal(summary$actor_role, "mixed")
  expect_equal(summary$sample_count, 3L)
  expect_equal(summary$segment_count, 2L)
  expect_equal(summary$total_observed_distance, 10)
  expect_equal(summary$displacement, 10)

  source_only <- wclR::wcl_actor_movement(
    events,
    actor_ids = 10,
    actor_role = "source",
    output = "track"
  )
  target_only <- wclR::wcl_actor_movement(
    events,
    actor_ids = 10,
    actor_role = "target",
    output = "track"
  )
  expect_equal(source_only$timestamp, c(0, 2000))
  expect_equal(source_only$step_distance, c(NA, 10))
  expect_equal(target_only$timestamp, 1000)
})

test_that("an explicit role is a safe fallback when ownership is absent", {
  events <- movement_test_events(
    timestamp = c(0, 1000),
    source_id = c(10, 10),
    target_id = c(20, 20),
    resource_actor = c(1, 1),
    x = c(0, 300),
    y = c(0, 400)
  )
  events$resourceActor <- NULL

  track <- wclR::wcl_actor_movement(
    events,
    actor_ids = 10,
    actor_role = "source",
    output = "track"
  )
  expect_equal(nrow(track), 2L)
  expect_true(all(track$actor_role == "source"))
  expect_true(all(is.na(track$resourceActor)))
  expect_error(
    wclR::wcl_actor_movement(events, actor_ids = 10),
    "resourceActor"
  )
})

test_that("duplicate timestamps are collapsed and conflicts are diagnosed", {
  events <- movement_test_events(
    timestamp = c(1000, 0, 1000, 1000),
    source_id = 10L,
    target_id = 20L,
    resource_actor = 1L,
    x = c(100, 0, 100, 200),
    y = c(0, 0, 0, 0)
  )

  expect_warning(
    track <- wclR::wcl_actor_movement(
      events,
      actor_ids = 10,
      output = "track"
    ),
    "conflicting actor position"
  )
  expect_equal(nrow(track), 2L)
  expect_equal(track$timestamp, c(0, 1000))
  expect_equal(track$x, c(0, 1))
  expect_equal(track$cumulative_distance, c(0, 1))
})

test_that("movement resets across instances, maps, and fights", {
  events <- movement_test_events(
    timestamp = c(0, 1000, 0, 1000, 0, 1000, 0, 1000),
    source_id = 10L,
    target_id = 20L,
    resource_actor = 1L,
    x = c(0, 100, 1000, 1100, 2000, 2100, 3000, 3100),
    y = 0,
    fight_id = c(1, 1, 1, 1, 1, 1, 2, 2),
    map_id = c(100, 100, 100, 100, 200, 200, 100, 100),
    source_instance = c(1, 1, 2, 2, 1, 1, 1, 1)
  )

  summary <- wclR::wcl_actor_movement(events, actor_ids = 10)

  expect_equal(nrow(summary), 4L)
  expect_true(all(summary$sample_count == 2L))
  expect_true(all(summary$total_observed_distance == 1))
  expect_setequal(summary$actorInstanceID, c(1L, 2L))
  expect_setequal(summary$mapID, c(100L, 200L))
  expect_setequal(summary$fightID, c(1L, 2L))
})

test_that("movement validates inputs and returns stable typed empties", {
  summary <- wclR::wcl_actor_movement(tibble::tibble())
  track <- wclR::wcl_actor_movement(tibble::tibble(), output = "track")

  expect_equal(nrow(summary), 0L)
  expect_equal(nrow(track), 0L)
  expect_type(summary$sample_count, "integer")
  expect_type(summary$total_observed_distance, "double")
  expect_type(track$actorID, "integer")
  expect_type(track$x, "double")
  expect_s3_class(track$event_at, "POSIXct")

  expect_error(wclR::wcl_actor_movement(list()), "data frame")
  expect_error(
    wclR::wcl_actor_movement(tibble::tibble(x = 1), actor_role = "source"),
    "missing required"
  )
  expect_error(
    wclR::wcl_actor_movement(tibble::tibble(), actor_ids = c(1, NA)),
    "integer-like"
  )
  expect_error(
    wclR::wcl_actor_movement(tibble::tibble(), actor_role = "owner"),
    "arg"
  )
})

test_that("actor distance uses LOCF on the union timeline without extrapolation", {
  track_a <- movement_source_track(
    actor_id = 10,
    timestamp = c(0, 1000, 3000),
    x = c(0, 100, 300),
    y = 0
  )
  track_b <- movement_source_track(
    actor_id = 20,
    timestamp = c(500, 2000, 3000),
    x = c(0, 200, 300),
    y = 100
  )

  series <- wclR::wcl_actor_distance(track_a, track_b, output = "series")
  summary <- wclR::wcl_actor_distance(track_a, track_b)

  expect_equal(series$timestamp, c(500, 1000, 2000, 3000))
  expect_equal(series$actor_a_sample_timestamp, c(0, 1000, 1000, 3000))
  expect_equal(series$actor_b_sample_timestamp, c(500, 500, 2000, 3000))
  expect_equal(series$actor_a_age_ms, c(500, 0, 1000, 0))
  expect_equal(series$actor_b_age_ms, c(0, 500, 0, 0))
  expect_equal(series$distance, c(1, sqrt(2), sqrt(2), 1))
  expect_equal(series$covered_interval_ms, c(500, 1000, 1000, 0))
  expect_true(all(series$within_hold))

  expect_equal(summary$overlap_start_timestamp, 500)
  expect_equal(summary$overlap_end_timestamp, 3000)
  expect_equal(summary$overlap_span_s, 2.5)
  expect_equal(summary$comparison_coverage_s, 2.5)
  expect_equal(summary$coverage_fraction, 1)
  expect_equal(summary$fight_coverage_fraction, 2.5 / 60)
  expect_equal(summary$max_actor_a_age_ms, 1000)
  expect_equal(summary$max_actor_b_age_ms, 500)
  expect_equal(summary$max_held_position_age_ms, 1000)
  expect_equal(
    summary$mean_distance,
    (500 + 2000 * sqrt(2)) / 2500,
    tolerance = 1e-12
  )
  expect_equal(summary$min_distance, 1)
  expect_equal(summary$max_distance, sqrt(2))
})

test_that("max_hold_ms limits distance validity and duration coverage", {
  track_a <- movement_source_track(
    actor_id = 10,
    timestamp = c(0, 1000, 3000),
    x = c(0, 100, 300),
    y = 0
  )
  track_b <- movement_source_track(
    actor_id = 20,
    timestamp = c(500, 2000, 3000),
    x = c(0, 200, 300),
    y = 100
  )

  series <- wclR::wcl_actor_distance(
    track_a,
    track_b,
    max_hold_ms = 600,
    output = "series"
  )
  summary <- wclR::wcl_actor_distance(track_a, track_b, max_hold_ms = 600)

  expect_identical(series$within_hold, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(series$covered_interval_ms, c(100, 100, 0, 0))
  expect_true(is.na(series$distance[[3L]]))
  expect_equal(summary$comparison_count, 3L)
  expect_equal(summary$comparison_coverage_s, 0.2)
  expect_equal(summary$coverage_fraction, 0.08)
  expect_equal(summary$fight_coverage_fraction, 0.2 / 60)
  expect_equal(summary$max_actor_a_age_ms, 500)
  expect_equal(summary$max_actor_b_age_ms, 500)
  expect_equal(summary$max_held_position_age_ms, 500)
  expect_equal(
    summary$mean_distance,
    (1 + sqrt(2)) / 2,
    tolerance = 1e-12
  )
})

test_that("distance respects maps and reports non-overlapping actor windows", {
  point_a <- movement_source_track(10, 0, 0, 0)
  point_b <- movement_source_track(20, 1000, 100, 0)

  summary <- wclR::wcl_actor_distance(point_a, point_b)
  series <- wclR::wcl_actor_distance(point_a, point_b, output = "series")
  expect_equal(nrow(summary), 1L)
  expect_equal(summary$comparison_count, 0L)
  expect_equal(summary$comparison_coverage_s, 0)
  expect_equal(summary$fight_coverage_fraction, 0)
  expect_true(is.na(summary$max_actor_a_age_ms))
  expect_true(is.na(summary$max_actor_b_age_ms))
  expect_true(is.na(summary$max_held_position_age_ms))
  expect_true(is.na(summary$mean_distance))
  expect_equal(nrow(series), 0L)

  other_map <- movement_source_track(20, 0, 100, 0, map_id = 200L)
  expect_equal(nrow(wclR::wcl_actor_distance(point_a, other_map)), 0L)
})

test_that("actor distance validates arguments and has stable empty schemas", {
  empty_track <- wclR::wcl_actor_movement(tibble::tibble(), output = "track")
  summary <- wclR::wcl_actor_distance(empty_track, empty_track)
  series <- wclR::wcl_actor_distance(empty_track, empty_track, output = "series")

  expect_equal(nrow(summary), 0L)
  expect_equal(nrow(series), 0L)
  expect_type(summary$comparison_count, "integer")
  expect_type(summary$coverage_fraction, "double")
  expect_type(summary$fight_coverage_fraction, "double")
  expect_type(summary$max_actor_a_age_ms, "double")
  expect_type(summary$max_actor_b_age_ms, "double")
  expect_type(summary$max_held_position_age_ms, "double")
  expect_type(series$within_hold, "logical")
  expect_type(series$distance, "double")
  expect_s3_class(series$event_at, "POSIXct")

  no_duration_a <- movement_source_track(10, c(0, 1000), c(0, 100), 0)
  no_duration_b <- movement_source_track(20, c(0, 1000), c(0, 100), 100)
  no_duration_a$fight_duration_s <- NA_real_
  no_duration_b$fight_duration_s <- NA_real_
  expect_true(is.na(
    wclR::wcl_actor_distance(no_duration_a, no_duration_b)$fight_coverage_fraction
  ))

  expect_error(wclR::wcl_actor_distance(list(), empty_track), "track_a")
  expect_error(wclR::wcl_actor_distance(empty_track, list()), "track_b")
  expect_error(
    wclR::wcl_actor_distance(empty_track, empty_track, max_hold_ms = -1),
    "non-negative"
  )
  expect_error(
    wclR::wcl_actor_distance(empty_track, empty_track, output = "points"),
    "arg"
  )
})
