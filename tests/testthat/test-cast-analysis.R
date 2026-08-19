cast_analysis_events <- function(
    type,
    timestamp,
    source_id = 10L,
    target_id = 20L,
    ability_id = 100L,
    source_instance_id = 7L,
    target_instance_id = 1L,
    fight_id = 1L,
    log_id = "LOG") {
  tibble::tibble(
    logID = log_id,
    report_title = "Test report",
    report_link = "https://classic.warcraftlogs.com/reports/LOG#fight=1",
    report_start_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    fightID = fight_id,
    encounterID = 123L,
    encounterName = "Test encounter",
    fight_start_at = as.POSIXct("2026-01-01 00:00:01", tz = "UTC"),
    timestamp = timestamp,
    event_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + timestamp / 1000,
    type = type,
    sourceID = source_id,
    sourceInstanceID = source_instance_id,
    targetID = target_id,
    targetInstanceID = target_instance_id,
    abilityGameID = ability_id
  )
}

test_that("cast interruptions distinguish completed, confirmed, inferred, and instant attempts", {
  events <- dplyr::bind_rows(
    cast_analysis_events("begincast", 1000),
    cast_analysis_events("cast", 2000),
    cast_analysis_events("begincast", 3000),
    cast_analysis_events("begincast", 4000),
    cast_analysis_events("cast", 5000, ability_id = 200L)
  )
  interrupts <- cast_analysis_events(
    "interrupt",
    3500,
    source_id = 99L,
    target_id = 10L,
    ability_id = 6552L,
    source_instance_id = 1L,
    target_instance_id = 7L
  )
  interrupts$extraAbilityGameID <- 100L

  attempts <- wcl_cast_interruptions(
    events,
    interrupt_events = interrupts,
    output = "attempts"
  )

  expect_equal(
    attempts$status,
    c("completed", "interrupted_confirmed", "interrupted_inferred", "completed_instant")
  )
  expect_equal(attempts$cast_time_ms, c(1000, NA, NA, 0))
  expect_equal(attempts$elapsed_ms, c(1000, 500, NA, NA))
  expect_equal(attempts$interrupterID, c(NA, 99L, NA, NA))
  expect_true(attempts$confirmed_interruption[[2L]])
  expect_true(attempts$inferred_interruption[[3L]])
  expect_true(attempts$instant_cast[[4L]])

  summary <- wcl_cast_interruptions(events, interrupt_events = interrupts)
  ability_100 <- summary[summary$abilityGameID == 100L, , drop = FALSE]
  ability_200 <- summary[summary$abilityGameID == 200L, , drop = FALSE]

  expect_equal(ability_100$attempt_count, 3L)
  expect_equal(ability_100$completed_cast_count, 1L)
  expect_equal(ability_100$confirmed_interruption_count, 1L)
  expect_equal(ability_100$inferred_interruption_count, 1L)
  expect_equal(ability_100$mean_hardcast_time_ms, 1000)
  expect_equal(ability_200$instant_cast_count, 1L)
})

test_that("standalone and nested stopped-ability interrupts are retained", {
  events <- cast_analysis_events("begincast", 1000)
  nested_interrupt <- tibble::tibble(
    logID = "LOG",
    fightID = 1L,
    timestamp = 1500,
    type = "interrupt",
    source = list(list(id = 99L)),
    sourceInstance = 1L,
    target = list(list(id = 10L)),
    targetInstance = 7L,
    ability = list(list(gameID = 6552L)),
    stoppedAbility = list(list(gameID = 100L))
  )

  nested <- wcl_cast_interruptions(
    events,
    interrupt_events = nested_interrupt,
    output = "attempts"
  )
  expect_equal(nested$status, "interrupted_confirmed")
  expect_equal(nested$abilityGameID, 100L)
  expect_equal(nested$interruptAbilityGameID, 6552L)

  standalone <- nested_interrupt
  standalone$stoppedAbility <- list(list(gameID = 300L))
  standalone_attempt <- wcl_cast_interruptions(
    events[0, ],
    interrupt_events = standalone,
    output = "attempts"
  )
  expect_equal(standalone_attempt$status, "interrupted_confirmed")
  expect_false(standalone_attempt$begincast_observed)
  expect_equal(standalone_attempt$abilityGameID, 300L)
})

test_that("cast interruption filters apply to caster and stopped ability", {
  events <- dplyr::bind_rows(
    cast_analysis_events("cast", 1000, source_id = 10L, ability_id = 100L),
    cast_analysis_events("cast", 1100, source_id = 11L, ability_id = 200L)
  )

  result <- wcl_cast_interruptions(
    events,
    source_ids = 11L,
    ability_ids = 200L,
    output = "attempts"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$sourceID, 11L)
  expect_equal(result$abilityGameID, 200L)
})

test_that("cast interruption output is typed when no attempts match", {
  attempts <- wcl_cast_interruptions(tibble::tibble(), output = "attempts")
  summary <- wcl_cast_interruptions(tibble::tibble())

  expect_s3_class(attempts, "tbl_df")
  expect_equal(nrow(attempts), 0L)
  expect_type(attempts$sourceID, "integer")
  expect_s3_class(attempts$begin_at, "POSIXct")
  expect_equal(nrow(summary), 0L)
  expect_type(summary$confirmed_interruption_count, "integer")
})

test_that("cast event preparation keeps flat precedence and nested fallback", {
  events <- tibble::tibble(
    type = c("cast", "cast", "damage"),
    sourceID = c(10L, NA_integer_, 99L),
    source = list(list(id = 90L), list(id = 11L), list(id = 98L)),
    abilityGameID = c(100L, NA_integer_, 999L),
    ability = list(
      list(gameID = 900L),
      list(gameID = 101L),
      list(gameID = 998L)
    )
  )

  prepared <- .wcl_cast_prepare_events(events, "events", types = "cast")

  expect_equal(nrow(prepared), 2L)
  expect_equal(prepared$sourceID, c(10L, 11L))
  expect_equal(prepared$abilityGameID, c(100L, 101L))
})

test_that("cast interruption inputs reject missing stopped ability identity", {
  events <- cast_analysis_events("begincast", 1000)
  interrupt <- cast_analysis_events(
    "interrupt",
    1200,
    source_id = 99L,
    target_id = 10L,
    ability_id = 6552L
  )
  interrupt$targetInstanceID <- 7L

  expect_error(
    wcl_cast_interruptions(events, interrupt_events = interrupt),
    "stoppedAbilityGameID",
    fixed = TRUE
  )
})

test_that("travel time performs deterministic FIFO matching with ambiguity diagnostics", {
  casts <- dplyr::bind_rows(
    cast_analysis_events("cast", 1000),
    cast_analysis_events("cast", 1100)
  )
  landings <- dplyr::bind_rows(
    cast_analysis_events("damage", 1200),
    cast_analysis_events("damage", 1300)
  )

  pairs <- wcl_spell_travel_time(casts, landings, 500, output = "pairs")

  expect_equal(pairs$match_status, c("ambiguous", "ambiguous"))
  expect_equal(pairs$landing_timestamp, c(1200, 1300))
  expect_equal(pairs$travel_time_ms, c(200, 200))
  expect_true(all(pairs$ambiguous))
  expect_equal(pairs$candidate_landing_count, c(2L, 2L))
  expect_equal(pairs$candidate_cast_count, c(2L, 2L))

  summary <- wcl_spell_travel_time(casts, landings, 500)
  expect_equal(summary$cast_count, 2L)
  expect_equal(summary$matched_count, 2L)
  expect_equal(summary$ambiguous_match_count, 2L)
  expect_equal(summary$mean_travel_time_ms, 200)
})

test_that("travel time respects instance identity and the required window", {
  casts <- dplyr::bind_rows(
    cast_analysis_events("cast", 1000, target_instance_id = 1L),
    cast_analysis_events("cast", 2000, target_instance_id = 2L)
  )
  landings <- dplyr::bind_rows(
    cast_analysis_events("damage", 1150, target_instance_id = 1L),
    cast_analysis_events("damage", 2600, target_instance_id = 2L)
  )

  pairs <- wcl_spell_travel_time(casts, landings, 500, output = "pairs")
  expect_equal(pairs$match_status, c("matched", "unmatched"))
  expect_equal(pairs$travel_time_ms, c(150, NA))
  expect_equal(pairs$unmatched_reason, c(NA, "no_landing_in_window"))
})

test_that("travel time applies ability mappings and excludes periodic ticks", {
  casts <- cast_analysis_events("cast", 1000, ability_id = 100L)
  landings <- dplyr::bind_rows(
    cast_analysis_events("damage", 1050, ability_id = 101L),
    cast_analysis_events("damage", 1200, ability_id = 101L)
  )
  landings$tick <- c(TRUE, FALSE)
  mapping <- tibble::tibble(
    cast_ability_id = 100L,
    landing_ability_id = 101L
  )

  pairs <- wcl_spell_travel_time(
    casts,
    landings,
    500,
    ability_map = mapping,
    output = "pairs"
  )
  expect_equal(pairs$landingAbilityGameID, 101L)
  expect_equal(pairs$landing_timestamp, 1200)
  expect_equal(pairs$travel_time_ms, 200)
  expect_false(pairs$ambiguous)
})

test_that("travel summary reports unused direct landings", {
  casts <- cast_analysis_events("cast", 1000)
  landings <- dplyr::bind_rows(
    cast_analysis_events("damage", 1100),
    cast_analysis_events("damage", 1200)
  )

  summary <- wcl_spell_travel_time(casts, landings, 500)
  expect_equal(summary$landing_count, 2L)
  expect_equal(summary$matched_count, 1L)
  expect_equal(summary$unmatched_landing_count, 1L)
  expect_equal(summary$ambiguous_match_count, 1L)
})

test_that("travel time validates its window and one-to-one ability map", {
  casts <- cast_analysis_events("cast", 1000)
  landings <- cast_analysis_events("damage", 1100)

  expect_error(
    wcl_spell_travel_time(casts, landings, Inf),
    "finite positive"
  )
  expect_error(
    wcl_spell_travel_time(
      casts,
      landings,
      500,
      ability_map = tibble::tibble(
        cast_ability_id = c(100L, 200L),
        landing_ability_id = c(101L, 101L)
      )
    ),
    "one-to-one"
  )
})

test_that("travel output is typed when there are no casts", {
  empty <- tibble::tibble()
  pairs <- wcl_spell_travel_time(empty, empty, 500, output = "pairs")
  summary <- wcl_spell_travel_time(empty, empty, 500)

  expect_equal(nrow(pairs), 0L)
  expect_type(pairs$castAbilityGameID, "integer")
  expect_s3_class(pairs$cast_at, "POSIXct")
  expect_equal(nrow(summary), 0L)
  expect_type(summary$matched_count, "integer")
})
