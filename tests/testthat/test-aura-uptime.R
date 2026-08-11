aura_test_events <- function(
    type,
    time_s,
    source_id,
    target_id,
    ability_id,
    duration_s = 100,
    stacks = NA_real_,
    fight_id = 1L,
    log_id = "AURA_TEST",
    source_instance_id = NA_integer_,
    target_instance_id = NA_integer_) {
  n <- length(type)
  fight_start <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  fight_duration <- duration_s

  tibble::tibble(
    logID = rep.int(log_id, n),
    report_title = rep.int("Aura test", n),
    report_link = rep.int("https://classic.warcraftlogs.com/reports/AURA_TEST#fight=1", n),
    report_start_at = rep.int(fight_start, n),
    report_end_at = rep.int(fight_start + fight_duration, n),
    fightID = rep.int(as.integer(fight_id), n),
    encounterID = rep.int(123L, n),
    encounterName = rep.int("Training Dummy", n),
    difficulty = rep.int(3L, n),
    size = rep.int(25L, n),
    kill = rep.int(TRUE, n),
    duration_s = rep.int(fight_duration, n),
    fight_start_at = rep.int(fight_start, n),
    fight_end_at = rep.int(fight_start + fight_duration, n),
    startTime = rep.int(0, n),
    timestamp = as.numeric(time_s) * 1000,
    event_at = fight_start + as.numeric(time_s),
    type = as.character(type),
    sourceID = as.integer(source_id),
    sourceInstanceID = rep_len(as.integer(source_instance_id), n),
    targetID = as.integer(target_id),
    targetInstanceID = rep_len(as.integer(target_instance_id), n),
    abilityGameID = as.integer(ability_id),
    stacks = as.numeric(stacks)
  )
}

test_that("wcl_aura_uptime summarizes observed buff and debuff combinations", {
  events <- dplyr::bind_rows(
    aura_test_events(
      c("applybuff", "removebuff"),
      c(10, 50),
      c(1, 1),
      c(10, 10),
      c(1001, 1001)
    ),
    aura_test_events(
      c("applydebuff", "refreshdebuff", "removedebuff"),
      c(20, 40, 80),
      c(2, 2, 2),
      c(11, 11, 11),
      c(2002, 2002, 2002)
    ),
    aura_test_events("cast", 30, 99, 99, 9999)
  )

  out <- wclR::wcl_aura_uptime(events)
  out <- dplyr::arrange(out, abilityGameID)

  expect_equal(nrow(out), 2L)
  expect_equal(out$aura_type, c("buff", "debuff"))
  expect_equal(out$uptime_s, c(40, 60))
  expect_equal(out$uptime_pct, c(40, 60))
  expect_equal(out$uptime_fraction, c(0.4, 0.6))
  expect_equal(out$interval_count, c(1L, 1L))
  expect_equal(out$application_count, c(1L, 1L))
  expect_equal(out$refresh_count, c(0L, 1L))
  expect_false("sourceID" %in% names(out))
})

test_that("CombatantInfo snapshots identify target, caster, and pull-time state", {
  events <- aura_test_events(
    type = c("refreshbuff", "refreshbuff"),
    time_s = c(40, 50),
    source_id = c(7, 8),
    target_id = c(20, 20),
    ability_id = c(3003, 3004),
    source_instance_id = c(2L, NA_integer_),
    target_instance_id = c(4L, 4L)
  )
  combatant <- aura_test_events(
    type = "combatantinfo",
    time_s = 1,
    source_id = 20,
    target_id = NA_integer_,
    ability_id = NA_integer_,
    source_instance_id = 4L
  )
  combatant$auras <- list(list(
    list(
      abilityGameId = 3003,
      source = 7,
      stacks = 2,
      name = "Snapshot Aura",
      icon = "snapshot_icon"
    ),
    list(
      abilityGameId = 3004,
      source = 8,
      sourceInstanceId = 3,
      stacks = 1
    )
  ))

  out <- wclR::wcl_aura_uptime(
    events,
    combatant_info = combatant,
    by_source = TRUE
  )

  out <- dplyr::arrange(out, abilityGameID)
  expect_equal(nrow(out), 2L)
  expect_equal(out$targetID, c(20L, 20L))
  expect_equal(out$targetInstanceID, c(4L, 4L))
  expect_equal(out$sourceID, c(7L, 8L))
  # The first instance comes only from lifecycle data; the second comes only
  # from the nested CombatantInfo aura. Neither creates a duplicate state.
  expect_equal(out$sourceInstanceID, c(2L, 3L))
  expect_equal(out$abilityGameID, c(3003L, 3004L))
  expect_equal(out$aura_type, c("buff", "buff"))
  expect_equal(out$uptime_s, c(100, 100))
  expect_equal(out$uptime_pct, c(100, 100))
  expect_equal(out$initial_stacks, c(2, 1))
  expect_true(all(out$started_at_pull))
  expect_true(all(out$active_at_end))
  expect_false(any(out$initial_state_inferred))
})

test_that("a snapshot-only aura is retained as unknown with 100 percent uptime", {
  combatant <- aura_test_events(
    type = "combatantinfo",
    time_s = 2,
    source_id = 22,
    target_id = NA_integer_,
    ability_id = NA_integer_,
    duration_s = 75
  )
  combatant$auras <- list(list(list(
    ability = 4040,
    source = list(id = 9),
    stacks = 0
  )))

  out <- wclR::wcl_aura_uptime(
    aura_test_events(character(), numeric(), integer(), integer(), integer()),
    combatant_info = combatant,
    by_source = TRUE
  )

  expect_equal(out$aura_type, "unknown")
  expect_equal(out$targetID, 22L)
  expect_equal(out$sourceID, 9L)
  expect_equal(out$uptime_s, 75)
  expect_equal(out$uptime_pct, 100)
})

test_that("source grouping unions overlaps and source_ids filters before union", {
  events <- aura_test_events(
    c("applybuff", "applybuff", "removebuff", "removebuff"),
    c(10, 30, 50, 70),
    c(1, 2, 1, 2),
    c(10, 10, 10, 10),
    c(5005, 5005, 5005, 5005)
  )

  combined <- wclR::wcl_aura_uptime(events)
  separated <- wclR::wcl_aura_uptime(events, by_source = TRUE)
  filtered <- wclR::wcl_aura_uptime(events, source_ids = 2)

  expect_equal(combined$uptime_s, 60)
  expect_equal(combined$interval_count, 1L)
  expect_equal(sort(separated$sourceID), c(1L, 2L))
  expect_equal(separated$uptime_s[order(separated$sourceID)], c(40, 40))
  expect_equal(filtered$uptime_s, 40)
})

test_that("source filters do not fabricate state from unmatched removals", {
  events <- aura_test_events(
    c("applybuff", "removebuff"),
    c(10, 20),
    c(8L, NA_integer_),
    c(10L, 10L),
    c(5050L, 5050L)
  )

  expect_warning(
    out <- wclR::wcl_aura_uptime(
      events,
      source_ids = 7L,
      by_source = TRUE
    ),
    "could not be matched"
  )
  expect_equal(nrow(out), 0L)
})

test_that("concrete and missing source instances have deterministic removal semantics", {
  mismatched <- aura_test_events(
    c("applybuff", "removebuff"),
    c(10, 30),
    c(7L, 7L),
    c(20L, 20L),
    c(5151L, 5151L),
    source_instance_id = c(1L, 2L)
  )

  expect_warning(
    separate <- wclR::wcl_aura_uptime(mismatched, by_source = TRUE),
    "Initial aura state was inferred"
  )
  separate <- dplyr::arrange(separate, sourceInstanceID)
  expect_equal(separate$sourceInstanceID, c(1L, 2L))
  expect_equal(separate$uptime_s, c(90, 30))

  ambiguous <- aura_test_events(
    c("applybuff", "applybuff", "removebuff"),
    c(10, 20, 30),
    c(7L, 7L, 7L),
    c(20L, 20L, 20L),
    c(5252L, 5252L, 5252L),
    source_instance_id = c(1L, 2L, NA_integer_)
  )
  resolved <- wclR::wcl_aura_uptime(ambiguous, by_source = TRUE)
  resolved <- dplyr::arrange(resolved, sourceInstanceID)

  expect_equal(nrow(resolved), 2L)
  expect_equal(resolved$sourceInstanceID, c(1L, 2L))
  expect_equal(resolved$uptime_s, c(20, 10))
  expect_true(all(resolved$source_ambiguous))
})

test_that("promoted snapshot instances still honor ambiguous wildcard removals", {
  events <- aura_test_events(
    c("applybuff", "applybuff", "removebuff"),
    c(10, 20, 30),
    c(7L, 7L, 7L),
    c(20L, 20L, 20L),
    c(5353L, 5353L, 5353L),
    source_instance_id = c(1L, 2L, NA_integer_)
  )
  combatant <- aura_test_events(
    "combatantinfo",
    1,
    20L,
    NA_integer_,
    NA_integer_
  )
  combatant$auras <- list(list(list(
    abilityGameId = 5353L,
    source = 7L,
    stacks = 1L
  )))

  out <- wclR::wcl_aura_uptime(
    events,
    combatant_info = combatant,
    by_source = TRUE
  )
  out <- dplyr::arrange(out, sourceInstanceID)

  expect_equal(out$sourceInstanceID, c(1L, 2L))
  expect_equal(out$uptime_s, c(30, 10))
  expect_true(all(out$source_ambiguous))
})

test_that("source ambiguity flags only the affected intervals", {
  events <- aura_test_events(
    c("applybuff", "applybuff", "removebuff", "applybuff", "removebuff"),
    c(10, 10, 20, 30, 40),
    c(1L, 2L, NA_integer_, 1L, 1L),
    c(20L, 20L, 20L, 20L, 20L),
    c(5454L, 5454L, 5454L, 5454L, 5454L)
  )

  out <- wclR::wcl_aura_uptime(
    events,
    by_source = TRUE,
    output = "intervals"
  )
  source_one <- out[out$sourceID == 1L, , drop = FALSE]
  source_one <- dplyr::arrange(source_one, interval_start_s)

  expect_equal(source_one$interval_start_s, c(10, 30))
  expect_equal(source_one$source_ambiguous, c(TRUE, FALSE))
  expect_true(out$source_ambiguous[out$sourceID == 2L])
})

test_that("ambiguous trailing removals do not taint a later exact interval", {
  events <- aura_test_events(
    c(
      "applybuff", "applybuff", "removebuff", "removebuff",
      "removebuff", "applybuff", "removebuff"
    ),
    c(10, 10, 20, 20, 30, 40, 50),
    c(1L, 2L, 1L, 2L, NA_integer_, 1L, 1L),
    rep(20L, 7),
    rep(5500L, 7)
  )

  out <- wclR::wcl_aura_uptime(
    events,
    by_source = TRUE,
    output = "intervals"
  )
  source_one <- out[out$sourceID == 1L, , drop = FALSE]
  source_one <- dplyr::arrange(source_one, interval_start_s)

  expect_equal(source_one$interval_start_s, c(10, 40))
  expect_equal(source_one$source_ambiguous, c(FALSE, FALSE))
})

test_that("missing-instance refreshes prefer the one compatible active state", {
  events <- aura_test_events(
    c("applybuff", "removebuff", "applybuff", "refreshbuff"),
    c(10, 20, 30, 40),
    c(7L, 7L, 7L, 7L),
    c(20L, 20L, 20L, 20L),
    c(5555L, 5555L, 5555L, 5555L),
    source_instance_id = c(1L, 1L, 2L, NA_integer_)
  )

  out <- wclR::wcl_aura_uptime(events, by_source = TRUE)
  out <- dplyr::arrange(out, sourceInstanceID)

  expect_equal(nrow(out), 2L)
  expect_equal(out$sourceInstanceID, c(1L, 2L))
  expect_equal(out$uptime_s, c(10, 70))
  expect_equal(out$refresh_count, c(0L, 1L))
  expect_false(any(out$initial_state_inferred))
})

test_that("promoted snapshot keys do not capture later wildcard refreshes", {
  events <- aura_test_events(
    c("applybuff", "removebuff", "applybuff", "refreshbuff"),
    c(10, 20, 30, 40),
    c(7L, 7L, 7L, 7L),
    c(20L, 20L, 20L, 20L),
    c(5600L, 5600L, 5600L, 5600L),
    source_instance_id = c(1L, 1L, 2L, NA_integer_)
  )
  combatant <- aura_test_events(
    "combatantinfo",
    1,
    20L,
    NA_integer_,
    NA_integer_
  )
  combatant$auras <- list(list(list(
    abilityGameId = 5600L,
    source = 7L,
    stacks = 1L
  )))

  out <- wclR::wcl_aura_uptime(
    events,
    combatant_info = combatant,
    by_source = TRUE
  )
  out <- dplyr::arrange(out, sourceInstanceID)

  expect_equal(out$sourceInstanceID, c(1L, 2L))
  expect_equal(out$uptime_s, c(20, 70))
  expect_equal(out$refresh_count, c(0L, 1L))
})

test_that("trailing wildcard removals do not invent a pull-time state", {
  events <- aura_test_events(
    c("applybuff", "removebuff", "applybuff", "removebuff", "removebuff"),
    c(10, 20, 12, 22, 30),
    c(7L, 7L, 7L, 7L, 7L),
    c(20L, 20L, 20L, 20L, 20L),
    c(5650L, 5650L, 5650L, 5650L, 5650L),
    source_instance_id = c(1L, 1L, 2L, 2L, NA_integer_)
  )

  out <- wclR::wcl_aura_uptime(events, by_source = TRUE)
  out <- dplyr::arrange(out, sourceInstanceID)

  expect_equal(nrow(out), 2L)
  expect_equal(out$sourceInstanceID, c(1L, 2L))
  expect_equal(out$uptime_s, c(10, 10))
  expect_false(any(out$initial_state_inferred))
})

test_that("source-less refreshes update existing active state", {
  events <- aura_test_events(
    c("applybuff", "refreshbuff", "removebuff"),
    c(10, 20, 30),
    c(1L, NA_integer_, 1L),
    c(20L, 20L, 20L),
    c(5656L, 5656L, 5656L)
  )

  out <- wclR::wcl_aura_uptime(events, by_source = TRUE)

  expect_equal(nrow(out), 1L)
  expect_equal(out$sourceID, 1L)
  expect_equal(out$uptime_s, 20)
  expect_equal(out$refresh_count, 1L)
  expect_false(out$initial_state_inferred)
})

test_that("target-level event counts do not duplicate ambiguous routing", {
  events <- aura_test_events(
    c("applybuff", "applybuff", "refreshbuff", "removebuff", "removebuff"),
    c(10, 10, 20, 30, 30),
    c(1L, 2L, NA_integer_, 1L, 2L),
    rep(20L, 5),
    rep(5700L, 5)
  )

  combined <- wclR::wcl_aura_uptime(events)
  by_source <- wclR::wcl_aura_uptime(events, by_source = TRUE)

  expect_equal(combined$application_count, 2L)
  expect_equal(combined$refresh_count, 1L)
  expect_equal(by_source$refresh_count, c(1L, 1L))
})

test_that("stack variants keep an aura active until the current stack is zero", {
  events <- aura_test_events(
    c("applydebuffstack", "removedebuffstack", "removedebuffstack"),
    c(10, 30, 40),
    c(4, 4, 4),
    c(30, 30, 30),
    c(6006, 6006, 6006),
    stacks = c(2, 1, 0)
  )

  out <- wclR::wcl_aura_uptime(events)

  expect_equal(out$aura_type, "debuff")
  expect_equal(out$uptime_s, 30)
  expect_equal(out$initial_stacks, 2)
  expect_equal(out$max_stacks, 2)
})

test_that("actor instance aliases are canonicalized and included in grouping", {
  first <- aura_test_events(
    c("applybuff", "removebuff"),
    c(10, 20),
    c(1, 1),
    c(9, 9),
    c(6500, 6500),
    source_instance_id = c(1L, 1L),
    target_instance_id = c(10L, 10L)
  )
  second <- aura_test_events(
    c("applybuff", "removebuff"),
    c(30, 50),
    c(1, 1),
    c(9, 9),
    c(6500, 6500),
    source_instance_id = c(2L, 2L),
    target_instance_id = c(11L, 11L)
  )
  events <- dplyr::bind_rows(first, second)
  names(events)[names(events) == "sourceInstanceID"] <- "sourceInstance"
  names(events)[names(events) == "targetInstanceID"] <- "targetInstanceId"

  out <- wclR::wcl_aura_uptime(events, by_source = TRUE)
  out <- dplyr::arrange(out, targetInstanceID)

  expect_equal(nrow(out), 2L)
  expect_equal(out$targetInstanceID, c(10L, 11L))
  expect_equal(out$sourceInstanceID, c(1L, 2L))
  expect_equal(out$uptime_s, c(10, 20))
})

test_that("snapshot target instances reconcile within an ability before actor fallback", {
  lifecycle <- dplyr::bind_rows(
    aura_test_events(
      c("applybuff", "removebuff"),
      c(10, 50),
      c(7L, 7L),
      c(20L, 20L),
      c(3003L, 3003L),
      source_instance_id = c(1L, 1L),
      target_instance_id = c(4L, 4L)
    ),
    aura_test_events(
      c("applybuff", "removebuff"),
      c(20, 40),
      c(8L, 8L),
      c(20L, 20L),
      c(9999L, 9999L),
      source_instance_id = c(2L, 2L),
      target_instance_id = c(5L, 5L)
    )
  )
  combatant <- aura_test_events(
    "combatantinfo",
    1,
    20L,
    NA_integer_,
    NA_integer_
  )
  combatant$auras <- list(list(list(
    abilityGameId = 3003L,
    source = 7L,
    stacks = 1L
  )))

  out <- wclR::wcl_aura_uptime(
    lifecycle,
    combatant_info = combatant
  )
  ability <- out[out$abilityGameID == 3003L, , drop = FALSE]

  expect_equal(nrow(ability), 1L)
  expect_equal(ability$targetInstanceID, 4L)
  expect_equal(ability$aura_type, "buff")
  expect_equal(ability$uptime_s, 50)
  expect_false(ability$initial_state_inferred)
})

test_that("AA_test leading removal and trailing application use full-fight inference", {
  fixture <- utils::read.csv(
    fixture_path("aura_uptime_aa_distilled.csv"),
    stringsAsFactors = FALSE
  )
  expect_false("startTime" %in% names(fixture))

  make_pair <- function(source_id, target_id, ability_id, apply_at, remove_at) {
    rows <- fixture[rep.int(1L, 2L), , drop = FALSE]
    rows$type <- c("applybuff", "removebuff")
    rows$sourceID <- source_id
    rows$targetID <- target_id
    rows$abilityGameID <- ability_id
    rows$timestamp <- c(apply_at, remove_at)
    rows
  }

  extra_2825 <- dplyr::bind_rows(lapply(2:28, function(target_id) {
    make_pair(21L, target_id, 2825L, 796725, 836732)
  }))
  extra_26635 <- dplyr::bind_rows(
    make_pair(2L, 101L, 26635L, 800424, 810434),
    make_pair(5L, 102L, 26635L, 844639, 854644),
    make_pair(7L, 103L, 26635L, 803683, 813695)
  )
  fixture <- dplyr::bind_rows(fixture, extra_2825, extra_26635)

  expect_warning(
    out <- wclR::wcl_aura_uptime(fixture),
    "Initial aura state was inferred"
  )
  out <- dplyr::arrange(out, abilityGameID)

  expect_equal(nrow(out), 32L)
  expect_equal(
    as.numeric(difftime(out$fight_end_at[[1L]], out$fight_start_at[[1L]], units = "secs")),
    115.966,
    tolerance = 1e-9
  )
  expect_equal(out$uptime_s[out$abilityGameID == 24907L], 113.118, tolerance = 1e-9)
  expect_equal(out$uptime_pct[out$abilityGameID == 24907L], 97.5441077557, tolerance = 1e-8)
  expect_equal(out$interval_count[out$abilityGameID == 24907L], 2L)
  expect_true(out$initial_state_inferred[out$abilityGameID == 24907L])
  expect_true(out$active_at_end[out$abilityGameID == 24907L])
  expect_equal(unique(out$uptime_s[out$abilityGameID == 2825L]), 40.007, tolerance = 1e-9)
  expect_equal(
    sort(out$uptime_s[out$abilityGameID == 26635L]),
    c(10.005, 10.010, 10.012),
    tolerance = 1e-9
  )

  filtered <- wclR::wcl_aura_uptime(fixture, source_ids = 21L)
  expect_equal(nrow(filtered), 28L)
  expect_true(all(filtered$abilityGameID == 2825L))
})

test_that("interval output has typed relative and absolute boundaries", {
  events <- aura_test_events(
    c("applybuff", "removebuff"),
    c(12.5, 20.25),
    c(1, 1),
    c(2, 2),
    c(7007, 7007)
  )

  out <- wclR::wcl_aura_uptime(events, output = "intervals", by_source = TRUE)

  expect_equal(nrow(out), 1L)
  expect_equal(out$sourceID, 1L)
  expect_equal(out$interval_id, 1L)
  expect_equal(out$interval_start_ms, 12500)
  expect_equal(out$interval_end_ms, 20250)
  expect_equal(out$interval_start_s, 12.5)
  expect_equal(out$interval_end_s, 20.25)
  expect_equal(out$interval_duration_s, 7.75)
  expect_s3_class(out$interval_start_at, "POSIXct")
  expect_s3_class(out$interval_end_at, "POSIXct")
})

test_that("empty inputs return stable typed schemas", {
  events <- aura_test_events(character(), numeric(), integer(), integer(), integer())

  summary <- wclR::wcl_aura_uptime(events)
  intervals <- wclR::wcl_aura_uptime(events, output = "intervals", by_source = TRUE)

  expect_equal(nrow(summary), 0L)
  expect_equal(nrow(intervals), 0L)
  expect_type(summary$uptime_s, "double")
  expect_type(summary$uptime_fraction, "double")
  expect_type(summary$interval_count, "integer")
  expect_type(summary$targetInstanceID, "integer")
  expect_type(summary$abilityGameID, "integer")
  expect_false(any(c("n_intervals", "n_applications", "n_refreshes") %in% names(summary)))
  expect_type(intervals$interval_id, "integer")
  expect_type(intervals$interval_start_ms, "double")
  expect_type(intervals$interval_end_ms, "double")
  expect_type(intervals$interval_duration_s, "double")
  expect_s3_class(intervals$interval_start_at, "POSIXct")
  expect_type(intervals$sourceID, "integer")
  expect_type(intervals$sourceInstanceID, "integer")
})

test_that("public arguments and fight metadata are validated", {
  events <- aura_test_events("applybuff", 10, 1, 2, 3)

  expect_error(wclR::wcl_aura_uptime(events, by_source = NA), "TRUE or FALSE")
  expect_error(wclR::wcl_aura_uptime(events, ability_ids = c(1, NA)), "integer-like")
  expect_error(wclR::wcl_aura_uptime(events, target_ids = 1.5), "integer-like")
  expect_error(wclR::wcl_aura_uptime(events, source_ids = character()), "integer-like")
  expect_error(wclR::wcl_aura_uptime(events, output = "wide"), "arg")
  expect_error(wclR::wcl_aura_uptime(list()), "data frame")

  events$duration_s <- -1
  expect_error(wclR::wcl_aura_uptime(events), "positive `duration_s`")

  inconsistent <- aura_test_events(
    c("applybuff", "removebuff"),
    c(10, 20),
    c(1, 1),
    c(2, 2),
    c(3, 3)
  )
  inconsistent$fight_start_at[[2L]] <- inconsistent$fight_start_at[[2L]] + 1
  expect_error(wclR::wcl_aura_uptime(inconsistent), "inconsistent `fight_start_at`")

  inconsistent_bounds <- aura_test_events("applybuff", 10, 1, 2, 3)
  inconsistent_bounds$fight_end_at <- inconsistent_bounds$fight_end_at + 2
  expect_error(wclR::wcl_aura_uptime(inconsistent_bounds), "inconsistent with `duration_s`")

  parsed <- wclR:::.wcl_aura_time(c(
    "2026-08-07T09:41:59.966Z",
    "2026-08-07T11:41:59.966+02:00"
  ))
  expect_equal(as.numeric(parsed[[1L]]), as.numeric(parsed[[2L]]), tolerance = 1e-9)
  expect_equal(as.numeric(parsed[[1L]]) %% 1, 0.966, tolerance = 1e-6)
})
