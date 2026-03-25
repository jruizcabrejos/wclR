test_that("legacy query shortcuts still build compatible query strings", {
  logs_query <- wclR::wcl_query("logs", 1020, 1)
  events_query <- wclR::wcl_query("dmgtaken", "ABC123", 7, "ability.id in (70911)")
  custom_query <- wclR::wcl_query_custom("{ reportData { report(code: \"%s\") { code } } }", "ABC123")

  expect_match(logs_query, "reports\\(zoneID: 1020, page: 1\\)")
  expect_match(events_query, "dataType: DamageTaken")
  expect_match(custom_query, "ABC123")
})
