test_that("wcl_request sends raw queries to the documented client endpoint", {
  ns <- asNamespace("wclR")
  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) {
      captured$url <- url
      captured$body_json <- body_json
      list(text = "{\"data\":{\"ok\":true}}", status_code = 200L)
    },
    .env = ns,
    .package = "wclR"
  )

  payload <- wclR::wcl_request(
    token = "fixture-token",
    query = "{ ok }"
  )

  expect_identical(payload$data$ok, TRUE)
  expect_identical(captured$url, "https://classic.warcraftlogs.com/api/v2/client")
  expect_match(captured$body_json, "\\{ ok \\}")
})

test_that("wcl_request stops on GraphQL errors", {
  ns <- asNamespace("wclR")

  local_mocked_bindings(
    .wcl_http_post = function(url, headers, body_json) fixture_response("graphql_error.json"),
    .env = ns,
    .package = "wclR"
  )

  expect_error(
    wclR::wcl_request(token = "fixture-token", query = "{ broken }"),
    "Bad query"
  )
})
