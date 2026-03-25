test_that("wcl_client uses environment defaults and caches tokens", {
  ns <- asNamespace("wclR")
  calls <- 0L
  old_id <- Sys.getenv("WCLR_CLIENT_ID", unset = NA_character_)
  old_secret <- Sys.getenv("WCLR_CLIENT_SECRET", unset = NA_character_)

  on.exit({
    if (is.na(old_id)) Sys.unsetenv("WCLR_CLIENT_ID") else Sys.setenv(WCLR_CLIENT_ID = old_id)
    if (is.na(old_secret)) Sys.unsetenv("WCLR_CLIENT_SECRET") else Sys.setenv(WCLR_CLIENT_SECRET = old_secret)
  }, add = TRUE)

  Sys.setenv(
    WCLR_CLIENT_ID = "fixture-id",
    WCLR_CLIENT_SECRET = "fixture-secret"
  )

  local_mocked_bindings(
    .wcl_http_post_token = function(url, client_id, client_secret) {
      calls <<- calls + 1L
      fixture_response("token_response.json")
    },
    .env = ns,
    .package = "wclR"
  )

  client <- wclR::wcl_client()
  expect_s3_class(client, "wcl_client")

  token_one <- wclR:::.wcl_get_access_token(client)
  token_two <- wclR:::.wcl_get_access_token(client)

  expect_equal(token_one, "fixture-token")
  expect_equal(token_two, "fixture-token")
  expect_equal(calls, 1L)
})
