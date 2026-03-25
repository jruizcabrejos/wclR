fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

read_fixture <- function(name) {
  paste(readLines(fixture_path(name), warn = FALSE), collapse = "\n")
}

fixture_response <- function(name, status_code = 200L) {
  list(
    text = read_fixture(name),
    status_code = status_code
  )
}

mock_client <- function() {
  client <- wclR::wcl_client(client_id = "fixture-id", client_secret = "fixture-secret")
  client$token <- "fixture-token"
  client$expires_at <- Sys.time() + 3600
  client
}
