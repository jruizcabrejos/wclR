#' Get a bearer token for the public Warcraft Logs API
#'
#' `wcl_token()` is kept as a compatibility helper. Newer code should prefer
#' [wcl_client()], which caches tokens and refreshes them automatically.
#'
#' @param clientid Client ID for the Warcraft Logs application. Defaults to
#'   `Sys.getenv("WCLR_CLIENT_ID")`.
#' @param clientsecret Client secret for the Warcraft Logs application.
#'   Defaults to `Sys.getenv("WCLR_CLIENT_SECRET")`.
#' @param host Warcraft Logs host. Defaults to `"classic"`.
#'
#' @return A bearer token string.
#' @export
wcl_token <- function(
    clientid = Sys.getenv("WCLR_CLIENT_ID", unset = ""),
    clientsecret = Sys.getenv("WCLR_CLIENT_SECRET", unset = ""),
    host = "classic") {
  client <- wcl_client(
    client_id = clientid,
    client_secret = clientsecret,
    host = host
  )

  .wcl_get_access_token(client)
}
