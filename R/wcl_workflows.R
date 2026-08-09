#' Expand zone report pages into fight rows
#'
#' @param zone_id Warcraft Logs zone ID.
#' @param pages Page number or vector of exact page numbers. Defaults to pages
#'   1 through 3. Use `NULL` for automatic discovery through page 25.
#' @param distinct Whether to apply [wcl_distinct_fights()] to the combined
#'   fight table. Defaults to `TRUE`.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble with one row per fight plus report-page metadata from
#'   [wcl_reports()].
#' @export
wcl_zone_fights <- function(zone_id, pages = 1:3, distinct = TRUE, client = NULL) {
  client <- .wcl_resolve_client(client)
  reports <- wcl_reports(zone_id = zone_id, pages = pages, client = client)

  if (!nrow(reports)) {
    return(.wcl_empty_zone_fights())
  }

  report_meta <- reports
  rename_map <- c(
    visibility = "report_visibility",
    region = "report_region",
    revision = "report_revision",
    segments = "report_segments",
    page = "report_page",
    current_page = "report_current_page",
    last_page = "report_last_page",
    has_more_pages = "report_has_more_pages",
    per_page = "report_per_page",
    total = "report_total",
    startTime = "report_start_ms",
    endTime = "report_end_ms"
  )

  for (old_name in names(rename_map)) {
    if (old_name %in% names(report_meta)) {
      names(report_meta)[names(report_meta) == old_name] <- rename_map[[old_name]]
    }
  }

  report_cols <- intersect(
    c(
      "logID",
      "zone_id",
      "report_page",
      "report_current_page",
      "report_last_page",
      "report_has_more_pages",
      "report_per_page",
      "report_total",
      "report_visibility",
      "report_region",
      "report_revision",
      "report_segments",
      "report_start_ms",
      "report_end_ms"
    ),
    names(report_meta)
  )
  report_meta <- report_meta[report_cols]

  fights <- purrr::map_dfr(reports$logID, wcl_fights, client = client)

  if (!nrow(fights)) {
    return(.wcl_empty_zone_fights())
  }

  if (isTRUE(distinct)) {
    fights <- wcl_distinct_fights(fights)
  }

  out <- dplyr::left_join(fights, report_meta, by = "logID")

  dplyr::relocate(
    out,
    dplyr::any_of(c(
      "zone_id",
      "report_page",
      "report_current_page",
      "report_last_page",
      "report_has_more_pages",
      "report_per_page",
      "report_total",
      "report_visibility",
      "report_region",
      "report_revision",
      "report_segments",
      "report_start_ms",
      "report_end_ms"
    )),
    .after = report_end_at
  )
}

#' Retrieve multiple ranking metrics and pages in one call
#'
#' @param zone_id Warcraft Logs zone ID.
#' @param metrics Ranking metrics such as `"speed"` or `"progress"`.
#' @param pages Ranking page number or vector of page numbers.
#' @param encounter_id Optional encounter ID filter.
#' @param class_name Optional class name. If provided, character rankings are
#'   requested instead of fight rankings.
#' @param client Optional [wcl_client()] object.
#'
#' @return A tibble with one row per ranking entry across all requested metrics
#'   and pages.
#' @export
wcl_rankings_set <- function(
    zone_id,
    metrics,
    pages,
    encounter_id = NULL,
    class_name = NULL,
    client = NULL) {
  client <- .wcl_resolve_client(client)
  metrics <- unique(as.character(metrics))

  purrr::map_dfr(
    metrics,
    function(metric) {
      wcl_rankings(
        zone_id = zone_id,
        metric = metric,
        page = pages,
        encounter_id = encounter_id,
        class_name = class_name,
        client = client
      )
    }
  )
}
