#' Build a Warcraft Logs ability filter expression
#'
#' @param ids One or more numeric ability IDs.
#'
#' @return A scalar filter-expression string.
#' @export
wcl_filter_abilities <- function(ids) {
  ids <- unique(stats::na.omit(as.integer(ids)))

  if (!length(ids)) {
    return("")
  }

  paste0("ability.id in (", paste(ids, collapse = ", "), ")")
}

#' Build a Warcraft Logs event-type filter expression
#'
#' @param types One or more event-type strings such as `"cast"` or
#'   `"combatantinfo"`.
#'
#' @return A scalar filter-expression string.
#' @export
wcl_filter_types <- function(types) {
  types <- unique(trimws(as.character(types)))
  types <- types[!is.na(types) & nzchar(types)]

  if (!length(types)) {
    return("")
  }

  parts <- paste0(
    "type = '",
    gsub("'", "\\\\'", types, fixed = TRUE),
    "'"
  )

  paste(parts, collapse = " or ")
}

#' Combine filter-expression fragments with `and`
#'
#' @param ... Filter-expression fragments.
#'
#' @return A scalar filter-expression string.
#' @export
wcl_filter_and <- function(...) {
  parts <- unlist(list(...), recursive = TRUE, use.names = FALSE)
  parts <- trimws(as.character(parts))
  parts <- parts[!is.na(parts) & nzchar(parts)]

  if (!length(parts)) {
    return("")
  }

  paste(parts, collapse = " and ")
}
