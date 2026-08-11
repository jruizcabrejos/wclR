#' Build common Warcraft Logs filter expressions
#'
#' These helpers construct reusable fragments for the `filter_expression`
#' argument of [wcl_events()]. A filter expression is written in the Warcraft
#' Logs site query language, not in GraphQL. It is evaluated by Warcraft Logs
#' to select matching events; it does not select fields from the event JSON or
#' remove columns from the returned tibble.
#'
#' `wcl_filter_abilities()` builds an `ability.id in (...)` expression. IDs are
#' coerced to integers, and missing and duplicate values are removed.
#'
#' `wcl_filter_types()` builds equality tests for the raw event `type`
#' identifier and joins multiple types with `or`. Values are trimmed, and
#' missing, empty, and duplicate values are removed. Raw event types such as
#' `"damage"` and `"cast"` are distinct from the GraphQL `data_type` values
#' accepted by [wcl_events()], such as `"DamageTaken"` and `"Casts"`.
#'
#' `wcl_filter_and()` recursively flattens its inputs, removes missing and empty
#' fragments, and joins the remainder with `and`. It concatenates fragments
#' verbatim and does not add parentheses. Explicitly parenthesize any fragment
#' containing `or` before combining it with this helper.
#'
#' The helpers cover only these common cases. Any other valid Warcraft Logs
#' expression can be supplied directly as one character string. The helpers do
#' not validate expression syntax or check event-type names; Warcraft Logs is
#' the authority for the supported operators, identifiers, and values.
#'
#' The official expression language is SQL-like. It includes logical operators
#' such as `AND`, `OR`, and `NOT`; comparison and arithmetic operators;
#' `BETWEEN`, `IN`, and `NOT IN`; quoted strings; dotted identifiers such as
#' `source.name` and `ability.id`; and documented function calls. Consult the
#' official guide for the complete and current language rather than treating
#' these helpers as an exhaustive grammar.
#'
#' @param ids One or more numeric ability IDs.
#' @param types One or more raw event-type strings such as `"damage"` or
#'   `"cast"`. These are values of the expression-language `type` identifier,
#'   not GraphQL `EventDataType` values.
#' @param ... Filter-expression fragments.
#'
#' @return A scalar filter-expression string. A helper returns `""` when no
#'   usable input remains.
#'
#' @seealso [wcl_events()]
#'
#' @references
#' [Warcraft Logs Report schema](https://www.warcraftlogs.com/v2-api-docs/warcraft/report.doc.html)
#'
#' [Warcraft Logs expression-language and Pins guide](https://www.warcraftlogs.com/help/pins)
#'
#' [Warcraft Logs EventDataType enum](https://www.warcraftlogs.com/v2-api-docs/warcraft/eventdatatype.doc.html)
#'
#' @examples
#' wcl_filter_abilities(c(70911, 72293, 70911, NA))
#' #> "ability.id in (70911, 72293)"
#'
#' wcl_filter_types(c("damage", "miss"))
#' #> "type = 'damage' or type = 'miss'"
#'
#' # Group an `or` fragment explicitly before joining it with `and`.
#' type_filter <- paste0(
#'   "(",
#'   wcl_filter_types(c("damage", "miss")),
#'   ")"
#' )
#' wcl_filter_and(wcl_filter_abilities(70911), type_filter)
#' #> "ability.id in (70911) and (type = 'damage' or type = 'miss')"
#'
#' # More advanced expressions can be written directly.
#' custom_filter <- "source.type = 'player' and effectiveDamage > 0"
#' @name wcl_filters
NULL

#' @rdname wcl_filters
#' @export
wcl_filter_abilities <- function(ids) {
  ids <- unique(stats::na.omit(as.integer(ids)))

  if (!length(ids)) {
    return("")
  }

  paste0("ability.id in (", paste(ids, collapse = ", "), ")")
}

#' @rdname wcl_filters
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

#' @rdname wcl_filters
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
