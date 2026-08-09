# wclR

`wclR` wraps the Warcraft Logs public GraphQL API with a layered R interface:

- a cached API client
- typed helpers for reports, fights, actors, player details, events, and both
  report-level and zone-level rankings
- joined workflow helpers for zone crawls, roster enrichment, and relative event timing
- a raw GraphQL escape hatch for unsupported queries

The [official Warcraft Logs v2 Warcraft schema](https://www.warcraftlogs.com/v2-api-docs/warcraft/)
is the reference for fields, arguments, enums, and custom GraphQL queries.

## Installation

```r
remotes::install_github("jruizcabrejos/wclR")
```

## Authentication

Create a Warcraft Logs API client and store the credentials in environment
variables:

```r
Sys.setenv(
  WCLR_CLIENT_ID = "your-client-id",
  WCLR_CLIENT_SECRET = "your-client-secret"
)
```

Then create a reusable client:

```r
library(wclR)

client <- wcl_client()
```

## Quick start

### Raid report workflow

```r
report_code <- wcl_report_code("https://classic.warcraftlogs.com/reports/ZXaDV9FcAtyWnJPg")

fights <- wcl_fights(report_code, client = client)
actors <- wcl_actors(report_code, client = client)
players <- wcl_player_details(report_code, fight_id = fights$fightID[[1]], client = client)
events <- wcl_events(
  report_code,
  fight_id = fights$fightID[[1]],
  data_type = "DamageTaken",
  hostility_type = "Friendlies",
  include_resources = FALSE,
  filter_expression = wcl_filter_and(
    wcl_filter_abilities(70911),
    wcl_filter_types("damage")
  ),
  client = client
)

events <- wcl_join_actors(events, actors)
events <- wcl_join_players(events, players)
events <- wcl_add_relative_time(events)
```

`wcl_events()` defaults `include_resources`, `use_ability_ids`, and
`use_actor_ids` to `TRUE`. Set only the detail flags you do not need to
`FALSE` to reduce the API payload. For example, `include_resources = FALSE`
avoids requesting detailed unit resources such as `classResources`. Disabling
an ID flag also changes which corresponding detail or ID columns may be present.

Warcraft Logs returns [`ReportEventPaginator.data`](https://www.warcraftlogs.com/v2-api-docs/warcraft/reporteventpaginator.doc.html)
as a [JSON scalar](https://www.warcraftlogs.com/v2-api-docs/warcraft/json.doc.html),
so arbitrary nested event columns cannot be excluded server-side by name.
Dropping a column after the request changes the returned R table but does not
save API bandwidth:

```r
events <- dplyr::select(events, -dplyr::any_of("classResources"))
```

### Report rankings

Omit `fight_ids` to retrieve tidy rankings for every applicable fight in a
report, or supply one or more IDs to restrict the API request. Whole-report
mode returns the report's individual fight rankings, not one aggregate rank:

```r
report_rankings <- wcl_report_rankings(
  report_code,
  player_metric = "dps",
  client = client
)

fight_rankings <- wcl_report_rankings(
  report_code,
  fight_ids = fights$fightID[[1]],
  player_metric = "dps",
  client = client
)
```

The default `output = "tidy"` returns one row per ranked character and fight.
Use `output = "raw"` when you need the exact parsed `report$rankings` object
returned by Warcraft Logs:

```r
raw_rankings <- wcl_report_rankings(
  report_code,
  fight_ids = fights$fightID[[1]],
  output = "raw",
  client = client
)
```

Tidy rows include `segments`, `exportedSegments`, and `rankings_complete`.
`wcl_report_rankings()` warns when Warcraft Logs has not processed every
uploaded segment yet. See the [rankings and parses guide](https://www.warcraftlogs.com/help/ranks/)
for how comparisons and timeframes are interpreted.

Direct scalar ranking fields become columns, while arrays and nested or
unstable values remain list-columns. If Warcraft Logs returns a new non-empty
shape that the tidy parser does not recognize, retry with `output = "raw"`.

### Population workflow

```r
first_three_pages <- wcl_reports(zone_id = 1020, client = client)
first_page <- wcl_reports(zone_id = 1020, pages = 1, client = client)

zone_fights <- wcl_zone_fights(zone_id = 1020, pages = 1:3, client = client)
zone_rankings <- wcl_rankings_set(
  zone_id = 1018,
  metrics = c("speed", "progress"),
  pages = 1:2,
  client = client
)
```

`wcl_reports()` retrieves pages `1:3` by default; pass `pages = 1` when only
the first page is wanted. Each requested page prints progress in the form
`Report page <page>: <page count> reports (<running total> total).`
Requested page numbers above 25 warn before retrieval and are capped to page
25.

`wcl_rankings()` and `wcl_rankings_set()` retrieve encounter leaderboards for
a zone. `wcl_report_rankings()` instead retrieves the rankings contained in one
specific uploaded report.

### Raw query escape hatch

```r
query <- wcl_query_custom(
  '{
     reportData {
       report(code: "%s") {
         code
         title
       }
     }
   }',
  "ZXaDV9FcAtyWnJPg"
)

raw_payload <- wcl_request(client = client, query = query)
```

Use the [official API schema](https://www.warcraftlogs.com/v2-api-docs/warcraft/)
to identify the fields and arguments to include in custom queries.
