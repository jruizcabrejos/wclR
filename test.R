library(wclR)

options(scipen = 100)
options(Encoding = "UTF-8")

client <- wcl_client()

report_code <- wcl_report_code("https://classic.warcraftlogs.com/reports/ZXaDV9FcAtyWnJPg")

fights <- wcl_fights(report_code, client = client)
actors <- wcl_actors(report_code, client = client)

example_query <- wcl_query(
  query = "dmgtaken",
  log = report_code,
  fights$fightID[[1]],
  "ability.id in (70911)"
)

raw_payload <- wcl_request(client = client, query = example_query)
