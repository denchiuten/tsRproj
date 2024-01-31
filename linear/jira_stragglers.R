# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse,
  lubridate,
  scales,
  zoo,
  patchwork,
  keyring,
  DBI,
  RPostgreSQL,
  googlesheets4
)

query <- read_file("jira_stragglers.sql")
gsheet_url <- "https://docs.google.com/spreadsheets/d/10NHg1GFP2NK8NzxVJBRyZ8hOrx-5oXBR3NIZYpJHrdI/edit#gid=0"
gs4_auth("dennis@terrascope.com")

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))

ss <- gs4_get(gsheet_url)
write_sheet(df_raw, ss)
