# purpose -----------------------------------------------------------------

# pull pages from Projects DB in Notion and create Projects in Linear

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
  httr,
  jsonlite,
  stringr,
  googlesheets4
)

notion_query <- read_file("pull_notion_projects.sql")

# pull raw data -----------------------------------------------------------
con <- aws_connect()
df_notion_raw <- dbFetch(dbSendQuery(con, notion_query))
