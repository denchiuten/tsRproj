# purpose -----------------------------------------------------------------

# prep tasks for import into Linear

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
  RJSONIO,
  stringr,
  googlesheets4
)

tasks_query <- read_file("tasks.sql")
user_query <- read_file("user_lookup.sql")
source("json_functions.R")

# pull raw data -----------------------------------------------------------

con <- aws_connect()
df_tasks_raw <- dbFetch(dbSendQuery(con, tasks_query))
df_user_lookup <- dbFetch(dbSendQuery(con, user_query))

# now clean the tasks df to parse the json --------------------------------

df_tasks_clean <- df_tasks_raw %>%
  mutate(
    assignee_id = sapply(assignee_json, parse_id),
    status = sapply(task_status_json, parse_id),
    title = sapply(title_json, parse_title)
  ) |> 
  select(!contains("json"))

# View
