
# purpose -----------------------------------------------------------------

# fix parent_child_mappings from jira to linear
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
  RJSONIO
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

sql_query <- read_file("child_parent.sql")
api_url <- "https://api.linear.app/graphql"


# pull raw data -----------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, sql_query))

# self-join ---------------------------------------------------------------

df_map <- df_raw |> 
  select(
    linear_issue_id,
    linear_issue_key,
    jira_issue_id,
    jira_issue_key
  )

df_child_parent <- df_raw |> 
  left_join(
    df_map,
    by = c("jira_parent_issue_id" = "jira_issue_id"),
    suffix = c(".child", ".parent")
    )

# check what parent issue types are missing
df_check <- df_child_parent |> 
  filter(
    !is.na(jira_parent_issue_id),
    is.na(linear_issue_id.parent)
  ) |> 
  count(jira_parent_issue_type)
