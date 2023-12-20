
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

# now prep the final data frame for processing
df_final <- df_child_parent |> 
  filter(!is.na(linear_issue_id.parent))


# function ----------------------------------------------------------------

assign_parent <- function(child_id, parent_id, url) {

  mutation <- str_glue(
    "
    mutation{{
      issueUpdate(
        id: \"{child_id}\"
        input: {{
          parentId: \"{parent_id}\" 
        }}
        ) {{
        success
      }}
    }}
  ")
  
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
}

# run a loop --------------------------------------------------------------
for (i in 1:nrow(df_final)) {
  
  child_id <- df_final$linear_issue_id.child[i]
  parent_id <- df_final$linear_issue_id.parent[i]
  
  child_key <- df_final$linear_issue_key.child[i]
  parent_key <- df_final$linear_issue_key.parent[i]
  
  response <- assign_parent(child_id, parent_id, api_url)
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("{parent_key} is now the parent of {child_key}"))
  } else {
    print(str_glue("Failed to update issue {child_key}: Error {status_code(response)}"))
  }
}

