
# purpose -----------------------------------------------------------------
#  fix issues that had their assignees removed by Linear 2-way sync

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
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

query <- "
  SELECT	
  	hist.issue_id,
  	i.key,
  	hist.value AS assignee_id,
  	u.name AS assignee_name,
  	hist.time
  FROM jra.issue_field_history AS hist
  INNER JOIN jra.issue AS i
    ON hist.issue_id = i.id
  INNER JOIN(
  	SELECT 
  		field.issue_id
  	FROM jra.vw_latest_issue_field_value AS field
  	WHERE
  		1 = 1	
  		AND field.field_id = 'assignee'
  		AND field.value IS NULL
  		AND field.time::DATE >= '2023-12-14'
  		AND field.author_id = '63c50741cd6a09abe71e007c' -- Bryan
  ) AS latest
  	ON hist.issue_id = latest.issue_id
  LEFT JOIN jra.user AS u
  	ON hist.value = u.id
  WHERE
  	1 = 1
  	AND hist.field_id = 'assignee'
"

jira_base <- "https://gpventure.atlassian.net"
jira_username <- "dennis@terrascope.com"
# pull data ---------------------------------------------------------------
con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# data prep ---------------------------------------------------------------

# group by issue id and rank by update time

df_ranked <- df_raw |> 
  arrange(issue_id, desc(time)) |> 
  group_by(issue_id) |> 
  mutate(rank = dense_rank(desc(time))) |> 
  ungroup()

# pull out just the last valid assignee 
df_last <- df_ranked |> 
  filter(rank == 2) |> 
  select(-rank)


# function ----------------------------------------------------------------

# Function to change the assignee of an issue
update_assignee <- function(issue_key, user_id, user_name) {
  url <- str_glue("{jira_base}/rest/api/2/issue/{issue_key}")
  data <- toJSON(
    list(
      fields = list(
        assignee = c(id = user_id)
        )
      )
    )
  
  # PUT request to update the issue
  response <- PUT(
    url, 
    body = data, 
    authenticate(jira_username, key_get("jira", jira_username), "basic"),
    add_headers("Content-Type" = "application/json", "Accept" = "application/json")
    )
}
# run a loop --------------------------------------------------------------

for (i in 1:nrow(df_last)) {
  
  issue_key <- df_last$key[i]
  user_id <- df_last$assignee_id[i]
  user_name <- df_last$assignee_name[i]
 
  response <- update_assignee(issue_key, user_id, user_name)
  # Check response
  if (status_code(response) == 204) {
    print(str_glue("Updated assignee for issue {issue_key} to {user_name}"))
  } else {
    print(str_glue("Failed to update issue {issue_id}: Error {status_code(response)}"))
  }
}
