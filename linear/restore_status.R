# purpose -----------------------------------------------------------------

# Fix the status of issues that were migrated from SEC to DEV

# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse,
  keyring,
  DBI,
  RPostgreSQL,
  httr,
  RJSONIO,
  stringr
)

query <- "
  SELECT
  	i.id AS issue_id,
  	i.identifier,
  	i.title,
  	from_ws.name AS from_state,
  	to_ws.name AS to_state,
  	newstate.id AS new_state_id
  FROM linear.issue_history AS h
  INNER JOIN linear.team AS from_team
  	ON h.from_team_id = from_team.id
  	AND from_team.key = 'SEC'
  INNER JOIN linear.team AS to_team
  	ON h.to_team_id = to_team.id
  	AND to_team.key = 'DEV'
  INNER JOIN linear.issue AS i
  	ON h.issue_id = i.id
  	AND i._fivetran_deleted IS FALSE
  INNER JOIN linear.workflow_state AS from_ws
  	ON h.from_state_id = from_ws.id
  INNER JOIN linear.workflow_state AS to_ws
  	ON h.to_state_id = to_ws.id
  LEFT JOIN (
  	SELECT 
  		ws.type,
  		ws.name,
  		ws.id 
  	FROM linear.workflow_state AS ws
  	INNER JOIN linear.team AS t
  		ON ws.team_id = t.id
  		AND t.key = 'DEV'
  	WHERE
  		1 = 1
  		AND ws._fivetran_deleted IS FALSE
  ) AS newstate
  	ON from_ws.name = newstate.name
  WHERE
  	1 = 1
  	AND h._fivetran_deleted IS FALSE
  	AND from_ws.name <> to_ws.name
"

source("linear_functions.R")


# redshift query ----------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))

# prep data ---------------------------------------------------------------

df_clean <- df_raw |> 
  arrange(identifier) |> 
  mutate(
    new_state_id = case_when(
      !is.na(new_state_id) ~ new_state_id,
      from_state == "Daily Standup" ~ "e7eedec3-c901-4a0d-9992-5fdba89b202e", #In Progress
      from_state == "Ready For Integration" ~ "9bfa487e-0dbc-41c9-bef4-767726410214" # Pending Release
    )
  )


# loop --------------------------------------------------------------------


for (i in 1:nrow(df_clean)) {
  
  issue_id <- df_clean$issue_id[i]
  issue_key <- df_clean$identifier[i]
  state_id <- df_clean$new_state_id[i]
  
  response <- update_state(issue_id, state_id)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Updated state of {issue_key} ({i} of {nrow(df_clean)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_clean)})"))
  }
}

