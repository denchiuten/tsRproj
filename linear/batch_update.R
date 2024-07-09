# purpose -----------------------------------------------------------------

# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse,
  keyring,
  DBI,
  RPostgreSQL,
  httr,
  RJSONIO,
  stringr,
  glue
)

con <- aws_connect()

query <- "
  -- CTE to generate mapping of Linear user IDs to HiBob Team IDs and names
  WITH employee_team AS (
  	SELECT 
  		u.id AS user_id,
  		u.email,
  		et.team_id,
  		et.team_name
  	FROM linear.users AS u
  	INNER JOIN bob.employee AS e
  		ON LOWER(u.email) = LOWER(e.email)
  	INNER JOIN bob.vw_employee_team AS et
  		ON e.id = et.employee_id
  )
  
  SELECT
  	l2.id AS label_id,
  	creator.team_name AS requesting_team,
  	i.id AS issue_id
  FROM linear.issue AS i
  INNER JOIN linear.team AS t
  	ON i.team_id = t.id
  	AND t.key IN ('OPS', 'LAW', 'CLAW', 'PPL', 'IT', 'DEV') -- only care about teams that receive a lot of requests from internal stakeholders
  
  -- identify issues that are already tagged with a label in the Requesting Team group so we can exclude them
  LEFT JOIN (
  	SELECT il.*
  	FROM linear.issue_label AS il
  	INNER JOIN linear.label AS l
  		ON il.label_id = l.id
  		AND l._fivetran_deleted IS FALSE
  		AND l.parent_id = '591c8377-11b9-43ab-9b39-58b3f7e9a36b' -- parent_id for Requesting Team label group
  	WHERE il._fivetran_deleted IS FALSE
  ) AS request_team
  	ON i.id = request_team.issue_id
  
  -- first JOIN to the CTE to get the Team name for the issue creator
  INNER JOIN employee_team AS creator
  	ON i.creator_id = creator.user_id
  
  -- second JOIN to the CTE to get the Team name for the issue assignee
  INNER JOIN employee_team AS assignee
  	ON i.assignee_id = assignee.user_id
  	AND creator.team_id <> assignee.team_id -- exclude cases where the creator and assignee are from the same team
  INNER JOIN linear.label AS l2
  	ON creator.team_name || ' Team' = l2.name
  	AND l2.parent_id = '591c8377-11b9-43ab-9b39-58b3f7e9a36b' -- parent_id for Requesting Team label group
  WHERE
  	i._fivetran_deleted IS FALSE
  	AND request_team.issue_id IS NULL -- only pull issues not already labeled


"

source("linear_functions.R")


# redshift query ----------------------------------------------------------


df_raw <- dbFetch(dbSendQuery(con, query))

# clean -------------------------------------------------------------------

df_clean <- df_raw |> 
  group_by(
    label_id, requesting_team
  ) |> 
  summarise(
    ids = paste0('\"', issue_id, '\"', collapse = ", "),
    .groups = "drop"
    ) |> 
  arrange(requesting_team)

# loop --------------------------------------------------------------------
i <- 2

for (i in 1:nrow(df_clean)) {
  
  label_id <- df_clean$label_id[i]
  requesting_team <- df_clean$requesting_team[i]
  issue_ids <- df_clean$ids[i]
  
  response <- issue_batch_labels_update(label_id, issue_ids)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Added Security label to {issue_key} ({i} of {nrow(df_clean)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_clean)})"))
  }
}


# brute force -------------------------------------------------------------

df_filtered <- df_raw |> 
  filter(requesting_team != "Strategy")

for (i in 1:nrow(df_filtered)) {
  
  label_id <- df_filtered$label_id[i]
  issue_id <- df_filtered$issue_id[i]
  requesting_team <- df_filtered$requesting_team[i]
  
  response <- assign_label(issue_id, label_id)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Added {requesting_team} label to {issue_id} ({i} of {nrow(df_filtered)})"))
  } else {
    print(str_glue("Failed to update issue {issue_id}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_filtered)})"))
  }
}


