# purpose -----------------------------------------------------------------

# Apply Security label to issues moved from SEC team to DEV

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

linear_label_id <- "f13116e0-20f8-4716-ae4b-a61b517c10cb" #id for Security label
con <- aws_connect()

query <- glue_sql(
  "SELECT
	i.id AS issue_id,
	i.identifier,
	i.title
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
LEFT JOIN linear.issue_label AS il
	ON i.id = il.issue_id
	AND il.label_id = 'f13116e0-20f8-4716-ae4b-a61b517c10cb'
WHERE
	1 = 1
	AND h._fivetran_deleted IS FALSE
	AND il.label_id IS NULL",
  .con = con
)

source("linear_functions.R")


# redshift query ----------------------------------------------------------


df_raw <- dbFetch(dbSendQuery(con, query))
df_clean <- df_raw |> 
  arrange(from_state, identifier)

# loop --------------------------------------------------------------------


for (i in 1:nrow(df_clean)) {
  
  issue_id <- df_clean$issue_id[i]
  issue_key <- df_clean$identifier[i]
  
  response <- assign_label(issue_id, linear_label_id)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Added Security label to {issue_key} ({i} of {nrow(df_clean)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_clean)})"))
  }
}

