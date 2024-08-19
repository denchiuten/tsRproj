# purpose -----------------------------------------------------------------

# Apply Requesting team label

# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse,
  keyring,
  DBI,
  RPostgreSQL,
  stringr,
  httr,
  RJSONIO
)

query <- read_file("issue_creator_team_query.sql")
source("linear_functions.R")


# redshift query ----------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))

df_final <- df_raw |> 
  arrange(identifier, label_id)

# loop --------------------------------------------------------------------

for (i in 1:nrow(df_final)) {
  
  issue_id <- df_final$issue_id[i]
  issue_key <- df_final$identifier[i]
  linear_label_id <- df_final$label_id[i]
  
  response <- assign_label(issue_id, linear_label_id)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Added Requesting Team label to {issue_key} ({i} of {nrow(df_final)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_final)})"))
  }
}

