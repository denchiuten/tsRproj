
# purpose -----------------------------------------------------------------

# assign cycle to all issues migrated from PCF and CCF to APPS

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
  stringr
)
# api_url <- "https://api.linear.app/graphql"
query <- read_file("cycle_change_map.sql")

source("linear_functions.R")
# pull jira data from redshift --------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))
df_clean <- df_raw %>% 
  arrange(identifier)


# run loop to assign cycle to every issue ---------------------------------

for (i in 1:nrow(df_clean)) {
  
  issue_id <- df_clean$id[i]
  cycle_id <- df_clean$new_cycle_id[i]
  issue_key <- df_clean$identifier[i]
  
  response <- assign_cycle(issue_id, cycle_id)
  
  # Check response
  if (!is.null(response$data)) {
    print(str_glue("Assigned {issue_key} to cycle ({i} of {nrow(df_clean)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error \"{response$errors[[1]]$extensions$userPresentableMessage}\""))
  }
}
