
# purpose -----------------------------------------------------------------

# apply labels to PCC issues

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

query <- read_file("open_project_query.sql")
source("linear_functions.R")
cutoff_date <- as.Date("2023-12-01")
# Redshift query -----------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# filter the data frame ---------------------------------------------------

df_final <- df_raw |> 
  filter(
    n_issues == 0,
    (created_at <= cutoff_date | creator_email == "dennis@terrascope.com")
    )


# now loop to add label----------------------------------------------------------------

for (i in 1:nrow(df_final)) {
  
  project_id <- df_final$project_id[i]
  project_name <- df_final$project_name[i]
  
  response <- cancel_project(project_id)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Canceled project {project_name} ({i} of {nrow(df_final)})"))
  } else {
    print(str_glue("Failed to cancel {project_name}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_final)})"))
  }
}


