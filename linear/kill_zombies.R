# purpose -----------------------------------------------------------------

# complete or archive zombie projects

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
query <- read_file("zombie_project_query.sql")
source("linear_functions.R")

# ignore any projects that were created less than this many days ago
days_offset_created <- 60
days_offset_updated <- 30

# redshift query ----------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# wrangle -----------------------------------------------------------------

df_wide <- df_raw |> 
  pivot_wider(values_from = where(is.numeric), names_from = issue_status) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0))) |> 
  rowwise() |> 
  mutate(total_issues = sum(c_across(where(is.numeric))))

df_final <- df_wide |> 
  # exclude any recently created projects
  filter(
    created_date <= today() - days_offset_created,
    updated_date <= today() - days_offset_updated
    ) |> 
  mutate(
    new_state = case_when(
      total_issues == 0 ~ "canceled",
      total_issues == completed + canceled ~ "completed",
      started > 0 ~ "started",
      TRUE ~ "canceled"
    )
  )

# loop --------------------------------------------------------------------


for (i in 1:nrow(df_final)) {
  
  project_id <- df_final$project_id[i]
  project_name <- df_final$project_name[i]
  newstate <- df_final$new_state[i]
  
  response <- update_project_state(project_id, newstate)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Updated state of project: {project_name} to {newstate} ({i} of {nrow(df_final)})"))
  } else {
    print(str_glue("Failed to update project {project_name}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_final)})"))
  }
}

