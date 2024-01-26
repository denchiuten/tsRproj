
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
  stringr,
  googlesheets4
)

query <- read_file("open_project_query.sql")
source("linear_functions.R")
cutoff_date <- as.Date("2023-12-01")
gsheet_url <- "https://docs.google.com/spreadsheets/d/1LUTtPt3D8NThtKnRHIxTss9oXcNt7kBq-QsMKa8rAaY/edit#gid=0"
gs4_auth("dennis@terrascope.com")
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


# write data frame to GSheet ----------------------------------------------

ss <- gs4_get(gsheet_url)
write_sheet(df_final, ss, str_glue("canceled_{today()}"))
