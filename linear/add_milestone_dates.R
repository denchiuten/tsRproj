# purpose -----------------------------------------------------------------

# add milestone target dates to a bunch of Implementation projects

# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse,
  keyring,
  DBI,
  RPostgreSQL,
  stringr,
  httr,
  RJSONIO,
  googlesheets4
)

gs4_auth("dennis@terrascope.com")
gs_url <- "https://docs.google.com/spreadsheets/d/125HexAaqlhUn_8hmyrtNtiGymd0lQDMd7IPIEzal0A8/edit?gid=0#gid=0"
source("linear_functions.R")


# redshift query ----------------------------------------------------------
ss <- gs4_get(gs_url)
df_raw <- read_sheet(ss, sheet = "Sheet1", range = "A:F")

df_final <- df_raw |> 
  filter(!is.na(target_date)) |> 
  arrange(project, target_date)

# loop --------------------------------------------------------------------


for (i in 1:nrow(df_final)) {
  
  project <- df_final$project[i]
  milestone <- df_final$milestone[i]
  milestone_id <- df_final$milestone_id[i]
  target_date <- df_final$target_date[i]
  
  response <- add_milestone_date(milestone_id, target_date)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Assigned {milestone} in {project} to {target_date} ({i} of {nrow(df_final)})"))
  } else {
    print(str_glue("Failed to update issue {milestone_id}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_final)})"))
  }
}

