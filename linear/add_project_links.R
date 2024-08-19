# purpose -----------------------------------------------------------------

# add a bunch of links to projects

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

source("linear_functions.R")

gs4_auth("dennis@terrascope.com")
# gs_url <- "https://docs.google.com/spreadsheets/d/1OLsTk-b4sWqgAEeIJ4u3umcEPd0DUGW-BIOMD42qO0s/"
gs_url <- "https://docs.google.com/spreadsheets/d/1DOeC6eHFOy8hinDo23RbU_aNHDDrQx4CaDhl90z-4Uk/"
ss <- gs4_get(gs_url)


# redshift query ----------------------------------------------------------

df_raw <- read_sheet(ss, sheet = "Sheet1", range = "A:D")

df_final <- df_raw
  # mutate(url = str_glue("https://app.hubspot.com/contacts/22313216/record/0-2/{hubspot_company_id}"))

# loop --------------------------------------------------------------------

for (i in 31:nrow(df_final)) {
  
  project_id <- df_final$project_id[i]
  project_name <- df_final$project[i]
  url <- df_final$url[i]
  label <- df_final$deal[i]
  
  
  response <- add_project_link(project_id, url, label)
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Added link to {label} to {project_name} ({i} of {nrow(df_final)})"))
  } else {
    print(str_glue("Failed to update {project_name}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_final)})"))
  }
}

