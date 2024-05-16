
# purpose -----------------------------------------------------------------
# bulk merge a set of duplicate companies in Hubspot


# setup -------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  keyring,
  DBI,
  RPostgreSQL,
  httr,
  RJSONIO,
  stringr,
  googlesheets4,
  jsonlite
)

source("hubspot_functions.R")
gs4_auth("dennis@terrascope.com")
gsheet_url <- "https://docs.google.com/spreadsheets/d/1bWIipd1bG13vRUBknS2O3xMlN1YeazmeFX0CB-ouon4/edit#gid=2008747884"


# pull data ---------------------------------------------------------------

ss <- gs4_get(gsheet_url)
df_raw <- read_sheet(ss, sheet = "All companies", range = "A:B")
df_clean <- df_raw |> 
  filter(!is.na(primaryObjectId)) |> 
  arrange(objectIdToMerge)
# Iterate over each row and merge companies
for (i in 1:nrow(df_clean)) {
  objectIdToMerge <- df_clean$objectIdToMerge[i]
  primaryObjectId <- df_clean$primaryObjectId[i]
  
  response <- merge_companies(primaryObjectId, objectIdToMerge)
}

