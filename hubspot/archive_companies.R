
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
gsheet_url <- "https://docs.google.com/spreadsheets/d/1HzJwY-ui3eY9kYYt-OSP6a-GaX6DuuAP0xhu26UBuNA/edit?gid=1137649238#gid=1137649238"


# pull data ---------------------------------------------------------------

ss <- gs4_get(gsheet_url)
df_raw <- read_sheet(ss)
df_clean <- df_raw |> 
  arrange(id)
i <- 2
# Iterate over each row and merge companies
for (i in 3:nrow(df_clean)) {
  companyId <- df_clean$id[i]
  
  response <- archive_company(companyId)
}

