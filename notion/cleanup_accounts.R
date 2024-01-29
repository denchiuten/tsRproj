
# purpose -----------------------------------------------------------------

# find contractor or inactive accounts and remove or convert to guests

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
  googlesheets4
)
query <- read_file("invalid_notion_accounts.sql")
gsheet_url <- "https://docs.google.com/spreadsheets/d/1iUL-h8lWL1JERbX9p6GK3_z_EmJ8ku7NXmsADG817TU/edit#gid=0"
gs4_auth("dennis@terrascope.com")


# run query ---------------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# apply logic and export --------------------------------------------------

df_clean <- df_raw |> 
  mutate(
    action = case_when(
      payroll_employment_type == "Agency Contractor" ~ "Convert to Guest",
      email %in% c("askhr@terrascope.com", "joydeep@terrascope.com", "suresh@terrascope.com") ~ "Convert to Guest",
      bob_status != "Active" ~ "Remove"
    )
  ) |> 
  arrange(action, email)

ss <- gs4_get(gsheet_url)    
write_sheet(df_clean, ss, sheet = str_glue("{today()}"))
