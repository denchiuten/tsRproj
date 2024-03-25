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
  readxl,
  googlesheets4
)

gs4_auth("dennis@terrascope.com", scopes = "https://www.googleapis.com/auth/spreadsheets")
# actuals_url <- "https://docs.google.com/spreadsheets/d/174HpknrYpoBDT1XAGz_RfJh-lMLNaeNxrDi2SCqzmwE/edit#gid=0"
# actuals_url <- "https://docs.google.com/spreadsheets/d/1lUw1IX8xnQ6pBbCvbXOY7ZhXcTbnjdcA4JfCo26xmLM/edit#gid=0"
actuals_url <- "https://docs.google.com/spreadsheets/d/1X_y4x7BuER_oddeyIzFaDcZbH3wZPmq27gcV7EMd_-I/edit?usp=drivesdk"
ss <- gs4_get(actuals_url)
close_date <- as.Date("2024-03-22")
# read in file and clean it -----------------------------------------------

df_raw <- read_sheet(ss)
# filter out empty rows
df_clean <- df_raw |> 
  filter(
    if_any(everything(), ~ !is.na(.)),
    !is.na(`P&L`),
    !is.na(CC)
  )

df_long <- df_clean |> 
  pivot_longer(
    cols = matches("^[0-9]"),
    names_to = "date"
  ) |> 
  filter(value != 0) |> 
  rename(
    lt_ppt_mapping = `LT PPT Mapping`,
    location = Location,
    olam_view = `Olam View`,
    mv_cost_centre = `MV Cost center`,
    finance_cost_centre = `Finance Cost Centre`,
    team = Team,
    country = Country,
    pnl = `P&L`,
    cash_commitment = CC,
    mgmt_pnl_cost_type =`Cost type - Management P&L`,
    general_ledger_desc = `G/L Description`,
    description = Description
  ) |> 
  mutate(
    close_date = close_date,
    import_date = today(),
    across(
      c(pnl, cash_commitment), 
      ~ifelse(. == 1, TRUE, FALSE)
    ),
    across(date, ~ dmy(.)),
    across(finance_cost_centre, as.numeric),
    across(finance_cost_centre, ~replace_na(., 0))
  )


# # write and import --------------------------------------------------------

write_csv(df_long, file = str_glue("actuals_{today()}.csv"))

