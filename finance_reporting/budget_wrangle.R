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
  readxl
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())


# read in file and clean it -----------------------------------------------

df_raw <- read_xlsx("fy24_budget_for_tableau_test.xlsx")

# filter out empty rows
df_clean <- df_raw |> 
  filter(if_any(everything(), ~ !is.na(.)))

df_long <- df_clean |> 
  pivot_longer(
    cols = starts_with("4"),
    names_to = "date"
    ) |> 
  mutate(across(date, ~ as.Date(as.numeric(.), origin = "1899-12-30"))) |> 
  filter(value != 0) |> 
  rename(
    olam_view = `Olam View`,
    mv_cost_centre = `MV Cost center`,
    finance_cost_centre = `Finance Cost Centre`,
    team = Team,
    country = Country,
    pnl = `P&L`,
    cash_commitment_binary = CC,
    mgmt_pnl_cost_type =`Cost type - Management P&L`,
    general_ledger_desc = `G/L Description`,
    description = Description
  )
