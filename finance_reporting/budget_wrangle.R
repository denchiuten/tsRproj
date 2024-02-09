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

gs4_auth("dennis@terrascope.com")

# read in file and clean it -----------------------------------------------

df_raw <- read_xlsx("Terrascope FY24 Budget_Final.xlsx")

# filter out empty rows
df_clean <- df_raw |> 
  filter(
    if_any(everything(), ~ !is.na(.)),
    !is.na(`P&L`),
    !is.na(CC)
    )

df_long <- df_clean |> 
  pivot_longer(
    cols = starts_with("4"),
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
    location = "",
    across(date, ~ as.Date(as.numeric(.), origin = "1899-12-30")),
    import_date = today(),
    across(
      c(pnl, cash_commitment), 
      ~ifelse(. == 1, TRUE, FALSE)
    ),
    across(finance_cost_centre, as.numeric),
    across(finance_cost_centre, ~replace_na(., 0))
  )

# # write and import --------------------------------------------------------

write_csv(df_long, file = str_glue("budget_{today()}.csv"))

