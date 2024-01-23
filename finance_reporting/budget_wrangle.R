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

x <- df_clean |> 
  select(where(~ !is.na(as.numeric(.))))

df_long <- df_clean |> 
  pivot_longer(
    cols = starts_with("4"),
    names_to = "date"
    ) |> 
  mutate(across(date, ~ as.Date(as.numeric(.), origin = "1899-12-30"))) |> 
  filter(value != 0)
