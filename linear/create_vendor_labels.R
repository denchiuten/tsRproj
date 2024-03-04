
# purpose -----------------------------------------------------------------

# batch-create labels for Vendor Name - Singapore

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

gs4_auth("dennis@terrascope.com")
gsheet_url <- "https://docs.google.com/spreadsheets/d/1BQQmRd_nkGhgKD1fF_N-hrOWSW6HtDVlS-DHadTvAYo/edit#gid=1820078108"

source("linear_functions.R")
# pull and prep data from GSheet ------------------------------------------

ss <- gs4_get(gsheet_url)
df_raw <- read_sheet(ss)

df_final <- df_raw |> 
  mutate(label_name = str_glue("{`Entity Name`} - {`Vendor Code`}")) |> 
  arrange(label_name) |> 
  # assign parent label IDs based on letter range of child label name
  mutate(
    parent_id = case_when(
      tolower(substr(label_name, 1, 1)) >= "a" & tolower(substr(label_name, 1, 1)) <= "c" ~ "768db20f-6ff3-43f9-accb-579be6e93e35",
      tolower(substr(label_name, 1, 1)) >= "d" & tolower(substr(label_name, 1, 1)) <= "k" ~ "fe764f7b-3b39-49ba-b453-9a4926630772",
      tolower(substr(label_name, 1, 1)) >= "l" & tolower(substr(label_name, 1, 1)) <= "r" ~ "5c895c36-1888-42b0-bc5c-8992cec62f75",
      tolower(substr(label_name, 1, 1)) >= "s" & tolower(substr(label_name, 1, 1)) <= "z" ~ "d1b6b2ea-2aec-4d84-9e44-7e41acf721f7"
    )
  )


# run a loop to create the labels -----------------------------------------

for (i in 1:nrow(df_final)) {
  
  label_name <- df_final$label_name[i]
  parent_id <- df_final$parent_id[i]
  response <- create_label(label_name, parent_id)
  # Check response
  if (!is.null(response$data)) {
    print(str_glue("Created label {label_name} ({i} of {nrow(df_final)})"))
  } else {
    print(str_glue("Failed to create label {label_name}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_final)})"))
  }
}


