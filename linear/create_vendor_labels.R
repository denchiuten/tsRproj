
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
parent_id <- "768db20f-6ff3-43f9-accb-579be6e93e35" #label_id for the label group, Vendor Name - Singapore

source("linear_functions.R")
# pull and prep data from GSheet ------------------------------------------

ss <- gs4_get(gsheet_url)
df_raw <- read_sheet(ss)

df_final <- df_raw |> 
  mutate(label_name = str_glue("{`Entity Name`} - {`Vendor Code`}"))


# run a loop to create the labels -----------------------------------------

for (i in 2:nrow(df_final)) {
  
  label_name <- df_final$label_name[i]
  
  response <- create_label(label_name, parent_id)
  # Check response
  if (!is.null(response$data)) {
    print(str_glue("Created label {label_name} ({i} of {nrow(df_final)})"))
  } else {
    print(str_glue("Failed to create label {label_name}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_final)})"))
  }
}


