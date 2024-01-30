
# purpose -----------------------------------------------------------------

# populate a label group with auth0 org names and IDs

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
  stringr
)

query <- read_file("auth0_label_query.sql")
source("linear_functions.R")
parent_id <- "d88f13bf-6c43-41c8-8869-9deb9e516f1e"
# pull jira issues --------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# now loop ----------------------------------------------------------------
for (i in 1:nrow(df_raw)) {
  
  label_name <- df_raw$label_name[i]
  
  response <- create_label(label_name, parent_id)
  # Check response
  if (!is.null(response$data)) {
    print(str_glue("Created label {label_name} ({i} of {nrow(df_raw)})"))
  } else {
    print(str_glue("Failed to create label {label_name}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_raw)})"))
  }
}
