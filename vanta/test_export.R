
# purpose -----------------------------------------------------------------
# export list of controls owned by Dennis

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
  RJSONIO,
  googlesheets4,
  httr
)
source("gen_token.R")
gs_url <- "https://docs.google.com/spreadsheets/d/1FN8inxqmj-2oB9WaQj0bp02z-Ax12TZY6UmpvzP_SaQ/edit?gid=1768292589#gid=1768292589"

# get GSheet for writing output later and Dennis' user ID---------------------------------------------------------

gs4_auth("dennis@terrascope.com")
ss <- gs4_get(gs_url)


# get test data from Vanta -----------------------------------------------

#set token in keyring
key_set_with_value(service = "vanta", username = "bearer_token", password = gen_token())

# get Dennis' Vanta user ID from Redshift
con <- aws_connect()
query <- "SELECT id FROM vanta.users WHERE email = 'dennis@terrascope.com'"
df_vanta <- dbFetch(dbSendQuery(con, query))
userId <- df_vanta$id

# function to list tests owned by a specific users
list_tests <- function(userId) {
  response <- GET(
    url = stringr::str_glue("https://api.vanta.com/v1/tests?pageSize=100&ownerFilter={userId}"), 
    encode = "json", 
    add_headers(
      Authorization = stringr::str_glue("Bearer {key_get('vanta', 'bearer_token')}"), 
      "Content-Type" = "application/json"
    )
  )
  content_response <- RJSONIO::fromJSON(httr::content(response, as = "text"), flatten = FALSE)
  return(map_dfr(content_response$results$data, ~list(id = .x$id, name = .x$name)))
}

df_tests <- list_tests(userId)
write_sheet(df_tests, ss, sheet = "tests")


