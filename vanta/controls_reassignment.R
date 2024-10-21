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

# get GSheet data ---------------------------------------------------------

gs4_auth("dennis@terrascope.com")
ss <- gs4_get(gs_url)
df_raw <- read_sheet(ss, "controls")


# get Vanta users from Redshift -------------------------------------------

con <- aws_connect()
query <- "SELECT * FROM vanta.users"
df_vanta_users <- dbFetch(dbSendQuery(con, query))


# join --------------------------------------------------------------------

df_joined <- df_raw |> 
  left_join(
    df_vanta_users,
    by = c("New Owner Email" = "email")
  ) |> 
  select(
    readId = ID,
    controlId = UID,
    title = `Title`,
    email = `New Owner Email`,
    userId = id
  ) |> 
  arrange(readId)
    


# function to assign owner to a control----------------------------------------------------------------

assign_control <- function(controlId, userId) {
  response <- POST(
    url = stringr::str_glue("https://api.vanta.com/v1/controls/{controlId}/set-owner"), 
    body = RJSONIO::toJSON(list(userId = userId)), 
    encode = "json", 
    add_headers(
      Authorization = stringr::str_glue("Bearer {key_get('vanta')}"), 
      "Content-Type" = "application/json"
    )
  )
  return(RJSONIO::fromJSON(httr::content(response, as = "text"), flatten = TRUE))
}


# loop time ---------------------------------------------------------------
i <- 6

#set token in keyring
key_set_with_value(service = "vanta", password = gen_token())

for (i in 11:nrow(df_joined)) {
  
  readId <- df_joined$readId[i]
  controlId <- df_joined$controlId[i]
  userId <- df_joined$userId[i]
  email <- df_joined$email[i]
  
  response <- assign_control(controlId, userId)
  print(str_glue("Assigned {readId} to {email} ({i} of {nrow(df_joined)})"))
}
