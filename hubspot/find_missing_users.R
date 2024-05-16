
# purpose -----------------------------------------------------------------

# find external users from Auth0 who do not have corresponding records in Hubspot
# generate csv file of those users to import into Hubspot 

# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse,
  keyring,
  DBI,
  RPostgreSQL
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

query_auth0 <- "
  SELECT DISTINCT 
  	id AS auth0_id,
  	email
  FROM auth0.users
"

query_hs <- "
  SELECT
  	id AS hubspot_id,
  	email
  FROM hubs.contact_to_emails
"


# pull data ---------------------------------------------------------------

con <- aws_connect()
df_hs <- dbSendQuery(con, query_hs) |> 
  dbFetch()
df_auth0 <- dbSendQuery(con, query_auth0) |> 
  dbFetch()


# identify missing --------------------------------------------------------

df_hs_norm <- df_hs |> 
  mutate(across(email, tolower))

df_auth0_norm <- df_auth0 |>
  mutate(across(email, tolower)) |> 
  mutate(across(where(is.character), ~str_remove(., "'"))) |> 
  filter(
    !str_detect(email, "terrascope.com"),
    !str_detect(email, "gmail.com"),
    !str_detect(email, "@mobileprogramming.com"),
    !str_detect(email, "mailinator.com"),
    !str_detect(email, "thoughtworks.com")
    ) |> 
  distinct(auth0_id, email)

df_missing <- df_auth0_norm |> 
  anti_join(df_hs_norm, by = "email") |> 
  select(email)

write_csv(df_missing, "contact_import.csv")
