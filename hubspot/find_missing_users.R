
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
  SELECT 
  	id AS auth0_id,
  	email
  FROM auth0.users
  WHERE
  	1 = 1
  	AND email NOT LIKE '%@terrascope.com'
  	AND email NOT LIKE '%@gmail.com';
"

query_hs <- "
  SELECT
  	id AS hubspot_id,
  	property_email AS email
  FROM hubs.contact

"