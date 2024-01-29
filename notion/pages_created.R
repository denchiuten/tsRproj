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
  googlesheets4,
  glue
)

gsheet_url <- "https://docs.google.com/spreadsheets/d/1xJjevHU6Xi9UQY38KY09Wj1WUd4Y9vmNC1rEFYyHUXg/edit#gid=886698867"
gs4_auth("dennis@terrascope.com")
ss <- gs4_get(gsheet_url)
df_raw <- read_sheet(ss, sheet = "2024-01-29")
df_clean <- df_raw |> 
  filter(is.na(`action taken`))

c_creators <- df_clean$email


# query -------------------------------------------------------------------

con <- aws_connect()
query <- glue_sql(
  "SELECT
    	p.id AS page_id,
    	p.url,
    	p.description,
    	u.email AS creator_email
    FROM notion.page AS p
    INNER JOIN notion.users AS u
    	ON p.created_by = u.id
    WHERE
    	1 = 1
    	AND p._fivetran_deleted IS FALSE
    	AND u.email IN  ({c_creators*})
  ",
  .con = con
)

df_pages <- dbFetch(dbSendQuery(con, query)) |> 
  arrange(creator_email, url)

write_sheet(df_pages, ss, "pages_created")
