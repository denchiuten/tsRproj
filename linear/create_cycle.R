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
  RJSONIO
)

api_url <- "https://api.linear.app/graphql"

create_cycle <- function(issue_id, label_id, url) {
  
  mutation <- str_glue(
    "
    mutation{{
      issueAddLabel(
        id: \"{issue_id}\"
        labelId: \"{label_id}\" 
      ) {{
        success
      }}
    }}
  ")
  
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
}