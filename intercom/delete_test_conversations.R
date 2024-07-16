
# setup -------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  keyring,
  DBI,
  RPostgreSQL,
  stringr,
  httr,
  RJSONIO
)

query <- "
  SELECT DISTINCT c.id AS conversation_id
  FROM intercom.conversation_tag_history AS ct
  INNER JOIN intercom.conversation_history AS c
  	ON ct.conversation_id = c.id
  WHERE ct.tag_id = 9587203 -- tag_id for Test Ticket
"


# query -------------------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))
df_final <- df_raw |> 
  arrange(conversation_id)


# function ----------------------------------------------------------------

# Loop through each conversation ID in the data frame
for (i in 1:nrow(df_final)) {
  conversation_id <- df_final$conversation_id[i]
  url <- str_glue("https://api.intercom.io/conversations/{conversation_id}")
  
  response <- DELETE(
    url,
    add_headers(
      Authorization = paste("Bearer", key_get("intercom", "david.tomicek@terrascope.com")),
      Accept = "application/json",
      `Content-Type` = "application/json",
      `Intercom-Version` = "Unstable"
    )
  )
  
  if (status_code(response) == 200) {
    print(paste("Conversation", conversation_id, "deleted successfully"))
  } else {
    print(paste("Failed to delete conversation", conversation_id, ":", status_code(response)))
  }
}

