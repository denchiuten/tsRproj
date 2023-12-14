
# purpose -----------------------------------------------------------------

# bulk archive public channels with no messages after a given cutoff
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
  stringr
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

query <- "
  SELECT 
  	c.name,
  	c.id,
  	COUNT(m.*) AS n_messages,
  	MAX(m.ts) As max_ts
  FROM slack.message AS m
  INNER JOIN slack.channel AS c
  	ON m.message_channel_id = c.id
  	AND c.name IS NOT NULL
  	AND c.is_archived = FALSE
  GROUP BY 1,2
  ORDER BY 1
"

offset_months <- 6
# pull data ---------------------------------------------------------------
con <- aws_connect()
df_raw <- dbSendQuery(con,query) |> 
  dbFetch()

df_cleaned <- df_raw |> 
  mutate(
    seconds = as.numeric(sub("\\..*", "", max_ts)),
    posix_time = as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"),
    date = as.Date(posix_time)
  )



# functions ---------------------------------------------------------------

archive_channel <- function(channel_id) {
  url <- "https://slack.com/api/conversations.archive"
  response <- POST(
    url, 
    add_headers(
      Authorization = paste("Bearer", key_get("slack"))
    ), 
    body = list(channel = channel_id), 
    encode = "json"
  )
  return(content(response, "parsed"))
}  

post_message <- function(channel_id, message) {
  url <- "https://slack.com/api/chat.postMessage"
  body = list(channel = channel_id, text = message)
  
  response <- POST(
    url, 
    add_headers(
      Authorization = paste("Bearer", key_get("slack"))
    ), 
    body = body, 
    encode = "json"
  )
  return(content(response, "parsed"))
}  

# filter and run ----------------------------------------------------------

df_filtered <- df_cleaned |> 
  filter(date < today() %m-% months(offset_months))

channel_name <- "test"

for (i in 1:nrow(df_filtered)) {
  channel_id <- df_filtered$channel_id[i]
  channel_name <- df_filtered$name[i]
  message <- str_glue(
    "#{channel_name} is being auto-archived because it hasn't been used in {offset_months} months. You can still find it in search results and unarchive it at any time."
    )
  
  # Join channel
  message_response <- post_message(channel_id, message)
  if (message_response$ok) {
    archive_response <- archive_channel(channel_id)
    if (archive_response$ok) {
      print(str_glue("Archived channel: #{channel_name} \n"))
    } else {
      print(str_glue("Failed to archive channel: #{channel_name}. Error: {archive_response$error} \n"))
    }
  } else {
    print(str_glue("Failed to post message in: #{channel_name}. Error: {message_response$error} \n"))
  }
}