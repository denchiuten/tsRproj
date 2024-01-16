#add user to channel
invite_user <- function(channel_id, user_id) {
  url <- "https://slack.com/api/conversations.invite"
  response <- POST(
    url, 
    add_headers(
      Authorization = paste("Bearer", key_get("slack"))
    ), 
    body = list(channel = channel_id, users = user_id), 
    encode = "json"
  )
  return(content(response, "parsed"))
}  