
# purpose -----------------------------------------------------------------
# script to add fivetran app to all public channels

# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse, 
  keyring, 
  DBI, 
  RPostgreSQL, 
  stringr, 
  glue,
  httr
)
my_id <- "U03F7MMQSHJ" #replace with your own Slack user ID
app_id <- "U0698LW7SRY"  #fivetran

# app_id <- "U042GKVS75Y" #golinks

# connect to redshift -----------------------------------------------------
con <- aws_connect()
query <- glue_sql(
  "
    SELECT
    	c.id AS channel_id,
    	c.name AS channel_name,
    	SUM(
    	CASE WHEN m.user_id =  {my_id} THEN 1 ELSE 0 END
    	) AS am_member -- return 1 if Dennis is already a member
    FROM slack.channel AS c
    INNER JOIN slack.channel_member	AS m
    	ON c.id = m.channel_id
    WHERE
    	1 = 1
    	AND c.is_archived = FALSE
    	AND c.is_private = FALSE
    	AND c.name NOT LIKE 'app-deployment-alerts%' 
    GROUP BY 1,2
    HAVING SUM(
      CASE WHEN m.user_id = {app_id} THEN 1 ELSE 0 END
      ) = 1 -- only pull channels where app is already a member
  ", .con = con
)

df_raw <- dbSendQuery(con, query) |> 
  dbFetch()


# Slack API functions -----------------------------------------------------

#join channels I'm not already in
join_channel <- function(channel_id) {
  url <- "https://slack.com/api/conversations.join"
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

#add user to channel
remove_user <- function(channel_id, user_id) {
  url <- "https://slack.com/api/conversations.kick"
  response <- POST(
    url, 
    add_headers(
      Authorization = paste("Bearer", key_get("slack"))
    ), 
    body = list(channel = channel_id, user = user_id), 
    encode = "json"
  )
  return(content(response, "parsed"))
}  

#leave the channel
leave_channel <- function(channel_id) {
  url <- "https://slack.com/api/conversations.leave"
  response <- POST(
    url, 
    add_headers(
      Authorization = paste("Bearer", key_get("slack"))), 
    body = list(channel = channel_id), 
    encode = "json"
  )
  return(content(response, "parsed"))
}


# run loop for channels I'm not in yet ------------------------------------

df_not_in <- df_raw |> 
  filter(am_member == 0) |> 
  arrange(channel_name)

for (i in 1:nrow(df_not_in)) {
  channel_id <- df_not_in$channel_id[i]
  channel_name <- df_not_in$channel_name[i]
  
  # Join channel
  join_response <- join_channel(channel_id)
  if (join_response$ok) {
    print(str_glue("Joined channel: #{channel_name} \n"))
    
    # If successful, invite app
    remove_response <- remove_user(channel_id, app_id)
    if (remove_response$ok) {
      print(str_glue("Removed app from channel: #{channel_name} \n"))
    } else {
      print(str_glue("Failed to remove app from channel: #{channel_name}. Error: {remove_response$error} \n"))
    }
    
    # Leave channel after adding app
    leave_response <- leave_channel(channel_id)
    if (leave_response$ok) {
      print(str_glue("Left channel: #{channel_name} \n"))
    } else {
      print(str_glue("Failed to leave channel: #{channel_name}. Error: {leave_response$error} \n"))
    }
  } else {
    print(str_glue("Failed to join channel: #{channel_name}. Error: {join_response$error} \n"))
  }
}


# run loop for channels I'm already in ------------------------------------

df_already_in <- df_raw |> 
  filter(am_member == 1) |> 
  arrange(channel_name)

for (i in 1:nrow(df_already_in)) {
  channel_id <- df_already_in$channel_id[i]
  channel_name <- df_already_in$channel_name[i]
  
  # Invite app
  remove_response <- remove_user(channel_id, app_id)
  if (remove_response$ok) {
    print(str_glue("Removed app from channel: #{channel_name} \n"))
  } else {
    print(str_glue("Failed to remove app from channel: #{channel_name}. Error: {remove_response$error} \n"))
  }
}
