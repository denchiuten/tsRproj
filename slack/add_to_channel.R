# purpose -----------------------------------------------------------------
# script to add all users to a specific channel

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

channel_id <- "C061TE45FS5" #proj-linear
source("slack_functions.R")

# query -------------------------------------------------------------------

con <- aws_connect()
query <- glue_sql(
  "
    SELECT
    	u.id AS user_id,
    	u.name AS display_name
    FROM slack.users AS u
    LEFT JOIN slack.channel_member AS cm
    	ON u.id = cm.user_id
    	AND cm.channel_id = {channel_id}
    	AND cm._fivetran_deleted IS FALSE
    WHERE
    	1 = 1
    	AND u._fivetran_deleted IS FALSE
    	AND cm.user_id IS NULL
    	AND u.is_bot IS FALSE
    	AND u.deleted IS FALSE
    	AND u.name NOT IN ('suresh', 'summer.chua', 'priyanka.mukherjee', 'joydeep', 'slackbot')
  ", 
  .con = con
)

df_raw <- dbSendQuery(con, query) |> 
  dbFetch()


# loop to add user to channel -----------------------------------------
  
for(i in 1:nrow(df_raw)) {
  user_id <- df_raw$user_id[i]
  user_name <- df_raw$display_name[i]
  
  invite_response <- invite_user(channel_id, user_id)
  if (invite_response$ok) {
    print(str_glue("Invited {user_name} to channel \n"))
  } else {
    print(str_glue("Failed to invite {user_name} to channel}. Error: {invite_response$error} \n"))
  }
}
  


