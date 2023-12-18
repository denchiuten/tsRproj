
# purpose -----------------------------------------------------------------
#  fix issues that had their assignees removed by Linear 2-way sync

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
  httr,
  jsonlite
  
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

query <- "
  SELECT	
  	hist.issue_id,
    	i.key,
    	hist.value AS status_id,
    	s.name AS status_name,
    	hist.time
  FROM jra.issue_field_history AS hist
  INNER JOIN jra.issue AS i
  	ON hist.issue_id = i.id
  INNER JOIN(
  	SELECT field.issue_id
  	FROM jra.vw_latest_issue_field_value AS field
  	WHERE
  		1 = 1	
  		AND field.field_id = 'status'
  		AND field.time::DATE >= '2023-12-14'
  		AND field.author_id = '63c50741cd6a09abe71e007c' -- Bryan
   ) AS latest
    	ON hist.issue_id = latest.issue_id
    LEFT JOIN jra.status AS s
    	ON hist.value = s.id
    WHERE
    	1 = 1
    	AND hist.field_id = 'status'
"

jira_base <- "https://gpventure.atlassian.net"
jira_username <- "dennis@terrascope.com"
# pull data ---------------------------------------------------------------
con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# data prep ---------------------------------------------------------------

# group by issue id and rank by update time

df_ranked <- df_raw |> 
  arrange(issue_id, desc(time)) |> 
  group_by(issue_id) |> 
  mutate(rank = dense_rank(desc(time))) |> 
  ungroup()

# pull out just the current and prior status value
df_last <- df_ranked |> 
  filter(rank <= 2) 

#pivot wider so we can compare the current and prior value for each issue 
df_wide <- df_last |> 
  pivot_wider(
    id_cols = c(issue_id, key),
    values_from = status_name, 
    names_from = rank
    ) |> 
  filter(`1` != `2`) # filter for just the issues where the two values are different