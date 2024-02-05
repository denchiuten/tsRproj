
# purpose -----------------------------------------------------------------

# import records from Roadmap Planning DB in Notion into Linear

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
  RJSONIO,
  httr
)

team_id <- "73900fcf-59bd-45c2-92f4-1683392cc398" #id for Product Roamap planning team in Linear
source("json_functions.R")
query <- read_file("roadmap_planning.sql")
default_assignee_id <- "156b619c-e746-498d-9141-f687442db3ee" #Nic Myers' Linear user_id. Will assign unassigned issues to him

# run query ---------------------------------------------------------------
con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))
df_user_map <- dbFetch(dbSendQuery(con, "SELECT notion_user_id, linear_user_id FROM plumbing.vw_user_id_lookup WHERE notion_user_id IS NOT NULL"))


# clean up ----------------------------------------------------------------

df_clean <- df_raw %>%
  mutate(
    notion_assignee_id = sapply(assignee_json, parse_id),
    notion_status = sapply(status_json, parse_name),
    notion_title = sapply(title_json, parse_title),
    notion_timeline = sapply(timeline_json, parse_name)
    # notion_pod = sapply(pod_json, parse_first_name_robust),
    # notion_opp_grouping = sapply(opp_grouping_json, parse_first_name_robust)
  ) |> 
  select(!contains("json"))

df_final <- df_clean |> 
  mutate(
    linear_status_id = case_match(
      notion_status,
      "Not started" ~ "a4f6903d-ae08-48e5-be8e-5576610835e3",
      "In progress" ~ "cde1928e-9021-4259-bd36-928b5bddff96",
      "Done" ~ "c0b3795b-2802-4e92-818d-dc77ab4dc880"
    ),
    across(notion_timeline, ~str_replace(., "H2", "Q4")), #convert H2 to Q4
    linear_due_date = as.Date(as.yearqtr(notion_timeline, format = "%Y - Q%q"))
  )
    
df_joined <- df_final |> 
  left_join(df_user_map, by = c("notion_assignee_id" = "notion_user_id")) |> 
  select(
    notion_title,
    page_url,
    contains("linear")
  ) |> 
  mutate(across(linear_user_id, ~replace_na(., default_assignee_id)))


# functions ---------------------------------------------------------------

# create issue
create_issue <- function(title, assignee_id, team_id, state_id, due_date) {
  
  mutation <- str_glue(
    "mutation{{
      issueCreate(
        input: {{
          title: \"{title}\"
          assigneeId: \"{assignee_id}\"
          teamId: \"{team_id}\"
          stateId: \"{state_id}\"
          dueDate: \"{due_date}\"
        }}
      ) {{
      success
      issue {{id}}
      }}
    }}"
  )
  response <- POST(
    url = "https://api.linear.app/graphql", 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

add_issue_link <- function(issue_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      attachmentLinkURL(
          url: \"{url}\" 
          issueId: \"{issue_id}\"
      ) {{success}}
    }}"
  )
  response <- POST(
    url = "https://api.linear.app/graphql", 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

# loop time ---------------------------------------------------------------
df_joined$result <- NA
for (i in 3:nrow(df_joined)) {
  
  title <- df_joined$notion_title[i]
  assignee_id <- df_joined$linear_user_id[i]
  state_id <- df_joined$linear_status_id[i]
  due_date <- df_joined$linear_due_date[i]
  page_url <- df_joined$page_url[i]
  # first create the project
  create_response <- create_issue(title, assignee_id, team_id, state_id, due_date)
  if (!is.null(create_response$data)) {
    # success
    print(str_glue("Created {title} ({i} of {nrow(df_joined)})"))
    df_joined$result[i] <-  TRUE
    issue_id <- create_response$data$issueCreate$issue
    
    link_response <- add_issue_link(issue_id, page_url)
    if (!is.null(link_response$data)) {
      # success
      print(str_glue("Added {page_url} to {title}"))
    } else {
      # what went wrong
      print(str_glue("Failed to add link to {title}: \"{link_response$errors[[1]]$extensions$userPresentableMessage}\""))
    }
  } else {
    # what went wrong
    print(str_glue("Failed to create {title}: \"{response$errors[[1]]$extensions$userPresentableMessage}\" ({i} of {nrow(df_joined)})"))
    df_joined$result[i] <-  FALSE
  }
}
