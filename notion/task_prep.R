# purpose -----------------------------------------------------------------

# prep tasks for import into Linear

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
  RJSONIO,
  stringr,
  googlesheets4
)

tasks_query <- read_file("tasks.sql")
user_query <- read_file("user_lookup.sql")
source("json_functions.R")

# pull raw data -----------------------------------------------------------

con <- aws_connect()
df_tasks_raw <- dbFetch(dbSendQuery(con, tasks_query))
df_user_lookup <- dbFetch(dbSendQuery(con, user_query))
df_project_map <- dbFetch(dbSendQuery(con, "SELECT * FROM plumbing.notion_project_to_linear_project"))

# now clean the tasks df to parse the json --------------------------------

df_tasks_clean <- df_tasks_raw %>%
  mutate(
    notion_assignee_id = sapply(assignee_json, parse_id),
    notion_status = sapply(task_status_json, parse_id),
    notion_project_id = sapply(project_id_json, parse_project),
    notion_title = sapply(title_json, parse_title)
  ) |> 
  select(!contains("json")) |> 
  filter(!notion_status %in% c("done", "archived"))

df_joined <- df_tasks_clean |> 
  inner_join(
    df_user_lookup,
    by = c("notion_assignee_id" = "notion_user_id")
    ) |> 
  inner_join(
    df_project_map,
    by = c("notion_project_id" = "notion_project_page_id")
  ) |> 
  mutate(
    linear_state_id = case_match(
      notion_status,
      "not-started" ~ "611f5eb3-42df-41b9-a16e-498e3da04c98",
      "in-progress" ~ "415e190a-359c-4b3d-923c-5dba04cfaee2",
      "tNkH" ~ "e8243ae1-b854-4c0d-bd2b-4aff459618d9",
      ":>?`" ~ "820e6ef8-2dfe-49f0-9f9f-0aa03d6d386d"
    )
  )


# functions ---------------------------------------------------------------

# create issue
create_issue <- function(title, project_id, assignee_id, state_id) {
  
  mutation <- str_glue(
    "mutation{{
      issueCreate(
        input: {{
          title: \"{title}\"
          assigneeId: \"{assignee_id}\"
          projectId: \"{project_id}\"
          teamId: \"87fa2ccb-12f4-4a4c-8d7f-37db1d23e8e4\"
          stateId: \"{state_id}\" 
        }}
        ) {{
        success
        }}
      }}"
  )
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


# loop time ---------------------------------------------------------------

for (i in 1:nrow(df_joined)) {
  
  title <- df_joined$title[i]
  project_id <- df_joined$linear_project_id[i]
  assignee_id <- df_joined$linear_user_id[i]
  state_id <- df_joined$linear_state_id[i]
  
  # first create the project
  create_response <- create_issue(title, project_id, assignee_id, state_id)
  if (!is.null(create_response$data)) {
    # success
    print(str_glue("Created {title} ({i} of {nrow(df_joined)})"))
  } else {
    # what went wrong
    print(str_glue("Failed to create {title}: \"{create_response$errors[[1]]$extensions$userPresentableMessage}\" ({i} of {nrow(df_joined)})"))
  }
}
