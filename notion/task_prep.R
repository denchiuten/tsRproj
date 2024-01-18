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
api_url <- "https://api.linear.app/graphql"

gsheet_url <- "https://docs.google.com/spreadsheets/d/1Z9BTi1iyPMDvWuw4Bo5mQUrN7kGIorFTgjpNqQJ3LRg/edit#gid=0"
gs4_auth("dennis@terrascope.com")
ss <- gs4_get(gsheet_url)
# pull raw data -----------------------------------------------------------

con <- aws_connect()
df_tasks_raw <- dbFetch(dbSendQuery(con, tasks_query))
df_user_lookup <- dbFetch(dbSendQuery(con, user_query))
df_project_map <- dbFetch(dbSendQuery(con, "SELECT * FROM plumbing.notion_project_to_linear_project"))
df_all_teams <- dbFetch(dbSendQuery(con, "SELECT id, name, key FROM linear.team WHERE _fivetran_deleted IS FALSE"))
df_all_workflows <- dbFetch(dbSendQuery(con, "SELECT id, name, team_id FROM linear.workflow_state WHERE _fivetran_deleted IS FALSE"))
df_all_projects <- dbFetch(dbSendQuery(con, "SELECT id, name FROM linear.project WHERE _fivetran_deleted IS FALSE"))

# write_sheet(df_all_teams, ss)

df_user_team_map <- read_sheet(ss, sheet = "map")
df_workflows_clean <- df_all_workflows |> 
  mutate(
    status_name = case_when(
      name == "To Do" ~ "Todo",
      name == "Cancelled" ~ "Canceled",
      TRUE ~ name
    )
  ) |> 
  select(
    team_id, 
    status_name,
    status_id = id
  )
   
# now clean the tasks df to parse the json --------------------------------

df_tasks_clean <- df_tasks_raw %>%
  mutate(
    notion_assignee_id = sapply(assignee_json, parse_id),
    notion_status = sapply(task_status_json, parse_id),
    notion_project_id = sapply(project_id_json, parse_project),
    notion_title = sapply(title_json, parse_title)
  ) |> 
  select(!contains("json"))

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
    linear_state_name = case_match(
      notion_status,
      "not-started" ~ "Todo",
      "in-progress" ~ "In Progress",
      "tNkH" ~ "Todo",
      ":>?`" ~ "Blocked",
      "done" ~ "Done",
      "archived" ~ "Canceled"
    )
  ) |> 
  left_join(df_user_team_map, by = "email") |> 
  left_join(
    df_workflows_clean,
    by = c("team_id", "linear_state_name" = "status_name")
    )

# write_sheet(df_joined, ss)
# functions ---------------------------------------------------------------

# create issue
create_issue <- function(title, project_id, assignee_id, team_id, state_id) {
  
  mutation <- str_glue(
    "mutation{{
      issueCreate(
        input: {{
          title: \"{title}\"
          assigneeId: \"{assignee_id}\"
          projectId: \"{project_id}\"
          teamId: \"{team_id}\"
          stateId: \"{state_id}\"
        }}
      ) {{success}}
    }}"
  )
  response <- POST(
    url = api_url, 
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
for (i in 1:nrow(df_joined)) {
  
  title <- df_joined$notion_title[i]
  project_id <- df_joined$linear_project_id [i]
  assignee_id <- df_joined$linear_user_id[i]
  team_id <- df_joined$team_id[i]
  state_id <- df_joined$status_id[i]
  
  # first create the project
  response <- create_issue(title, project_id, assignee_id, team_id, state_id)
  if (!is.null(response$data)) {
    # success
    print(str_glue("Created {title} ({i} of {nrow(df_joined)})"))
    df_joined$result[i] <-  TRUE
  } else {
    # what went wrong
    print(str_glue("Failed to create {title}: \"{response$errors[[1]]$extensions$userPresentableMessage}\" ({i} of {nrow(df_joined)})"))
    df_joined$result[i] <-  FALSE
  }
}


# find the ones that failed -----------------------------------------------
text <- readLines("fails.rtf")
failed_lines <- grep("^Failed", text, value = TRUE)
# Assuming failed_lines is already defined as shown previously

# Initialize an empty vector to store the numbers
# Assuming failed_lines is already defined as shown previously

# Initialize an empty vector to store the numbers
numbers <- numeric(length(failed_lines))

# Loop through each line to extract the number
for (i in seq_along(failed_lines)) {
  # Extract the string that contains the number
  num_str <- regmatches(failed_lines[i], regexpr("\\((\\d+) of", failed_lines[i]))
  
  # Remove the parentheses and 'of' from the string
  num_str <- gsub("[^0-9]", "", num_str) 
  
  # Convert to numeric and store in the vector
  numbers[i] <- as.numeric(num_str)
}

# The 'numbers' vector now contains the first number in parentheses from each line
print(numbers)


df_fail <- df_joined |> 
  slice(numbers) |> 
  left_join(df_all_projects, by = c("linear_project_id" = "id"))

df_fail$result <- NA

df_project_team_combos <- df_fail |> 
  distinct(name, team_name, linear_project_id)

# loop again ---------------------------------------------------------------
for (i in 1:nrow(df_fail)) {
  
  title <- df_fail$notion_title[i]
  project_id <- df_fail$linear_project_id [i]
  assignee_id <- df_fail$linear_user_id[i]
  team_id <- df_fail$team_id[i]
  state_id <- df_fail$status_id[i]
  
  # first create the project
  response <- create_issue(title, project_id, assignee_id, team_id, state_id)
  if (!is.null(response$data)) {
    # success
    print(str_glue("Created {title} ({i} of {nrow(df_fail)})"))
    df_fail$result[i] <-  TRUE
  } else {
    # what went wrong
    print(str_glue("Failed to create {title}: \"{response$errors[[1]]$extensions$userPresentableMessage}\" ({i} of {nrow(df_fail)})"))
    df_fail$result[i] <-  FALSE
  }
}


# keep going --------------------------------------------------------------

df_failed_again <- df_fail |> 
  filter(
    result == FALSE,
    !is.na(name)
    )
  # left_join(df_all_projects, by = c("linear_project_id" = "id"))

df_project_team_combos <- df_failed_again |> 
  distinct(name, team_name, linear_project_id)


# one more gain -----------------------------------------------------------

for (i in 1:nrow(df_failed_again)) {
  
  title <- df_failed_again$notion_title[i]
  project_id <- df_failed_again$linear_project_id [i]
  assignee_id <- df_failed_again$linear_user_id[i]
  team_id <- df_failed_again$team_id[i]
  state_id <- df_failed_again$status_id[i]
  
  # first create the project
  response <- create_issue(title, project_id, assignee_id, team_id, state_id)
  if (!is.null(response$data)) {
    # success
    print(str_glue("Created {title} ({i} of {nrow(df_failed_again)})"))
    df_failed_again$result[i] <-  TRUE
  } else {
    # what went wrong
    print(str_glue("Failed to create {title}: \"{response$errors[[1]]$extensions$userPresentableMessage}\" ({i} of {nrow(df_failed_again)})"))
    df_failed_again$result[i] <-  FALSE
  }
}

df_failed_again |> filter(result == FALSE)
