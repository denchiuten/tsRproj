# purpose -----------------------------------------------------------------

# pull pages from Projects DB in Notion and create Projects in Linear

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

notion_query <- read_file("pull_notion_projects.sql")
user_query <- read_file("user_lookup.sql")
source("json_functions.R")

api_url <- "https://api.linear.app/graphql"
# pull raw data -----------------------------------------------------------
con <- aws_connect()
df_notion_raw <- dbFetch(dbSendQuery(con, notion_query))
df_user_lookup <- dbFetch(dbSendQuery(con, user_query))

# clean up ----------------------------------------------------------------
df_notion_clean <- df_notion_raw |> 
  mutate(
    driver_id = sapply(driver_json, parse_id),
    approver_id = sapply(approver_json, parse_id),
    contributors_id = sapply(contributors_json, parse_id),
    informed_id = sapply(informed_json, parse_id),
    status = sapply(project_status_json, parse_id),
    title = sapply(project_title_json, parse_title)
  ) |> 
  select(!contains("json")) |> 
  filter(!status %in% c("canceled", "done"))

# Linear API function to create project -----------------------------------

create_project <- function(project_name, lead_id) {
  
  mutation <- str_glue(
    "mutation{{
      projectCreate(
        input: {{
          teamIds: \"87fa2ccb-12f4-4a4c-8d7f-37db1d23e8e4\"
          name: \"{project_name}\" 
          response$data$projectCreate$project
        }}
        ) {{
          success
          project {{id}}
        }}
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

add_project_link <- function(project_id, url, link_label) {
  
  mutation <- str_glue(
    "mutation{{
      projectLinkCreate(
        input: {{
          projectId: \"{project_id}\"
          url: \"{url}\" 
          label: \"{link_label}\"
        }}
      ) {{
        success
      }}
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

response <- create_project("test_R4")
response$data$projectCreate$project

response_2 <- add_project_link(project_id, url)
project_id <- response$data$projectCreate$project
url <- "https://linear.app"



# loop time ---------------------------------------------------------------

for (i in 1:nrow(df_notion_clean)) {
  
  project_name <- df_notion_clean$title[i]
  lead_id <- df_notion_clean$driver_id[i]
  url <- df_notion_clean$page_url[i]

  # first create the project
  create_response <- create_project(project_name, lead_id)
  if (!is.null(create_response$data)) {
    # success
    print(str_glue("Created {project_name} ({i} of {nrow(df_notion_clean)})"))
    
    # now add the link to the Notion page
    project_id <- create_response$data$projectCreate$project
    link_response <- add_project_link(project_id, url, "Notion Project")
    if (!is.null(link_response$data)) {
      # success
      print(str_glue("Added {url} to {project_name}"))
    } else {
      # what went wrong
      print(str_glue("Failed to add link to {project_name}: \"{link_response$errors[[1]]$message}\""))
    }
  } else {
    # what went wrong
    print(str_glue("Failed to create {project_name}: \"{create_response$errors[[1]]$message}\" ({i} of {nrow(df_notion_clean)})"))
  }
}
