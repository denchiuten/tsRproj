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
gsheet_url <- "https://docs.google.com/spreadsheets/d/14zGbsRF_86pGHOE3qIG9OJRXrSfRpJJV2OJWM4TwgQk/edit#gid=0"
gs4_auth("dennis@terrascope.com")
api_url <- "https://api.linear.app/graphql"
# pull raw data -----------------------------------------------------------
con <- aws_connect()
df_notion_raw <- dbFetch(dbSendQuery(con, notion_query))
df_user_lookup <- dbFetch(dbSendQuery(con, user_query))
df_project_lookup <- dbFetch(dbSendQuery(con, "SELECT * FROM plumbing.notion_project_to_linear_project"))
# clean up ----------------------------------------------------------------
df_notion_clean <- df_notion_raw |> 
  # parse the stupid json columns
  mutate(
    linear_project_id = NA,
    driver_id = sapply(driver_json, parse_id),
    approver_id = sapply(approver_json, parse_id),
    contributors_id = sapply(contributors_json, parse_id),
    informed_id = sapply(informed_json, parse_id),
    status = sapply(project_status_json, parse_id),
    title = sapply(project_title_json, parse_title),
  ) |> 
  # now do some processing on the output columns
  mutate(
    # replace driver with the creator when driver is missing
    across(driver_id, ~ ifelse(is.na(.), creator_id, .)),
    # adapt title for character limit in Linear project name
    across(title, ~substr(., 1, 80))
  ) |> 
  select(!contains("json")) |> 
  left_join(
    df_user_lookup,
    by = c("driver_id" = "notion_user_id")
    ) |> 
  rename(driver_linear_id = linear_user_id) |> 
  filter(
    !status %in% c("canceled", "done"),
    title != "New Project",
    !is.na(driver_linear_id)
    ) |> 
  arrange(title) |> 
  # now filter out any that already have a project created
  anti_join(
    df_project_lookup,
    by = c("project_page_id" = "notion_project_page_id")
    )

# Linear API function to create project -----------------------------------

create_project <- function(project_name, lead_id) {
  
  mutation <- str_glue(
    "mutation{{
      projectCreate(
        input: {{
          teamIds: \"87fa2ccb-12f4-4a4c-8d7f-37db1d23e8e4\"
          name: \"{project_name}\" 
          leadId: \"{lead_id}\"
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

# loop time ---------------------------------------------------------------

for (i in 1:nrow(df_notion_clean)) {
  
  project_name <- df_notion_clean$title[i]
  lead_id <- df_notion_clean$driver_linear_id[i]
  url <- df_notion_clean$page_url[i]

  # first create the project
  create_response <- create_project(project_name, lead_id)
  if (!is.null(create_response$data)) {
    # success
    print(str_glue("Created {project_name} ({i} of {nrow(df_notion_clean)})"))
    
    # now add the link to the Notion page
    project_id <- create_response$data$projectCreate$project
    df_notion_clean$linear_project_id[i] <- project_id
    link_response <- add_project_link(project_id, url, "Notion Project")
    if (!is.null(link_response$data)) {
      # success
      print(str_glue("Added {url} to {project_name}"))
    } else {
      # what went wrong
      print(str_glue("Failed to add link to {project_name}: \"{link_response$errors[[1]]$extensions$userPresentableMessage}\""))
    }
  } else {
    # what went wrong
    print(str_glue("Failed to create {project_name}: \"{create_response$errors[[1]]$extensions$userPresentableMessage}\" ({i} of {nrow(df_notion_clean)})"))
  }
}


# write output to gsheet --------------------------------------------------

df_output <- df_notion_clean |> 
  select(
    notion_project_page_id = project_page_id,
    linear_project_id
  ) 

ss <- gs4_get(gsheet_url)  
write_sheet(df_output, ss, sheet = str_glue("df_output_{now()}"))

