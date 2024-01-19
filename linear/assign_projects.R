
# purpose -----------------------------------------------------------------

# fix for issues that belong to Jira Epics but are missing Projects in Linear

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
  stringr
)
api_url <- "https://api.linear.app/graphql"

jira_query <- read_file("jira_issues_under_epics.sql")
linear_projects_query <- read_file("linear_project_query.sql")
jira_url_base <- "https://gpventure.atlassian.net/browse/"

source("linear_functions.R")
gsheet_url <- "https://docs.google.com/spreadsheets/d/1SuCn4aibEHpx5lGOIJleV919A8hxd8UoziHFu3tn9OA/edit#gid=0"
gs4_auth("dennis@terrascope.com")
ss <- gs4_get(gsheet_url)

# redshift queries --------------------------------------------------------

con <- aws_connect()
df_jira_raw <- dbFetch(dbSendQuery(con, jira_query))
df_linear_projects <- dbFetch(dbSendQuery(con, linear_projects_query))


# function to fetch linear issues, paginated ------------------------------

fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            project: {{null: true}}
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            state: {{name: {{nin: [\"Duplicate\"]}}}}
          }} 
          first: 100
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
            state {{name}}
            attachments {{
              nodes {{url}}
            }}
          }}
        }}
      }}"
    )
  } else {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            project: {{null: true}}
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            state: {{name: {{nin: [\"Duplicate\"]}}}}
          }} 
          first: 100 
          after: \"{cursor}\"
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
            state {{name}}
            attachments {{
              nodes {{url}}
            }}
          }}
        }}
      }}"
    )
  }
  
  response <- POST(
    url, 
    body = toJSON(list(query = query)), 
    add_headers(
      Authorization = key_get("linear"),
      "Content-Type" = "application/json"
    )
  )
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

# run a loop to fetch issues with pagination--------------------------------------------------------------

# initialize variables for pagination
all_issues <- list()
has_next_page <- TRUE
cursor <- NULL

# loop
while(has_next_page == TRUE) {
  response_data <- fetch_issues(api_url, cursor)
  all_issues <- c(all_issues, response_data$data$issues$nodes)
  cursor <- response_data$data$issues$pageInfo$endCursor
  has_next_page <- response_data$data$issues$pageInfo$hasNextPage
}

# Flatten the data and create a data frame, thanks ChatGPT ----------------
df_linear_issues <- map_df(
  all_issues, 
  ~ {
    issue_id <- .x[["id"]]
    issue_identifier <- .x[["identifier"]]
    status <- .x[["state"]][["name"]]
    
    # Initialize an empty data frame for attachments
    attachments_df <- data.frame(
      attachment_url = character(),
      stringsAsFactors = FALSE
    )
    
    # Check if attachments are present and correctly structured
    if (!is.null(.x[["attachments"]]) && "nodes" %in% names(.x[["attachments"]])) {
      if (length(.x[["attachments"]][["nodes"]]) > 0) {
        attachments_df <- map_df(
          .x[["attachments"]][["nodes"]], 
          ~ data.frame(
            attachment_url = .x[["url"]],
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # If there are no attachments, create a single row with NAs
    if (nrow(attachments_df) == 0) {
      attachments_df <- data.frame(
        attachment_url = NA, 
        stringsAsFactors = FALSE
      )
    }
    
    # Combine issue information with attachments
    data.frame(
      issue_id, 
      issue_identifier, 
      status,
      attachments_df
    )
  }, 
  .id = NULL
)

# clean up and find the ones that belong to Epics in Jira----------------------------------------------------------------

df_linear_clean <- df_linear_issues |> 
  filter(
    str_detect(attachment_url, jira_url_base)
  ) |> 
  mutate(
    jira_key = str_remove(attachment_url, jira_url_base)
  ) |> 
  select(
    linear_issue_id = issue_id,
    linear_issue_key = issue_identifier,
    linear_status = status,
    jira_key
  )

df_missing_epics <- df_linear_clean |> 
  inner_join(
    df_jira_raw,
    by = c("jira_key" = "child_key")
  )

df_joined <- df_missing_epics |> 
  inner_join(
    df_linear_projects,
    by = c("parent_summary" = "linear_project_name")
    ) |> 
  arrange(parent_summary, linear_issue_key)

df_no_project_match <- df_missing_epics |> 
  anti_join(
    df_linear_projects,
    by = c("parent_summary" = "linear_project_name")
  )

write_sheet(df_no_project_match, ss)
# run loop to assign label to every issue ---------------------------------

df_joined$error <- NA

for (i in 1:nrow(df_joined)) {
  
  issue_id <- df_joined$linear_issue_id[i]
  project_id <- df_joined$linear_project_id[i]
  issue_key <- df_joined$linear_issue_key[i]
  project_name <- df_joined$parent_summary[i]
  
  response <- assign_project(issue_id, project_id, api_url)
  
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Assigned {issue_key} to \"{project_name}\" ({i} of {nrow(df_joined)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error \"{response$errors[[1]]$extensions$userPresentableMessage}\""))
    df_joined$error[i] <- response$errors[[1]]$extensions$userPresentableMessage
  }
}


# now look at the errors --------------------------------------------------

df_failed <- df_joined |> 
  filter(!is.na(error))

combos <- df_failed |> 
  mutate(team_key = str_extract(linear_issue_key, "^[A-Za-z]+")) |> 
  distinct(team_key, parent_summary, linear_project_id)

df_failed$error <- NA

for (i in 1:nrow(df_failed)) {
  
  issue_id <- df_failed$linear_issue_id[i]
  project_id <- df_failed$linear_project_id[i]
  issue_key <- df_failed$linear_issue_key[i]
  project_name <- df_failed$parent_summary[i]
  
  response <- assign_project(issue_id, project_id, api_url)
  
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Assigned {issue_key} to \"{project_name}\" ({i} of {nrow(df_failed)})"))
  } else {
    print(str_glue("Failed to add issue {issue_key} to \"{project_name}\": Error \"{response$errors[[1]]$extensions$userPresentableMessage}\""))
    df_failed$error[i] <- response$errors[[1]]$extensions$userPresentableMessage
  }
}


# one more gain -----------------------------------------------------------

df_failed <- df_failed |> 
  filter(!is.na(error))


df_failed$error <- NA

for (i in 1:nrow(df_failed)) {
  
  issue_id <- df_failed$linear_issue_id[i]
  project_id <- df_failed$linear_project_id[i]
  issue_key <- df_failed$linear_issue_key[i]
  project_name <- df_failed$parent_summary[i]
  
  response <- assign_project(issue_id, project_id, api_url)
  
  # Check response
  if (is.null(response$errors)) {
    print(str_glue("Assigned {issue_key} to \"{project_name}\" ({i} of {nrow(df_failed)})"))
  } else {
    print(str_glue("Failed to add issue {issue_key} to \"{project_name}\": Error \"{response$errors[[1]]$extensions$userPresentableMessage}\""))
    df_failed$error[i] <- response$errors[[1]]$extensions$userPresentableMessage
  }
}
