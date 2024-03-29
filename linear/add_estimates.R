
# purpose -----------------------------------------------------------------
# add estimate values

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
api_url <- "https://api.linear.app/graphql"
jira_url_base <- "https://gpventure.atlassian.net/browse/"

jira_query <- read_file("jira_issues_with_points.sql")
source("linear_functions.R")

# run SQL -----------------------------------------------------------------

con <- aws_connect()
df_jira_raw <- dbFetch(dbSendQuery(con, jira_query))
# pull all Linear issues ---------------------------------------------

# function to fetch linear issues, paginated
fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            team: {{key: {{in: [\"CCF\", \"PLAT\", \"PCF\", \"QA\", \"DSCI\"] }} }}
            state: {{name: {{nin: [\"Duplicate\", \"Done\", \"Rejected\", \"Graveyard\", \"Cancelled\", \"Canceled\"]}}}}
            
          }}
          first: 100
        ) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{
              id 
              identifier
              state {{name}}
              assignee {{id}}
              attachments {{
                nodes {{sourceType, url}}
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
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            team: {{key: {{in: [\"CCF\", \"PLAT\", \"PCF\", \"QA\", \"DSCI\"] }} }}
            state: {{name: {{nin: [\"Duplicate\", \"Done\", \"Rejected\", \"Graveyard\", \"Cancelled\", \"Canceled\"]}}}}
          }}
          first: 100
          after: \"{cursor}\"
        ) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{
              id 
              identifier
              state {{name}}
              assignee {{id}}
              attachments {{
                nodes {{sourceType, url}}
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
      attachment_source = character(),
      attachment_url = character(),
      stringsAsFactors = FALSE
    )
    
    # Check if attachments are present and correctly structured
    if (!is.null(.x[["attachments"]]) && "nodes" %in% names(.x[["attachments"]])) {
      if (length(.x[["attachments"]][["nodes"]]) > 0) {
        attachments_df <- map_df(
          .x[["attachments"]][["nodes"]], 
          ~ data.frame(
            attachment_source = .x[["sourceType"]],
            attachment_url = .x[["url"]],
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # If there are no attachments, create a single row with NAs
    if (nrow(attachments_df) == 0) {
      attachments_df <- data.frame(
        attachment_source = NA, 
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


# cleanup -----------------------------------------------------------------

df_linear_clean <- df_linear_issues |> 
  filter(
    str_detect(attachment_url, jira_url_base)
  ) |> 
  mutate(
    jira_key = str_remove(attachment_url, jira_url_base),
    jira_project = str_extract(jira_key, "^[^-]+"),
  ) |> 
  select(
    linear_issue_id = issue_id,
    linear_issue_key = issue_identifier,
    jira_key,
    jira_project
  )

df_joined <- df_linear_clean |> 
  inner_join(
    df_jira_raw,
    by = c("jira_key" = "issue_key")
  ) |> 
  mutate(across(story_points, as.integer))


# loop --------------------------------------------------------------------

for (i in 1:nrow(df_joined)) {
  
  issue_id <- df_joined$linear_issue_id[i]
  points <- df_joined$story_points[i]
  issue_key <- df_joined$linear_issue_key[i]
  
  response <- add_estimate(issue_id, points, api_url)
  
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("Assigned estimate of {points} to {issue_key}({i} of {nrow(df_joined)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {status_code(response)}"))
  }
}

