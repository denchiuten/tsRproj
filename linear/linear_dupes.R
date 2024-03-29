
# purpose -----------------------------------------------------------------
# mark as duplicates any Linear issues that are linked to the same Jira issue

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
source("linear_functions.R")
# pull all Linear issues ---------------------------------------------

# function to fetch linear issues, paginated
fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            state: {{name: {{nin: [\"Duplicate\", \"Done\", \"Canceled\"] }} }}
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
            state: {{name: {{nin: [\"Duplicate\", \"Done\", \"Canceled\"] }} }}
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


# clean up and find duplicates----------------------------------------------------------------

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
    jira_key,
    attachment_source
  )

df_dupes <- df_linear_clean |> 
  filter(linear_status != "Duplicate") |> 
  arrange(jira_key, linear_issue_key) |> 
  group_by(jira_key) |> 
  mutate(
    total = n(),
    column = row_number()
    ) |> 
  filter(total > 1) |> 
  ungroup()

# now make into a wide data frame with one row per Jira issue 
# and one column per associated Linear issue
df_dupes_wide <- df_dupes |> 
  pivot_wider(
    id_cols = jira_key,
    values_from = c(linear_issue_id, linear_issue_key),
    names_from = attachment_source
  ) |> 
  filter(linear_issue_id_import != "NULL") |> 
  arrange(jira_key)


# loop through ------------------------------------------------------------

for (i in 5:nrow(df_dupes_wide)) {
  
  issue_id <- df_dupes_wide$linear_issue_id_import[i]
  duplicate_of_id <- df_dupes_wide$linear_issue_id_jira[i]
  
  issue_key <- df_dupes_wide$linear_issue_key_import[i]
  duplicate_of_key <- df_dupes_wide$linear_issue_key_jira[i]
  
  response <- mark_dupe(issue_id, duplicate_of_id, api_url)
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("{issue_key} is now marked as a duplicate of {duplicate_of_key} ({i} of {nrow(df_dupes_wide)})"))
  } else {
    print(str_glue("Failed to mark {issue_key} as a duplicate of {duplicate_of_key} ({i} of {nrow(df_dupes_wide)})"))
  }
}
