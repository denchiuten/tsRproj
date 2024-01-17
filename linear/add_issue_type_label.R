
# purpose -----------------------------------------------------------------

# apply labels to Jira stories

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
jira_query <- read_file("all_jira_issues.sql")
source("linear_functions.R")

# pull jira issues --------------------------------------------------------

con <- aws_connect()
df_jira_raw <- dbFetch(dbSendQuery(con, jira_query))

# GraphQL query -----------------------------------------------------------

fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            labels: {{every: {{parent: {{name: {{neqIgnoreCase: \"Issue Type\"}} }} }} }}
            state: {{name: {{nin: [\"Duplicate\"]}}}}
          }} 
          first: 100
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
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
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            labels: {{every: {{parent: {{name: {{neqIgnoreCase: \"Issue Type\"}} }} }} }}
            state: {{name: {{nin: [\"Duplicate\"]}}}}
          }} 
          first: 100 
          after: \"{cursor}\"
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
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



# loop to pull all issues --------------------------------------------------------------------
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

#flatten into a data frame
df_linear_issues <- map_df(
  all_issues, 
  ~ {
    issue_id <- .x[["id"]]
    issue_identifier <- .x[["identifier"]]
    
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
      attachments_df
    )
  }, 
  .id = NULL
)

# now pull all labels for join ---------------------------------
all_labels <- list()
has_next_page <- TRUE
cursor <- NULL

while(has_next_page == TRUE) {
  response_data <- get_labels(api_url, cursor)
  all_labels <- c(all_labels, response_data$data$issueLabels$nodes)
  cursor <- response_data$data$issueLabels$pageInfo$endCursor
  has_next_page <- response_data$data$issueLabels$pageInfo$hasNextPage
}

df_labels <- map_df(
  all_labels, 
  ~ data.frame(
    id = .x[["id"]],
    name = .x[["name"]],
    stringsAsFactors = FALSE
  )
)


# cleanup -----------------------------------------------------------------

df_linear_clean <- df_linear_issues |> 
  filter(
    str_detect(attachment_url, jira_url_base)
  ) |> 
  mutate(
    jira_key = str_remove(attachment_url, jira_url_base)
  ) |> 
  select(
    linear_id = issue_id,
    linear_key = issue_identifier,
    jira_key
  )

df_jira_clean <- df_jira_raw |> 
  filter(issue_type %in% c("Epic", "Story", "Bug", "Task", "Sub-task")) |> 
  select(issue_type, issue_key) |> 
  inner_join(df_labels, by = c("issue_type" = "name"))

df_joined <- df_linear_clean |> 
  inner_join(df_jira_clean, by = c("jira_key" = "issue_key"))

# function to assign labels -----------------------------------------------

assign_label <- function(issue_id, label_id, url) {
  
  mutation <- str_glue(
    "
    mutation{{
      issueAddLabel(
        id: \"{issue_id}\"
        labelId: \"{label_id}\" 
      ) {{
        success
      }}
    }}
  ")
  
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

# now loop ----------------------------------------------------------------
for (i in 1:nrow(df_joined)) {
  
  issue_id <- df_joined$linear_id[i]
  issue_key <- df_joined$linear_key[i]
  label_name <- df_joined$issue_type[i]
  label_id <- df_joined$id[i]
  
  response <- assign_label(issue_id, label_id, api_url)
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("Added label {label_name} to {issue_key} ({i} of {nrow(df_joined)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {status_code(response)} ({i} of {nrow(df_joined)})"))
  }
}
