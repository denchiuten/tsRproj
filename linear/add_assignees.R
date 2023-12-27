
# purpose -----------------------------------------------------------------

# add assignees to all issues in Linear if they are missing but have an assignee in Jira

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
pacman::p_load_current_gh("denchiuten/tsViz")
api_url <- "https://api.linear.app/graphql"

jira_query <- read_file("linear_jira_user_map.sql")
jira_url_base <- "https://gpventure.atlassian.net/browse/"


# redshift_query ----------------------------------------------------------
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
            assignee: {{null: true}}
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
          }}
          first: 100
        ) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{
              id 
              identifier
              state {{name}}
              attachments {{
                nodes {{id, url}}
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
            assignee: {{null: true}} 
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
          }}
          first: 100
          after: \"{cursor}\"
        ) {{
            pageInfo {{
              endCursor
              hasNextPage
            }} 
            nodes {{
              id 
              identifier
               state {{name}}
              attachments {{
                nodes {{id, url}}
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
    
    # Initialize an empty data frame for attachments
    attachments_df <- data.frame(
      attachment_id = character(),
      attachment_url = character(),
      stringsAsFactors = FALSE
    )
    
    # Check if attachments are present and correctly structured
    if (!is.null(.x[["attachments"]]) && "nodes" %in% names(.x[["attachments"]])) {
      if (length(.x[["attachments"]][["nodes"]]) > 0) {
        attachments_df <- map_df(
          .x[["attachments"]][["nodes"]], 
          ~ data.frame(
            attachment_id = .x[["id"]],
            attachment_url = .x[["url"]],
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    # If there are no attachments, create a single row with NAs
    if (nrow(attachments_df) == 0) {
      attachments_df <- data.frame(
        attachment_id = NA, 
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


# clean up the Linear data frame ------------------------------------------

# filter for only those with a linked Jira URL, and remove unnecessary columns
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
    jira_key
  )

# now join to Jira issues that have a fixVersion value
df_joined <- df_linear_clean |> 
  inner_join(
    df_jira_raw, 
    by = c("jira_key" = "jira_issue_key")
  ) |> 
  arrange(email, linear_issue_key)

# function to assign assignees -----------------------------------------------

assign_assignee <- function(issue_id, user_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueUpdate(
        id: \"{issue_id}\"
        input: {{
          assigneeId: \"{user_id}\" 
        }}
        ) {{success}}
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
  
  issue_id <- df_joined$linear_issue_id[i]
  user_id <- df_joined$linear_user_id[i]
  user_email <- df_joined$email[i]
  issue_key <- df_joined$linear_issue_key[i]
  
  response <- assign_assignee(issue_id, user_id, api_url)
  
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("Assigned {issue_key} to {user_email} ({i} of {nrow(df_joined)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {status_code(response)}"))
  }
}
