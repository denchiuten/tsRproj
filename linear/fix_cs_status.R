
# purpose -----------------------------------------------------------------
# fix status of issues in Customer Support Team

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

jira_query <- read_file("jira_issue_status.sql")
jira_url_base <- "https://gpventure.atlassian.net/browse/"

# pull jira data from redshift --------------------------------------------

con <- aws_connect()
df_jira_raw <- dbFetch(dbSendQuery(con, jira_query))


# linear query ------------------------------------------------------------

fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            team: {{key: {{in: [\"CS\"] }} }}
            state: {{name: {{neqIgnoreCase: \"duplicate\"}}}}
          }} 
          first: 100
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
            state {{name}}
            attachments {{nodes {{url}}}}
          }}
        }}
      }}"
    )
  } else {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            team: {{key: {{in: [\"CS\"] }} }}
            state: {{name: {{neqIgnoreCase: \"duplicate\"}}}}
          }} 
          first: 100 
          after: \"{cursor}\"
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
            state {{name}}
            attachments {{nodes {{url}}}}
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

# get list of workflow values ------------------------------------------------

workflow_query <- "
  {workflowStates(
      filter: { 
        team: {key: { eq: \"CS\" }}
      }
    ) {
        nodes {id, name}
      }
  }"

workflow_response <- POST(
  url = api_url,
  body = toJSON(list(query = workflow_query)),
  add_headers(
    Authorization = key_get("linear"),
    "Content-Type" = "application/json"
  )
)
parsed_response <- content(workflow_response, as = "text") |> 
  fromJSON(flatten = T)

# convert to data frame 
df_workflows <- bind_rows(parsed_response$data$workflowStates)


# prep final data frame ---------------------------------------------------

# filter for only those with a linked Jira URL, and remove unnecessary columns
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
    linear_status = status,
    jira_key
  )

# fix the label mapping for To Do
df_jira_clean <- df_jira_raw |> 
  mutate(jira_status = ifelse(jira_status == "To Do", "Todo", jira_status))

# now join to Jira issues that have a fixVersion value
df_joined <- df_linear_clean |> 
  inner_join(
    df_jira_clean, 
    by = c("jira_key" = "jira_issue_key")
  ) |> 
  filter(
    linear_status != jira_status
    ) |> 
  left_join(
    df_workflows,
    by = c("jira_status" = "name")
    )


# function to update status -----------------------------------------------

fix_state <- function(issue_id, state_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueUpdate(
        id: \"{issue_id}\"
        input: {{
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
  
  issue_id <- df_joined$linear_id[i]
  state_id <- df_joined$id[i]
  state_name <- df_joined$jira_status[i]
  issue_key <- df_joined$linear_key[i]
  
  response <- fix_state(issue_id, state_id, api_url)
  
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("Updated status of {issue_key} to {state_name} ({i} of {nrow(df_joined)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {status_code(response)} ({i} of {nrow(df_joined)})"))
  }
}
