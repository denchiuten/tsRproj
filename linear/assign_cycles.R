
# purpose -----------------------------------------------------------------

# assign cycle to all issues in Linear if they are assigned to an upcoming sprint in Jira

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

jira_query <- read_file("jira_sprint_issues.sql")
jira_url_base <- "https://gpventure.atlassian.net/browse/"

source("linear_functions.R")
# pull jira data from redshift --------------------------------------------

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
            state: {{name: {{nin: [\"Canceled\", \"Duplicate\"]}}}}
            team: {{key: {{in: [\"DSCI\"] }} }}
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
            state: {{name: {{nin: [\"Canceled\", \"Duplicate\"]}}}}
            team: {{key: {{in: [\"DSCI\"] }} }}
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
    linear_id = issue_id,
    linear_key = issue_identifier,
    jira_key
  )


# in Linear, an issue can only be assigned to one cycle at a time, so filter
# for the earliest sprint each issue is assigned to in Jira
df_jira_clean <- df_jira_raw |> 
  # filter to only include cycles that have not yet ended
  filter(cycle_end_date >= today()) |> 
  arrange(issue_key, start_date) |>
  group_by(issue_key) |> 
  mutate(row = row_number()) |> 
  ungroup() |> 
  filter(row == 1) |> 
  select(!row)

# now join to Jira issues that have a fixVersion value
df_joined <- df_linear_clean |> 
  inner_join(
    df_jira_clean, 
    by = c("jira_key" = "issue_key")
  )

# run loop to assign label to every issue ---------------------------------

for (i in 1:nrow(df_joined)) {
  
  issue_id <- df_joined$linear_id[i]
  cycle_id <- df_joined$linear_cycle_id[i]
  issue_key <- df_joined$linear_key[i]
  
  response <- assign_cycle(issue_id, cycle_id, api_url)
  
  # Check response
  if (!is.null(response$data)) {
    print(str_glue("Assigned {issue_key} to cycle ({i} of {nrow(df_joined)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error \"{response$errors[[1]]$extensions$userPresentableMessage}\""))
  }
}
