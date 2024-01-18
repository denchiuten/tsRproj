
# purpose -----------------------------------------------------------------

# assign fix version to all issues in Linear if they have one in Jira

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

jira_query <- read_file("jira_issues_with_fix_versions.sql")
jira_url_base <- "https://gpventure.atlassian.net/browse/"

source("linear_functions.R")
# pull jira data from redshift --------------------------------------------

con <- aws_connect()
df_jira_raw <- dbFetch(dbSendQuery(con, jira_query))
df_label_map <- dbFetch(dbSendQuery(con, "SELECT * FROM plumbing.jira_fix_version_to_linear_label"))

# pull all Linear issues ---------------------------------------------

# function to fetch linear issues, paginated
fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            labels: {{every: {{parent: {{name: {{neqIgnoreCase: \"Release Version\"}} }} }} }},
            team: {{key: {{in: [\"CCF\", \"PLAT\"] }} }}
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
          }},
          first: 100
        ) {{
          pageInfo {{
            endCursor
            hasNextPage
          }} 
          nodes {{
            id 
            identifier
            state {{
              name
            }}
            attachments {{
              nodes {{
                id
                url
              }}
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
            labels: {{
              every: {{
                parent: {{
                  name: {{
                    neqIgnoreCase: \"Release Version\"
                  }}
                }}
              }}
            }},
            team: {{key: {{in: [\"CCF\", \"PLAT\"] }} }}
            attachments: {{url: {{contains: \"{jira_url_base}\"}}}}
          }},
          first: 100, 
          after: \"{cursor}\"
        ) {{
          pageInfo {{
            endCursor
            hasNextPage
          }} 
          nodes {{
            id 
            identifier
             state {{
              name
            }}
            attachments {{
              nodes {{
                id
                url
              }}
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
    linear_id = issue_id,
    linear_key = issue_identifier,
    jira_key
  )


# clean up the Jira issues for those that have multiple fix versions

df_jira_clean <- df_jira_raw |> 
  arrange(key, desc(version_name)) |> 
  group_by(key) |> 
  mutate(row = row_number()) |> 
  ungroup() |> 
  filter(row == 1) |> 
  select(!row)

# now join to Jira issues that have a fixVersion value
df_joined <- df_linear_clean |> 
  inner_join(
    df_jira_clean, 
    by = c("jira_key" = "key")
    )

df_with_labels <- df_joined |> 
  left_join(
    df_label_map, 
    by = c("version_name" = "jira_version_name")
    )

# run loop to assign label to every issue ---------------------------------

for (i in 1:nrow(df_with_labels)) {
  
  issue_id <- df_with_labels$linear_id[i]
  label_id <- df_with_labels$linear_label_id[i]
  label_name <- df_with_labels$linear_label_name[i]
  issue_key <- df_with_labels$linear_key[i]
  
  response <- assign_label(issue_id, label_id, api_url)
  
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("Added label {label_name} to {issue_key} ({i} of {nrow(df_with_labels)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {status_code(response)}"))
  }
}
