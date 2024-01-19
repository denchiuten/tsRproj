
# purpose -----------------------------------------------------------------

# apply labels to PTINC issues

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
# GraphQL query -----------------------------------------------------------

fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            attachments: {{url: {{contains: \"{jira_url_base}PTINC\"}} }}
            labels: {{every: {{id: {{neq: \"049096e6-b6a2-4f56-b9ea-e5c643e9e279\"}} }} }}
          }} 
          first: 100
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
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
            attachments: {{url: {{contains: \"{jira_url_base}PTINC\"}} }}
            labels: {{every: {{id: {{neq: \"049096e6-b6a2-4f56-b9ea-e5c643e9e279\"}} }} }}
          }} 
          first: 100 
          after: \"{cursor}\"
        ) {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{
            id 
            identifier
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



# loop --------------------------------------------------------------------
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
  ) |> 
  filter(str_detect(jira_key, "PTINC"))


# now loop ----------------------------------------------------------------
label_id <- "049096e6-b6a2-4f56-b9ea-e5c643e9e279"
for (i in 1:nrow(df_linear_clean)) {
  
  issue_id <- df_linear_clean$linear_id[i]
  issue_key <- df_linear_clean$linear_key[i]
  
  response <- assign_label(issue_id, label_id, api_url)
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("Added label to {issue_key} ({i} of {nrow(df_linear_clean)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {status_code(response)} ({i} of {nrow(df_linear_clean)})"))
  }
}
