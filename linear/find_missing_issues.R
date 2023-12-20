
# purpose -----------------------------------------------------------------

# fix parent_child_mappings from jira to linear
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
  purrr
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

sql_query <- read_file("all_jira_issues.sql")
api_url <- "https://api.linear.app/graphql"


# pull raw data -----------------------------------------------------------

con <- aws_connect()
df_jira_raw <- dbFetch(dbSendQuery(con, sql_query))

# function to find linear issues with attachments ---------------------------------------------------------------

fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "
      {{
        issues(first: 200) {{
          pageInfo {{
            endCursor
            hasNextPage
          }} 
          nodes {{
            id 
            attachments {{
              nodes {{
                id
                url
              }}
            }}
          }}
        }}
      }}
    "
    )
  } else {
    query <- str_glue(
      "
        {{
          issues(first: 200, after: \"{cursor}\") {{
            pageInfo {{
              endCursor
              hasNextPage
            }} 
            nodes {{
              id 
              attachments {{
                nodes {{
                  id
                  url
                }}
              }}
            }}
          }}
        }}
      "
    )
  }
    
  response <- POST(
    url, 
    body = toJSON(
      list(query = query)
      ), 
    # encode = "json", 
    add_headers(
      Authorization = key_get("linear"),
      "Content-Type" = "application/json"
      )
    )
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}


# run loop ----------------------------------------------------------------

# Initialize variables for pagination
all_issues <- list()
has_next_page <- TRUE
cursor <- NULL

# Fetch all issues
while(has_next_page == TRUE) {
  response_data <- fetch_issues(api_url, cursor)
  all_issues <- c(all_issues, response_data$data$issues$nodes)
  cursor <- response_data$data$issues$pageInfo$endCursor
  has_next_page <- response_data$data$issues$pageInfo$hasNextPage
}

# Flatten the data and create a data frame

df_issues_attachments <- map_df(all_issues, ~ {
  issue_id <- .x[["id"]]  # Safe access
  
  # Check if attachments exist and are in the expected format
  if (!is.null(.x[["attachments"]]) && "nodes" %in% names(.x[["attachments"]])) {
    attachments <- .x[["attachments"]][["nodes"]]  # Safe access
    
    if (length(attachments) > 0 && is.list(attachments)) {
      attachments_df <- map_df(attachments, ~ data.frame(
        issue_id = issue_id,
        attachment_id = .x[["id"]],  # Safe access
        attachment_url = .x[["url"]],  # Safe access
        stringsAsFactors = FALSE
      ))
    } else {
      data.frame(issue_id = issue_id, attachment_id = NA, attachment_url = NA, stringsAsFactors = FALSE)
    }
  } else {
    data.frame(issue_id = issue_id, attachment_id = NA, attachment_url = NA, stringsAsFactors = FALSE)
  }
})

