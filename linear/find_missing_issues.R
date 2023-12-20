
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
  purrr,
  googlesheets4
  
)
library(googlesheets4)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

sql_query <- read_file("all_jira_issues.sql")
api_url <- "https://api.linear.app/graphql"
jira_url_base <- "https://gpventure.atlassian.net/browse/"
gsheet_url <- "https://docs.google.com/spreadsheets/d/1xD75YN5hLfgm5gyW7JQRPJbrVLb1DEQ3qhUSA8v5V1Q/edit#gid=0"
gs4_auth("dennis@terrascope.com")

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
    body = toJSON(list(query = query)), 
    add_headers(
      Authorization = key_get("linear"),
      "Content-Type" = "application/json"
      )
    )
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}


# run loop to get all Linear issues with Jira URLs attached----------------------------------------------------------------

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

# Flatten the data and create a data frame, thanks ChatGPT

df_issues_attachments <- map_df(
  all_issues, 
  ~ {
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
  }
)


# prep final data set -----------------------------------------------------

# filter the Linear list for only those with Jira url links
df_linear_filtered <- df_issues_attachments |> 
  filter(
    str_detect(attachment_url, jira_url_base)
    ) |> 
  mutate(
    jira_key = str_remove(attachment_url, jira_url_base)
    ) |> 
  select(
    linear_id = issue_id,
    jira_key
    )

# find jira issues with no corresponding issue in Linear
df_missing <- df_jira_raw |> 
  anti_join(df_linear_filtered, by = c("issue_key" = "jira_key"))

# save output to google sheet
ss <- gs4_get(gsheet_url)
write_sheet(df_missing, ss, sheet = as.character(today()))
