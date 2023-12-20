
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

# functions ---------------------------------------------------------------

# fetch linear issues and their attachments
fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "
        {{
          issues(first: 100) {{
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
              parent {{
                id
                identifier
              }}
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
          issues(first: 100, after: \"{cursor}\") {{
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
               parent {{
                id
                identifier
              }}
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

# assign parent to child issues in Linear
assign_parent <- function(child_id, parent_id, url) {
  
  mutation <- str_glue(
    "
    mutation{{
      issueUpdate(
        id: \"{child_id}\"
        input: {{
          parentId: \"{parent_id}\" 
        }}
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

df_linear_issues <- map_df(
  all_issues, 
  ~ {
      issue_id <- .x[["id"]]
      issue_identifier <- .x[["identifier"]]
      parent_id <- if (!is.null(.x[["parent"]])) .x[["parent"]][["id"]] else NA
      parent_identifier <- if (!is.null(.x[["parent"]])) .x[["parent"]][["identifier"]] else NA
      status <- if (!is.null(.x[["state"]])) .x[["state"]][["name"]] else NA
  
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
        status,
        issue_id, 
        issue_identifier, 
        parent_id, 
        parent_identifier,
        attachments_df
      )
  }, 
  .id = NULL
)



# prep final data set -----------------------------------------------------

# filter the Linear list for only those with Jira url links
df_linear_filtered <- df_linear_issues |> 
  filter(
    str_detect(attachment_url, jira_url_base)
    ) |> 
  mutate(
    jira_key = str_remove(attachment_url, jira_url_base)
    ) |> 
  select(
    linear_id = issue_id,
    linear_key = issue_identifier,
    linear_parent_id = parent_id,
    linear_parent_key = parent_identifier,
    jira_key
    )

# find jira issues with no corresponding issue in Linear
df_missing <- df_jira_raw |> 
  anti_join(df_linear_filtered, by = c("issue_key" = "jira_key"))

# save output to google sheet
ss <- gs4_get(gsheet_url)
write_sheet(df_missing, ss, sheet = as.character(today()))


# spot check for one issue --------------------------------------------------------------------

query <- "
  {
    issue(id : \"PLAT-1055\") {
      id 
      identifier
      attachments {
        nodes {
          id
          url
        }
      }
    }
  }
"

response <- POST(
  api_url, 
  body = toJSON(list(query = query)), 
  add_headers(
    Authorization = key_get("linear"),
    "Content-Type" = "application/json"
  )
)
df_plat_1055 <- fromJSON(content(response, as = "text"), flatten = TRUE)
x <- df_plat_1055$data$issue



# fix parent child mappings -----------------------------------------------
# get rid of unnec columns
df_linear_clean <- df_linear_filtered |> 
  select(linear_id, linear_key, jira_key, linear_parent_id) |> 
  arrange(jira_key, linear_key) |> 
  group_by(jira_key) |> 
  #identify cases where multiple Linear issues are linked to the same Jira issue
  mutate(n = row_number()) |>  
  ungroup() |> 
  # and just take the first one
  filter(n == 1) |> 
  select(!n)

df_jira_clean <-  df_jira_raw |> 
  select(jira_key = issue_key, jira_parent_key = parent_key)

df_orphans <- df_linear_clean |> 
  
  # only the ones that don't already have a parent assigned in Linear
  # then get rid of the column because it's confusing
  filter(is.na(linear_parent_id)) |> 
  select(-linear_parent_id) |> 
  
  # inner join to Jira query results to find Jira parent key
  inner_join( 
    df_jira_clean,  
    by = c("jira_key")
  ) |> 
  
  # only those issues that have a parent in Jira
  filter(!is.na(jira_parent_key)) |>   
  
  # join it back to the linear data to get the linear ID of Jira parent issues
  left_join(
    select(df_linear_clean, -linear_parent_id),
    by = c("jira_parent_key" = "jira_key"),
    suffix = c(".child", ".parent"),
  )

# this is the final list for which we need to assign the parents in Linear via the API
df_final_orphans <- df_orphans |> 
  filter(!is.na(linear_id.parent))

# run loop
for (i in 1:nrow(df_final_orphans)) {
  
  child_id <- df_final_orphans$linear_id.child[i]
  parent_id <- df_final_orphans$linear_id.parent[i]
  
  child_key <- df_final_orphans$linear_key.child[i]
  parent_key <- df_final_orphans$linear_key.parent[i]
  
  response <- assign_parent(child_id, parent_id, api_url)
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("{parent_key} is now the parent of {child_key}"))
  } else {
    print(str_glue("Failed to update issue {child_key}: Error {status_code(response)}"))
  }
}
# x <- df_orphans |> distinct(jira_parent_key)
# write_sheet(x, ss, sheet = "check")
