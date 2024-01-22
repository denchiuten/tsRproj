
# purpose -----------------------------------------------------------------

# apply Customer Name - ID labels

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
jira_query <- read_file("jira_customer_name.sql")
source("linear_functions.R")
gsheet_url <- "https://docs.google.com/spreadsheets/d/1NOkzqJeHA2n45KjHm_4ddj6q7-GUBDyzp22JG_a9m8U/edit#gid=0"
gs4_auth("dennis@terrascope.com")
ss <- gs4_get(gsheet_url)


# run redshift query ------------------------------------------------------

con <- aws_connect()
df_jira_raw <- dbFetch(dbSendQuery(con, jira_query))


# pull all label values and write them to gsheet --------------------------

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
) |> 
  arrange(name)

write_sheet(df_labels, ss, "df_labels")

# now pull map ------------------------------------------------------------

df_map <- read_sheet(ss, sheet = "map")


# pull all Linear issues --------------------------------------------------

# function to fetch linear issues, paginated
fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            attachments: {{url: {{contains: \"{jira_url_base}\"}} }}
            state: {{name: {{nin: [\"Duplicate\"] }} }}
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
            state: {{name: {{nin: [\"Duplicate\"] }} }}
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


# prep final data ---------------------------------------------------------

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

df_joined <- df_linear_clean |> 
  inner_join(df_jira_raw, by = c("jira_key" = "jira_issue_key")) |> 
  inner_join(df_map, by = "customer_name") |> 
  arrange(label_name, linear_issue_key)
  
# now loop ----------------------------------------------------------------
for (i in 1:nrow(df_joined)) {
  
  issue_id <- df_joined$linear_issue_id[i]
  issue_key <- df_joined$linear_issue_key[i]
  label_name <- df_joined$label_name[i]
  label_id <- df_joined$label_id[i]

  response <- assign_label(issue_id, label_id, api_url)
  # Check response
  if (!is.null(response$data)) {
    print(str_glue("Added label {label_name} to {issue_key} ({i} of {nrow(df_joined)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {response$errors[[1]]$extensions$userPresentableMessage} ({i} of {nrow(df_joined)})"))
  }
}
