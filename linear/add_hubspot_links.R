
# purpose -----------------------------------------------------------------
# mark as duplicates any Linear issues that are linked to the same Jira issue

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
jira_url_base <- "https://gpventure.atlassian.net/browse/"
hubspot_url_base <- "https://app.hubspot.com/"



# function to fetch linear issues, paginated ------------------------------

fetch_issues <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            attachments: {{ every: {{ url: {{ notContains: \"{hubspot_url_base}\" }} }} }}
            state: {{ name: {{ nin: [\"Duplicate\"] }} }}
            team: {{key: {{ eq: \"CS\" }} }}
          }}
          first: 100
        ) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{
              id 
              identifier
              description
            }}
          }}
        }}"
    )
  } else {
    query <- str_glue(
      "{{
        issues(
          filter: {{ 
            attachments: {{ every: {{ url: {{ notContains: \"{hubspot_url_base}\" }} }} }}
            state: {{ name: {{ nin: [\"Duplicate\"] }} }}
            team: {{key: {{ eq: \"CS\" }} }}
          }}
          first: 100
          after: \"{cursor}\"
        ) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{
              id 
              identifier
              description
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
    description <- .x[["description"]]
    
    # Initialize an empty data frame for attachments
    # attachments_df <- data.frame(
    #   attachment_id = character(),
    #   attachment_url = character(),
    #   stringsAsFactors = FALSE
    # )
    
    # Check if attachments are present and correctly structured
    # if (!is.null(.x[["attachments"]]) && "nodes" %in% names(.x[["attachments"]])) {
    #   if (length(.x[["attachments"]][["nodes"]]) > 0) {
    #     attachments_df <- map_df(
    #       .x[["attachments"]][["nodes"]], 
    #       ~ data.frame(
    #         attachment_url = .x[["url"]],
    #         stringsAsFactors = FALSE
    #       )
    #     )
    #   }
    # }
    
    # If there are no attachments, create a single row with NAs
    # if (nrow(attachments_df) == 0) {
    #   attachments_df <- data.frame(
    #     attachment_url = NA, 
    #     stringsAsFactors = FALSE
    #   )
    # }
    
    # Combine issue information with attachments
    data.frame(
      issue_id, 
      issue_identifier, 
      description
      # attachments_df
    )
  }, 
  .id = NULL
)

# clean up the df ---------------------------------------------------------

df_linear_clean <- df_linear_issues |> 
  mutate(url = str_extract(description, "https://app\\.hubspot\\.com/contacts/[^\\s)]*")) |> 
  filter(
    !is.na(url),
    !str_detect(url, "Chttps")
    ) |> 
  select(-description)

library(googlesheets4)
gs4_auth("dennis@terrascope.com")
write_sheet(df_linear_clean)
