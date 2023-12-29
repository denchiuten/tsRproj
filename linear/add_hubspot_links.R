
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
    
    # Combine issue information with attachments
    data.frame(
      issue_id, 
      issue_identifier, 
      description
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
  select(-description) |> 
  arrange(issue_identifier)


# function to attach links ------------------------------------------------

attach_link <- function(issue_id, attachment_url, url) {
  
  mutation <- str_glue(
    "mutation{{
      attachmentLinkURL(
        url: \"{attachment_url}\"
        issueId: \"{issue_id}\"
        title: \"Hubspot Ticket Link\"
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
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

# loop --------------------------------------------------------------------

for (i in 16:nrow(df_linear_clean)) {
  issue_id <- df_linear_clean$issue_id[i]
  attachment_url <- df_linear_clean$url[i]
  issue_key <- df_linear_clean$issue_identifier[i]
  
  response <- attach_link(issue_id, attachment_url, api_url)
  # Check response
  if (!is.null(response$data)) {
    print(str_glue("Attached {attachment_url} to {issue_key} ({i} of {nrow(df_linear_clean)})"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: \"{response$errors[[1]]$extensions$userPresentableMessage}\" ({i} of {nrow(df_linear_clean)})"))
  }
}


