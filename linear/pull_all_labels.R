
# purpose -----------------------------------------------------------------

# retrieve all Linear label ids and values via Linear API

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
# pacman::p_load_current_gh("denchiuten/tsViz")
# theme_set(theme_ts())
api_url <- "https://api.linear.app/graphql"


# function ----------------------------------------------------------------
get_labels <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
          issueLabels(first : 100) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{id, name}}
          }}
        }}"
    )
  } else {
    query <- str_glue(
      "{{
        issueLabels(first : 100, after: \"{cursor}\") {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{id, name}}
        }}
      }}"
    )
  }
  
  response <- POST(
    url = url,
    body = toJSON(list(query = query)),
    add_headers(
      Authorization = key_get("linear"),
      "Content-Type" = "application/json"
    )
  )
  parsed_response <- content(response, as = "text") |> 
    fromJSON(flatten = T)
}


# run a loop --------------------------------------------------------------

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
  )
