
# purpose -----------------------------------------------------------------

# assign Old Pod labels to issues created from Jira import
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
  RJSONIO
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

query <- read_file("linear_issue_query.sql")

# pull data from Redshift ---------------------------------------------------------------
con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# get list of label values ------------------------------------------------

labelQuery <- "
  {
    issueLabels(filter: {
      name: {contains: 'Pod'}
      }
    ) {
      nodes {
        id
        name
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
"

label_query <- "
  {
    issueLabels(
      filter: { 
        name: { contains: \"Pod\" }
        }
      ) {
      nodes {id, name}
      }
  }"

url <- "https://api.linear.app/graphql"

label_response <- POST(
  url,
  body = toJSON(list(query = label_query)),
  add_headers(
    Authorization = key_get("linear"),
    "Content-Type" = "application/json"
    )
  )
parsed_response <- content(label_response, as = "text") |> 
  fromJSON(flatten = T)

# convert to data frame 
df_labels <- bind_rows(parsed_response$data$issueLabels)

# review query output -----------------------------------------------------

# check to see if any linear issues are linked to multiple Jira issues
df_grouped <- df_raw |> 
  count(linear_issue_id) |> 
  filter(n > 1)

df_joined <- df_raw |> 
  mutate(
    label = str_glue("{jira_project_key} Pod")
    ) |> 
  inner_join(
    df_labels,
    by = c("label" = "name")
    )


# function to assign labels -----------------------------------------------

assign_label <- function(issue_id, label_id) {
  mutation <- "mutation{
    issueAddLabel(
      id: \"<ISSUE_ID>\"
      labelId: \"<LABEL_ID>\" 
    ) {
      success
    }
  }"

  mutation <- gsub("<ISSUE_ID>", issue_id, mutation)
  mutation <- gsub("<LABEL_ID>", label_id, mutation)
  
  url <- "https://api.linear.app/graphql"
  response <- POST(
    url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
      )
    )
}