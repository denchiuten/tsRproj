
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
api_url <- "https://api.linear.app/graphql"
# pull data from Redshift ---------------------------------------------------------------
con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))


# get list of label values ------------------------------------------------

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
  url = api_url,
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



# get issues without the label --------------------------------------------
issue_query <- "
  {
    issueLabels(
      filter: { 
        name: { contains: \"Pod\" }
        }
      ) {
      nodes {id, name}
      }
  }"

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

assign_label <- function(issue_id, label_id, url) {
  
  mutation <- str_glue(
  "
    mutation{{
      issueAddLabel(
        id: \"{issue_id}\"
        labelId: \"{label_id}\" 
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


# now loop ----------------------------------------------------------------

for (i in 1:nrow(df_joined)) {
  
  issue_id <- df_joined$linear_issue_id[i]
  label_id <- df_joined$id[i]
  label_name <- df_joined$label[i]
  issue_key <- df_joined$linear_issue_key[i]
  
  response <- assign_label(issue_id, label_id, api_url)
  # Check response
  if (status_code(response) == 200) {
    print(str_glue("Added label {label_name} to {issue_key}"))
  } else {
    print(str_glue("Failed to update issue {issue_key}: Error {status_code(response)}"))
  }
}
