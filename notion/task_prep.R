# purpose -----------------------------------------------------------------

# prep tasks for import into Linear

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
  jsonlite,
  stringr,
  googlesheets4
)

tasks_query <- read_file("tasks.sql")
user_query <- read_file("user_lookup.sql")

# pull raw data -----------------------------------------------------------
con <- aws_connect()
df_tasks_raw <- dbFetch(dbSendQuery(con, tasks_query))
df_user_lookup <- dbFetch(dbSendQuery(con, user_query))


# clean -------------------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(RJSONIO)


# stupid JSON parsing functions -------------------------------------------
parse_id <- function(x) {
  # Safely parse the JSON string
  json_parsed <- tryCatch({
    fromJSON(x)
  }, error = function(e) NULL)
  
  # Check if the parsed JSON is not NULL
  if (!is.null(json_parsed)) {
    # If the parsed JSON is a list and has at least one element
    if (is.list(json_parsed) && length(json_parsed) >= 1) {
      # Extract the 'id' from the first element, if available
      if (is.list(json_parsed[[1]]) && "id" %in% names(json_parsed[[1]])) {
        return(json_parsed[[1]]$id)
      } else {
        return(NA)
      }
    } 
    # If the parsed JSON is an atomic vector
    else if (is.atomic(json_parsed)) {
      return(json_parsed["id"])
    } 
    else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

parse_title <- function(x) {
  # Safely parse the JSON string
  json_parsed <- tryCatch({
    fromJSON(x)
  }, error = function(e) NULL)
  
  # Initialize a variable to store concatenated content
  concatenated_content <- ""
  
  # Check if the parsed JSON is not NULL and is a list
  if (!is.null(json_parsed) && is.list(json_parsed)) {
    # Loop through each element in the list
    for (element in json_parsed) {
      # Check if the 'text' field exists and is a list
      if (is.list(element$text)) {
        # Extract the 'content' from the 'text' field, if available
        if ("content" %in% names(element$text)) {
          concatenated_content <- paste(concatenated_content, element$text$content, sep=" ")
        }
      }
    }
    return(trimws(concatenated_content))
  } else {
    return(NA)
  }
}
# now apply ---------------------------------------------------------------


# And assignee_json is the column with JSON strings
df_tasks_clean <- df_tasks_raw %>%
  mutate(
    assignee_id = sapply(assignee_json, parse_id),
    status = sapply(task_status_json, parse_id),
    title = sapply(title_json, parse_title)
  ) |> 
  select(!contains("json"))

# View
