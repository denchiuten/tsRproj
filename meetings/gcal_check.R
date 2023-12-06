# Starting Stuff ----------------------------------------------------------
suppressMessages(
  {    
    library(tidyverse)
    library(lubridate)
    library(scales)
    library(zoo)
    library(patchwork)
    library(tsViz)
    library(httr)
    library(jsonlite)
    library(keyring)
  }
)
theme_set(theme_ts())
google_endpoint <- oauth_endpoint(
  authorize = "https://accounts.google.com/o/oauth2/auth",
  access = "https://accounts.google.com/o/oauth2/token"
)
myapp <- oauth_app(
  "google", 
  key = google_key,
  secret = key_get("google", google_key)
  )
start_date <- "2023-08-01T00:00:00Z"

scopes <- "https://www.googleapis.com/auth/calendar"
token <- oauth2.0_token(google_endpoint, myapp, scope = scopes)

res <- GET(
  url = "https://www.googleapis.com/calendar/v3/calendars/primary/events",
  query = list(timeMin = start_date),
  config(token = token)
  )

# res <- GET(
#   "https://www.googleapis.com/calendar/v3/calendars/primary/events", 
#   config(token = token)
#   )

# calendar_id <- "primary"
# # event_id <- "3bb6j22kppii8utgrlb8muklnr"
# event_id <- "5g434nt3i8aq7vukb1779jgrhb"
# res <- GET(
#   url = paste0("https://www.googleapis.com/calendar/v3/calendars/", calendar_id, "/events/", event_id),
#   config(token = token)
# )

events_json <- content(res, "text")
events_list <- fromJSON(events_json, flatten = TRUE)
df_events <- as.data.frame(events_list$items)

df_clean <- df_events |> 
  select(
    created,
    start.dateTime,
    start.date,
    id,
    summary
  ) |> 
  mutate(across(c(created, start.dateTime), as.Date))
