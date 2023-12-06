# Starting Stuff ----------------------------------------------------------
suppressMessages(
  {    
    library(tidyverse)
    library(lubridate)
    library(scales)
    library(zoo)
    library(patchwork)
    library(tsViz)
    # library(combinat)
    library(igraph)
    library(RPostgreSQL)
  }
)
username <- "awsuser"
theme_set(theme_ts())
drv <- dbDriver("PostgreSQL")
con <- dbConnect(
  drv, dbname = "dev",
  host = "redshift-cluster-1.cqmhnipsf8nw.ap-southeast-1.redshift.amazonaws.com",
  port = 5439,
  user = username
  password = keyring::key_get("aws", username)
)


# query -------------------------------------------------------------------
query <- "
  SELECT
    att.event_id,
    ev.summary,
    att.email,
  	bob.first_name AS name,
  	bob.work_department AS department,
  	bob.work_title AS title,
  	bob.work_manager AS manager,
  	bob.work_second_level_manager AS second_level_manager,
  	ev.start_date_time::DATE AS date,
  	DATEDIFF(MINUTE, ev.start_date_time::TIMESTAMP, ev.end_date_time::TIMESTAMP) AS total_minutes
  FROM gcal.attendee AS att
  INNER JOIN bob.employee AS bob
  	ON att.email = bob.email 
  	AND bob.internal_status = 'Active'
  INNER JOIN gcal.event AS ev
  	ON att.event_id = ev.id
  	AND ev.event_type NOT IN ('outOfOffice', 'focusTime')
  	AND ev.status = 'confirmed'
  -- only include meetings with > 1 attendee
  INNER JOIN (
  	SELECT event_id
  	FROM gcal.attendee 
  	GROUP BY 1
  	HAVING COUNT(email) > 1
  ) AS multi
  	ON ev.id = multi.event_id
  -- identify meetings with external attendees; we'll filter out later
  LEFT JOIN (
  	SELECT DISTINCT 
  		event_id
  	FROM gcal.attendee AS att
  	WHERE 
  	  1 = 1
  	  AND LOWER(email) NOT LIKE '%@terrascope.com'
  	  AND LOWER(email) NOT LIKE '%@resource.calendar.google.com'
  	  AND LOWER(email) NOT LIKE '%@bcgdv.com'
  	  AND email NOT IN (
  	    'calendar-notification@google.com',
  	    'mayaglow@gmail.com',
  	    'the.natsuko@gmail.com',
  	    'jeannette82@gmail.com'
  	)
  ) AS ext
  	ON ev.id = ext.event_id
  WHERE
  	1 = 1	
  	AND ext.event_id IS NULL -- 	filter out the meetings external attendees
  	AND ev.start_date_time::DATE < CURRENT_DATE
  	AND att.response_status = 'accepted'
  	AND ev.summary != 'Friday Focus Time'
"

# run query ---------------------------------------------------------------------
result <- dbSendQuery(con, query)
df_raw <- dbFetch(result)


# create pairs ------------------------------------------------------------

df_clean <- df_raw |> 
  select(event_id, email, name, department, total_minutes)

df_pairs <- df_clean |> 
  inner_join(
    df_clean, 
    by = c("event_id", "total_minutes"), 
    relationship = "many-to-many"
    ) |> 
  filter(email.x < email.y)

df_final <- df_pairs |> 
  group_by(name.x, name.y) |> 
  summarise(across(total_minutes, sum), .groups = "drop") |> 
  mutate(hours = total_minutes / 60)

total_mins_per_attendee <- df_final |> 
  select(name.x, name.y, hours) |> 
  pivot_longer(
    cols = starts_with("name"),
    values_to = "attendee"
  ) |> 
  group_by(attendee) |> 
  summarise(across(hours, sum), .groups = "drop")
  

g <- graph_from_data_frame(df_final, directed = FALSE)
E(g)$weight <- df_final$hours
V(g)$hours <- total_mins_per_attendee$hours[match(V(g)$name, total_mins_per_attendee$attendee)]

layout <- layout_with_kk(g)
max_coord <- max(abs(layout))
scaling_factor <- 25  # adjust this factor as needed
layout <- layout / max_coord * scaling_factor
plot(
  g, 
  # vertex.size = 10, 
  vertex.size = V(g)$hours / max(V(g)$hours) * 10, # Adjust the node size
  vertex.label.cex = 0.8, 
  edge.width = E(g)$weight / max(E(g)$weight) * 5, # adjust edge width based on weights
  # edge.label = E(g)$weight, # show total minutes on edges
  # layout = layout_nicely(g) # layout algorithm
  layout = layout
)
