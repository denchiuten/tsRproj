# Starting Stuff ----------------------------------------------------------
suppressMessages(
  {    
    library(tidyverse)
    library(lubridate)
    library(scales)
    library(zoo)
    library(patchwork)
    library(tsViz)
    library(keyring)
    library(DBI)
    library(googlesheets4)
  }
)
theme_set(theme_ts())
sheet_id <- "1KE_2WH_IjR_0nH7YLHuxOfwp_yTG1_9DI2DeKN3hrYk"
query <- "
SELECT
	fb.id,
	fb.created_at::DATE AS created,
	contact.property_email AS email,	
	fb.property_survey_name AS survey_name,
	fb.property_survey_submission_date AS date_submitted,
	fb.property_rating AS rating,
	fb.property_csat_rating AS csat,
	fb.property_nps_rating AS nps
FROM hubs.customer_feedback_submissions AS fb
-- find contact associated with submission
INNER JOIN hubs.customer_feedback_submissions_to_contact AS fc
	ON fb.id = fc.from_id
INNER JOIN hubs.contact AS contact
	ON fc.to_id = contact.id	
	AND contact.property_email NOT LIKE '%@terrascope.com'
WHERE
	1 = 1
	AND LOWER(fb.property_survey_name) LIKE '%csm%'
	AND fb.is_merged = FALSE
"

# get data ---------------------------------------------------------------

# run redshift query
con <- aws_connect()
result <- dbSendQuery(con, query)
df_redshift <- dbFetch(result)
df_redshift_cleaned <- df_redshift |> 
  select(
    email,
    date = date_submitted,
    survey_name,
    rating
  )

# pull data from GSheet
gs4_auth("dennis@terrascope.com")
ss <- gs4_get(sheet_id)
df_gsheet <- read_sheet(ss, sheet = "Sheet2")

df_gsheet_cleaned <- df_gsheet |> 
  select(
    email = Email,
    date = Date,
    rating = `How would you rate your overall experience with Terrascope? - Number 1`
    ) |> 
  mutate(across(date, as.Date))


# find missing or dupes ---------------------------------------------------

df_redshift_left <- df_redshift_cleaned |> 
  anti_join(df_gsheet_cleaned, by = c("email", "date")
  )

