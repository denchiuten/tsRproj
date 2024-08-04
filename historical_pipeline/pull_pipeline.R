# Starting Stuff ----------------------------------------------------------
pacman::p_load(
  tidyverse,
  lubridate,
  scales,
  zoo,
  patchwork,
  keyring,
  DBI,
  RPostgreSQL
)
pacman::p_load_current_gh("denchiuten/tsViz")
theme_set(theme_ts())

query <- "
  WITH stages AS (
  	SELECT
  		ds.deal_id,
  	  	ds._fivetran_start::DATE AS start,
  	  	ds._fivetran_end::DATE AS end,
  	  	dps.label AS stage
  	FROM hubs.deal_stage AS ds
  	INNER JOIN hubs.deal_pipeline_stage AS dps
  	  	ON ds.value = dps.stage_id
  	  	AND dps.pipeline_id = 19800993
  	  	AND dps.label NOT IN ('#11 Backburner', '#10 Closed lost', '#01 Discovery', '#02 Demo')
  )
  
  SELECT
    	dph.deal_id,
  	dph._fivetran_start::DATE AS start,
  	dph._fivetran_end::DATE AS end,
  	AVG(dph.value::NUMERIC) AS tcv
    FROM hubs.deal_property_history AS dph
    INNER JOIN stages 
    	ON dph.deal_id = stages.deal_id
    	AND dph._fivetran_end::DATE BETWEEN stages.start AND stages.end
    WHERE
    	1 = 1
    	AND dph.name = 'amount_in_home_currency'
    	AND dph.value != ''
    GROUP BY 1,2,3
"


# Create a vector of dates
date <- as.Date(c("2022-12-31", "2023-12-31", "2024-07-31"))
bins <- c(0, 60000, 150000, 999999)
bin_labels <- c("$0 - $59k", "$60k - $149k", ">= $150k")
# run queries -------------------------------------------------------------

con <- aws_connect()
df_raw <- dbFetch(dbSendQuery(con, query))
df_clean <- df_raw %>% 
  filter(tcv > 0)

# combine -----------------------------------------------------------------

df_joined <- data.frame(date) %>% 
  rowwise() %>%
  mutate(joined = list(df_clean %>% filter(date >= start & date <= end))) %>%
  unnest(joined, keep_empty = TRUE) %>% 
  mutate(across(c(date, start, end), as.yearmon))

df_binned <- df_joined %>% 
  mutate(value_bin = cut(tcv, breaks = bins, labels = bin_labels, include.lowest = TRUE, right = TRUE))

df_summary <- df_binned %>% 
  count(date, value_bin)


# plot --------------------------------------------------------------------

(
  p <- df_summary %>% 
    ggplot(aes(x = value_bin, y = n)) +
    geom_col(fill = tsColor("darkpurple")) +
    facet_wrap(~factor(date)) +
    labs(
      title = "Number of Deals in Pipeline by TCV",
      subtitle = "Excludes non sales-qualified opportunities and deals in backburner",
      x = NULL, y = NULL
    )
)

