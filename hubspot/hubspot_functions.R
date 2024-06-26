merge_companies <- function(primaryObjectId, objectIdToMerge) {
  url <- "https://api.hubapi.com/crm/v3/objects/companies/merge"
  
  body <- list(
    objectIdToMerge = objectIdToMerge,
    primaryObjectId = primaryObjectId
  )
  
  response <- POST(
    url,
    add_headers(
      Authorization = stringr::str_glue("Bearer {keyring::key_get('hubspot', 'jolly-kitchen')}"),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
  
  if (status_code(response) == 200) {
    message(str_glue("Successfully merged company {objectIdToMerge} into company {primaryObjectId}."))
  } else {
    message(str_glue("Failed to merge company {objectIdToMerge} into company {primaryObjectId}. Status code: {status_code(response)}"))
  }
}


archive_company <- function(companyId) {
  url <- stringr::str_glue("https://api.hubapi.com/crm/v3/objects/companies/{companyId}")
  
  body <- list(
    companyId = companyId
  )
  
  response <- DELETE(
    url,
    add_headers(
      Authorization = stringr::str_glue("Bearer {keyring::key_get('hubspot', 'jolly-kitchen')}"),
      `Content-Type` = "application/json"
    )
    # body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
  
  if (status_code(response) == 204) {
    message(str_glue("Successfully archived company {companyId}"))
  } else {
    message(str_glue("Failed to archive company {companyId}. Status code: {status_code(response)}"))
  }
}
