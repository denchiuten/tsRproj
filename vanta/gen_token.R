gen_token <- function() {
  
  url <- "https://api.vanta.com/oauth/token"
  
  payload <- RJSONIO::toJSON(
    list(
      client_id = keyring::key_get("vanta", "client_id"),
      client_secret = keyring::key_get("vanta", "client_secret"),
      scope = "vanta-api.all:read vanta-api.all:write",
      grant_type = "client_credentials"
    )
  )
  
  # Set content type header
  headers <- c(
    `Content-Type` = "application/json"
  )
  
  # Send POST request
  response <- httr::POST(
    url = url, 
    body = payload, 
    addHeaders = headers
  )
  
  # Parse the response content and return the access token value
  parsed_response <- httr::content(response)
  return(parsed_response$access_token)
}