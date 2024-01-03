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