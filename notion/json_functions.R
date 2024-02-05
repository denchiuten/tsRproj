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

parse_project <- function(x) {
  # Safely parse the JSON string using RJSONIO
  json_parsed <- tryCatch({
    RJSONIO::fromJSON(x)
  }, error = function(e) {
    print(paste("Error parsing JSON:", x))
    return(NULL)
  })
  
  # Check if the parsed JSON is a list with at least one element
  if (is.list(json_parsed) && length(json_parsed) >= 1) {
    # Extract the 'id' from the first element, if it is a named character vector
    if (is.character(json_parsed[[1]]) && !is.null(names(json_parsed[[1]])) && "id" %in% names(json_parsed[[1]])) {
      return(json_parsed[[1]])
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

parse_name <- function(x) {
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
      return(json_parsed["name"])
    } 
    else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

parse_first_name <- function(x) {
  # Safely parse the JSON string using RJSONIO
  json_parsed <- tryCatch({
    RJSONIO::fromJSON(x)
  }, error = function(e) {
    print(paste("Error parsing JSON:", x))
    return(NULL)
  })
  
  # Check if the parsed JSON is a list with at least one element
  if (is.list(json_parsed) && length(json_parsed) >= 1) {
    # Check if the first element is a list and has a 'name' field
    if (is.list(json_parsed[[1]]) && "name" %in% names(json_parsed[[1]])) {
      return(json_parsed[[1]]$name)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}


parse_first_name_debug <- function(x) {
  # Safely parse the JSON string using RJSONIO
  json_parsed <- tryCatch({
    RJSONIO::fromJSON(x)
  }, error = function(e) {
    print(paste("Error parsing JSON:", x))
    return(NULL)
  })
  
  # Print the parsed JSON to understand its structure
  print(json_parsed)
  
  # Check if the parsed JSON is a list with at least one element
  if (is.list(json_parsed) && length(json_parsed) >= 1) {
    # Attempt to extract the 'name' from the first element
    if (is.list(json_parsed[[1]]) && "name" %in% names(json_parsed[[1]])) {
      return(json_parsed[[1]]$name)
    } else {
      print("First element does not have a 'name' field or is not a list.")
      return(NA)
    }
  } else {
    print("Parsed JSON is not a list or has no elements.")
    return(NA)
  }
}

parse_first_name_corrected <- function(x) {
  # Safely parse the JSON string using RJSONIO
  json_parsed <- tryCatch({
    RJSONIO::fromJSON(x)
  }, error = function(e) {
    print(paste("Error parsing JSON:", x))
    return(NULL)
  })
  
  # Check if the parsed JSON is a list with at least one element and that element is a list
  if (is.list(json_parsed) && length(json_parsed) >= 1) {
    # Check if the first element has a 'name' field directly
    name_field <- json_parsed[[1]]$name
    if (!is.null(name_field)) {
      return(name_field)
    } else {
      print("Name field is missing in the first element.")
      return(NA)
    }
  } else {
    print("Parsed JSON is not a list or has no elements.")
    return(NA)
  }
}

# Revised function for extracting the first 'name' value
parse_first_name_robust <- function(x) {
  # Safely parse the JSON string using RJSONIO
  json_parsed <- tryCatch({
    RJSONIO::fromJSON(x)
  }, error = function(e) {
    print(paste("Error parsing JSON:", x))
    return(NULL)
  })
  
  # Check if the parsed JSON is a list with at least one element
  if (is.list(json_parsed) && length(json_parsed) >= 1) {
    # Access the first element, and then attempt to access the 'name' field
    first_element <- json_parsed[[1]]
    if (is.list(first_element) && !is.null(first_element[["name"]])) {
      return(first_element[["name"]])
    } else {
      print("Name field is missing or the first element is not a list.")
      return(NA)
    }
  } else {
    print("Parsed JSON is not a list or has no elements.")
    return(NA)
  }
}