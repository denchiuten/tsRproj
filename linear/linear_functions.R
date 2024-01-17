get_labels <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
          issueLabels(first : 100) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{id, name}}
          }}
        }}"
    )
  } else {
    query <- str_glue(
      "{{
        issueLabels(first : 100, after: \"{cursor}\") {{
          pageInfo {{endCursor, hasNextPage}} 
          nodes {{id, name}}
        }}
      }}"
    )
  }
  
  response <- POST(
    url = url,
    body = toJSON(list(query = query)),
    add_headers(
      Authorization = key_get("linear"),
      "Content-Type" = "application/json"
    )
  )
  parsed_response <- content(response, as = "text") |> 
    fromJSON(flatten = T)
}