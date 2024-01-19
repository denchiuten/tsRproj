
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

add_estimate <- function(issue_id, points, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueUpdate(
        id: \"{issue_id}\"
        input: {{
          estimate: {points}
        }}
        ) {{success}}
      }}"
  )
  
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

assign_assignee <- function(issue_id, user_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueUpdate(
        id: \"{issue_id}\"
        input: {{
          assigneeId: \"{user_id}\" 
        }}
        ) {{success}}
      }}"
  )
  
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

assign_cycle <- function(issue_id, cycle_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueUpdate(
        id: \"{issue_id}\"
        input: {{
          cycleId: \"{cycle_id}\" 
        }}
        ) {{
        success
        }}
      }}"
  )
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

assign_label <- function(issue_id, label_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueAddLabel(
        id: \"{issue_id}\"
        labelId: \"{label_id}\" 
        ) {{
        success
        }}
      }}"
  )
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

mark_dupe <- function(issue_id, duplicate_of_id, url) {
  mutation <- str_glue(
    "mutation {{
        issueRelationCreate(
          input : {{
            issueId: \"{issue_id}\"
            relatedIssueId: \"{duplicate_of_id}\"
            type: duplicate
          }}
        ) {{success}}
      }}
    "
  )
  
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

assign_project <- function(issue_id, project_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueUpdate(
        id: \"{issue_id}\"
        input: {{
          projectId: \"{project_id}\" 
        }}
        ) {{
        success
        }}
      }}"
  )
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}