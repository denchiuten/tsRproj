
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
    url = "https://api.linear.app/graphql", 
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

assign_parent <- function(child_id, parent_id, url) {
  
  mutation <- str_glue(
    "
    mutation{{
      issueUpdate(
        id: \"{child_id}\"
        input: {{
          parentId: \"{parent_id}\" 
        }}
        ) {{
        success
      }}
    }}
  ")
  
  response <- POST(
    url = url, 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
}

fetch_projects <- function(url, cursor = NULL) {
  if(is.null(cursor)) {
    query <- str_glue(
      "{{
          projects(first : 100) {{
            pageInfo {{endCursor, hasNextPage}} 
            nodes {{id, name}}
          }}
        }}"
    )
  } else {
    query <- str_glue(
      "{{
        projects(first : 100, after: \"{cursor}\") {{
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
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

update_state <- function(issue_id, state_id, url) {
  
  mutation <- str_glue(
    "mutation{{
      issueUpdate(
        id: \"{issue_id}\"
        input: {{
          stateId: \"{state_id}\"
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


cancel_project <- function(project_id) {
  
  mutation <- str_glue(
    "mutation{{
      projectUpdate(
        id: \"{project_id}\"
        input: {{
          state: \"canceled\" 
        }}
        ) {{success}}
      }}"
  )
  
  response <- POST(
    url = "https://api.linear.app/graphql", 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}

create_label <- function(label_name, parent_id) {
  
  mutation <- str_glue(
    "mutation{{
      issueLabelCreate(
        input: {{
          name: \"{label_name}\"
          parentId: \"{parent_id}\" 
        }}
        ) {{success}}
      }}"
  )
  
  response <- POST(
    url = "https://api.linear.app/graphql", 
    body = toJSON(list(query = mutation)), 
    encode = "json", 
    add_headers(
      Authorization = key_get("linear"), 
      "Content-Type" = "application/json"
    )
  )
  
  return(fromJSON(content(response, as = "text"), flatten = TRUE))
}