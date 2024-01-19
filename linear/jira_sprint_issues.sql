SELECT
	i.id AS issue_id,
	i.key AS issue_key,
	p.key AS project_key,
	s.id AS sprint_id,
	s.start_date::DATE AS start_date,
	s.end_date::DATE AS end_date,
	map.linear_cycle_id,
	c.name As cycle_name,
	c.starts_at::DATE AS cycle_start_date,
	c.ends_at::DATE AS cycle_end_date,
	t.name AS team_name,
	t.key AS team_key
FROM jra.issue AS i
INNER JOIN jra.project AS p
	ON i.project = p.id
	AND p.key NOT IN ('CUSIMP')
INNER JOIN jra.vw_latest_issue_multiselect_value AS f
	ON i.id = f.issue_id
	AND f.field_id = 'customfield_10020' -- custom field_id for sprint
INNER JOIN jra.sprint AS s
	ON f.value = s.id
	AND s.state <> 'closed'
	AND s.start_date >= '2023-12-20'
INNER JOIN plumbing.jira_sprint_to_linear_cycle AS map
	ON s.id = map.jira_sprint_id
INNER JOIN linear.cycle AS c
	ON map.linear_cycle_id = c.id
	AND c._fivetran_deleted IS FALSE
INNER JOIN linear.team AS t
	ON c.team_id = t.id
WHERE
	1 = 1
	AND i._fivetran_deleted IS FALSE