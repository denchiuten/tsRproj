-- pull all open Jira issues with non-zero story point values

SELECT
	i.key AS issue_key,
	p.key AS project_key,
	fld.value AS story_points
FROM jra.issue AS i
INNER JOIN jra.status AS s
	ON i.status = s.id
	AND s.status_category_id <> 3 -- exclude Done issues
INNER JOIN jra.project AS p
	ON i.project = p.id
INNER JOIN jra.vw_latest_issue_field_value AS fld
	ON i.id = fld.issue_id
	AND fld.is_active IS TRUE
	AND fld.field_id IN ('customfield_10041', 'customfield_10028', 'customfield_11331')
	AND fld.value IS NOT NULL
	AND fld.value > 0
WHERE
	1 = 1
	AND i._fivetran_deleted IS FALSE
	
