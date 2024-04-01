SELECT
	i.id AS issue_id,
	i.identifier,
	u.email AS creator_email,
	et.team_name,
	l2.id AS label_id
FROM linear.issue AS i
INNER JOIN linear.team AS t
	ON i.team_id = t.id
	AND t.key IN ('OPS', 'LAW', 'CLAW', 'PPL', 'IT', 'DEV')
LEFT JOIN (
	SELECT
		il.*
	FROM linear.issue_label AS il
	INNER JOIN linear.label AS l
		ON il.label_id = l.id
		AND l._fivetran_deleted IS FALSE
		AND l.parent_id = '591c8377-11b9-43ab-9b39-58b3f7e9a36b'
	WHERE
		1 = 1
		AND il._fivetran_deleted IS FALSE
) AS request_team
	ON i.id = request_team.issue_id
INNER JOIN linear.users AS u
	ON i.creator_id = u.id
INNER JOIN bob.employee AS e
	ON lower(u.email) = LOWER(e.email)
INNER JOIN bob.vw_employee_team AS et
	ON e.id = et.employee_id
LEFT JOIN linear.label AS l2
	ON et.team_name || ' Team' = l2.name
	AND l2.parent_id = '591c8377-11b9-43ab-9b39-58b3f7e9a36b'
WHERE
	1 = 1
	AND i._fivetran_deleted IS FALSE
	AND request_team.issue_id IS NULL