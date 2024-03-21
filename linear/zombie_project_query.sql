SELECT
	p.id AS project_id,
	p.name AS project_name,
	p.created_at::DATE AS created_date,
	p.updated_at::DATE AS updated_date,
	lead.name AS lead_name,
	lead.id AS lead_id,
	creator.name AS creator_name,
	creator.id AS creator_id,
	ws.type AS issue_status,
	COUNT(i.id) AS n_issues
FROM linear.project AS p
LEFT JOIN linear.users AS lead
	ON p.lead_id = lead.id
LEFT JOIN linear.users AS creator
	ON p.creator_id = creator.id
	AND creator._fivetran_deleted IS FALSE
LEFT JOIN linear.roadmap_to_project AS rp
	ON p.id = rp.project_id
	-- roadmap IDs for 2024 Features and Implementation Roadmaps, so we can filter out projects on these roadmaps
	AND rp.roadmap_id IN('bcaa52ba-7dc0-4004-9a1f-c493dac497b3', '1e8b8685-6651-4046-a784-2748b351581f')
LEFT JOIN linear.team_project AS tp
	ON p.id = tp.project_id
	AND tp._fivetran_deleted IS FALSE

-- so we can filter out any projects associated with the MAP, aka Future Roadmap, team in Linear
LEFT JOIN linear.team AS t
	ON tp.team_id = t.id
	AND t.key = 'MAP'
LEFT JOIN linear.issue AS i
	ON p.id = i.project_id
	AND i._fivetran_deleted IS FALSE
LEFT JOIN linear.workflow_state AS ws
	ON i.state_id = ws.id
	AND ws._fivetran_deleted IS FALSE
WHERE
	1 = 1
	AND p._fivetran_deleted IS FALSE
	AND p.state NOT IN ('canceled', 'paused', 'completed')
	AND p.target_date IS NULL
	-- exclude any that belong to the roadmap
	AND rp.project_id IS NULL
	AND t.id IS NULL
GROUP BY 1,2,3,4,5,6,7,8,9