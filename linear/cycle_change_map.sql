WITH latest_cycle_change AS (
-- identify the most recent row in the issue_history table where an issue's cycle was changed
	SELECT
		issue_id,
		MAX(updated_at) AS last_updated
	FROM linear.issue_history
	WHERE
		1 = 1
		AND _fivetran_deleted IS FALSE
		AND from_cycle_id IS NOT NULL
	GROUP BY 1
),

last_cycle AS (
-- get the cycle id corresponding to the latest update
	SELECT
		h.issue_id,
		h.from_cycle_id	
	FROM linear.issue_history AS h
	INNER JOIN latest_cycle_change AS lcc
		ON h.issue_id = lcc.issue_id
		AND h.updated_at = lcc.last_updated
	WHERE
		1 = 1
		AND h._fivetran_deleted IS FALSE
		AND h.from_cycle_id IS NOT NULL
),

new_cycle AS (
-- find cycle id for the cycles in SELF	
	SELECT
		c.id AS cycle_id,
		c.starts_at,
		c.ends_at
	FROM linear.cycle AS c
	INNER JOIN linear.team AS t
		ON c.team_id = t.id
		AND t.key = 'POPS'
	WHERE
		1 = 1
		AND c._fivetran_deleted IS FALSE
)

SELECT
	i.id,
	i.identifier,
	c.ends_at,
	new_cycle.cycle_id AS new_cycle_id
FROM linear.issue AS i
INNER JOIN last_cycle AS h
	ON i.id = h.issue_id
INNER JOIN linear.team AS t
	ON i.team_id = t.id
	AND t.key = 'POPS'
INNER JOIN linear.cycle AS c
	ON h.from_cycle_id = c.id
	AND c._fivetran_deleted IS FALSE
	AND c.ends_at >= CURRENT_TIMESTAMP
LEFT JOIN new_cycle
	-- cast ends_at to DATE to account for differences in time zone
	ON c.ends_at::DATE = new_cycle.ends_at::DATE
WHERE
	1 = 1
	AND i._fivetran_deleted IS FALSE